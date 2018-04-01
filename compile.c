#include "compile.h"
#include "instr.h"
#include "any.h"

#include <stdio.h>
#include <stdlib.h>

#define EXPAND_IMPLEMENTATION
#define NAME LabelMap
#define KEY_TYPE const Symbol *
#define VALUE_TYPE uint32_t
#define HASH_FUNC(x) ((x)->hash)
#define EQUAL_FUNC(x, y) ((x) == (y))
#include "hashtable.h"

#define EXPAND_IMPLEMENTATION
#define NAME BindingMap
#define KEY_TYPE const Symbol *
#define VALUE_TYPE Binding *
#define HASH_FUNC(x) ((x)->hash)
#define EQUAL_FUNC(x, y) ((x) == (y))
#include "hashtable.h"


#define CHECK_4_BITS(offset) \
    if (!(word & ((uint64_t)1 << (offset + 0)))) { return offset + 0; } \
    if (!(word & ((uint64_t)1 << (offset + 1)))) { return offset + 1; } \
    if (!(word & ((uint64_t)1 << (offset + 2)))) { return offset + 2; } \
    return offset + 3;
#define CHECK_8_BITS(offset) \
    if ((word & ((uint64_t)0xf << (offset))) != ((uint64_t)0xf << (offset))) { CHECK_4_BITS(offset) } \
    else { CHECK_4_BITS(offset + 4) }
#define CHECK_16_BITS(offset) \
    if ((word & ((uint64_t)0xff << (offset))) != ((uint64_t)0xff << (offset))) { CHECK_8_BITS(offset) } \
    else { CHECK_8_BITS(offset + 8) }
#define CHECK_32_BITS(offset) \
    if ((word & ((uint64_t)0xffff << (offset))) != ((uint64_t)0xffff << (offset))) { CHECK_16_BITS(offset) } \
    else { CHECK_16_BITS(offset + 16) }
static int32_t first_unset_bit_index(uint64_t word) {
    if (word == 0xffffffffffffffff) { return -1;  }
    if ((word & 0xffffffff) != 0xffffffff) { CHECK_32_BITS(0) }
    else { CHECK_32_BITS(32) }
}

static void reserve_register(ReservedRegs *regs, uint32_t reg) {
    uint32_t w = reg / 64;
    if (w >= regs->words_used) {
        if (!regs->words_capacity) {
            regs->words_capacity = 16;
        }
        while (w >= regs->words_capacity) {
            regs->words_capacity *= 2;
        }
        regs->words = realloc(regs->words, sizeof(uint64_t) * regs->words_capacity);
        while (w >= regs->words_used) {
            regs->words[regs->words_used++] = 0;
        }
    }
    uint32_t b = reg % 64;
    regs->words[w] |= (uint64_t)1 << b;
}

static void release_register(ReservedRegs *regs, uint32_t reg) {
    uint32_t w = reg / 64;
    if (w < regs->words_used) {
        uint32_t b = reg % 64;
        regs->words[w] &= ~((uint64_t)1 << b);
    }
}

static uint32_t alloc_register(ReservedRegs *regs) {
    for (uint32_t i = 0; i < regs->words_used; ++i) {
        int32_t index = first_unset_bit_index(regs->words[i]);
        if (index >= 0) {
            regs->words[i] |= (uint64_t)1 << index;
            return i * 64 + index;
        }
    }
    if (regs->words_used == regs->words_capacity) {
        regs->words_capacity = regs->words_capacity ? regs->words_capacity * 2 : 16;
        regs->words = realloc(regs->words, sizeof(uint64_t) * regs->words_capacity);
    }
    regs->words[regs->words_used] = 1;
    return regs->words_used++ * 64;
}

static void push_label(CompilerCtx *cctx, const Symbol *sym, uint32_t label) {
    if (cctx->label_stack_used == cctx->label_stack_capacity) {
        cctx->label_stack_capacity = cctx->label_stack_capacity ? cctx->label_stack_capacity * 2 : 16;
        cctx->label_stack = realloc(cctx->label_stack, cctx->label_stack_capacity * sizeof(PrevLabel));
    }
    uint32_t prev = 0;
    LabelMap_get(&cctx->label_map, sym, &prev);
    LabelMap_put(&cctx->label_map, sym, label);
    cctx->label_stack[cctx->label_stack_used].symbol = sym;
    cctx->label_stack[cctx->label_stack_used].label = prev;
    ++cctx->label_stack_used;
}

static void pop_labels(CompilerCtx *cctx, uint32_t count) {
    for (uint32_t i = 0; i < count; ++i) {
        PrevLabel prev = cctx->label_stack[--cctx->label_stack_used];
        if (prev.label) {
            LabelMap_put(&cctx->label_map, prev.symbol, prev.label);
        }
        else {
            LabelMap_remove(&cctx->label_map, prev.symbol);
        }
    }
}

static uint32_t lookup_label(CompilerCtx *cctx, const Symbol *sym) {
    uint32_t label;
    if (!LabelMap_get(&cctx->label_map, sym, &label)) {
        printf("label not found: %s\n", sym->name->data);
        assert(0 && "bad label");
    }
    return label;
}

static void push_binding(CompilerCtx *cctx, Binding *binding) {
    if (cctx->binding_stack_used == cctx->binding_stack_capacity) {
        cctx->binding_stack_capacity = cctx->binding_stack_capacity ? cctx->binding_stack_capacity * 2 : 16;
        cctx->binding_stack = realloc(cctx->binding_stack, cctx->binding_stack_capacity * sizeof(PrevBinding));
    }
    Binding *prev = NULL;
    BindingMap_get(&cctx->binding_map, binding->symbol, &prev);
    BindingMap_put(&cctx->binding_map, binding->symbol, binding);
    cctx->binding_stack[cctx->binding_stack_used].symbol = binding->symbol;
    cctx->binding_stack[cctx->binding_stack_used].binding = prev;
    ++cctx->binding_stack_used;
}

static void pop_bindings(CompilerCtx *cctx, uint32_t count) {
    assert(cctx->binding_stack_used >= count);
    for (uint32_t i = 0; i < count; ++i) {
        PrevBinding prev = cctx->binding_stack[--cctx->binding_stack_used];

        Binding *curr = NULL;
        BindingMap_get(&cctx->binding_map, prev.symbol, &curr);
        assert(curr);

        if (prev.binding) {
            BindingMap_put(&cctx->binding_map, prev.symbol, prev.binding);
        }
        else {
            BindingMap_remove(&cctx->binding_map, prev.symbol);
        }

        release_register(&cctx->regs, curr->reg);
        free(curr);
    }
}

static uint32_t gen_label(CompilerCtx *cctx) {
    return ++cctx->label_counter;
}

static void emit(CompilerCtx *cctx, uint64_t instr) {
    if (cctx->code_used == cctx->code_capacity) {
        cctx->code_capacity = cctx->code_capacity ? cctx->code_capacity * 2 : 128;
        cctx->code = realloc(cctx->code, cctx->code_capacity * sizeof(uint64_t));
    }
    cctx->code[cctx->code_used++] = instr;
}

static void emit_op1(CompilerCtx *cctx, uint32_t op, uint32_t a) {
    emit(cctx, MK_INSTR_A(op, a));
}

static void emit_op2(CompilerCtx *cctx, uint32_t op, uint32_t a, uint32_t b) {
    emit(cctx, MK_INSTR_A_B(op, a, b));
}

static void emit_op3(CompilerCtx *cctx, uint32_t op, uint32_t a, uint32_t b, uint32_t c) {
    emit(cctx, MK_INSTR_A_B_C(op, a, b, c));
}

static void emit_label(CompilerCtx *cctx, uint32_t label) {
    emit(cctx, MK_INSTR_BC(OP_LABEL, label));
}

static void emit_jump(CompilerCtx *cctx, uint32_t label) {
    emit(cctx, MK_INSTR_BC(OP_JUMP_LABEL, label));

}

static void emit_jfalse(CompilerCtx *cctx, uint32_t cond_reg, uint32_t label) {
    emit(cctx, MK_INSTR_A_BC(OP_JFALSE_LABEL, cond_reg, label));
}

static void emit_jtrue(CompilerCtx *cctx, uint32_t cond_reg, uint32_t label) {
    emit(cctx, MK_INSTR_A_BC(OP_JTRUE_LABEL, cond_reg, label));
}

static void emit_call(CompilerCtx *cctx, uint32_t callable_reg, uint32_t result_reg) {
    emit_op2(cctx, OP_CALL, callable_reg, result_reg);
}

static void emit_ret(CompilerCtx *cctx) {
    emit(cctx, MK_INSTR(OP_RET));
}

#define DEFINE_LIT_EMITTER_32(UNAME, LNAME, TYPE, FMT) \
static void emit_lit_ ## LNAME(CompilerCtx *cctx, uint32_t reg, TYPE val) { emit(cctx, MK_INSTR_A_BC(OP_LIT_ ## UNAME, reg, val)); }

#define DEFINE_LIT_EMITTER_64(UNAME, LNAME, TYPE, FMT) \
static void emit_lit_ ## LNAME(CompilerCtx *cctx, uint32_t reg, TYPE val) { emit(cctx, MK_INSTR_A(OP_LIT_ ## UNAME, reg)); emit(cctx, *(uint64_t *)&val); }

static void emit_lit_f32(CompilerCtx *cctx, uint32_t reg, float val) { emit(cctx, MK_INSTR_A_BC(OP_LIT_F32, reg, *(uint32_t *)&val)); }

FOR_ALL_PRIM_INT_32(DEFINE_LIT_EMITTER_32)
FOR_ALL_PRIM_64(DEFINE_LIT_EMITTER_64)



static uint32_t instr_word_count(uint64_t instr) {
    switch (INSTR_OP(instr)) {
    case OP_LIT_U64:
    case OP_LIT_I64:
    case OP_LIT_F64:
    case OP_LIT_PTR:
        return 2;
    default:
        return 1;
    }
}

#define EXPAND_INTERFACE
#define EXPAND_IMPLEMENTATION
#define NAME U32Map
#define KEY_TYPE uint32_t
#define VALUE_TYPE uint32_t
#define HASH_FUNC(x) hashutil_uint32_mix(x)
#define EQUAL_FUNC(x, y) ((x) == (y))
#include "hashtable.h"
typedef struct U32Map U32Map;

static void strip_labels(CompilerCtx *cctx) {
    U32Map label_offsets;
    U32Map_init(&label_offsets, 128);

    /* map from labels directly preceding jumps, to the target labels of those jumps */
    U32Map replace_jump_labels;
    U32Map_init(&replace_jump_labels, 128);

    uint32_t in = 0;
    uint32_t out = 0;
    uint64_t *code = cctx->code;

    while (in < cctx->code_used) {
        switch (INSTR_OP(code[in])) {
        case OP_NOP:
            ++in;
            break;
        case OP_LABEL: {
            uint32_t label = INSTR_BC(code[in]);
            U32Map_put(&label_offsets, label, out);
            if (in < cctx->code_used - 1 && INSTR_OP(code[in+1]) == OP_JUMP_LABEL) {
                U32Map_put(&replace_jump_labels, label, INSTR_BC(code[in+1]));
                printf("store label replacement: %u -> %u\n", label, INSTR_BC(code[in+1]));
            }
            ++in;
            break;
        }
        case OP_JUMP_LABEL:
            if (out > 0 && INSTR_OP(code[out-1]) == OP_JUMP_LABEL) {
                /* skip the jump, since nobody will jump here, and no control flow
                will get here naturally since it is precededy by a jump */
                ++in;
            } else {
                code[out++] = code[in++];
            }
            break;
        default:
            switch (instr_word_count(code[in])) {
            case 1: code[out++] = code[in++]; break;
            case 2: code[out++] = code[in++]; code[out++] = code[in++]; break;
            default: assert(0 && "bad instruction size");
            }
            break;
        }
    }

    cctx->code_used = out;

    uint32_t i = 0;
    while (i < cctx->code_used) {
        uint64_t instr = code[i];
        uint32_t new_op;

        switch (INSTR_OP(instr)) {
        case OP_JUMP_LABEL: new_op = OP_JUMP; break;
        case OP_JFALSE_LABEL: new_op = OP_JFALSE; break;
        case OP_JTRUE_LABEL: new_op = OP_JTRUE; break;
        default: i += instr_word_count(instr); continue;
        }

        uint32_t label = INSTR_BC(instr);
        while (U32Map_get(&replace_jump_labels, label, &label)) {}

        uint32_t abs_offset;
        if (!U32Map_get(&label_offsets, label, &abs_offset)) {
            assert(0 && "label unexpectedly not found");
        }
        int32_t rel_offset = (int32_t)abs_offset - i;
        assert(rel_offset);
        code[i++] = MK_INSTR_A_BC(new_op, INSTR_A(instr), rel_offset);
    }

    U32Map_free(&label_offsets);
    U32Map_free(&replace_jump_labels);
}


#define ANY_REG UINT32_MAX

typedef struct CompileResult CompileResult;
struct CompileResult {
    const Type *type;
    uint32_t reg;
    bool reg_owned;
};

const CompileResult compile(CompilerCtx *cctx, Any form, uint32_t dst_reg_hint);

static CompileResult compile_bin_op(CompilerCtx *cctx, Any form, uint32_t dst_reg_hint, uint32_t base_op, uint32_t max_kind) {
    Any temp = cdr(form);
    Any a = car(temp);
    temp = cdr(temp);
    Any b = car(temp);

    bool reg_owned = false;
    uint32_t dst_reg = dst_reg_hint;
    if (dst_reg == ANY_REG) {
        dst_reg = alloc_register(&cctx->regs);
        reg_owned = true;
    }

    CompileResult a_result = compile(cctx, a, dst_reg);
    CompileResult b_result = compile(cctx, b, ANY_REG);

    assert(a_result.type == b_result.type);
    assert(a_result.type->kind >= KIND_U8 && a_result.type->kind <= max_kind);

    emit_op3(cctx, base_op + (a_result.type->kind - KIND_U8), dst_reg, a_result.reg, b_result.reg);

    if (a_result.reg_owned) {
        assert(a_result.reg != dst_reg);
        release_register(&cctx->regs, a_result.reg);
    }
    if (b_result.reg_owned) {
        release_register(&cctx->regs, b_result.reg);
    }
    return (CompileResult) { a_result.type, dst_reg, reg_owned };
}

const CompileResult compile(CompilerCtx *cctx, Any form, uint32_t dst_reg_hint) {
    const Type *form_type = ANY_TYPE(form);

    if (form_type == type_ptr_symbol) {
        Binding *binding;
        if (!BindingMap_get(&cctx->binding_map, form.val.symbol_ptr, &binding)) {
            assert(0 && "not found");
        }
        return (CompileResult) { binding->type, binding->reg };
    }

    if (form_type == type_ref_cons) {
        Any head = car(form);

        if (ANY_TYPE(head) == type_ptr_symbol) {
            if (head.val.symbol_ptr == symbol_fun) {
                Any temp = cdr(form);
                Any params_form = car(temp);
                temp = cdr(temp);
                Any body_form = car(temp);

                uint32_t param_count = list_length(params_form);
            }

            if (head.val.symbol_ptr == symbol_tagbody) {
                uint32_t label_count = 0;

                Any temp = cdr(form);
                for (; ANY_KIND(temp) != KIND_UNIT; temp = cdr(temp)) {
                    Any stmt_form = car(temp);
                    if (ANY_TYPE(stmt_form) == type_ptr_symbol) {
                        uint32_t label = gen_label(cctx);
                        push_label(cctx, stmt_form.val.symbol_ptr, label);
                        ++label_count;
                    }
                }

                temp = cdr(form);
                for (; ANY_KIND(temp) != KIND_UNIT; temp = cdr(temp)) {
                    Any stmt_form = car(temp);
                    if (ANY_TYPE(stmt_form) == type_ptr_symbol) {
                        uint32_t label = lookup_label(cctx, stmt_form.val.symbol_ptr);
                        emit_label(cctx, label);
                    }
                    else {
                        CompileResult result = compile(cctx, stmt_form, ANY_REG);
                        if (result.reg_owned) {
                            release_register(&cctx->regs, result.reg);
                        }
                    }
                }

                pop_labels(cctx, label_count);
                return (CompileResult) { type_unit, ANY_REG };
            }

            if (head.val.symbol_ptr == symbol_go) {
                Any temp = cdr(form);
                Any label_sym = car(temp);
                temp = cdr(temp);
                assert(ANY_KIND(temp) == KIND_UNIT);
                assert(ANY_TYPE(label_sym) == type_ptr_symbol);
                uint32_t label = lookup_label(cctx, label_sym.val.symbol_ptr);
                emit_jump(cctx, label);
                return (CompileResult) { type_unit, ANY_REG };
            }

            if (head.val.symbol_ptr == symbol_let) {
                Any temp = cdr(form);
                Any binds = car(temp);
                temp = cdr(temp);
                Any body = car(temp);

                uint32_t binds_length = list_length(binds);
                assert((binds_length % 2) == 0);
                uint32_t bind_count = binds_length / 2;

                temp = binds;
                for (uint32_t i = 0; i < bind_count; ++i) {
                    Any sym_form = car(temp);
                    temp = cdr(temp);
                    Any init_form = car(temp);
                    temp = cdr(temp);
                    assert(ANY_TYPE(sym_form) == type_ptr_symbol);

                    uint32_t binding_reg = alloc_register(&cctx->regs);
                    CompileResult init_result = compile(cctx, init_form, binding_reg);
                    if (init_result.reg != binding_reg) {
                        emit_op2(cctx, OP_MOVE, binding_reg, init_result.reg);
                        if (init_result.reg_owned) {
                            release_register(&cctx->regs, init_result.reg);
                        }
                    }
                    assert(init_result.reg == binding_reg);

                    Binding *binding = calloc(1, sizeof(Binding));
                    binding->reg = binding_reg;
                    binding->symbol = sym_form.val.symbol_ptr;
                    binding->type = init_result.type;
                    push_binding(cctx, binding);
                }

                CompileResult body_result = compile(cctx, body, dst_reg_hint);

                pop_bindings(cctx, bind_count);
                return body_result;
            }

            if (head.val.symbol_ptr == symbol_if) {
                Any temp = cdr(form);
                Any cond_form = car(temp);
                temp = cdr(temp);
                Any then_form = car(temp);
                temp = cdr(temp);
                Any else_form;
                if (ANY_KIND(temp) != KIND_UNIT) {
                    else_form = car(temp);
                    temp = cdr(temp);
                    assert(ANY_KIND(temp) == KIND_UNIT);
                } else {
                    else_form = ANY_UNIT;
                }

                uint32_t else_label = gen_label(cctx);
                uint32_t end_label = gen_label(cctx);

                CompileResult cond_result = compile(cctx, cond_form, ANY_REG);
                emit_jfalse(cctx, cond_result.reg, else_label);
                if (cond_result.reg_owned) {
                    release_register(&cctx->regs, cond_result.reg);
                }
                
                CompileResult then_result = compile(cctx, then_form, dst_reg_hint);
                emit_jump(cctx, end_label);
                emit_label(cctx, else_label);
                if (then_result.reg_owned) {
                    release_register(&cctx->regs, then_result.reg); /* temporarily release */
                }

                CompileResult else_result = compile(cctx, else_form, then_result.reg);
                assert(then_result.type == else_result.type);
                if (then_result.reg != else_result.reg) {
                    reserve_register(&cctx->regs, then_result.reg);
                    emit_op2(cctx, OP_MOVE, then_result.reg, else_result.reg);
                    if (else_result.reg_owned) {
                        release_register(&cctx->regs, else_result.reg);
                    }
                }
                emit_label(cctx, end_label);

                return then_result;
            }

            if (head.val.symbol_ptr == symbol_assign) {
                Any temp = cdr(form);
                Any sym_form = car(temp);
                temp = cdr(temp);
                Any val_form = car(temp);
                temp = cdr(temp);
                assert(ANY_KIND(temp) == KIND_UNIT);
                assert(ANY_TYPE(sym_form) == type_ptr_symbol);

                Binding *binding;
                if (!BindingMap_get(&cctx->binding_map, sym_form.val.symbol_ptr, &binding)) {
                    assert(0 && "not found");
                }

                CompileResult val_result = compile(cctx, val_form, binding->reg);
                if (val_result.reg != binding->reg) {
                    emit_op2(cctx, OP_MOVE, binding->reg, val_result.reg);
                    if (val_result.reg_owned) {
                        release_register(&cctx->regs, val_result.reg);
                    }
                }
                return val_result;
            }

            if (head.val.symbol_ptr == symbol_plus) { return compile_bin_op(cctx, form, dst_reg_hint, OP_ADD_U8, KIND_F64); }
            if (head.val.symbol_ptr == symbol_minus) { return compile_bin_op(cctx, form, dst_reg_hint, OP_SUB_U8, KIND_F64); }
            if (head.val.symbol_ptr == symbol_mul) { return compile_bin_op(cctx, form, dst_reg_hint, OP_MUL_U8, KIND_F64); }
            if (head.val.symbol_ptr == symbol_div) { return compile_bin_op(cctx, form, dst_reg_hint, OP_DIV_U8, KIND_F64); }
            if (head.val.symbol_ptr == symbol_mod) { return compile_bin_op(cctx, form, dst_reg_hint, OP_MOD_U8, KIND_I64); }
            if (head.val.symbol_ptr == symbol_eq) { return compile_bin_op(cctx, form, dst_reg_hint, OP_EQ_BOOL, KIND_F64); }
            if (head.val.symbol_ptr == symbol_lt) { return compile_bin_op(cctx, form, dst_reg_hint, OP_LT_U8, KIND_F64); }
            if (head.val.symbol_ptr == symbol_gt) { return compile_bin_op(cctx, form, dst_reg_hint, OP_GT_U8, KIND_F64); }
            if (head.val.symbol_ptr == symbol_lteq) { return compile_bin_op(cctx, form, dst_reg_hint, OP_LTEQ_U8, KIND_F64); }
            if (head.val.symbol_ptr == symbol_gteq) { return compile_bin_op(cctx, form, dst_reg_hint, OP_GTEQ_U8, KIND_F64); }

            if (head.val.symbol_ptr == symbol_print) {
                Any temp = cdr(form);
                Any val_form = car(temp);

                CompileResult val_result = compile(cctx, val_form, ANY_REG);

                assert(val_result.type->kind >= KIND_BOOL && val_result.type->kind <= KIND_PTR);
                emit_op1(cctx, OP_PRINT_BOOL + (val_result.type->kind - KIND_BOOL), val_result.reg);

                if (val_result.reg_owned) {
                    release_register(&cctx->regs, val_result.reg);
                }

                return (CompileResult) { type_unit, ANY_REG };
            }
        }

        /* call */

        /*uint32_t form_count = list_length(form);
        uint32_t base_reg = cctx->stack_offset;
        cctx->stack_offset += form_count;
        
        uint32_t callable_reg = base_reg;
        const Type *callable_type = compile(cctx, head, callable_reg);

        Any temp = cdr(form);
        for (uint32_t i = 1; i < form_count; ++i) {
            Any arg_form = car(temp);
            temp = cdr(temp);

            uint32_t arg_reg = base_reg + i;
            const Type *arg_type = compile(cctx, arg_form, arg_reg);
        }

        emit_call(cctx, callable_reg, dst_reg_hint);
        cctx->stack_offset -= form_count;*/
    }

#define EMIT_PRIM(UNAME, LNAME, TYPE, FMT) \
    case KIND_ ## UNAME: { \
        bool reg_owned = false; \
        uint32_t dst_reg = dst_reg_hint; \
        if (dst_reg == ANY_REG) { \
            dst_reg = alloc_register(&cctx->regs); \
            reg_owned = true; \
        } \
        emit_lit_ ## LNAME(cctx, dst_reg, form.val.LNAME); \
        return (CompileResult) { form_type, dst_reg, reg_owned }; \
    }

    switch (form_type->kind) {
    case KIND_UNIT: return (CompileResult) { type_unit, ANY_REG };
    FOR_ALL_PRIM(EMIT_PRIM)
    }

    assert(0 && "bad form");
}

CodeBlock compile_block(Any form) {
    CompilerCtx cctx = {0};
    LabelMap_init(&cctx.label_map, 32);
    BindingMap_init(&cctx.binding_map, 32);

    compile(&cctx, form, ANY_REG);
    emit_ret(&cctx);
    
    printf("\nbefore strip:\n");
    print_code(cctx.code, cctx.code_used);

    strip_labels(&cctx);

    printf("\nafter strip:\n");
    print_code(cctx.code, cctx.code_used);

    return (CodeBlock) { cctx.code, cctx.code_used };
}

void init_compile(void) {
    ReservedRegs regs = { 0 };
    assert(alloc_register(&regs) == 0);
    assert(alloc_register(&regs) == 1);
    reserve_register(&regs, 2);
    assert(alloc_register(&regs) == 3);
    release_register(&regs, 2);
    assert(alloc_register(&regs) == 2);
    for (uint32_t i = 4; i < 100; ++i) {
        assert(alloc_register(&regs) == i);
    }
    reserve_register(&regs, 1000);
    for (uint32_t i = 100; i < 1000; ++i) {
        assert(alloc_register(&regs) == i);
    }
    assert(alloc_register(&regs) == 1001);
    release_register(&regs, 1000);
    assert(alloc_register(&regs) == 1000);
}