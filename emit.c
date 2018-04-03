#include "parse.h"

#include <stdio.h>
#include <stdlib.h>

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
#define LINKAGE static
#include "hashtable.h"
typedef struct U32Map U32Map;

void strip_labels(CompilerCtx *cctx) {
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



static uint32_t alloc_reg(CompilerCtx *cctx, AstNode *node) {
    if (node->dst_binding) {
        return node->dst_binding->reg;
    }
    return cctx->stack_offset++;
}

static uint32_t maybe_move(CompilerCtx *cctx, uint32_t dst_reg, uint32_t result_reg) {
    if (dst_reg != result_reg) {
        emit_op2(cctx, OP_MOVE, dst_reg, result_reg);
    }
    return dst_reg;
}

uint32_t emit_code(CompilerCtx *cctx, AstNode *node);

static uint32_t emit_bin_op(CompilerCtx *cctx, uint32_t base_op, AstPrimNode *prim) {
    uint32_t dst_reg = alloc_reg(cctx, (AstNode *)prim);
    uint32_t reg0 = emit_code(cctx, prim->arg_nodes[0]);
    uint32_t reg1 = emit_code(cctx, prim->arg_nodes[1]);
    uint32_t op = base_op + (prim->arg_nodes[0]->type->kind - KIND_U8);
    emit_op3(cctx, op, dst_reg, reg0, reg1);
    return dst_reg;
}

#define EMIT_PRIM(UNAME, LNAME, TYPE, FMT) \
    case KIND_ ## UNAME: { \
        uint32_t dst_reg = alloc_reg(cctx, node); \
        emit_lit_ ## LNAME(cctx, dst_reg, lit->form.val.LNAME); \
        return dst_reg; \
    }

uint32_t emit_code(CompilerCtx *cctx, AstNode *node) {
    switch (node->kind) {
    case AST_LITERAL: {
        AstLiteralNode *lit = (AstLiteralNode *)node;
        switch (node->type->kind) {
        case KIND_UNIT: return 0;
        FOR_ALL_PRIM(EMIT_PRIM)
        default: assert(0);
        }
        break;
    }
    case AST_VAR_LOCAL: {
        AstVarNode *var = (AstVarNode *)node;
        uint32_t dst_reg = alloc_reg(cctx, node);
        return maybe_move(cctx, dst_reg, var->binding->reg);
    }
    case AST_LET: {
        AstLetNode *let = (AstLetNode *)node;
        uint32_t dst_reg = alloc_reg(cctx, node);
        uint32_t base_offset = cctx->stack_offset;

        for (uint32_t i = 0; i < let->binding_count; ++i) {
            Binding *binding = let->bindings + i;
            binding->reg = cctx->stack_offset++;
            emit_code(cctx, binding->init_node);
        }

        emit_code(cctx, let->body_node);
        cctx->stack_offset = base_offset;
        return dst_reg;
    }
    case AST_IF: {
        AstIfNode *_if = (AstIfNode *)node;
        uint32_t dst_reg = alloc_reg(cctx, node);
        uint32_t else_label = gen_label(cctx);
        uint32_t end_label = gen_label(cctx);

        uint32_t base_offset = cctx->stack_offset;
        uint32_t cond_reg = emit_code(cctx, _if->cond_node);
        cctx->stack_offset = base_offset;
        emit_jfalse(cctx, cond_reg, else_label);

        uint32_t then_reg = emit_code(cctx, _if->then_node);
        emit_jump(cctx, end_label);
        emit_label(cctx, else_label);
        
        uint32_t else_reg = emit_code(cctx, _if->else_node);
        emit_label(cctx, end_label);

        assert(then_reg == else_reg);
        return dst_reg;
    }
    case AST_PRIM_TAGBODY: {
        AstPrimNode *prim = (AstPrimNode *)node;
        for (uint32_t i = 0; i < prim->arg_count; ++i) {
            AstNode *arg = prim->arg_nodes[i];
            if (arg->kind == AST_LABEL) {
                AstLabelNode *label = (AstLabelNode *)arg;
                emit_label(cctx, label->id);
            } else {
                uint32_t base_offset = cctx->stack_offset;
                emit_code(cctx, arg);
                cctx->stack_offset = base_offset;
            }
        }
        return 0;
    }
    case AST_PRIM_GO: {
        AstPrimNode *prim = (AstPrimNode *)node;
        AstLabelNode *label = (AstLabelNode *)prim->arg_nodes[0];
        emit_jump(cctx, label->id);
        return 0;
    }
    case AST_PRIM_ASSIGN: {
        AstPrimNode *prim = (AstPrimNode *)node;
        AstVarNode *var = (AstVarNode *)prim->arg_nodes[0];
        AstNode *val = prim->arg_nodes[1];
        uint32_t reg = emit_code(cctx, val);
        return maybe_move(cctx, var->binding->reg, reg);
    }
    case AST_PRIM_NOT: {
        AstPrimNode *prim = (AstPrimNode *)node;
        uint32_t dst_reg = alloc_reg(cctx, prim->arg_nodes[0]);
        uint32_t reg = emit_code(cctx, prim->arg_nodes[0]);
        emit_op2(cctx, OP_NOT_BOOL, dst_reg, reg);
        return dst_reg;
    }
    case AST_PRIM_INC: {
        AstPrimNode *prim = (AstPrimNode *)node;
        AstVarNode *var = (AstVarNode *)prim->arg_nodes[0];
        emit_op1(cctx, OP_INC_U8 + (var->n.type->kind - KIND_U8), var->binding->reg);
        return var->binding->reg;
    }
    case AST_PRIM_DEC: {
        AstPrimNode *prim = (AstPrimNode *)node;
        AstVarNode *var = (AstVarNode *)prim->arg_nodes[0];
        emit_op1(cctx, OP_DEC_U8 + (var->n.type->kind - KIND_U8), var->binding->reg);
        return var->binding->reg;
    }
    case AST_PRIM_ADD: return emit_bin_op(cctx, OP_ADD_U8, (AstPrimNode *)node);
    case AST_PRIM_SUB: return emit_bin_op(cctx, OP_SUB_U8, (AstPrimNode *)node);
    case AST_PRIM_MUL: return emit_bin_op(cctx, OP_MUL_U8, (AstPrimNode *)node);
    case AST_PRIM_DIV: return emit_bin_op(cctx, OP_DIV_U8, (AstPrimNode *)node);
    case AST_PRIM_MOD: return emit_bin_op(cctx, OP_MOD_U8, (AstPrimNode *)node);
    case AST_PRIM_EQ: return emit_bin_op(cctx, OP_EQ_U8, (AstPrimNode *)node);
    case AST_PRIM_LT: return emit_bin_op(cctx, OP_LT_U8, (AstPrimNode *)node);
    case AST_PRIM_GT: return emit_bin_op(cctx, OP_GT_U8, (AstPrimNode *)node);
    case AST_PRIM_LTEQ: return emit_bin_op(cctx, OP_LTEQ_U8, (AstPrimNode *)node);
    case AST_PRIM_GTEQ: return emit_bin_op(cctx, OP_GTEQ_U8, (AstPrimNode *)node);
    case AST_PRIM_PRINT: {
        AstPrimNode *prim = (AstPrimNode *)node;
        uint32_t base_offset = cctx->stack_offset;
        uint32_t reg = emit_code(cctx, prim->arg_nodes[0]);
        cctx->stack_offset = base_offset;
        uint32_t op = OP_PRINT_BOOL + (prim->arg_nodes[0]->type->kind - KIND_BOOL);
        emit_op1(cctx, op, reg);
        return 0;
    }
    default: assert(0);
    }
}

void emit_code_block(CompilerCtx *cctx, AstNode *node) {
    emit_code(cctx, node);
    emit_ret(cctx);
}