#include "parse.h"
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


static const Type *parse_type(Any form) {
    if (symbolp(form)) {
        const Symbol *sym = to_symbol(form);
        if (sym == symbol_bool) { return type_b32; }
        if (sym == symbol_u8) { return type_u8; }
        if (sym == symbol_u16) { return type_u16; }
        if (sym == symbol_u32) { return type_u32; }
        if (sym == symbol_u64) { return type_u64; }
        if (sym == symbol_i8) { return type_i8; }
        if (sym == symbol_i16) { return type_i16; }
        if (sym == symbol_i32) { return type_i32; }
        if (sym == symbol_i64) { return type_i64; }
        if (sym == symbol_f32) { return type_f32; }
        if (sym == symbol_f64) { return type_f64; }
    }
    if (consp(form)) {
        Any head = car(form);
        if (symbolp(head)) {
            const Symbol *sym = to_symbol(head);
            if (sym == symbol_array) {
                form = cdr(form);
                const Type *elem_type = parse_type(car(form));
                form = cdr(form);
                if (nullp(form)) {
                    return intern_array_type(elem_type);
                }
                form = cdr(form);
                uint32_t length = to_u32(car(form));
                assert(nullp(cdr(form)));
                return intern_array_type_sized(elem_type, length);
            }
            if (sym == symbol_fun) {
                form = cdr(form);
                Any params_form = car(form);
                form = cdr(form);
                const Type *ret_type = parse_type(car(form));
                assert(nullp(cdr(form)));
                uint32_t param_count = list_length(params_form);
                FunParam *params = calloc(1, sizeof(FunParam) * param_count);
                for (uint32_t i = 0; i < param_count; ++i) {
                    params[i].type = parse_type(car(params_form));
                    params_form = cdr(params_form);
                }
                const Type *result = intern_fun_type(ret_type, param_count, params);
                free(params);
                return result;
            }
        }
    }
    assert(0 && "can't parse type");
}

uint32_t gen_label(CompilerCtx *cctx) {
    return ++cctx->label_counter;
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

static void push_binding(GlobalEnv *global_env, Binding *binding) {
    if (global_env->savestack_used == global_env->savestack_capacity) {
        global_env->savestack_capacity = global_env->savestack_capacity ? global_env->savestack_capacity * 2 : 16;
        global_env->savestack = realloc(global_env->savestack, global_env->savestack_capacity * sizeof(PrevBinding));
    }
    Binding *prev = NULL;
    BindingMap_get(&global_env->map, binding->symbol, &prev);
    BindingMap_put(&global_env->map, binding->symbol, binding);
    global_env->savestack[global_env->savestack_used].symbol = binding->symbol;
    global_env->savestack[global_env->savestack_used].binding = prev;
    ++global_env->savestack_used;
}

static void pop_bindings(GlobalEnv *global_env, uint32_t count) {
    assert(global_env->savestack_used >= count);
    for (uint32_t i = 0; i < count; ++i) {
        PrevBinding prev = global_env->savestack[--global_env->savestack_used];

        Binding *curr = NULL;
        BindingMap_get(&global_env->map, prev.symbol, &curr);
        assert(curr);

        if (prev.binding) {
            BindingMap_put(&global_env->map, prev.symbol, prev.binding);
        }
        else {
            BindingMap_remove(&global_env->map, prev.symbol);
        }
    }
}

static Binding *lookup_binding(GlobalEnv *global_env, const Symbol *name) {
    Binding *b = NULL;
    BindingMap_get(&global_env->map, name, &b);
    return b;
}


static void *create_node(CompilerCtx *cctx, AstNodeKind kind, uint32_t size) {
    AstNode *node = calloc(1, size);
    node->kind = kind;
    return node;
}

static AstLiteralNode *create_literal(CompilerCtx *cctx, Any form, Binding *dst_binding) {
    AstLiteralNode *node = create_node(cctx, AST_LITERAL, sizeof(AstLiteralNode));
    node->n.type = ANY_TYPE(form);
    node->n.dst_binding = dst_binding;
    node->form = form;
    return node;
}

static AstPrimNode *alloc_prim_node(CompilerCtx *cctx, AstNodeKind kind, Any args) {
    uint32_t arg_count = list_length(args);
    AstPrimNode *node = create_node(cctx, kind, sizeof(AstPrimNode) + sizeof(AstNode *) * arg_count);
    node->arg_count = arg_count;
    return node;
}

static AstPrimNode *parse_prim(CompilerCtx *cctx, AstNodeKind kind, Any args, Binding *dst_binding) {
    AstPrimNode *node = alloc_prim_node(cctx, kind, args);

    assert(node->arg_count <= 32);
    uint32_t store_counts[32];

    for (uint32_t i = 0; i < node->arg_count; ++i) {
        node->arg_nodes[i] = parse_form(cctx, car(args), i == 0 ? dst_binding : NULL);
        Binding *dst = node->arg_nodes[i]->dst_binding;
        if (dst) {
            store_counts[i] = dst->store_count;
        }
        args = cdr(args);
    }

    for (uint32_t i = 0; i < node->arg_count; ++i) {
        Binding *dst = node->arg_nodes[i]->dst_binding;
        if (dst && store_counts[i] != dst->store_count) {
            /* binding returned from this form was modified in a later form, so it must be copied */
            node->arg_nodes[i]->dst_binding = NULL;
        }
    }

    if (node->arg_nodes[0]->dst_binding == dst_binding) {
        node->n.dst_binding = dst_binding;
    }
    return node;
}

static AstPrimNode *parse_arithmetic(CompilerCtx *cctx, AstNodeKind kind, Any args, Binding *dst_binding) {
    AstPrimNode *node = parse_prim(cctx, kind, args, dst_binding);
    assert(node->arg_count == 2);
    for (uint32_t i = 1; i < node->arg_count; ++i) {
        assert(node->arg_nodes[i]->type == node->arg_nodes[0]->type);
    }
    const Type *type = node->arg_nodes[0]->type;
    assert(type->kind >= KIND_U8 && type->kind <= (kind == AST_PRIM_MOD ? KIND_I64 : KIND_F64));
    node->n.type = type;
    return node;
}

static AstPrimNode *parse_compare(CompilerCtx *cctx, AstNodeKind kind, Any args, Binding *dst_binding) {
    AstPrimNode *node = parse_prim(cctx, kind, args, dst_binding);
    assert(node->arg_count == 2);
    for (uint32_t i = 1; i < node->arg_count; ++i) {
        assert(node->arg_nodes[i]->type == node->arg_nodes[0]->type);
    }
    const Type *type = node->arg_nodes[0]->type;
    assert(type->kind >= KIND_BOOL && type->kind <= KIND_F64);
    node->n.type = type_b32;
    return node;
}

static AstLabelNode *create_label(CompilerCtx *cctx, const Symbol *name, uint32_t label_id) {
    AstLabelNode *node = create_node(cctx, AST_LABEL, sizeof(AstLabelNode));
    node->name = name;
    node->id = label_id;
    node->n.type = type_unit; /* just assign a type, but this will not be used */
    return node;
}

static AstPrimNode *parse_inc_dec(CompilerCtx *cctx, AstNodeKind kind, Any args) {
    AstPrimNode *node = alloc_prim_node(cctx, kind, args);
    assert(node->arg_count == 1);
    node->arg_nodes[0] = parse_form(cctx, car(args), NULL);
    AstVarNode *var = (AstVarNode *)node->arg_nodes[0];
    assert(var->n.kind == AST_VAR_LOCAL);
    assert(IS_INTEGRAL_KIND(var->n.type->kind));
    ++var->binding->store_count;
    node->n.type = node->arg_nodes[0]->type;
    node->n.dst_binding = var->binding;
    assert(node->n.type == node->n.dst_binding->type);
    return node;
}

static AstVarNode *create_var(CompilerCtx *cctx, Binding *binding) {
    assert(binding->type);
    AstVarNode *node = (AstVarNode *)create_node(cctx, AST_VAR_LOCAL, sizeof(AstVarNode));
    node->binding = binding;
    node->n.type = binding->type;
    node->n.dst_binding = node->binding;
    return node;
}

typedef struct BindingName BindingName;
struct BindingName {
    const Symbol *symbol;
    const Type *type;
};

BindingName parse_binding_name(CompilerCtx *cctx, Any form) {
    BindingName b = {NULL, NULL};
    if (symbolp(form)) {
        b.symbol = to_symbol(form);
        return b;
    }
    Any temp = car(form);
    assert(symbolp(temp));
    assert(temp.val.symbol_ptr == symbol_the);
    form = cdr(form);
    b.type = parse_type(car(form));
    form = cdr(form);
    temp = car(form);
    assert(symbolp(temp));
    b.symbol = temp.val.symbol_ptr;
    return b;
}

AstNode *parse_form(CompilerCtx *cctx, Any form, Binding *dst_binding) {
    const Type *type = ANY_TYPE(form);

    if (type == type_ptr_symbol) {
        Binding *binding = lookup_binding(cctx->global_env, form.val.symbol_ptr);
        assert(binding);
        return (AstNode *)create_var(cctx, binding);
    }

    if (type == type_ref_cons) {
        Any head = car(form);

        if (ANY_TYPE(head) == type_ptr_symbol) {
            const Symbol *symbol = head.val.symbol_ptr;

            if (symbol == symbol_quote) {
                Any rest = cdr(form);
                return (AstNode *)create_literal(cctx, car(rest), dst_binding);
            }

            if (symbol == symbol_let) {
                Any rest = cdr(form);
                Any bindings_form = car(rest);
                rest = cdr(rest);
                Any body_form = car(rest);
                rest = cdr(rest);
                assert(nullp(rest));

                uint32_t bindings_length = list_length(bindings_form);
                uint32_t binding_count = bindings_length / 2;
                assert((bindings_length % 2) == 0);

                AstScopeNode *node = create_node(cctx, AST_SCOPE_LET, sizeof(AstScopeNode) + sizeof(Binding) * binding_count);
                node->binding_count = binding_count;

                for (uint32_t i = 0; i < binding_count; ++i) {
                    Any name_form = car(bindings_form);
                    bindings_form = cdr(bindings_form);
                    Any init_form = car(bindings_form);
                    bindings_form = cdr(bindings_form);

                    BindingName name = parse_binding_name(cctx, name_form);

                    Binding *binding = node->bindings + i;
                    binding->symbol = name.symbol;
                    binding->init_node = parse_form(cctx, init_form, binding);
                    assert(!name.type || name.type == binding->init_node->type);
                    binding->type = binding->init_node->type;
                    push_binding(cctx->global_env, binding);
                }
                assert(nullp(bindings_form));
                
                node->body_node = parse_form(cctx, body_form, dst_binding);
                node->n.type = node->body_node->type;
                node->n.dst_binding = node->body_node->dst_binding;
                pop_bindings(cctx->global_env, binding_count);
                return (AstNode *)node;
            }

            if (symbol == symbol_fun) {
                Any rest = cdr(form);
                Any params_form = car(rest);
                rest = cdr(rest);
                Any body_form = car(rest);
                rest = cdr(rest);
                assert(nullp(rest));

                uint32_t params_count = list_length(params_form);
                AstScopeNode *node = create_node(cctx, AST_SCOPE_FUN, sizeof(AstScopeNode) + sizeof(Binding) * (params_count + 1));
                node->binding_count = params_count;

                FunParam *params = calloc(1, sizeof(FunParam) * params_count);

                for (uint32_t i = 0; i < params_count; ++i) {
                    BindingName name = parse_binding_name(cctx, car(params_form));
                    params_form = cdr(params_form);
                    
                    Binding *binding = node->bindings + i;
                    binding->symbol = name.symbol;
                    binding->init_node = NULL;
                    binding->type = name.type ? name.type : type_any;
                    push_binding(cctx->global_env, binding);

                    params[i].type = binding->type;
                }
                assert(nullp(params_form));
                
                /* a pseudo-binding for the result, just to have a destination */
                Binding *result_binding = node->bindings + params_count;

                /* TODO: handle free vars and this being a closure */
                node->body_node = parse_form(cctx, body_form, result_binding);
                node->n.type = intern_fun_type(node->body_node->type, params_count, params);
                node->n.dst_binding = dst_binding;
                result_binding->type = node->body_node->type;
                free(params);
                return (AstNode *)node;
            }

            if (symbol == symbol_if) {
                Any rest = cdr(form);
                Any cond_form = car(rest);
                rest = cdr(rest);
                Any then_form = car(rest);
                rest = cdr(rest);
                Any else_form;
                if (!nullp(rest)) {
                    else_form = car(rest);
                    rest = cdr(rest);
                    assert(nullp(rest));
                }
                else {
                    else_form = ANY_UNIT;
                }

                AstIfNode *node = create_node(cctx, AST_IF, sizeof(AstIfNode));
                node->cond_node = parse_form(cctx, cond_form, NULL);
                node->then_node = parse_form(cctx, then_form, dst_binding);
                node->else_node = parse_form(cctx, else_form, dst_binding);
                assert(node->cond_node->type == type_any || node->cond_node->type == type_b32);
                assert(node->then_node->type == node->else_node->type);
                assert(node->then_node->dst_binding == node->else_node->dst_binding);
                node->n.type = node->then_node->type;
                node->n.dst_binding = node->then_node->dst_binding;
                return (AstNode *)node;
            }

            if (symbol == symbol_tagbody) {
                Any rest = cdr(form);
                AstPrimNode *node = alloc_prim_node(cctx, AST_PRIM_TAGBODY, rest);
                node->n.type = type_unit;

                uint32_t label_count = 0;
                for (uint32_t i = 0; i < node->arg_count; ++i) {
                    Any stmt = car(rest);
                    rest = cdr(rest);
                    if (ANY_TYPE(stmt) == type_ptr_symbol) {
                        AstLabelNode *label_node = create_label(cctx, stmt.val.symbol_ptr, gen_label(cctx));
                        node->arg_nodes[i] = (AstNode *)label_node;
                        push_label(cctx, label_node->name, label_node->id);
                        ++label_count;
                    }
                }

                rest = cdr(form);
                for (uint32_t i = 0; i < node->arg_count; ++i) {
                    Any stmt = car(rest);
                    rest = cdr(rest);
                    if (!node->arg_nodes[i]) {
                        node->arg_nodes[i] = parse_form(cctx, stmt, NULL);
                    }
                }

                pop_labels(cctx, label_count);
                return (AstNode *)node;
            }

            if (symbol == symbol_go) {
                Any rest = cdr(form);
                AstPrimNode *node = alloc_prim_node(cctx, AST_PRIM_GO, rest);
                assert(node->arg_count == 1);
                Any label_form = car(rest);
                assert(ANY_TYPE(label_form) == type_ptr_symbol);
                
                uint32_t label_id;
                if (!LabelMap_get(&cctx->label_map, label_form.val.symbol_ptr, &label_id)) {
                    assert(0 && "missing label");
                }
                node->arg_nodes[0] = (AstNode *)create_label(cctx, label_form.val.symbol_ptr, label_id);
                node->n.type = type_unit;
                return (AstNode *)node;
            }

            if (symbol == symbol_assign) {
                Any rest = cdr(form);
                AstPrimNode *node = alloc_prim_node(cctx, AST_PRIM_ASSIGN, rest);
                assert(node->arg_count == 2);
                
                node->arg_nodes[0] = parse_form(cctx, car(rest), NULL);
                AstVarNode *var = (AstVarNode *)node->arg_nodes[0];
                assert(var->n.kind == AST_VAR_LOCAL);

                rest = cdr(rest);
                node->arg_nodes[1] = parse_form(cctx, car(rest), var->binding);
                assert(var->n.type == node->arg_nodes[1]->type);

                node->n.type = node->arg_nodes[1]->type;
                node->n.dst_binding = var->binding;
                assert(node->n.type == node->n.dst_binding->type);
                ++var->binding->store_count;
                return (AstNode *)node;
            }

            if (symbol == symbol_not) {
                AstPrimNode *node = parse_prim(cctx, AST_PRIM_NOT, cdr(form), dst_binding);
                assert(node->arg_count == 1 && (node->arg_nodes[0]->type == type_any || node->arg_nodes[0]->type == type_b32));
                node->n.type = type_b32;
                return (AstNode *)node;
            }

            if (symbol == symbol_inc) { return (AstNode *)parse_inc_dec(cctx, AST_PRIM_INC, cdr(form)); }
            if (symbol == symbol_dec) { return (AstNode *)parse_inc_dec(cctx, AST_PRIM_DEC, cdr(form)); }

            if (symbol == symbol_plus) { return (AstNode *)parse_arithmetic(cctx, AST_PRIM_ADD, cdr(form), dst_binding); }
            if (symbol == symbol_minus) { return (AstNode *)parse_arithmetic(cctx, AST_PRIM_SUB, cdr(form), dst_binding); }
            if (symbol == symbol_mul) { return (AstNode *)parse_arithmetic(cctx, AST_PRIM_MUL, cdr(form), dst_binding); }
            if (symbol == symbol_div) { return (AstNode *)parse_arithmetic(cctx, AST_PRIM_DIV, cdr(form), dst_binding); }
            if (symbol == symbol_mod) { return (AstNode *)parse_arithmetic(cctx, AST_PRIM_MOD, cdr(form), dst_binding); }

            if (symbol == symbol_eq) { return (AstNode *)parse_compare(cctx, AST_PRIM_EQ, cdr(form), dst_binding); }
            if (symbol == symbol_lt) { return (AstNode *)parse_compare(cctx, AST_PRIM_LT, cdr(form), dst_binding); }
            if (symbol == symbol_gt) { return (AstNode *)parse_compare(cctx, AST_PRIM_GT, cdr(form), dst_binding); }
            if (symbol == symbol_lteq) { return (AstNode *)parse_compare(cctx, AST_PRIM_LTEQ, cdr(form), dst_binding); }
            if (symbol == symbol_gteq) { return (AstNode *)parse_compare(cctx, AST_PRIM_GTEQ, cdr(form), dst_binding); }

            if (symbol == symbol_print) {
                AstPrimNode *node = parse_prim(cctx, AST_PRIM_PRINT, cdr(form), dst_binding);
                assert(node->arg_count > 0);
                node->n.type = type_unit;
                return (AstNode *)node;
            }

            /* TODO: if symbol is a macro: return parse_form(macro_expand(form)) */
        }

        /* call */
        {
            Any args = cdr(form);
            uint32_t arg_count = list_length(args);
            struct AstCallNode *node = create_node(cctx, AST_CALL, sizeof(AstCallNode) + sizeof(AstNode *) * arg_count);
            node->fun_node = parse_form(cctx, head, NULL);
            node->arg_count = arg_count;
            for (uint32_t i = 0; i < arg_count; ++i) {
                node->arg_nodes[i] = parse_form(cctx, car(args), NULL);
                args = cdr(args);
            }
            /* TODO: handle ANY fun type */
            /* TODO: handle closures */
            assert(node->fun_node->type->kind == KIND_FUN);
            assert(node->fun_node->type->target);
            node->n.type = node->fun_node->type->target;
            return (AstNode *)node;
        }
    }

#define PRIM_CASE(UNAME, LNAME, TYPE, FMT) case KIND_ ## UNAME:
    switch (ANY_KIND(form)) {
    case KIND_UNIT: 
    FOR_ALL_PRIM(PRIM_CASE)
        return (AstNode *)create_literal(cctx, form, dst_binding);
    }

    assert(0 && "bad parse");
    return NULL;
}
