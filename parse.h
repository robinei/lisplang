#ifndef PARSE_H
#define PARSE_H

#include "types.h"
#include "compile.h"

typedef enum AstNodeKind AstNodeKind;
enum AstNodeKind {
    AST_LITERAL,
    AST_VAR_LOCAL,
    AST_SCOPE_LET,
    AST_SCOPE_FUN,
    AST_CALL,
    AST_IF,
    AST_LABEL,
    AST_PRIM_DO,
    AST_PRIM_TAGBODY,
    AST_PRIM_GO,
    AST_PRIM_ASSIGN,
    AST_PRIM_NOT,
    AST_PRIM_INC,
    AST_PRIM_DEC,
    AST_PRIM_ADD,
    AST_PRIM_SUB,
    AST_PRIM_MUL,
    AST_PRIM_DIV,
    AST_PRIM_MOD,
    AST_PRIM_EQ,
    AST_PRIM_LT,
    AST_PRIM_GT,
    AST_PRIM_LTEQ,
    AST_PRIM_GTEQ,
    AST_PRIM_PRINT,
};

typedef struct AstNode AstNode;
struct AstNode {
    AstNodeKind kind : 6;
    bool is_in_tailpos: 1;
    bool is_in_argpos: 1;
    const Type *type;
    Binding *dst_binding;
};

typedef struct AstLiteralNode AstLiteralNode;
struct AstLiteralNode {
    AstNode n;
    Any form;
};

typedef struct AstVarNode AstVarNode;
struct AstVarNode {
    AstNode n;
    Binding *binding;
};

typedef struct AstScopeNode AstScopeNode;
struct AstScopeNode {
    AstNode n;
    AstScopeNode *parent_fun;
    AstScopeNode *parent_scope;
    AstNode *body_node;
    uint32_t binding_count;
    Binding bindings[];
};

typedef struct AstIfNode AstIfNode;
struct AstIfNode {
    AstNode n;
    AstNode *cond_node;
    AstNode *then_node;
    AstNode *else_node;
};

typedef struct AstCallNode AstCallNode;
struct AstCallNode {
    AstNode n;
    AstNode *fun_node;
    uint32_t arg_count;
    AstNode *arg_nodes[];
};

typedef struct AstLabelNode AstLabelNode;
struct AstLabelNode {
    AstNode n;
    const Symbol *name;
    uint32_t id;
};

typedef struct AstPrimNode AstPrimNode;
struct AstPrimNode {
    AstNode n;
    uint32_t arg_count;
    AstNode *arg_nodes[];
};


enum {
    PARSE_FLAG_TOPLEVEL = 1 << 0,
    PARSE_FLAG_TAILPOS = 1 << 1,
    PARSE_FLAG_ARGPOS = 1 << 2,
};

AstNode *parse_form(CompilerCtx *cctx, Any form, Binding *dst_binding, uint32_t flags, AstScopeNode *scope);

uint32_t gen_label(CompilerCtx *cctx);

#endif
