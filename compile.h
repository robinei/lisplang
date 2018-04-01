#ifndef COMPILE_H
#define COMPILE_H

#include "instr.h"

#define EXPAND_INTERFACE
#define NAME LabelMap
#define KEY_TYPE const Symbol *
#define VALUE_TYPE uint32_t
#include "hashtable.h"
typedef struct LabelMap LabelMap;

typedef struct Binding Binding;
struct Binding {
    Any value;
    const Symbol *symbol;
    const Type *type;
    uint32_t reg;
};

#define EXPAND_INTERFACE
#define NAME BindingMap
#define KEY_TYPE const Symbol *
#define VALUE_TYPE Binding *
#include "hashtable.h"
typedef struct BindingMap BindingMap;

typedef struct PrevLabel PrevLabel;
struct PrevLabel {
    const Symbol *symbol;
    uint32_t label;
};

typedef struct PrevBinding PrevBinding;
struct PrevBinding {
    const Symbol *symbol;
    Binding *binding;
};

typedef struct ReservedRegs ReservedRegs;
struct ReservedRegs {
    uint32_t words_used;
    uint32_t words_capacity;
    uint64_t *words;
};

typedef struct CompilerCtx CompilerCtx;
struct CompilerCtx {
    uint64_t *code;
    uint32_t code_used;
    uint32_t code_capacity;

    uint32_t label_counter;
    LabelMap label_map;
    PrevLabel *label_stack;
    uint32_t label_stack_used;
    uint32_t label_stack_capacity;

    BindingMap binding_map;
    PrevBinding *binding_stack;
    uint32_t binding_stack_used;
    uint32_t binding_stack_capacity;

    ReservedRegs regs;
};

typedef struct CodeBlock CodeBlock;
struct CodeBlock {
    uint64_t *code;
    uint32_t length;
};

CodeBlock compile_block(Any form);

void init_compile(void);

#endif
