#ifndef INSTR_H
#define INSTR_H

#include "types.h"

#define DEF_LIT_ENUM(UNAME, LNAME, TYPE, FMT) OP_LIT_ ## UNAME,
#define DEF_INC_ENUM(UNAME, LNAME, TYPE, FMT) OP_INC_ ## UNAME,
#define DEF_DEC_ENUM(UNAME, LNAME, TYPE, FMT) OP_DEC_ ## UNAME,
#define DEF_ADD_ENUM(UNAME, LNAME, TYPE, FMT) OP_ADD_ ## UNAME,
#define DEF_SUB_ENUM(UNAME, LNAME, TYPE, FMT) OP_SUB_ ## UNAME,
#define DEF_MUL_ENUM(UNAME, LNAME, TYPE, FMT) OP_MUL_ ## UNAME,
#define DEF_DIV_ENUM(UNAME, LNAME, TYPE, FMT) OP_DIV_ ## UNAME,
#define DEF_MOD_ENUM(UNAME, LNAME, TYPE, FMT) OP_MOD_ ## UNAME,
#define DEF_EQ_ENUM(UNAME, LNAME, TYPE, FMT) OP_EQ_ ## UNAME,
#define DEF_LT_ENUM(UNAME, LNAME, TYPE, FMT) OP_LT_ ## UNAME,
#define DEF_GT_ENUM(UNAME, LNAME, TYPE, FMT) OP_GT_ ## UNAME,
#define DEF_LTEQ_ENUM(UNAME, LNAME, TYPE, FMT) OP_LTEQ_ ## UNAME,
#define DEF_GTEQ_ENUM(UNAME, LNAME, TYPE, FMT) OP_GTEQ_ ## UNAME,
#define DEF_TO_ANY_ENUM(UNAME, LNAME, TYPE, FMT) OP_ ## UNAME ## _TO_ANY,

struct size_12_dummy_struct { uint32_t a, b, c; };
struct size_16_dummy_struct { uint64_t a, b; };

#define FOR_ALL_PRIM_MOVE(X) \
    X(MOVE1, move1, uint8_t) \
    X(MOVE2, move2, uint16_t) \
    X(MOVE4, move4, uint32_t) \
    X(MOVE8, move8, uint64_t) \
    X(MOVE12, move12, struct size_12_dummy_struct) \
    X(MOVE16, move16, struct size_16_dummy_struct)

#define DEF_MOVE_ENUM(UNAME, LNAME, TYPE) OP_ ## UNAME,

enum {
    OP_NOP,

    /* these will only be generated for top-level code */
    OP_DEF,
    OP_MACRODEF,
    
    OP_LABEL, /* labels are eliminated during compilation */

    /* these specify a label. they will be replaced by the below */
    OP_JUMP_LABEL,
    OP_JFALSE_LABEL,
    OP_JTRUE_LABEL,

    /* these specify a relative instruction pointer offset. they replace the above */
    OP_JUMP,    /* jump to BC */
    OP_JFALSE,  /* jump to BC if not A */
    OP_JTRUE,   /* jump to BC if A */

    OP_TCALL,   /* tail call. jump to function in A */
    OP_CALL,    /* call function in A, store result in B */
    OP_RET,     /* return from function */

    /* move B to A */
    FOR_ALL_PRIM_MOVE(DEF_MOVE_ENUM)

    OP_NOT_BOOL,
    
    FOR_ALL_PRIM(DEF_LIT_ENUM)
    
    FOR_ALL_INT(DEF_INC_ENUM)
    FOR_ALL_INT(DEF_DEC_ENUM)
    
    FOR_ALL_NUM(DEF_ADD_ENUM)
    FOR_ALL_NUM(DEF_SUB_ENUM)
    FOR_ALL_NUM(DEF_MUL_ENUM)
    FOR_ALL_NUM(DEF_DIV_ENUM)
    FOR_ALL_INT(DEF_MOD_ENUM)

    FOR_ALL_PRIM(DEF_EQ_ENUM)
    FOR_ALL_NUM(DEF_LT_ENUM)
    FOR_ALL_NUM(DEF_GT_ENUM)
    FOR_ALL_NUM(DEF_LTEQ_ENUM)
    FOR_ALL_NUM(DEF_GTEQ_ENUM)

    FOR_ALL_BASIC(DEF_TO_ANY_ENUM)

    OP_CALL_BUILTIN_1_VOID,
    OP_CALL_BUILTIN_2_VOID,
    OP_CALL_BUILTIN_1,
    OP_CALL_BUILTIN_2,

    NUM_OPS
};

#define INSTR_OP_BITS 8
#define INSTR_OP_MASK 0xff
#define INSTR_A_BITS 16
#define INSTR_A_MASK 0xffff
#define INSTR_B_BITS 16
#define INSTR_B_MASK 0xffff
#define INSTR_C_BITS 16
#define INSTR_C_MASK 0xffff
#define INSTR_BC_BITS 32
#define INSTR_BC_MASK 0xffffffff

#define INSTR_OP(instr) ((uint32_t)((instr) & INSTR_OP_MASK))
#define INSTR_A(instr)  ((uint32_t)(((instr) >> INSTR_OP_BITS) & INSTR_A_MASK))
#define INSTR_B(instr)  ((uint32_t)(((instr) >> (INSTR_OP_BITS + INSTR_A_BITS)) & INSTR_B_MASK))
#define INSTR_C(instr)  ((uint32_t)(((instr) >> (INSTR_OP_BITS + INSTR_A_BITS + INSTR_B_BITS)) & INSTR_C_MASK))
#define INSTR_BC(instr) ((uint32_t)(((instr) >> (INSTR_OP_BITS + INSTR_A_BITS)) & INSTR_BC_MASK))

#define MK_INSTR(op)                (op)
#define MK_INSTR_A(op, a)           ((op) | ((uint64_t)(a)  << INSTR_OP_BITS))
#define MK_INSTR_A_B(op, a, b)      ((op) | ((uint64_t)(a)  << INSTR_OP_BITS) | ((uint64_t)(b)  << (INSTR_OP_BITS + INSTR_A_BITS)))
#define MK_INSTR_A_B_C(op, a, b, c) ((op) | ((uint64_t)(a)  << INSTR_OP_BITS) | ((uint64_t)(b)  << (INSTR_OP_BITS + INSTR_A_BITS)) | ((uint64_t)(c) << (INSTR_OP_BITS + INSTR_A_BITS + INSTR_B_BITS)))
#define MK_INSTR_A_BC(op, a, bc)    ((op) | ((uint64_t)(a)  << INSTR_OP_BITS) | ((uint64_t)(bc) << (INSTR_OP_BITS + INSTR_A_BITS)))
#define MK_INSTR_BC(op, bc)         ((op) | ((uint64_t)(bc) << (INSTR_OP_BITS + INSTR_A_BITS)))

uint64_t *print_instr(uint64_t *code);
void print_code(uint64_t *code, uint32_t length);

#endif
