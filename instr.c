#include "instr.h"

#include <stdio.h>

#define INAME_FMT "%-12s"
#define PRINT_OP3(UOP, LOP, UNAME, LNAME) case OP_ ## UOP ## _ ## UNAME: printf(INAME_FMT "r%u <- r%u r%u\n", #LOP "/" #LNAME, INSTR_A(instr), INSTR_B(instr), INSTR_C(instr)); break;
#define DEFINE_PRINT_ADD(UNAME, LNAME, TYPE, FMT) PRINT_OP3(ADD, add, UNAME, LNAME)
#define DEFINE_PRINT_SUB(UNAME, LNAME, TYPE, FMT) PRINT_OP3(SUB, sub, UNAME, LNAME)
#define DEFINE_PRINT_MUL(UNAME, LNAME, TYPE, FMT) PRINT_OP3(MUL, mul, UNAME, LNAME)
#define DEFINE_PRINT_DIV(UNAME, LNAME, TYPE, FMT) PRINT_OP3(DIV, div, UNAME, LNAME)
#define DEFINE_PRINT_MOD(UNAME, LNAME, TYPE, FMT) PRINT_OP3(MOD, mod, UNAME, LNAME)
#define DEFINE_PRINT_EQ(UNAME, LNAME, TYPE, FMT) PRINT_OP3(EQ, eq, UNAME, LNAME)
#define DEFINE_PRINT_LT(UNAME, LNAME, TYPE, FMT) PRINT_OP3(LT, lt, UNAME, LNAME)
#define DEFINE_PRINT_GT(UNAME, LNAME, TYPE, FMT) PRINT_OP3(GT, gt, UNAME, LNAME)
#define DEFINE_PRINT_LTEQ(UNAME, LNAME, TYPE, FMT) PRINT_OP3(LTEQ, lteq, UNAME, LNAME)
#define DEFINE_PRINT_GTEQ(UNAME, LNAME, TYPE, FMT) PRINT_OP3(GTEQ, gteq, UNAME, LNAME)
#define PRINT_PRINT_LIT_32(UNAME, LNAME, TYPE, FMT) case OP_LIT_ ## UNAME: temp32 = INSTR_BC(instr); printf(INAME_FMT "r%u <- " FMT "\n", "lit/" #LNAME, INSTR_A(instr), *(TYPE *)&temp32); break;
#define PRINT_PRINT_LIT_64(UNAME, LNAME, TYPE, FMT) case OP_LIT_ ## UNAME: temp64 = code[1];         printf(INAME_FMT "r%u <- " FMT "\n", "lit/" #LNAME, INSTR_A(instr), *(TYPE *)&temp64); return code + 2;;
#define DEFINE_PRINT_PRINT(UNAME, LNAME, TYPE, FMT) case OP_PRINT_ ## UNAME: printf(INAME_FMT "r%u\n", "print/" #LNAME, INSTR_A(instr)); break;

uint64_t *print_instr(uint64_t *code) {
    uint32_t temp32;
    uint64_t temp64;
    uint64_t instr = code[0];

    switch (INSTR_OP(instr)) {
    case OP_LABEL: printf(":%u\n", INSTR_BC(instr)); break;
    case OP_JUMP_LABEL: printf(INAME_FMT ":%u\n", "jump", INSTR_BC(instr)); break;
    case OP_JFALSE_LABEL: printf(INAME_FMT "r%u :%u\n", "jfalse", INSTR_A(instr), INSTR_BC(instr)); break;
    case OP_JTRUE_LABEL: printf(INAME_FMT "r%u :%u\n", "jtrue", INSTR_A(instr), INSTR_BC(instr)); break;
    case OP_JUMP: printf(INAME_FMT "%+d\n", "jump", (int32_t)INSTR_BC(instr)); break;
    case OP_JFALSE: printf(INAME_FMT "r%u %+d\n", "jfalse", INSTR_A(instr), (int32_t)INSTR_BC(instr)); break;
    case OP_JTRUE: printf(INAME_FMT "r%u %+d\n", "jtrue", INSTR_A(instr), (int32_t)INSTR_BC(instr)); break;
    case OP_RET: printf("ret\n"); break;
    case OP_MOVE: printf(INAME_FMT "r%u <- r%u\n", "move", INSTR_A(instr), INSTR_B(instr)); break;
    case OP_NOT_BOOL: printf(INAME_FMT "r%u <- r%u\n", "not/bool", INSTR_A(instr), INSTR_B(instr)); break;
    FOR_ALL_PRIM(DEFINE_PRINT_PRINT)
    FOR_ALL_PRIM_32(PRINT_PRINT_LIT_32)
    FOR_ALL_PRIM_64(PRINT_PRINT_LIT_64)
    FOR_ALL_NUM(DEFINE_PRINT_ADD)
    FOR_ALL_NUM(DEFINE_PRINT_SUB)
    FOR_ALL_NUM(DEFINE_PRINT_MUL)
    FOR_ALL_NUM(DEFINE_PRINT_DIV)
    FOR_ALL_INT(DEFINE_PRINT_MOD)
    FOR_ALL_PRIM(DEFINE_PRINT_EQ)
    FOR_ALL_NUM(DEFINE_PRINT_LT)
    FOR_ALL_NUM(DEFINE_PRINT_GT)
    FOR_ALL_NUM(DEFINE_PRINT_LTEQ)
    FOR_ALL_NUM(DEFINE_PRINT_GTEQ)
    }
    return code + 1;
}

void print_code(uint64_t *code, uint32_t length) {
    uint64_t *max = code + length;
    while (code < max) {
        code = print_instr(code);
    }
}
