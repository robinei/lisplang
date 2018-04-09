#include "instr.h"
#include "any.h"

#include <stdio.h>

#define INAME_FMT "%-12s"
#define PRINT_OP1(UOP, LOP, UNAME, LNAME) case OP_ ## UOP ## _ ## UNAME: printf(INAME_FMT "[%u]\n", #LOP "/" #LNAME, INSTR_A(instr)); break;
#define PRINT_OP3(UOP, LOP, UNAME, LNAME) case OP_ ## UOP ## _ ## UNAME: printf(INAME_FMT "[%u] <- [%u] [%u]\n", #LOP "/" #LNAME, INSTR_A(instr), INSTR_B(instr), INSTR_C(instr)); break;
#define DEFINE_PRINT_MOVE(UNAME, LNAME, TYPE) case OP_ ## UNAME: printf(INAME_FMT "[%u] <- [%u]\n", #LNAME, INSTR_A(instr), INSTR_B(instr)); break;
#define PRINT_LIT_32(UNAME, LNAME, TYPE, FMT) case OP_LIT_ ## UNAME: temp32 = INSTR_BC(instr); printf(INAME_FMT "[%u] <- " FMT "\n", "lit/" #LNAME, INSTR_A(instr), *(TYPE *)&temp32); break;
#define PRINT_LIT_64(UNAME, LNAME, TYPE, FMT) case OP_LIT_ ## UNAME: temp64 = code[1];         printf(INAME_FMT "[%u] <- " FMT "\n", "lit/" #LNAME, INSTR_A(instr), *(TYPE *)&temp64); return code + 2;
#define DEFINE_PRINT_INC(UNAME, LNAME, TYPE, FMT) PRINT_OP1(INC, inc, UNAME, LNAME)
#define DEFINE_PRINT_DEC(UNAME, LNAME, TYPE, FMT) PRINT_OP1(DEC, dec, UNAME, LNAME)
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
#define DEFINE_PRINT_TO_ANY(UNAME, LNAME, TYPE, FMT) case OP_ ## UNAME ## _TO_ANY: printf(INAME_FMT "[%u] <- [%u]\n", "to_any/" #LNAME, INSTR_A(instr), INSTR_B(instr)); break;

static const char *fun_name_or_default(void *builtin, const char *default_str) {
    if (builtin == print_any) { return "print_any"; }
    return default_str;
}

uint64_t *print_instr(uint64_t *code) {
    uint32_t temp32;
    uint64_t temp64;
    uint64_t instr = code[0];
    uint32_t op = INSTR_OP(instr);

    switch (op) {
    case OP_NOP: printf("nop\n"); break;
    case OP_LABEL: printf(":%u\n", INSTR_BC(instr)); break;
    case OP_JUMP_LABEL: printf(INAME_FMT ":%u\n", "jump", INSTR_BC(instr)); break;
    case OP_JFALSE_LABEL: printf(INAME_FMT "[%u] :%u\n", "jfalse", INSTR_A(instr), INSTR_BC(instr)); break;
    case OP_JTRUE_LABEL: printf(INAME_FMT "[%u] :%u\n", "jtrue", INSTR_A(instr), INSTR_BC(instr)); break;
    case OP_JUMP: printf(INAME_FMT "%+d\n", "jump", (int32_t)INSTR_BC(instr)); break;
    case OP_JFALSE: printf(INAME_FMT "[%u] %+d\n", "jfalse", INSTR_A(instr), (int32_t)INSTR_BC(instr)); break;
    case OP_JTRUE: printf(INAME_FMT "[%u] %+d\n", "jtrue", INSTR_A(instr), (int32_t)INSTR_BC(instr)); break;
    case OP_TCALL: printf(INAME_FMT "[%u] <- RUN [%u]\n", "tcall", INSTR_B(instr), INSTR_A(instr)); break;
    case OP_CALL: printf(INAME_FMT "[%u] <- RUN [%u]\n", "call", INSTR_B(instr), INSTR_A(instr)); break;
    case OP_RET: printf("ret\n"); break;
    FOR_ALL_PRIM_MOVE(DEFINE_PRINT_MOVE)
    case OP_NOT_BOOL: printf(INAME_FMT "[%u] <- [%u]\n", "not/bool", INSTR_A(instr), INSTR_B(instr)); break;
    FOR_ALL_PRIM_32(PRINT_LIT_32)
    FOR_ALL_PRIM_64(PRINT_LIT_64)
    FOR_ALL_INT(DEFINE_PRINT_INC)
    FOR_ALL_INT(DEFINE_PRINT_DEC)
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
    FOR_ALL_BASIC(DEFINE_PRINT_TO_ANY)
    case OP_CALL_BUILTIN_1_VOID: printf(INAME_FMT "[%u]\n", fun_name_or_default((void *)*(code + 1), "call_builtin_1_void"), INSTR_A(instr)); return code + 2;
    case OP_CALL_BUILTIN_2_VOID: printf(INAME_FMT "[%u] [%u]\n", fun_name_or_default((void *)*(code + 1), "call_builtin_2_void"), INSTR_A(instr), INSTR_B(instr)); return code + 2;
    case OP_CALL_BUILTIN_1: printf(INAME_FMT "[%u] <- [%u]\n", fun_name_or_default((void *)*(code + 1), "call_builtin_1"), INSTR_A(instr), INSTR_B(instr)); return code + 2;
    case OP_CALL_BUILTIN_2: printf(INAME_FMT "[%u] <- [%u] [%u]\n", fun_name_or_default((void *)*(code + 1), "call_builtin_2"), INSTR_A(instr), INSTR_B(instr), INSTR_C(instr)); return code + 2;
    default: assert(0 && "unknown instruction");
    }
    return code + 1;
}

void print_code(uint64_t *code, uint32_t length) {
    uint64_t *max = code + length;
    while (code < max) {
        code = print_instr(code);
    }
}
