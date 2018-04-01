#include "interpret.h"
#include "instr.h"

#include <stdio.h>

#ifdef __GNUC__

/* Direct treaded interpreter using computed goto (GNU extension) */

#define DEF_PRINT_LABEL(UNAME, LNAME, TYPE, FMT) &&label_ ## OP_PRINT_ ## UNAME,
#define DEF_LIT_LABEL(UNAME, LNAME, TYPE, FMT) &&label_ ## OP_LIT_ ## UNAME,
#define DEF_ADD_LABEL(UNAME, LNAME, TYPE, FMT) &&label_ ## OP_ADD_ ## UNAME,
#define DEF_SUB_LABEL(UNAME, LNAME, TYPE, FMT) &&label_ ## OP_SUB_ ## UNAME,
#define DEF_MUL_LABEL(UNAME, LNAME, TYPE, FMT) &&label_ ## OP_MUL_ ## UNAME,
#define DEF_DIV_LABEL(UNAME, LNAME, TYPE, FMT) &&label_ ## OP_DIV_ ## UNAME,
#define DEF_MOD_LABEL(UNAME, LNAME, TYPE, FMT) &&label_ ## OP_MOD_ ## UNAME,
#define DEF_EQ_LABEL(UNAME, LNAME, TYPE, FMT) &&label_ ## OP_EQ_ ## UNAME,
#define DEF_LT_LABEL(UNAME, LNAME, TYPE, FMT) &&label_ ## OP_LT_ ## UNAME,
#define DEF_GT_LABEL(UNAME, LNAME, TYPE, FMT) &&label_ ## OP_GT_ ## UNAME,
#define DEF_LTEQ_LABEL(UNAME, LNAME, TYPE, FMT) &&label_ ## OP_LTEQ_ ## UNAME,
#define DEF_GTEQ_LABEL(UNAME, LNAME, TYPE, FMT) &&label_ ## OP_GTEQ_ ## UNAME,

#define DISPATCH_CASE(OPNAME) label_ ## OPNAME:

#define DISPATCH_NEXT() instr = *ip++; goto *dispatch_table[INSTR_OP(instr)];

#define BEGIN_DISPATCH() \
    static void *dispatch_table[] = { \
        NULL, \
        NULL, \
        NULL, \
        NULL, \
        &&label_OP_JUMP, \
        &&label_OP_JFALSE, \
        &&label_OP_JTRUE, \
        &&label_OP_TCALL, \
        &&label_OP_CALL, \
        &&label_OP_RET, \
        &&label_OP_MOVE, \
        &&label_ ## OP_NOT_BOOL, \
        FOR_ALL_PRIM(DEF_PRINT_LABEL) \
        FOR_ALL_PRIM(DEF_LIT_LABEL) \
        FOR_ALL_NUM(DEF_ADD_LABEL) \
        FOR_ALL_NUM(DEF_SUB_LABEL) \
        FOR_ALL_NUM(DEF_MUL_LABEL) \
        FOR_ALL_NUM(DEF_DIV_LABEL) \
        FOR_ALL_INT(DEF_MOD_LABEL) \
        FOR_ALL_PRIM(DEF_EQ_LABEL) \
        FOR_ALL_NUM(DEF_LT_LABEL) \
        FOR_ALL_NUM(DEF_GT_LABEL) \
        FOR_ALL_NUM(DEF_LTEQ_LABEL) \
        FOR_ALL_NUM(DEF_GTEQ_LABEL) \
    }; \
    uint64_t instr; \
    DISPATCH_NEXT()

#define END_DISPATCH()

#else

/* Fallback regular switch interpreter */

#define DISPATCH_CASE(OPNAME) case OPNAME:

#define DISPATCH_NEXT() continue;

#define BEGIN_DISPATCH() \
    for (;;) { \
        uint64_t instr = *ip++; \
        switch (INSTR_OP(instr)) {

#define END_DISPATCH() \
        } \
    }

#endif

#define DEFINE_PRINT_INSTR(UNAME, LNAME, TYPE, FMT) \
    DISPATCH_CASE(OP_PRINT_ ## UNAME)  printf("printed: " FMT "\n", fp[INSTR_A(instr)].LNAME); DISPATCH_NEXT();
#define DEFINE_LIT_32_INSTR(UNAME, LNAME, TYPE, FMT) \
    DISPATCH_CASE(OP_LIT_ ## UNAME)   fp[INSTR_A(instr)].LNAME = (TYPE)INSTR_BC(instr); DISPATCH_NEXT();
#define DEFINE_LIT_64_INSTR(UNAME, LNAME, TYPE, FMT) \
    DISPATCH_CASE(OP_LIT_ ## UNAME)   fp[INSTR_A(instr)].LNAME = (TYPE)*ip++; DISPATCH_NEXT();
#define DEFINE_ADD_INSTR(UNAME, LNAME, TYPE, FMT) \
    DISPATCH_CASE(OP_ADD_ ## UNAME)   fp[INSTR_A(instr)].LNAME = fp[INSTR_B(instr)].LNAME +  fp[INSTR_C(instr)].LNAME; DISPATCH_NEXT();
#define DEFINE_SUB_INSTR(UNAME, LNAME, TYPE, FMT) \
    DISPATCH_CASE(OP_SUB_ ## UNAME)   fp[INSTR_A(instr)].LNAME = fp[INSTR_B(instr)].LNAME -  fp[INSTR_C(instr)].LNAME; DISPATCH_NEXT();
#define DEFINE_MUL_INSTR(UNAME, LNAME, TYPE, FMT) \
    DISPATCH_CASE(OP_MUL_ ## UNAME)   fp[INSTR_A(instr)].LNAME = fp[INSTR_B(instr)].LNAME *  fp[INSTR_C(instr)].LNAME; DISPATCH_NEXT();
#define DEFINE_DIV_INSTR(UNAME, LNAME, TYPE, FMT) \
    DISPATCH_CASE(OP_DIV_ ## UNAME)   fp[INSTR_A(instr)].LNAME = fp[INSTR_B(instr)].LNAME /  fp[INSTR_C(instr)].LNAME; DISPATCH_NEXT();
#define DEFINE_MOD_INSTR(UNAME, LNAME, TYPE, FMT) \
    DISPATCH_CASE(OP_MOD_ ## UNAME)   fp[INSTR_A(instr)].LNAME = fp[INSTR_B(instr)].LNAME %  fp[INSTR_C(instr)].LNAME; DISPATCH_NEXT();
#define DEFINE_EQ_INSTR(UNAME, LNAME, TYPE, FMT) \
    DISPATCH_CASE(OP_EQ_ ## UNAME)    fp[INSTR_A(instr)].b32   = fp[INSTR_B(instr)].LNAME == fp[INSTR_C(instr)].LNAME; DISPATCH_NEXT();
#define DEFINE_LT_INSTR(UNAME, LNAME, TYPE, FMT) \
    DISPATCH_CASE(OP_LT_ ## UNAME)    fp[INSTR_A(instr)].b32   = fp[INSTR_B(instr)].LNAME <  fp[INSTR_C(instr)].LNAME; DISPATCH_NEXT();
#define DEFINE_GT_INSTR(UNAME, LNAME, TYPE, FMT) \
    DISPATCH_CASE(OP_GT_ ## UNAME)    fp[INSTR_A(instr)].b32   = fp[INSTR_B(instr)].LNAME >  fp[INSTR_C(instr)].LNAME; DISPATCH_NEXT();
#define DEFINE_LTEQ_INSTR(UNAME, LNAME, TYPE, FMT) \
    DISPATCH_CASE(OP_LTEQ_ ## UNAME)  fp[INSTR_A(instr)].b32   = fp[INSTR_B(instr)].LNAME <= fp[INSTR_C(instr)].LNAME; DISPATCH_NEXT();
#define DEFINE_GTEQ_INSTR(UNAME, LNAME, TYPE, FMT) \
    DISPATCH_CASE(OP_GTEQ_ ## UNAME)  fp[INSTR_A(instr)].b32   = fp[INSTR_B(instr)].LNAME >= fp[INSTR_C(instr)].LNAME; DISPATCH_NEXT();


void interpret(uint64_t *ip, Word *fp) {
    BEGIN_DISPATCH()

    DISPATCH_CASE(OP_JUMP) ip += (int32_t)INSTR_BC(instr) - 1; DISPATCH_NEXT();
    DISPATCH_CASE(OP_JFALSE) if (!fp[INSTR_A(instr)].b32) { ip += (int32_t)INSTR_BC(instr) - 1; } DISPATCH_NEXT();
    DISPATCH_CASE(OP_JTRUE)  if ( fp[INSTR_A(instr)].b32) { ip += (int32_t)INSTR_BC(instr) - 1; } DISPATCH_NEXT();

    DISPATCH_CASE(OP_TCALL) {
        uint32_t fun_offset = INSTR_A(instr);
        Function *fun = (Function *)(fp + fun_offset)->ptr;
        fp += fun_offset;
        ip = fun->code; /* tail call means we just overwrite these, and don't grow the call stack */
        DISPATCH_NEXT();
    }
    DISPATCH_CASE(OP_CALL) {
        uint32_t fun_offset = INSTR_A(instr);
        uint32_t result_reg = INSTR_B(instr);
        Function *fun = (Function *)(fp + fun_offset)->ptr;
        interpret(fun->code, fp + fun_offset); /* just use the C call stack */
        fp[result_reg] = fp[fun_offset];
        DISPATCH_NEXT();
    }
    DISPATCH_CASE(OP_RET) return; /* since we use the C call stack we just return */

    DISPATCH_CASE(OP_MOVE) fp[INSTR_A(instr)] = fp[INSTR_B(instr)]; DISPATCH_NEXT();
    DISPATCH_CASE(OP_NOT_BOOL) fp[INSTR_A(instr)].b32 = !fp[INSTR_B(instr)].b32; DISPATCH_NEXT();

    FOR_ALL_PRIM(DEFINE_PRINT_INSTR)
    FOR_ALL_PRIM_32(DEFINE_LIT_32_INSTR)
    FOR_ALL_PRIM_64(DEFINE_LIT_64_INSTR)
    FOR_ALL_NUM(DEFINE_ADD_INSTR)
    FOR_ALL_NUM(DEFINE_SUB_INSTR)
    FOR_ALL_NUM(DEFINE_MUL_INSTR)
    FOR_ALL_NUM(DEFINE_DIV_INSTR)
    FOR_ALL_INT(DEFINE_MOD_INSTR)
    FOR_ALL_PRIM(DEFINE_EQ_INSTR)
    FOR_ALL_NUM(DEFINE_LT_INSTR)
    FOR_ALL_NUM(DEFINE_GT_INSTR)
    FOR_ALL_NUM(DEFINE_LTEQ_INSTR)
    FOR_ALL_NUM(DEFINE_GTEQ_INSTR)

    END_DISPATCH();
}
