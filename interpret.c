#include "interpret.h"
#include "instr.h"

#include <stdio.h>

#ifdef __GNUC__

/* Direct treaded interpreter using computed goto (GNU extension) */

#define DEF_MOVE_LABEL(UNAME, LNAME, TYPE) &&label_ ## OP_ ## UNAME,
#define DEF_LIT_LABEL(UNAME, LNAME, TYPE, FMT) &&label_ ## OP_LIT_ ## UNAME,
#define DEF_INC_LABEL(UNAME, LNAME, TYPE, FMT) &&label_ ## OP_INC_ ## UNAME,
#define DEF_DEC_LABEL(UNAME, LNAME, TYPE, FMT) &&label_ ## OP_DEC_ ## UNAME,
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
#define DEF_TO_ANY_LABEL(UNAME, LNAME, TYPE, FMT) &&label_ ## OP_ ## UNAME ## _TO_ANY,

#define DISPATCH_CASE(OPNAME) label_ ## OPNAME:

#define DISPATCH_NEXT() instr = *ip++; goto *dispatch_table[INSTR_OP(instr)];

#define BEGIN_DISPATCH() \
    static void *dispatch_table[] = { \
        NULL, \
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
        FOR_ALL_PRIM_MOVE(DEF_MOVE_LABEL) \
        &&label_ ## OP_NOT_BOOL, \
        FOR_ALL_PRIM(DEF_LIT_LABEL) \
        FOR_ALL_INT(DEF_INC_LABEL) \
        FOR_ALL_INT(DEF_DEC_LABEL) \
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
        FOR_ALL_BASIC(DEF_TO_ANY_LABEL) \
        &&label_OP_CALL_BUILTIN_1_VOID, \
        &&label_OP_CALL_BUILTIN_2_VOID, \
        &&label_OP_CALL_BUILTIN_1, \
        &&label_OP_CALL_BUILTIN_2, \
    }; \
    uint64_t instr; \
    DISPATCH_NEXT()

#define END_DISPATCH()

#else

/* Fallback regular switch interpreter */

#pragma message ("WARNING: Falling back to slower interpreter (missing computed goto support)")

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

#define DEFINE_MOVE_INSTR(UNAME, LNAME, TYPE) \
    DISPATCH_CASE(OP_ ## UNAME) *(TYPE *)(fp + INSTR_A(instr)) = *(TYPE *)(fp + INSTR_B(instr)); DISPATCH_NEXT();
#define DEFINE_LIT_32_INSTR(UNAME, LNAME, TYPE, FMT) \
    DISPATCH_CASE(OP_LIT_ ## UNAME)   *(TYPE *)(fp +INSTR_A(instr)) = (TYPE)INSTR_BC(instr); DISPATCH_NEXT();
#define DEFINE_LIT_64_INSTR(UNAME, LNAME, TYPE, FMT) \
    DISPATCH_CASE(OP_LIT_ ## UNAME)   *(TYPE *)(fp + INSTR_A(instr)) = (TYPE)*ip++; DISPATCH_NEXT();
#define DEFINE_INC_INSTR(UNAME, LNAME, TYPE, FMT) \
    DISPATCH_CASE(OP_INC_ ## UNAME)   ++*(TYPE *)(fp + INSTR_A(instr)); DISPATCH_NEXT();
#define DEFINE_DEC_INSTR(UNAME, LNAME, TYPE, FMT) \
    DISPATCH_CASE(OP_DEC_ ## UNAME)   --*(TYPE *)(fp + INSTR_A(instr)); DISPATCH_NEXT();
#define DEFINE_ADD_INSTR(UNAME, LNAME, TYPE, FMT) \
    DISPATCH_CASE(OP_ADD_ ## UNAME)   *(TYPE *)(fp + INSTR_A(instr)) = *(TYPE *)(fp + INSTR_B(instr)) +  *(TYPE *)(fp + INSTR_C(instr)); DISPATCH_NEXT();
#define DEFINE_SUB_INSTR(UNAME, LNAME, TYPE, FMT) \
    DISPATCH_CASE(OP_SUB_ ## UNAME)   *(TYPE *)(fp + INSTR_A(instr)) = *(TYPE *)(fp + INSTR_B(instr)) -  *(TYPE *)(fp + INSTR_C(instr)); DISPATCH_NEXT();
#define DEFINE_MUL_INSTR(UNAME, LNAME, TYPE, FMT) \
    DISPATCH_CASE(OP_MUL_ ## UNAME)   *(TYPE *)(fp + INSTR_A(instr)) = *(TYPE *)(fp + INSTR_B(instr)) *  *(TYPE *)(fp + INSTR_C(instr)); DISPATCH_NEXT();
#define DEFINE_DIV_INSTR(UNAME, LNAME, TYPE, FMT) \
    DISPATCH_CASE(OP_DIV_ ## UNAME)   *(TYPE *)(fp + INSTR_A(instr)) = *(TYPE *)(fp + INSTR_B(instr)) /  *(TYPE *)(fp + INSTR_C(instr)); DISPATCH_NEXT();
#define DEFINE_MOD_INSTR(UNAME, LNAME, TYPE, FMT) \
    DISPATCH_CASE(OP_MOD_ ## UNAME)   *(TYPE *)(fp + INSTR_A(instr)) = *(TYPE *)(fp + INSTR_B(instr)) %  *(TYPE *)(fp + INSTR_C(instr)); DISPATCH_NEXT();
#define DEFINE_EQ_INSTR(UNAME, LNAME, TYPE, FMT) \
    DISPATCH_CASE(OP_EQ_ ## UNAME)    *(bool *)(fp + INSTR_A(instr)) = *(TYPE *)(fp + INSTR_B(instr)) == *(TYPE *)(fp + INSTR_C(instr)); DISPATCH_NEXT();
#define DEFINE_LT_INSTR(UNAME, LNAME, TYPE, FMT) \
    DISPATCH_CASE(OP_LT_ ## UNAME)    *(bool *)(fp + INSTR_A(instr)) = *(TYPE *)(fp + INSTR_B(instr)) <  *(TYPE *)(fp + INSTR_C(instr)); DISPATCH_NEXT();
#define DEFINE_GT_INSTR(UNAME, LNAME, TYPE, FMT) \
    DISPATCH_CASE(OP_GT_ ## UNAME)    *(bool *)(fp + INSTR_A(instr)) = *(TYPE *)(fp + INSTR_B(instr)) >  *(TYPE *)(fp + INSTR_C(instr)); DISPATCH_NEXT();
#define DEFINE_LTEQ_INSTR(UNAME, LNAME, TYPE, FMT) \
    DISPATCH_CASE(OP_LTEQ_ ## UNAME)  *(bool *)(fp + INSTR_A(instr)) = *(TYPE *)(fp + INSTR_B(instr)) <= *(TYPE *)(fp + INSTR_C(instr)); DISPATCH_NEXT();
#define DEFINE_GTEQ_INSTR(UNAME, LNAME, TYPE, FMT) \
    DISPATCH_CASE(OP_GTEQ_ ## UNAME)  *(bool *)(fp + INSTR_A(instr)) = *(TYPE *)(fp + INSTR_B(instr)) >= *(TYPE *)(fp + INSTR_C(instr)); DISPATCH_NEXT();
#define DEFINE_TO_ANY_INSTR(UNAME, LNAME, TYPE, FMT) \
    DISPATCH_CASE(OP_ ## UNAME ## _TO_ANY) { Any *any = (Any *)(fp + INSTR_A(instr)); any->val.LNAME = *(TYPE *)(fp + INSTR_B(instr)); any->type = MK_ANY_TYPE(type_ ## LNAME); } DISPATCH_NEXT();

void interpret(uint64_t *ip, uint8_t *fp) {
    BEGIN_DISPATCH()

    DISPATCH_CASE(OP_JUMP) ip += (int32_t)INSTR_BC(instr) - 1; DISPATCH_NEXT();
    DISPATCH_CASE(OP_JFALSE) if (!*(bool *)(fp + INSTR_A(instr))) { ip += (int32_t)INSTR_BC(instr) - 1; } DISPATCH_NEXT();
    DISPATCH_CASE(OP_JTRUE)  if ( *(bool *)(fp + INSTR_A(instr))) { ip += (int32_t)INSTR_BC(instr) - 1; } DISPATCH_NEXT();

    DISPATCH_CASE(OP_TCALL) {
        uint32_t fun_offset = INSTR_A(instr);
        Function *fun = *(Function **)(fp + fun_offset);
        fp += fun_offset;
        ip = fun->code; /* tail call means we just overwrite these, and don't grow the call stack */
        DISPATCH_NEXT();
    }
    DISPATCH_CASE(OP_CALL) {
        uint32_t fun_offset = INSTR_A(instr);
        //uint32_t result_reg = INSTR_B(instr);
        Function *fun = *(Function **)(fp + fun_offset);
        interpret(fun->code, fp + fun_offset); /* just use the C call stack */
        //fp[result_reg] = fp[fun_offset];
        DISPATCH_NEXT();
    }
    DISPATCH_CASE(OP_RET) return; /* since we use the C call stack we just return */

    FOR_ALL_PRIM_MOVE(DEFINE_MOVE_INSTR)
    DISPATCH_CASE(OP_NOT_BOOL) *(bool *)(fp + INSTR_A(instr)) = !*(bool *)(fp + INSTR_B(instr)); DISPATCH_NEXT();
    FOR_ALL_PRIM_32(DEFINE_LIT_32_INSTR)
    FOR_ALL_PRIM_64(DEFINE_LIT_64_INSTR)
    FOR_ALL_INT(DEFINE_INC_INSTR)
    FOR_ALL_INT(DEFINE_DEC_INSTR)
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
    FOR_ALL_BASIC(DEFINE_TO_ANY_INSTR)

    DISPATCH_CASE(OP_CALL_BUILTIN_1_VOID) {
        (*(FunPtr1 *)ip++)(*(Any *)(fp + INSTR_A(instr)));
        DISPATCH_NEXT();
    }
    DISPATCH_CASE(OP_CALL_BUILTIN_2_VOID) {
        (*(FunPtr2 *)ip++)(*(Any *)(fp + INSTR_A(instr)), *(Any *)(fp + INSTR_B(instr)));
        DISPATCH_NEXT();
    }
    DISPATCH_CASE(OP_CALL_BUILTIN_1) {
        *(Any *)(fp + INSTR_A(instr)) = (*(FunPtr1 *)ip++)(*(Any *)(fp + INSTR_B(instr)));
        DISPATCH_NEXT();
    }
    DISPATCH_CASE(OP_CALL_BUILTIN_2) {
        *(Any *)(fp + INSTR_A(instr)) = (*(FunPtr2 *)ip++)(*(Any *)(fp + INSTR_B(instr)), *(Any *)(fp + INSTR_C(instr)));
        DISPATCH_NEXT();
    }

    END_DISPATCH();
}
