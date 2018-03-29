#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>
#include <assert.h>
#include "fnv.h"
#include "murmur3.h"


typedef union Word Word;
typedef struct Any Any;
typedef struct Type Type;
typedef struct Cons Cons;
typedef struct U8Array U8Array;
typedef U8Array Symbol;
typedef U8Array String;

typedef Any(*FunPtr0)(void);
typedef Any(*FunPtr1)(Any a);
typedef Any(*FunPtr2)(Any a, Any b);
typedef Any(*FunPtr3)(Any a, Any b, Any c);

union Word {
    struct {
        uint64_t op : 8;
        uint64_t extra : 56;
    } i;

    bool b32;

    uint8_t u8;
    uint16_t u16;
    uint32_t u32;
    uint64_t u64;

    int8_t i8;
    int16_t i16;
    int32_t i32;
    int64_t i64;

    float f32;
    double f64;

    Type *type;

    void *ptr;
    char *char_ptr;
    const Symbol *symbol_ptr;
    Cons *cons_ptr;
    U8Array *u8_array_ptr;

    FunPtr0 fun0;
    FunPtr1 fun1;
    FunPtr2 fun2;
    FunPtr3 fun3;
};


struct Any {
    union {
        const Type *type;
        uintptr_t bits;
    } t;
    Word val;
};


#define ANY_KIND_BITS 5
#define ANY_KIND(any) ((any).t.bits & ANY_KIND_BITS)
#define ANY_TYPE(any) (ANY_KIND(any) != KIND_REF_SLICE ? (const Type *)((any).t.bits & ~ANY_KIND_BITS) : NULL)

enum {
    KIND_ANY,

    KIND_UNIT,
    KIND_BOOL,
    KIND_U8,
    KIND_U16,
    KIND_U32,
    KIND_U64,
    KIND_I8,
    KIND_I16,
    KIND_I32,
    KIND_I64,
    KIND_F32,
    KIND_F64,
    KIND_PTR,

    /* regular pointer, but the pointed to value is preceded by a 32 bit reference count */
    KIND_REF,

    /* regular pointer to ref-counted array.
       specially handled by storing offset/length in the Any type field.
       so only the kind is available from there, and the type pointer must be
       retrieved from the boxed array object */
    KIND_REF_SLICE,

    /* ARRAYs and STRUCTs must be wrapped in a PTR or REF to be passed as Any,
       if they are larger than 8 bytes. */
    KIND_ARRAY,
    KIND_STRUCT,

    KIND_BUILTIN,
    KIND_FUN,
};

#define IS_UNSIGNED_KIND(kind) ((kind) >= KIND_U8 && (kind) <= KIND_U64)
#define IS_SIGNED_KIND(kind) ((kind) >= KIND_I8 && (kind) <= KIND_I64)
#define IS_INTEGRAL_KIND(kind) ((kind) >= KIND_U8 && (kind) <= KIND_I64)
#define IS_REAL_KIND(kind) ((kind) == KIND_F32 || (kind) == KIND_F64)
#define IS_PTR_KIND(kind) ((kind) == KIND_PTR || (kind) == KIND_REF)

enum {
    TYPE_FLAG_UNSIZED
};

typedef struct StructField StructField;
typedef struct StructFieldArray StructFieldArray;

typedef struct FunParam FunParam;
typedef struct FunParamArray FunParamArray;

struct Type {
    uint32_t kind;
    uint32_t flags;
    uint32_t size;

    uint32_t salt; /* used to make sure we can get a unique hash for each type */
    uint32_t hash; /* hash of this type. this field is not itself hashed! */

    const Type *target; /* for PTR, REF and ARRAY types */
    StructFieldArray *fields;
    FunParamArray *params;
};

struct FunParam {
    const Type *type;
};
struct FunParamArray {
    uint32_t length;
    FunParam params[];
};

struct StructField {
    const Symbol *name;
    const Type *type;
    uint32_t offset;
};
struct StructFieldArray {
    uint32_t length;
    StructField fields[];
};


struct Cons {
    Any car;
    Any cdr;
};

struct U8Array {
    uint32_t length;
    uint8_t data[];
};

typedef struct Function Function;
struct Function {
    Word *code;
};


const Type *type_type;
const Type *type_ptr_type;

const Type *type_any;

const Type *type_unit;
const Type *type_b32;

const Type *type_u8;
const Type *type_u16;
const Type *type_u32;
const Type *type_u64;

const Type *type_i8;
const Type *type_i16;
const Type *type_i32;
const Type *type_i64;

const Type *type_f32;
const Type *type_f64;

const Type *type_ptr_symbol;
const Type *type_ref_string;
const Type *type_ref_cons;

const Symbol *symbol_if;
const Symbol *symbol_let;
const Symbol *symbol_quote;
const Symbol *symbol_fun;
const Symbol *symbol_def;
const Symbol *symbol_do;
const Symbol *symbol_plus;
const Symbol *symbol_minus;
const Symbol *symbol_mul;
const Symbol *symbol_div;
const Symbol *symbol_not;
const Symbol *symbol_eq;
const Symbol *symbol_lt;
const Symbol *symbol_gt;
const Symbol *symbol_lteq;
const Symbol *symbol_gteq;


#define ANY_UNIT ((Any) { .t.type = type_unit })
#define ANY_TRUE ((Any) { .t.type = type_b32, .val.b32 = true })
#define ANY_FALSE ((Any) { .t.type = type_b32, .val.b32 = false })

#define MAKE_ANY_U8(x) ((Any) { .t.type = type_u8, .val.u8 = (x) })
#define MAKE_ANY_U16(x) ((Any) { .t.type = type_u16, .val.u16 = (x) })
#define MAKE_ANY_U32(x) ((Any) { .t.type = type_u32, .val.u32 = (x) })
#define MAKE_ANY_U64(x) ((Any) { .t.type = type_u64, .val.u64 = (x) })

#define MAKE_ANY_I8(x) ((Any) { .t.type = type_i8, .val.i8 = (x) })
#define MAKE_ANY_I16(x) ((Any) { .t.type = type_i16, .val.i16 = (x) })
#define MAKE_ANY_I32(x) ((Any) { .t.type = type_i32, .val.i32 = (x) })
#define MAKE_ANY_I64(x) ((Any) { .t.type = type_i64, .val.i64 = (x) })

#define MAKE_ANY_F32(x) ((Any) { .t.type = type_f32, .val.f32 = (x) })
#define MAKE_ANY_F64(x) ((Any) { .t.type = type_f64, .val.f64 = (x) })

#define MAKE_ANY_TYPE(x) ((Any) { .t.type = type_ptr_type, .val.type = (x) })

#define REFCOUNT(ptr) (((uint32_t *)(ptr))[-1])
#define MAYBE_ADDREF(any) (ANY_KIND(any) == KIND_REF && ++REFCOUNT((any).val.ptr))

uint64_t to_u64(Any num) {
    switch (ANY_KIND(num)) {
    case KIND_U8: return num.val.u8;
    case KIND_U16: return num.val.u16;
    case KIND_U32: return num.val.u32;
    case KIND_U64: return num.val.u64;
    case KIND_I8: assert(num.val.i8 >= 0); return num.val.i8;
    case KIND_I16: assert(num.val.i16 >= 0); return num.val.i16;
    case KIND_I32: assert(num.val.i32 >= 0); return num.val.i32;
    case KIND_I64: assert(num.val.i64 >= 0); return num.val.i64;
    default: assert(0 && "non-integral value"); return 0;
    }
}

uint32_t to_u32(Any num) {
    switch (ANY_KIND(num)) {
    case KIND_U8: return num.val.u8;
    case KIND_U16: return num.val.u16;
    case KIND_U32: return num.val.u32;
    case KIND_U64: assert(num.val.u64 <= UINT32_MAX); return (uint32_t)num.val.u64;
    case KIND_I8: assert(num.val.i8 >= 0); return num.val.i8;
    case KIND_I16: assert(num.val.i16 >= 0); return num.val.i16;
    case KIND_I32: assert(num.val.i32 >= 0); return num.val.i32;
    case KIND_I64: assert(num.val.i64 >= 0 && num.val.i64 <= (int64_t)UINT32_MAX); return (uint32_t)num.val.i64;
    default: assert(0 && "non-integral value"); return 0;
    }
}

Any to_any(void *ptr, const Type *type) {
    switch (type->kind) {
    case KIND_ANY:  return *(Any *)ptr;
    case KIND_UNIT: return ANY_UNIT;
    case KIND_BOOL: return *(bool *)ptr ? ANY_TRUE : ANY_FALSE;
    case KIND_U8:   return MAKE_ANY_U8(*(uint8_t *)ptr);
    case KIND_U16:  return MAKE_ANY_U16(*(uint16_t *)ptr);
    case KIND_U32:  return MAKE_ANY_U32(*(uint32_t *)ptr);
    case KIND_U64:  return MAKE_ANY_U64(*(uint64_t *)ptr);
    case KIND_I8:   return MAKE_ANY_I8(*(int8_t *)ptr);
    case KIND_I16:  return MAKE_ANY_I16(*(int16_t *)ptr);
    case KIND_I32:  return MAKE_ANY_I32(*(int32_t *)ptr);
    case KIND_I64:  return MAKE_ANY_I64(*(int64_t *)ptr);
    case KIND_F32:  return MAKE_ANY_F32(*(float *)ptr);
    case KIND_F64:  return MAKE_ANY_F64(*(double *)ptr);
    case KIND_PTR:
    case KIND_REF:
        return (Any) { .t.type = type, .val.ptr = *(void **)ptr };
    }
}

void from_any(Any any, void *dst) {
    switch (ANY_KIND(any)) {
    case KIND_ANY:  *(Any *)dst = any; break;
    case KIND_UNIT: break; /* zero size, so no write */
    case KIND_BOOL: *(bool     *)dst = any.val.b32; break;
    case KIND_U8:   *(uint8_t  *)dst = any.val.u8;  break;
    case KIND_U16:  *(uint16_t *)dst = any.val.u16; break;
    case KIND_U32:  *(uint32_t *)dst = any.val.u32; break;
    case KIND_U64:  *(uint64_t *)dst = any.val.u64; break;
    case KIND_I8:   *(int8_t   *)dst = any.val.u8;  break;
    case KIND_I16:  *(int16_t  *)dst = any.val.u16; break;
    case KIND_I32:  *(int32_t  *)dst = any.val.u32; break;
    case KIND_I64:  *(int64_t  *)dst = any.val.u64; break;
    case KIND_F32:  *(float    *)dst = any.val.f32; break;
    case KIND_F64:  *(double   *)dst = any.val.f64; break;
    case KIND_PTR:
    case KIND_REF:
        *(void **)dst = any.val.ptr; break;
    }
}

void *rc_alloc(uint32_t size) {
    uint32_t *rc_ptr = calloc(1, sizeof(uint32_t) + size);
    return rc_ptr + 1;
}

Any string(const char *str) {
    uint32_t len = strlen(str);
    U8Array *s = rc_alloc(sizeof(U8Array) + len + 1);
    s->length = len;
    memcpy(s->data, str, len + 1);
    return (Any) { .t.type = type_ref_string, .val.u8_array_ptr = s };
}

Any cons(Any car, Any cdr) {
    Cons *c = rc_alloc(sizeof(Cons));
    c->car = car;
    c->cdr = cdr;
    MAYBE_ADDREF(car);
    MAYBE_ADDREF(cdr);
    return (Any) { .t.type = type_ref_cons, .val.cons_ptr = c };
}

Any car(Any cons) {
    assert(ANY_TYPE(cons) == type_ref_cons);
    return cons.val.cons_ptr->car;
}

Any cdr(Any cons) {
    assert(ANY_TYPE(cons) == type_ref_cons);
    return cons.val.cons_ptr->cdr;
}

uint32_t list_length(Any lst) {
    uint32_t len = 0;
    while (ANY_KIND(lst) != KIND_UNIT) {
        ++len;
        lst = cdr(lst);
    }
    return len;
}

Any array_length(Any arr) {
    assert(IS_PTR_KIND(ANY_KIND(arr)));
    
    const Type *arr_type = ANY_TYPE(arr)->target;
    assert(arr_type);
    assert(arr_type->kind == KIND_ARRAY);
    
    const Type *value_type = arr_type->target;
    assert(value_type);

    if (arr_type->size < 0) {
        return MAKE_ANY_U64(*(uint32_t *)arr.val.ptr);
    }
    else {
        return MAKE_ANY_U64(arr_type->size / value_type->size);
    }
}

Any array_get(Any arr, Any idx) {
    uint32_t i = to_u32(idx);

    assert(IS_PTR_KIND(ANY_KIND(arr)));
    
    const Type *arr_type = ANY_TYPE(arr)->target;
    assert(arr_type);
    assert(arr_type->kind == KIND_ARRAY);

    const Type *value_type = arr_type->target;
    assert(value_type);

    if (arr_type->flags & TYPE_FLAG_UNSIZED) {
        uint32_t len = *(uint32_t *)arr.val.ptr;
        assert(i < len);
        return to_any(arr.val.char_ptr + sizeof(uint32_t) + i * value_type->size, value_type);
    }
    else {
        uint32_t len = arr_type->size / value_type->size;
        assert(i < len);
        return to_any((char *)arr.val.ptr + i * value_type->size, value_type);
    }
}

Any array_set(Any arr, Any idx, Any val) {
    uint32_t i = to_u32(idx);

    assert(IS_PTR_KIND(ANY_KIND(arr)));

    const Type *arr_type = ANY_TYPE(arr)->target;
    assert(arr_type);
    assert(arr_type->kind == KIND_ARRAY);

    const Type *value_type = arr_type->target;
    assert(value_type);

    if (arr_type->flags & TYPE_FLAG_UNSIZED) {
        uint32_t len = *(uint32_t *)arr.val.ptr;
        void *ptr = arr.val.char_ptr + sizeof(uint32_t) + i * value_type->size;
        assert(i < len);
        from_any(val, ptr);
    }
    else {
        uint32_t len = arr_type->size / value_type->size;
        void *ptr = (char *)arr.val.ptr + i * value_type->size;
        assert(i < len);
        from_any(val, ptr);
    }

    return ANY_UNIT;
}



#define FOR_ALL_UNSIGNED_TYPES(X) \
    X(U8, u8) \
    X(U16, u16) \
    X(U32, u32) \
    X(U64, u64) 

#define FOR_ALL_SIGNED_TYPES(X) \
    X(I8, i8) \
    X(I16, i16) \
    X(I32, i32) \
    X(I64, i64)

#define FOR_ALL_REAL_TYPES(X) \
    X(F32, f32) \
    X(F64, f64)

#define FOR_ALL_INT_TYPES(X) \
    FOR_ALL_UNSIGNED_TYPES(X) \
    FOR_ALL_SIGNED_TYPES(X)

#define FOR_ALL_NUM_TYPES(X) \
    FOR_ALL_INT_TYPES(X) \
    FOR_ALL_REAL_TYPES(X)

#define FOR_ALL_PRIM_TYPES(X) \
    FOR_ALL_NUM_TYPES(X) \
    X(BOOL, b32)


typedef struct VMCtx VMCtx;

#define DEFINE_ADD_OP_ENUMS(TYP, typ) OP_ADD_ ## TYP,
#define DEFINE_SUB_OP_ENUMS(TYP, typ) OP_SUB_ ## TYP,
#define DEFINE_MUL_OP_ENUMS(TYP, typ) OP_MUL_ ## TYP,
#define DEFINE_DIV_OP_ENUMS(TYP, typ) OP_DIV_ ## TYP,

enum {
    OP_LABEL, /* labels are eliminated during compilation */

    /* these specify a label. they will be replaced by the below */
    OP_JUMP_LABEL,
    OP_JFALSE_LABEL,
    OP_JTRUE_LABEL,

    /* these specify a relative instruction pointer offset. they replace the above */
    OP_JUMP,
    OP_JFALSE,
    OP_JTRUE,

    OP_TCALL, /* tail call. can be used in tail position instead of RET */
    OP_CALL,
    OP_RET,

    /* callable Any -> Any builtin C function */
    OP_CALL_BUILTIN_0,
    OP_CALL_BUILTIN_1,
    OP_CALL_BUILTIN_2,
    OP_CALL_BUILTIN_3,

    OP_DUP,
    OP_PRINT,

    /* copy literal data to the top of the stack */
    OP_LIT_7_BYTES,
    OP_LIT_1_WORD,
    OP_LIT_N_WORDS,

    /* copy arguments/local variables to the top of the stack */
    OP_LOCAL_1_WORD,
    OP_LOCAL_N_WORDS,

    OP_UNIT_TO_ANY,
    OP_BOOL_TO_ANY,
    OP_U8_TO_ANY,
    OP_U16_TO_ANY,
    OP_U32_TO_ANY,
    OP_U64_TO_ANY,
    OP_I8_TO_ANY,
    OP_I16_TO_ANY,
    OP_I32_TO_ANY,
    OP_I64_TO_ANY,
    OP_F32_TO_ANY,
    OP_F64_TO_ANY,

    OP_ANY_TO_UNIT,
    OP_ANY_TO_BOOL,
    OP_ANY_TO_U8,
    OP_ANY_TO_U16,
    OP_ANY_TO_U32,
    OP_ANY_TO_U64,
    OP_ANY_TO_I8,
    OP_ANY_TO_I16,
    OP_ANY_TO_I32,
    OP_ANY_TO_I64,
    OP_ANY_TO_F32,
    OP_ANY_TO_F64,

    FOR_ALL_NUM_TYPES(DEFINE_ADD_OP_ENUMS)
    FOR_ALL_NUM_TYPES(DEFINE_SUB_OP_ENUMS)
    FOR_ALL_NUM_TYPES(DEFINE_MUL_OP_ENUMS)
    FOR_ALL_NUM_TYPES(DEFINE_DIV_OP_ENUMS)
};


struct VMCtx {
    Word *stack;
    Word *sp;
};


#define MAX_STACK 4096
#define STACK_START (vmcx->stack)
#define STACK_END (STACK_START + MAX_STACK)

#define INSTR(Op) ((Word) { .i.op = (Op) })
#define INSTR_WITH_EXTRA(Op, Extra) ((Word) { .i.op = (Op), .i.extra = (Extra) })
#define INSTR_OP(instr) ((instr).i.op)
#define INSTR_I32(instr) ((int32_t)(instr).i.extra)
#define INSTR_U32(instr) ((uint32_t)(instr).i.extra)
#define INSTR_U64(instr) ((instr).i.extra)


#define DEFINE_TO_ANY_OP(TYP, typ) \
    case OP_ ## TYP ## _TO_ANY: \
        *(Any *)(sp - 1) = (Any) { .t.type = type_ ## typ, .val.typ = sp[-1].typ }; \
        ++sp; \
        continue;

#define DEFINE_FROM_ANY_OP(TYP, typ) \
    case OP_ANY_TO_ ## TYP: \
        assert(type_ ## typ == ANY_TYPE(*(Any *)(sp - 1))); \
        sp[-2].typ = ((Any *)(sp - 1))->val.typ; \
        --sp; \
        continue;

#define DEFINE_ADD_OP(TYP, typ) case OP_ADD_ ## TYP: sp[-2].typ += sp[-1].typ; --sp; continue;
#define DEFINE_SUB_OP(TYP, typ) case OP_SUB_ ## TYP: sp[-2].typ -= sp[-1].typ; --sp; continue;
#define DEFINE_MUL_OP(TYP, typ) case OP_MUL_ ## TYP: sp[-2].typ *= sp[-1].typ; --sp; continue;
#define DEFINE_DIV_OP(TYP, typ) case OP_DIV_ ## TYP: sp[-2].typ /= sp[-1].typ; --sp; continue;

void call(VMCtx *vmcx, Function *fun) {
    Word *sp = vmcx->sp; /* stack pointer */
    Word *fp = sp; /* frame pointer (used to address arguments) */
    Word *ip = fun->code; /* instruction pointer */

    while (true) {
        Word instr = *ip++;

        switch (INSTR_OP(instr)) {
        case OP_JUMP:
            ip += INSTR_I32(instr) - 1;
            continue;
        case OP_JFALSE:
            assert(sp > STACK_START);
            if (!sp[-1].u64) {
                ip += INSTR_I32(instr) - 1;
            }
            continue;
        case OP_JTRUE:
            assert(sp > STACK_START);
            if (sp[-1].u64) {
                ip += INSTR_I32(instr) - 1;
            }
            continue;
        case OP_TCALL: {
            Function *fun = *(Function **)(sp - 1);
            fp = ++sp;
            ip = fun->code; /* tail call means we just overwrite these, and don't grow the call stack */
            continue;
        }
        case OP_CALL: {
            Function *fun = *(Function **)(sp - 1);
            vmcx->sp = ++sp;
            call(vmcx, fun); /* just use the C call stack */
            sp = vmcx->sp;
            continue;
        }
        case OP_RET:
            vmcx->sp = sp;
            return; /* since we juse the C call stack we just return */

        case OP_CALL_BUILTIN_0: *(Any *)(sp) = sp->fun0(); ++sp; continue;
        case OP_CALL_BUILTIN_1: *(Any *)(sp - 2) = sp->fun1(*(Any *)(sp - 2)); --sp; continue;
        case OP_CALL_BUILTIN_2: *(Any *)(sp - 4) = sp->fun2(*(Any *)(sp - 4), *(Any *)(sp - 2)); sp -= 3; continue;
        case OP_CALL_BUILTIN_3: *(Any *)(sp - 6) = sp->fun3(*(Any *)(sp - 6), *(Any *)(sp - 4), *(Any *)(sp - 2)); sp -= 5; continue;

        case OP_DUP: *sp++ = sp[-1]; continue;
        case OP_PRINT: --sp; printf("%llu\n", sp->u64); continue;

        case OP_LIT_7_BYTES:
            assert(sp < STACK_END);
            sp->u64 = INSTR_U64(instr); /* 7 bytes encoded in op */
            ++sp;
            continue;
        case OP_LIT_1_WORD:
            assert(sp < STACK_END);
            *sp++ = *ip++; /* 1 word following op */
            continue;
        case OP_LIT_N_WORDS: {
            uint32_t n = INSTR_U32(instr); /* word count encoded in op */
            assert(sp + n < STACK_END);
            memcpy(sp, ip, n * sizeof(Word)); /* copy N words following op */
            sp += n;
            ip += n;
            continue;
        }

        case OP_LOCAL_1_WORD:
            *sp++ = *(fp - INSTR_U64(instr)); /* frame pointer offset is encoded in op */
            continue;
        case OP_LOCAL_N_WORDS: {
            uint32_t n = INSTR_U32(instr); /* word count encoded in op */
            memcpy(sp, fp - ip->i64, n * sizeof(Word)); /* copy N words from call frame (frame pointer offset following op) */
            ++ip;
            sp += n;
            continue;
        }

        case OP_UNIT_TO_ANY: *(Any *)(sp) = ANY_UNIT; ++sp; continue;
        FOR_ALL_PRIM_TYPES(DEFINE_TO_ANY_OP)
        FOR_ALL_PRIM_TYPES(DEFINE_FROM_ANY_OP)
        FOR_ALL_NUM_TYPES(DEFINE_ADD_OP)
        FOR_ALL_NUM_TYPES(DEFINE_SUB_OP)
        FOR_ALL_NUM_TYPES(DEFINE_MUL_OP)
        FOR_ALL_NUM_TYPES(DEFINE_DIV_OP)

        default:
            assert(0 && "bad opcode");
        }
    }

    assert(0 && "shouldn't get here");
}




typedef struct CompilerCtx CompilerCtx;

struct CompilerCtx {
    Word *code;
    uint32_t code_used;
    uint32_t code_capacity;

    uint32_t label_counter;
};


static uint32_t gen_label(CompilerCtx *cctx) {
    assert(cctx->label_counter < (1 << 24));
    return cctx->label_counter++;
}

static void emit(CompilerCtx *cctx, Word word) {
    if (cctx->code_used == cctx->code_capacity) {
        cctx->code_capacity = cctx->code_capacity ? cctx->code_capacity * 2 : 128;
        cctx->code = realloc(cctx->code, cctx->code_capacity * sizeof(Word));
    }
    cctx->code[cctx->code_used++] = word;
}

static void emit_words(CompilerCtx *cctx, Word *words, uint32_t count) {
    while (cctx->code_used + count > cctx->code_capacity) {
        cctx->code_capacity = cctx->code_capacity ? cctx->code_capacity * 2 : 128;
        cctx->code = realloc(cctx->code, cctx->code_capacity * sizeof(Word));
    }
    memcpy(cctx->code + cctx->code_used, words, count * sizeof(Word));
    cctx->code_used += count;
}

static void emit_u64(CompilerCtx *cctx, uint64_t word) {
    emit(cctx, (Word) { .u64 = word });
}

static void emit_label(CompilerCtx *cctx, uint32_t label) {
    assert(label < (1 << 24));
    emit(cctx, INSTR_WITH_EXTRA(OP_LABEL, label));
}

static void emit_jump(CompilerCtx *cctx, uint32_t label) {
    assert(label < (1 << 24));
    emit(cctx, INSTR_WITH_EXTRA(OP_JUMP_LABEL, label));
}

static void emit_jfalse(CompilerCtx *cctx, uint32_t label) {
    assert(label < (1 << 24));
    emit(cctx, INSTR_WITH_EXTRA(OP_JFALSE_LABEL, label));
}

static void emit_jtrue(CompilerCtx *cctx, uint32_t label) {
    assert(label < (1 << 24));
    emit(cctx, INSTR_WITH_EXTRA(OP_JTRUE_LABEL, label));
}

static void emit_lit_7_bytes(CompilerCtx *cctx, uint64_t word) {
    assert(word < (1 << 24));
    emit(cctx, INSTR_WITH_EXTRA(OP_LIT_7_BYTES, word));
}

static void emit_lit_1_word(CompilerCtx *cctx, uint64_t word) {
    emit(cctx, INSTR(OP_LIT_1_WORD));
    emit_u64(cctx, word);
}

static uint32_t instr_word_count(Word instr) {
    switch (INSTR_OP(instr)) {
    case OP_TCALL:
    case OP_CALL:
    case OP_LIT_1_WORD:
    case OP_LOCAL_N_WORDS:
        return 2;
    case OP_LIT_N_WORDS:
        return 1 + INSTR_U32(instr);
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

    uint32_t i = 0;
    Word *code = cctx->code;
    uint32_t count = cctx->code_used;

    cctx->code = NULL;
    cctx->code_used = 0;
    cctx->code_capacity = 0;

    while (i < count) {
        Word instr = code[i];

        if (INSTR_OP(instr) == OP_LABEL) {
            U32Map_put(&label_offsets, INSTR_U32(instr), cctx->code_used);
            ++i;
        }
        else {
            uint32_t n = instr_word_count(instr);
            emit_words(cctx, code + i, n);
            i += n;
        }
    }

    free(code);

    i = 0;
    code = cctx->code;
    count = cctx->code_used;

    while (i < count) {
        Word instr = code[i];
        uint32_t new_op;

        switch (INSTR_OP(instr)) {
        case OP_JUMP_LABEL:
            new_op = OP_JUMP;
            break;
        case OP_JFALSE_LABEL:
            new_op = OP_JFALSE;
            break;
        case OP_JTRUE_LABEL:
            new_op = OP_JTRUE;
            break;
        default:
            i += instr_word_count(instr);
            continue;
        }

        uint32_t offset;
        if (!U32Map_get(&label_offsets, INSTR_U32(instr), &offset)) {
            assert(0 && "label unexpectedly not found");
        }
        assert(offset < (1 << 24));
        int32_t relative_offset = (int32_t)offset - i;
        assert(relative_offset);
        code[i++] = INSTR_WITH_EXTRA(new_op, (uint64_t)relative_offset);
    }

    U32Map_free(&label_offsets);
}

#define DEFINE_PRINT_TO_ANY_OP(TYP, typ) case OP_ ## TYP ## _TO_ANY: printf(#typ "_to_any\n"); break;
#define DEFINE_PRINT_FROM_ANY_OP(TYP, typ) case OP_ANY_TO_ ## TYP: printf("any_to_" #typ "\n"); break;
#define DEFINE_PRINT_ADD_OP(TYP, typ) case OP_ADD_ ## TYP: printf("add_" #typ "\n"); break;
#define DEFINE_PRINT_SUB_OP(TYP, typ) case OP_SUB_ ## TYP: printf("sub_" #typ "\n"); break;
#define DEFINE_PRINT_MUL_OP(TYP, typ) case OP_MUL_ ## TYP: printf("mul_" #typ "\n"); break;
#define DEFINE_PRINT_DIV_OP(TYP, typ) case OP_DIV_ ## TYP: printf("div_" #typ "\n"); break;

static void print_code(CompilerCtx *cctx) {
    uint32_t i = 0;
    Word *code = cctx->code;
    uint32_t count = cctx->code_used;

    while (i < count) {
        Word instr = code[i];

        switch (INSTR_OP(instr)) {
        case OP_LABEL: printf("label %u\n", INSTR_U32(instr)); break;
        case OP_JUMP_LABEL: printf("jump_label %u\n", INSTR_U32(instr)); break;
        case OP_JFALSE_LABEL: printf("jfalse_label %u\n", INSTR_U32(instr)); break;
        case OP_JTRUE_LABEL: printf("jtrue_label %u\n", INSTR_U32(instr)); break;
        case OP_JUMP: printf("jump %d\n", INSTR_I32(instr)); break;
        case OP_JFALSE: printf("jfalse %d\n", INSTR_I32(instr)); break;
        case OP_JTRUE: printf("jtrue %d\n", INSTR_I32(instr)); break;
        case OP_TCALL: printf("tcall\n"); break;
        case OP_CALL: printf("call\n"); break;
        case OP_RET: printf("ret\n"); break;

        case OP_DUP: printf("dup\n"); break;
        case OP_PRINT: printf("print\n"); break;

        case OP_LIT_7_BYTES: printf("lit %llu\n", INSTR_U64(instr)); break;
        case OP_LIT_1_WORD: printf("lit %llu\n", code[i + 1].u64); break;
        case OP_LIT_N_WORDS: printf("litn %u ...\n", INSTR_U32(instr)); break;

        case OP_LOCAL_1_WORD: printf("local %u\n", INSTR_U32(instr)); break;
        case OP_LOCAL_N_WORDS: printf("localn %u@%llu\n", INSTR_U32(instr), code[i + 1].u64); break;

        case OP_UNIT_TO_ANY: printf("unit_to_any\n"); break;
        case OP_ANY_TO_UNIT: printf("any_to_unit\n"); break;
        FOR_ALL_NUM_TYPES(DEFINE_PRINT_TO_ANY_OP)
        FOR_ALL_PRIM_TYPES(DEFINE_PRINT_FROM_ANY_OP)
        FOR_ALL_NUM_TYPES(DEFINE_PRINT_ADD_OP)
        FOR_ALL_NUM_TYPES(DEFINE_PRINT_SUB_OP)
        FOR_ALL_NUM_TYPES(DEFINE_PRINT_MUL_OP)
        FOR_ALL_NUM_TYPES(DEFINE_PRINT_DIV_OP)

        }

        i += instr_word_count(instr);
    }
}

typedef struct CodeSegment CodeSegment;
struct CodeSegment {
    uint32_t length;
    Word code[];
};

static CodeSegment *cut_code_from(CompilerCtx *cctx, uint32_t from_index) {
    uint32_t len = cctx->code_used - from_index;
    CodeSegment *seg = malloc(sizeof(CodeSegment) + sizeof(Word) * len);
    seg->length = len;
    memcpy(seg->code, cctx->code + from_index, sizeof(Word) * len);
    cctx->code_used = from_index;
    return seg;
}

static void paste_code(CompilerCtx *cctx, CodeSegment *seg) {
    emit_words(cctx, seg->code, seg->length);
}

const Type *compile(CompilerCtx *cctx, Any form, const Type *coerce_to) {
    switch (ANY_KIND(form)) {
    case KIND_UNIT:
        return ANY_TYPE(form);
    case KIND_BOOL:
        emit_lit_7_bytes(cctx, form.val.b32);
        return ANY_TYPE(form);
    case KIND_U8:
    case KIND_I8:
        emit_lit_7_bytes(cctx, form.val.u8);
        return ANY_TYPE(form);
    case KIND_U16:
    case KIND_I16:
        emit_lit_7_bytes(cctx, form.val.u16);
        return ANY_TYPE(form);
    case KIND_U32:
    case KIND_I32:
    case KIND_F32:
        emit_lit_1_word(cctx, form.val.u32);
        return ANY_TYPE(form);
    case KIND_U64:
    case KIND_I64:
    case KIND_F64:
        emit_lit_1_word(cctx, form.val.u64);
        return ANY_TYPE(form);
    case KIND_STRUCT: {
        if (ANY_TYPE(form) == type_ref_cons) {
            Any head = car(form);

            if (ANY_TYPE(head) == type_ptr_symbol) {
                if (head.val.symbol_ptr == symbol_if) {
                    uint32_t else_label = gen_label(cctx);
                    uint32_t end_label = gen_label(cctx);

                    Any temp = cdr(form);
                    const Type *cond_type = compile(cctx, car(temp), type_b32);
                    if (cond_type->kind == KIND_ANY) {
                        emit_u64(cctx, OP_ANY_TO_BOOL);
                    }
                    else {
                        assert(cond_type->kind == KIND_BOOL);
                    }
                    emit_jfalse(cctx, else_label);

                    temp = cdr(temp);
                    const Type *then_type = compile(cctx, car(temp), NULL);
                    emit_jump(cctx, end_label);

                    emit_u64(cctx, else_label);
                    temp = cdr(temp);
                    const Type *else_type = compile(cctx, car(temp), then_type);

                    emit_u64(cctx, end_label);

                    temp = cdr(temp);
                    assert(ANY_KIND(temp) == KIND_UNIT);
                    assert(then_type == else_type);
                    return then_type;
                }
            }

            /* call */

            uint32_t mark = cctx->code_used;
            const Type *fun_type = compile(cctx, head, NULL);
            CodeSegment *callable_seg = cut_code_from(cctx, mark);

            Any args = cdr(form);
            uint32_t arg_count = list_length(args);
            if (fun_type->params) {
                assert(arg_count == fun_type->params->length);
            }

            uint32_t i = 0;
            while (ANY_KIND(args) != KIND_UNIT) {
                const Type *wanted_type = fun_type->params ? fun_type->params->params[i].type : type_any;
                
                const Type *arg_type = compile(cctx, car(args), wanted_type);
                args = cdr(args);
                ++i;
            }

            paste_code(cctx, callable_seg);
            free(callable_seg);

            if (fun_type->kind == KIND_BUILTIN) {
                assert(arg_count == fun_type->params->length);
                assert(arg_count <= 3);
                emit_u64(cctx, OP_CALL_BUILTIN_0 + arg_count);
            }
            else if (fun_type->kind == KIND_FUN) {
                emit_u64(cctx, OP_CALL);
            }
        }
    }
        
    }
}


typedef struct StrSlice StrSlice;
struct StrSlice {
    const char *str;
    uint32_t length;
};


uint32_t StrSlice_hash(StrSlice s) {
    uint32_t result;
    MurmurHash3_x86_32(s.str, s.length, 0, &result);
    return result;
}

bool StrSlice_equal(StrSlice a, StrSlice b) {
    return a.length == b.length && !memcmp(a.str, b.str, a.length);
}

#define EXPAND_INTERFACE
#define EXPAND_IMPLEMENTATION
#define NAME SymMap
#define KEY_TYPE StrSlice
#define VALUE_TYPE const Symbol *
#define HASH_FUNC(x) StrSlice_hash(x)
#define EQUAL_FUNC(x, y) StrSlice_equal(x, y)
#include "hashtable.h"
typedef struct SymMap SymMap;

static SymMap symbolmap;

const Symbol *intern_symbol(const char *str, uint32_t length) {
    StrSlice slice = { str, length };
    {
        const Symbol *sym;
        if (SymMap_get(&symbolmap, slice, &sym)) {
            return sym;
        }
    }
    Symbol *sym = malloc(sizeof(U8Array) + length + 1);
    sym->length = length;
    memcpy(sym->data, str, length);
    sym->data[length] = 0;
    slice.str = sym->data;
    SymMap_put(&symbolmap, slice, sym);
    return sym;
}

const Symbol *intern_symbol_str(const char *str) {
    return intern_symbol(str, strlen(str));
}



static bool type_equal(const Type *a, const Type *b) {
    if (a->kind != b->kind) {
        return false;
    }
    if (a->flags != b->flags) {
        return false;
    }
    if (a->size != b->size) {
        return false;
    }
    if (a->target != b->target) {
        return false;
    }
    if (!!a->fields != !!b->fields) {
        return false;
    }
    if (a->fields) {
        if (a->fields->length != b->fields->length) {
            return false;
        }
        for (uint32_t i = 0; i < a->fields->length; ++i) {
            StructField *fa = a->fields->fields + i;
            StructField *fb = b->fields->fields + i;
            if (fa->name != fb->name) {
                return false;
            }
            if (fa->type != fb->type) {
                return false;
            }
            if (fa->offset != fb->offset) {
                return false;
            }
        }
    }
    if (!!a->params != !!b->params) {
        return false;
    }
    if (a->params) {
        if (a->params->length != b->params->length) {
            return false;
        }
        for (uint32_t i = 0; i < a->params->length; ++i) {
            FunParam *pa = a->params->params + i;
            FunParam *pb = b->params->params + i;
            if (pa->type != pb->type) {
                return false;
            }
        }
    }
    return true;
}

static uint32_t hash_type(Type *t) {
    uint32_t hash = FNV_SEED;
    hash = fnv1a(&t->kind, sizeof(t->kind), hash);
    hash = fnv1a(&t->flags, sizeof(t->flags), hash);
    hash = fnv1a(&t->size, sizeof(t->size), hash);
    hash = fnv1a(&t->salt, sizeof(t->salt), hash);
    if (t->target) {
        hash = fnv1a(&t->target->hash, sizeof(t->target->hash), hash);
    }
    if (t->fields) {
        for (uint32_t i = 0; i < t->fields->length; ++i) {
            StructField *f = t->fields->fields + i;
            hash = fnv1a(&f->name, sizeof(f->name), hash);
            hash = fnv1a(&f->type->hash, sizeof(f->type->hash), hash);
            hash = fnv1a(&f->offset, sizeof(f->offset), hash);
        }
    }
    if (t->params) {
        for (uint32_t i = 0; i < t->params->length; ++i) {
            FunParam *p = t->params->params + i;
            hash = fnv1a(&p->type->hash, sizeof(p->type->hash), hash);
        }
    }
    return hash;
}

#define EXPAND_INTERFACE
#define EXPAND_IMPLEMENTATION
#define NAME TypeMap
#define KEY_TYPE uint32_t
#define VALUE_TYPE const Type *
#define HASH_FUNC(x) (x)
#define EQUAL_FUNC(x, y) ((x) == (y))
#include "hashtable.h"
typedef struct TypeMap TypeMap;

static TypeMap typemap;

const Type *intern_type(Type *type) {
    for (uint32_t salt = 0; salt < 10000; ++salt) {
        type->salt = salt;
        type->hash = hash_type(type);

        const Type *found;
        if (!TypeMap_get(&typemap, type->hash, &found)) {
            Type *new_type = malloc(sizeof(Type));
            *new_type = *type;
            TypeMap_put(&typemap, new_type->hash, new_type);
            return new_type;
        }
        if (type_equal(type, found)) {
            return found;
        }
    }

    assert(0 && "should not happen");
    return NULL;
}

const Type *prim_type(uint32_t kind, uint32_t size) {
    Type type = { .kind = kind,.size = size };
    return intern_type(&type);
}

const Type *array_type(const Type *elem_type) {
    Type type = { .kind = KIND_ARRAY, .flags = TYPE_FLAG_UNSIZED, .target = elem_type };
    return intern_type(&type);
}

const Type *array_type_sized(const Type *elem_type, uint32_t elem_count) {
    Type type = { .kind = KIND_ARRAY, .size = elem_type->size * elem_count, .target = elem_type };
    return intern_type(&type);
}

const Type *ptr_type(const Type *elem_type) {
    Type type = { .kind = KIND_PTR, .size = sizeof(void *), .target = elem_type };
    return intern_type(&type);
}

const Type *ref_type(const Type *elem_type) {
    Type type = { .kind = KIND_REF, .size = sizeof(void *), .target = elem_type };
    return intern_type(&type);
}

StructFieldArray *struct_field_array(uint32_t field_count, StructField *fields) {
    StructFieldArray *field_array = malloc(sizeof(StructFieldArray) + sizeof(StructField) * field_count);
    field_array->length = field_count;
    memcpy(field_array->fields, fields, sizeof(StructField) * field_count);
    return field_array;
}

const Type *struct_type(uint32_t size, uint32_t field_count, StructField *fields) {
    StructFieldArray *field_array = struct_field_array(field_count, fields);
    Type type = { .kind = KIND_STRUCT,.size = size,.fields = field_array };
    if (field_count > 0 && fields[field_count - 1].type->flags & TYPE_FLAG_UNSIZED) {
        type.flags |= TYPE_FLAG_UNSIZED;
    }
    return intern_type(&type);
}

void init_globals(void) {
    SymMap_init(&symbolmap, 512);

    TypeMap_init(&typemap, 512);

    Type *type = calloc(1, sizeof(Type));
    type->kind = KIND_STRUCT;
    type->size = sizeof(Type);
    type->hash = 0x12345678; /* we have to invent a hash here, since this type is self-recursive and can't be properly hashed */
    TypeMap_put(&typemap, type->hash, type);

    type_type = type;
    type_ptr_type = ptr_type(type_type);

    type_any = prim_type(KIND_ANY, sizeof(Any));

    type_unit = prim_type(KIND_UNIT, 0);
    type_b32 = prim_type(KIND_BOOL, sizeof(bool));

    type_u8 = prim_type(KIND_U8, sizeof(uint8_t));
    type_u16 = prim_type(KIND_U16, sizeof(uint16_t));
    type_u32 = prim_type(KIND_U32, sizeof(uint32_t));
    type_u64 = prim_type(KIND_U64, sizeof(uint64_t));

    type_i8 = prim_type(KIND_I8, sizeof(int8_t));
    type_i16 = prim_type(KIND_I16, sizeof(int16_t));
    type_i32 = prim_type(KIND_I32, sizeof(int32_t));
    type_i64 = prim_type(KIND_I64, sizeof(int64_t));

    type_f32 = prim_type(KIND_F32, sizeof(float));
    type_f64 = prim_type(KIND_F64, sizeof(double));

    type_ptr_symbol = ptr_type(array_type(type_u8));
    type_ref_string = ref_type(array_type(type_u8));

    const Type *type_cons = struct_type(sizeof(Cons), 2, (StructField[]) {
        { .name = intern_symbol_str("car"), .type = type_any, .offset = offsetof(Cons, car) },
        { .name = intern_symbol_str("cdr"), .type = type_any, .offset = offsetof(Cons, cdr) },
    });
    type_ref_cons = ref_type(type_cons);

    /* complete Type type */
    const Type *type_struct_field = struct_type(sizeof(StructField), 3, (StructField[]) {
        { .name = intern_symbol_str("name"), .type = type_ptr_symbol, .offset = offsetof(StructField, name) },
        { .name = intern_symbol_str("type"), .type = type_ptr_type, .offset = offsetof(StructField, type) },
        { .name = intern_symbol_str("offset"), .type = type_u32, .offset = offsetof(StructField, offset) },
    });
    type->fields = struct_field_array(5, (StructField[]) {
        { .name = intern_symbol_str("kind"), .type = type_u32, .offset = offsetof(Type, kind) },
        { .name = intern_symbol_str("flags"), .type = type_u32, .offset = offsetof(Type, flags) },
        { .name = intern_symbol_str("size"), .type = type_u32, .offset = offsetof(Type, size) },
        { .name = intern_symbol_str("target"), .type = type_ptr_type, .offset = offsetof(Type, target) },
        { .name = intern_symbol_str("fields"), .type = ptr_type(array_type(type_struct_field)), .offset = offsetof(Type, fields) },
    });

    symbol_if = intern_symbol_str("if");
}

int main() {
    init_globals();

    assert(intern_symbol_str("foo"));
    assert(intern_symbol_str("foo") == intern_symbol_str("foo"));
    assert(prim_type(KIND_U32, sizeof(uint32_t)));
    assert(prim_type(KIND_U32, sizeof(uint32_t)) == prim_type(KIND_U32, sizeof(uint32_t)));

    CompilerCtx *cctx = calloc(1, sizeof(CompilerCtx));

    emit_lit_7_bytes(cctx, 10);
    emit_label(cctx, 123);
    emit_u64(cctx, OP_DUP);
    emit_u64(cctx, OP_PRINT);
    emit_lit_7_bytes(cctx, 1);
    emit_u64(cctx, OP_SUB_U64);
    emit_jtrue(cctx, 123);
    emit_u64(cctx, OP_RET);

    printf("\ncode:\n");
    print_code(cctx);

    strip_labels(cctx);

    printf("\nstripped code:\n");
    print_code(cctx);
    printf("\n");

    Function fun = { .code = cctx->code };
    VMCtx vm = { .stack = malloc(sizeof(Word) * MAX_STACK) };
    vm.sp = vm.stack;

    fgetc(stdin);
    call(&vm, &fun);

    fgetc(stdin);
    return 0;
}

