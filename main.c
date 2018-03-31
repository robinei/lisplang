#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <stdarg.h>
#include <string.h>
#include <assert.h>
#include "fnv.h"
#include "murmur3.h"


typedef union Word Word;
typedef struct Any Any;
typedef struct Type Type;
typedef struct Symbol Symbol;
typedef struct Cons Cons;
typedef struct U8Array U8Array;
typedef U8Array String;

typedef Any(*FunPtr0)(void);
typedef Any(*FunPtr1)(Any a);
typedef Any(*FunPtr2)(Any a, Any b);
typedef Any(*FunPtr3)(Any a, Any b, Any c);

struct Any {
    uintptr_t type;
    union {
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
    } val;
};


#define ANY_KIND_BITS 5
#define ANY_KIND_MASK ((1 << ANY_KIND_BITS) - 1)
#define ANY_KIND(any) ((any).type & ANY_KIND_MASK)
#define ANY_TYPE(any) (ANY_KIND(any) != KIND_REF_SLICE ? (const Type *)((any).type & ~ANY_KIND_MASK) : NULL)
#define MK_ANY_TYPE(t) (t->kind | (uintptr_t)t)

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

struct Symbol {
    uint32_t hash;
    uint32_t length;
    uint8_t *data;
};

struct U8Array {
    uint32_t length;
    uint8_t data[];
};

typedef struct Function Function;
struct Function {
    uint64_t *code;
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
const Symbol *symbol_assign;
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
const Symbol *symbol_print;
const Symbol *symbol_tagbody;
const Symbol *symbol_go;


#define ANY_UNIT ((Any) { .type = MK_ANY_TYPE(type_unit) })
#define ANY_TRUE ((Any) { .type = MK_ANY_TYPE(type_b32), .val.b32 = true })
#define ANY_FALSE ((Any) { .type = MK_ANY_TYPE(type_b32), .val.b32 = false })

#define MAKE_ANY_U8(x) ((Any) { .type = MK_ANY_TYPE(type_u8), .val.u8 = (x) })
#define MAKE_ANY_U16(x) ((Any) { .type = MK_ANY_TYPE(type_u16), .val.u16 = (x) })
#define MAKE_ANY_U32(x) ((Any) { .type = MK_ANY_TYPE(type_u32), .val.u32 = (x) })
#define MAKE_ANY_U64(x) ((Any) { .type = MK_ANY_TYPE(type_u64), .val.u64 = (x) })

#define MAKE_ANY_I8(x) ((Any) { .type = MK_ANY_TYPE(type_i8), .val.i8 = (x) })
#define MAKE_ANY_I16(x) ((Any) { .type = MK_ANY_TYPE(type_i16), .val.i16 = (x) })
#define MAKE_ANY_I32(x) ((Any) { .type = MK_ANY_TYPE(type_i32), .val.i32 = (x) })
#define MAKE_ANY_I64(x) ((Any) { .type = MK_ANY_TYPE(type_i64), .val.i64 = (x) })

#define MAKE_ANY_F32(x) ((Any) { .type = MK_ANY_TYPE(type_f32), .val.f32 = (x) })
#define MAKE_ANY_F64(x) ((Any) { .type = MK_ANY_TYPE(type_f64), .val.f64 = (x) })

#define MAKE_ANY_SYM(x) ((Any) { .type = MK_ANY_TYPE(type_ptr_symbol), .val.symbol_ptr = (x) })
#define MAKE_ANY_TYPE(x) ((Any) { .type = MK_ANY_TYPE(type_ptr_type), .val.type = (x) })

#define REFCOUNT(ptr) (((uint32_t *)(ptr))[-1])
#define MAYBE_ADDREF(any) (ANY_KIND(any) == KIND_REF && ++REFCOUNT((any).val.ptr))

#define IS_ANY_SYM(any, sym) (ANY_TYPE(any) == type_ptr_symbol && (any).val.symbol_ptr == sym)

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
        return (Any) { .type = MK_ANY_TYPE(type), .val.ptr = *(void **)ptr };
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
    return (Any) { .type = MK_ANY_TYPE(type_ref_string), .val.u8_array_ptr = s };
}

Any cons(Any car, Any cdr) {
    assert(ANY_TYPE(cdr) == type_ref_cons || ANY_KIND(cdr) == KIND_UNIT);
    Cons *c = rc_alloc(sizeof(Cons));
    c->car = car;
    c->cdr = cdr;
    MAYBE_ADDREF(car);
    MAYBE_ADDREF(cdr);
    return (Any) { .type = MK_ANY_TYPE(type_ref_cons), .val.cons_ptr = c };
}

Any list(Any first, ...) {
    Any arr[128];
    arr[0] = first;
    int i = 0;

    va_list args;
    va_start(args, first);
    for (;;) {
        Any x = va_arg(args, Any);
        if (ANY_KIND(x) == KIND_UNIT) {
            break;
        }
        arr[++i] = x;
    }
    va_end(args);

    Any result = ANY_UNIT;
    for (; i >= 0; --i) {
        result = cons(arr[i], result);
    }

    return result;
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


#define PROPS_UNIT(X)   X(UNIT, unit, uint32_t, "unit")
#define PROPS_BOOL(X)   X(BOOL,  b32, bool,     "%u")
#define PROPS_U8(X)     X(U8,     u8, uint8_t,  "%u")
#define PROPS_U16(X)    X(U16,   u16, uint16_t, "%u")
#define PROPS_U32(X)    X(U32,   u32, uint32_t, "%u")
#define PROPS_U64(X)    X(U64,   u64, uint64_t, "%llu")
#define PROPS_I8(X)     X(I8,     i8, int8_t,   "%d")
#define PROPS_I16(X)    X(I16,   i16, int16_t,  "%d")
#define PROPS_I32(X)    X(I32,   i32, int32_t,  "%d")
#define PROPS_I64(X)    X(I64,   i64, int64_t,  "%lld")
#define PROPS_F32(X)    X(F32,   f32, float,    "%.2f")
#define PROPS_F64(X)    X(F64,   f64, double,   "%.2f")
#define PROPS_PTR(X)    X(PTR,   ptr, void *,   "%p")

#define FOR_ALL_NUM(X) \
    PROPS_U8(X) \
    PROPS_U16(X) \
    PROPS_U32(X) \
    PROPS_U64(X) \
    PROPS_I8(X) \
    PROPS_I16(X) \
    PROPS_I32(X) \
    PROPS_I64(X) \
    PROPS_F32(X) \
    PROPS_F64(X)

#define FOR_ALL_PRIM(X) \
    PROPS_BOOL(X) \
    FOR_ALL_NUM(X) \
    PROPS_PTR(X)

#define FOR_ALL_PRIM_INT_32(X) \
    PROPS_BOOL(X) \
    PROPS_U8(X) \
    PROPS_U16(X) \
    PROPS_U32(X) \
    PROPS_I8(X) \
    PROPS_I16(X) \
    PROPS_I32(X)

#define FOR_ALL_PRIM_32(X) \
    FOR_ALL_PRIM_INT_32(X) \
    PROPS_F32(X)

#define FOR_ALL_PRIM_64(X) \
    PROPS_U64(X) \
    PROPS_I64(X) \
    PROPS_F64(X) \
    PROPS_PTR(X)


#define DEF_PRINT_ENUM(UNAME, LNAME, TYPE, FMT) OP_PRINT_ ## UNAME,
#define DEF_LIT_ENUM(UNAME, LNAME, TYPE, FMT) OP_LIT_ ## UNAME,
#define DEF_ADD_ENUM(UNAME, LNAME, TYPE, FMT) OP_ADD_ ## UNAME,
#define DEF_SUB_ENUM(UNAME, LNAME, TYPE, FMT) OP_SUB_ ## UNAME,
#define DEF_MUL_ENUM(UNAME, LNAME, TYPE, FMT) OP_MUL_ ## UNAME,
#define DEF_DIV_ENUM(UNAME, LNAME, TYPE, FMT) OP_DIV_ ## UNAME,
#define DEF_EQ_ENUM(UNAME, LNAME, TYPE, FMT) OP_EQ_ ## UNAME,
#define DEF_LT_ENUM(UNAME, LNAME, TYPE, FMT) OP_LT_ ## UNAME,
#define DEF_GT_ENUM(UNAME, LNAME, TYPE, FMT) OP_GT_ ## UNAME,
#define DEF_LTEQ_ENUM(UNAME, LNAME, TYPE, FMT) OP_LTEQ_ ## UNAME,
#define DEF_GTEQ_ENUM(UNAME, LNAME, TYPE, FMT) OP_GTEQ_ ## UNAME,

enum {
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

    OP_MOVE,    /* move B to A */

    FOR_ALL_PRIM(DEF_PRINT_ENUM)
    FOR_ALL_PRIM(DEF_LIT_ENUM)
    OP_NOT_BOOL,
    FOR_ALL_NUM(DEF_ADD_ENUM)
    FOR_ALL_NUM(DEF_SUB_ENUM)
    FOR_ALL_NUM(DEF_MUL_ENUM)
    FOR_ALL_NUM(DEF_DIV_ENUM)
    FOR_ALL_PRIM(DEF_EQ_ENUM)
    FOR_ALL_NUM(DEF_LT_ENUM)
    FOR_ALL_NUM(DEF_GT_ENUM)
    FOR_ALL_NUM(DEF_LTEQ_ENUM)
    FOR_ALL_NUM(DEF_GTEQ_ENUM)

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


#define DEFINE_PRINT_INSTR(UNAME, LNAME, TYPE, FMT) \
    case OP_PRINT_ ## UNAME:  printf(FMT "\n", *(TYPE  *)(fp + INSTR_A(instr))); continue;
#define DEFINE_LIT_32_INSTR(UNAME, LNAME, TYPE, FMT) \
    case OP_LIT_ ## UNAME:   *(TYPE  *)(fp + INSTR_A(instr)) = (TYPE)INSTR_BC(instr); continue;
#define DEFINE_LIT_64_INSTR(UNAME, LNAME, TYPE, FMT) \
    case OP_LIT_ ## UNAME:   *(TYPE  *)(fp + INSTR_A(instr)) = (TYPE)*ip++; continue;
#define DEFINE_ADD_INSTR(UNAME, LNAME, TYPE, FMT) \
    case OP_ADD_ ## UNAME:   *(TYPE  *)(fp + INSTR_A(instr)) = *(TYPE  *)(fp + INSTR_B(instr)) +  *(TYPE  *)(fp + INSTR_C(instr)); continue;
#define DEFINE_SUB_INSTR(UNAME, LNAME, TYPE, FMT) \
    case OP_SUB_ ## UNAME:   *(TYPE  *)(fp + INSTR_A(instr)) = *(TYPE  *)(fp + INSTR_B(instr)) -  *(TYPE  *)(fp + INSTR_C(instr)); continue;
#define DEFINE_MUL_INSTR(UNAME, LNAME, TYPE, FMT) \
    case OP_MUL_ ## UNAME:   *(TYPE  *)(fp + INSTR_A(instr)) = *(TYPE  *)(fp + INSTR_B(instr)) *  *(TYPE  *)(fp + INSTR_C(instr)); continue;
#define DEFINE_DIV_INSTR(UNAME, LNAME, TYPE, FMT) \
    case OP_DIV_ ## UNAME:   *(TYPE  *)(fp + INSTR_A(instr)) = *(TYPE  *)(fp + INSTR_B(instr)) /  *(TYPE  *)(fp + INSTR_C(instr)); continue;
#define DEFINE_EQ_INSTR(UNAME, LNAME, TYPE, FMT) \
    case OP_EQ_ ## UNAME:    *(bool  *)(fp + INSTR_A(instr)) = *(TYPE  *)(fp + INSTR_B(instr)) == *(TYPE  *)(fp + INSTR_C(instr)); continue;
#define DEFINE_LT_INSTR(UNAME, LNAME, TYPE, FMT) \
    case OP_LT_ ## UNAME:    *(bool  *)(fp + INSTR_A(instr)) = *(TYPE  *)(fp + INSTR_B(instr)) <  *(TYPE  *)(fp + INSTR_C(instr)); continue;
#define DEFINE_GT_INSTR(UNAME, LNAME, TYPE, FMT) \
    case OP_GT_ ## UNAME:    *(bool  *)(fp + INSTR_A(instr)) = *(TYPE  *)(fp + INSTR_B(instr)) >  *(TYPE  *)(fp + INSTR_C(instr)); continue;
#define DEFINE_LTEQ_INSTR(UNAME, LNAME, TYPE, FMT) \
    case OP_LTEQ_ ## UNAME:  *(bool  *)(fp + INSTR_A(instr)) = *(TYPE  *)(fp + INSTR_B(instr)) <= *(TYPE  *)(fp + INSTR_C(instr)); continue;
#define DEFINE_GTEQ_INSTR(UNAME, LNAME, TYPE, FMT) \
    case OP_GTEQ_ ## UNAME:  *(bool  *)(fp + INSTR_A(instr)) = *(TYPE  *)(fp + INSTR_B(instr)) >= *(TYPE  *)(fp + INSTR_C(instr)); continue;

void interpret(uint64_t *ip, uint64_t *fp) {
    for(;;) {
        uint64_t instr = *ip++;

        switch (INSTR_OP(instr)) {
        case OP_JUMP: ip += (int32_t)INSTR_BC(instr) - 1; continue;
        case OP_JFALSE: if (!*(bool *)(fp + INSTR_A(instr))) { ip += (int32_t)INSTR_BC(instr) - 1; } continue;
        case OP_JTRUE:  if ( *(bool *)(fp + INSTR_A(instr))) { ip += (int32_t)INSTR_BC(instr) - 1; } continue;

        case OP_TCALL: {
            uint32_t fun_offset = INSTR_A(instr);
            Function *fun = *(Function **)(fp + fun_offset);
            fp += fun_offset;
            ip = fun->code; /* tail call means we just overwrite these, and don't grow the call stack */
            continue;
        }
        case OP_CALL: {
            uint32_t fun_offset = INSTR_A(instr);
            uint32_t result_reg = INSTR_B(instr);
            Function *fun = *(Function **)(fp + fun_offset);
            interpret(fun->code, fp + fun_offset); /* just use the C call stack */
            *(uint64_t *)(fp + result_reg) = *(uint64_t *)(fp + fun_offset);
            continue;
        }
        case OP_RET: return; /* since we use the C call stack we just return */

        case OP_MOVE: *(uint64_t *)(fp + INSTR_A(instr)) = *(uint64_t *)(fp + INSTR_B(instr)); continue;

        FOR_ALL_PRIM_32(DEFINE_PRINT_INSTR)
        FOR_ALL_PRIM_32(DEFINE_LIT_32_INSTR)
        FOR_ALL_PRIM_64(DEFINE_LIT_64_INSTR)
        case OP_NOT_BOOL: *(bool *)(fp + INSTR_A(instr)) = !*(bool *)(fp + INSTR_B(instr)); continue;
        FOR_ALL_NUM(DEFINE_ADD_INSTR)
        FOR_ALL_NUM(DEFINE_SUB_INSTR)
        FOR_ALL_NUM(DEFINE_MUL_INSTR)
        FOR_ALL_NUM(DEFINE_DIV_INSTR)
        FOR_ALL_PRIM(DEFINE_EQ_INSTR)
        FOR_ALL_NUM(DEFINE_LT_INSTR)
        FOR_ALL_NUM(DEFINE_GT_INSTR)
        FOR_ALL_NUM(DEFINE_LTEQ_INSTR)
        FOR_ALL_NUM(DEFINE_GTEQ_INSTR)
        }
    }

}

#define INAME_FMT "%-12s"
#define PRINT_OP3(UOP, LOP, UNAME, LNAME) case OP_ ## UOP ## _ ## UNAME: printf(INAME_FMT "r%u <- r%u r%u\n", #LOP "/" #LNAME, INSTR_A(instr), INSTR_B(instr), INSTR_C(instr)); break;
#define DEFINE_PRINT_ADD(UNAME, LNAME, TYPE, FMT) PRINT_OP3(ADD, add, UNAME, LNAME)
#define DEFINE_PRINT_SUB(UNAME, LNAME, TYPE, FMT) PRINT_OP3(SUB, sub, UNAME, LNAME)
#define DEFINE_PRINT_MUL(UNAME, LNAME, TYPE, FMT) PRINT_OP3(MUL, mul, UNAME, LNAME)
#define DEFINE_PRINT_DIV(UNAME, LNAME, TYPE, FMT) PRINT_OP3(DIV, div, UNAME, LNAME)
#define DEFINE_PRINT_EQ(UNAME, LNAME, TYPE, FMT) PRINT_OP3(EQ, eq, UNAME, LNAME)
#define DEFINE_PRINT_LT(UNAME, LNAME, TYPE, FMT) PRINT_OP3(LT, lt, UNAME, LNAME)
#define DEFINE_PRINT_GT(UNAME, LNAME, TYPE, FMT) PRINT_OP3(GT, gt, UNAME, LNAME)
#define DEFINE_PRINT_LTEQ(UNAME, LNAME, TYPE, FMT) PRINT_OP3(LTEQ, lteq, UNAME, LNAME)
#define DEFINE_PRINT_GTEQ(UNAME, LNAME, TYPE, FMT) PRINT_OP3(GTEQ, gteq, UNAME, LNAME)
#define PRINT_PRINT_LIT_32(UNAME, LNAME, TYPE, FMT) case OP_LIT_ ## UNAME: temp32 = INSTR_BC(instr); printf(INAME_FMT "r%u <- " FMT "\n", "lit/" #LNAME, INSTR_A(instr), *(TYPE *)&temp32); break;
#define PRINT_PRINT_LIT_64(UNAME, LNAME, TYPE, FMT) case OP_LIT_ ## UNAME: temp64 = code[i++];       printf(INAME_FMT "r%u <- " FMT "\n", "lit/" #LNAME, INSTR_A(instr), *(TYPE *)&temp64); break;
#define DEFINE_PRINT_PRINT(UNAME, LNAME, TYPE, FMT) case OP_PRINT_ ## UNAME: printf(INAME_FMT "r%u\n", "print/" #LNAME, INSTR_A(instr)); break;

void print_code(uint64_t *code, uint32_t length) {
    uint32_t temp32;
    uint64_t temp64;

    for (uint32_t i = 0; i < length; ++i) {
        uint64_t instr = code[i];

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
        FOR_ALL_PRIM(DEFINE_PRINT_EQ)
        FOR_ALL_NUM(DEFINE_PRINT_LT)
        FOR_ALL_NUM(DEFINE_PRINT_GT)
        FOR_ALL_NUM(DEFINE_PRINT_LTEQ)
        FOR_ALL_NUM(DEFINE_PRINT_GTEQ)
        }
    }
}



#define EXPAND_INTERFACE
#define EXPAND_IMPLEMENTATION
#define NAME LabelMap
#define KEY_TYPE const Symbol *
#define VALUE_TYPE uint32_t
#define HASH_FUNC(x) ((x)->hash)
#define EQUAL_FUNC(x, y) ((x) == (y))
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
#define EXPAND_IMPLEMENTATION
#define NAME BindingMap
#define KEY_TYPE const Symbol *
#define VALUE_TYPE Binding *
#define HASH_FUNC(x) ((x)->hash)
#define EQUAL_FUNC(x, y) ((x) == (y))
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

typedef struct CompilerCtx CompilerCtx;
struct CompilerCtx {
    uint32_t stack_offset;

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
};

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

static void push_binding(CompilerCtx *cctx, Binding *binding) {
    if (cctx->binding_stack_used == cctx->binding_stack_capacity) {
        cctx->binding_stack_capacity = cctx->binding_stack_capacity ? cctx->binding_stack_capacity * 2 : 16;
        cctx->binding_stack = realloc(cctx->binding_stack, cctx->binding_stack_capacity * sizeof(PrevBinding));
    }
    Binding *prev = NULL;
    BindingMap_get(&cctx->binding_map, binding->symbol, &prev);
    BindingMap_put(&cctx->binding_map, binding->symbol, binding);
    cctx->binding_stack[cctx->binding_stack_used].symbol = binding->symbol;
    cctx->binding_stack[cctx->binding_stack_used].binding = prev;
    ++cctx->binding_stack_used;
}

static void pop_bindings(CompilerCtx *cctx, uint32_t count) {
    for (uint32_t i = 0; i < count; ++i) {
        PrevBinding prev = cctx->binding_stack[--cctx->binding_stack_used];
        if (prev.binding) {
            Binding *curr;
            BindingMap_get(&cctx->binding_map, prev.symbol, &curr);
            BindingMap_put(&cctx->binding_map, prev.symbol, prev.binding);
            free(curr);
        }
        else {
            BindingMap_remove(&cctx->binding_map, prev.symbol);
        }
    }
}

static uint32_t gen_label(CompilerCtx *cctx) {
    return ++cctx->label_counter;
}

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
#include "hashtable.h"
typedef struct U32Map U32Map;

static void strip_labels(CompilerCtx *cctx) {
    U32Map label_offsets;
    U32Map_init(&label_offsets, 128);

    uint32_t i = 0;
    uint64_t *code = cctx->code;
    uint32_t count = cctx->code_used;

    cctx->code = NULL;
    cctx->code_used = 0;
    cctx->code_capacity = 0;

    while (i < count) {
        uint64_t instr = code[i];

        if (INSTR_OP(instr) == OP_LABEL) {
            U32Map_put(&label_offsets, INSTR_BC(instr), cctx->code_used);
            ++i;
        }
        else {
            switch (instr_word_count(instr)) {
            case 1: emit(cctx, code[i++]); break;
            case 2: emit(cctx, code[i++]); emit(cctx, code[i++]); break;
            }
        }
    }

    free(code);

    i = 0;
    code = cctx->code;
    count = cctx->code_used;

    while (i < count) {
        uint64_t instr = code[i];
        uint32_t new_op;

        switch (INSTR_OP(instr)) {
        case OP_JUMP_LABEL: new_op = OP_JUMP; break;
        case OP_JFALSE_LABEL: new_op = OP_JFALSE; break;
        case OP_JTRUE_LABEL: new_op = OP_JTRUE; break;
        default: i += instr_word_count(instr); continue;
        }

        uint32_t abs_offset;
        if (!U32Map_get(&label_offsets, INSTR_BC(instr), &abs_offset)) {
            assert(0 && "label unexpectedly not found");
        }
        int32_t rel_offset = (int32_t)abs_offset - i;
        assert(rel_offset);
        code[i++] = MK_INSTR_A_BC(new_op, INSTR_A(instr), rel_offset);
    }

    U32Map_free(&label_offsets);
}


const Type *compile(CompilerCtx *cctx, Any form, uint32_t dst_reg) {
    const Type *form_type = ANY_TYPE(form);

    if (form_type == type_ptr_symbol) {
        Binding *binding;
        if (!BindingMap_get(&cctx->binding_map, form.val.symbol_ptr, &binding)) {
            assert(0 && "not found");
        }
        emit_op2(cctx, OP_MOVE, dst_reg, binding->reg);
        return binding->type;
    }

    if (form_type == type_ref_cons) {
        Any head = car(form);

        if (ANY_TYPE(head) == type_ptr_symbol) {
            if (head.val.symbol_ptr == symbol_fun) {
                Any temp = cdr(form);
                Any params_form = car(temp);
                temp = cdr(temp);
                Any body_form = car(temp);

                uint32_t param_count = list_length(params_form);

                CompilerCtx new_cctx = { param_count + 1, };
                const Type *return_type = compile(&new_cctx, body_form, param_count + 1);
            }

            if (head.val.symbol_ptr == symbol_tagbody) {
                Any temp = cdr(form);
                uint32_t label_count = 0;

                for (; ANY_KIND(temp) != KIND_UNIT; temp = cdr(temp)) {
                    Any stmt_form = car(temp);
                    const Type *stmt_type = ANY_TYPE(stmt_form);

                    if (stmt_type == type_ptr_symbol) {
                        uint32_t label = gen_label(cctx);
                        emit_label(cctx, label);
                        push_label(cctx, stmt_form.val.symbol_ptr, label);
                        ++label_count;
                    }
                    else {
                        compile(cctx, stmt_form, dst_reg);
                    }
                }

                pop_labels(cctx, label_count);
                return type_unit;
            }

            if (head.val.symbol_ptr == symbol_go) {
                Any temp = cdr(form);
                Any label_sym = car(temp);
                temp = cdr(temp);
                assert(ANY_KIND(temp) == KIND_UNIT);
                assert(ANY_TYPE(label_sym) == type_ptr_symbol);
                uint32_t label;
                if (!LabelMap_get(&cctx->label_map, label_sym.val.symbol_ptr, &label)) {
                    printf("label not found: %s\n", label_sym.val.symbol_ptr->data);
                    assert(0 && "bad label");
                }
                emit_jump(cctx, label);
                return type_unit;
            }

            if (head.val.symbol_ptr == symbol_let) {
                Any temp = cdr(form);
                Any binds = car(temp);
                temp = cdr(temp);
                Any body = car(temp);

                uint32_t binds_length = list_length(binds);
                assert((binds_length % 2) == 0);
                uint32_t bind_count = binds_length / 2;
                uint32_t base_reg = cctx->stack_offset;
                cctx->stack_offset += bind_count;

                temp = binds;
                for (uint32_t i = 0; i < bind_count; ++i) {
                    Any sym_form = car(temp);
                    temp = cdr(temp);
                    Any init_form = car(temp);
                    temp = cdr(temp);
                    assert(ANY_TYPE(sym_form) == type_ptr_symbol);

                    uint32_t bind_reg = base_reg + i;
                    const Type *expr_type = compile(cctx, init_form, bind_reg);

                    Binding *binding = calloc(1, sizeof(Binding));
                    binding->reg = bind_reg;
                    binding->symbol = sym_form.val.symbol_ptr;
                    binding->type = expr_type;
                    push_binding(cctx, binding);
                }

                const Type *body_type = compile(cctx, body, dst_reg);
                cctx->stack_offset -= bind_count;
                pop_bindings(cctx, bind_count);
                return body_type;
            }

            if (head.val.symbol_ptr == symbol_if) {
                Any temp = cdr(form);
                Any cond_form = car(temp);
                temp = cdr(temp);
                Any then_form = car(temp);
                temp = cdr(temp);
                Any else_form = car(temp);
                temp = cdr(temp);
                assert(ANY_KIND(temp) == KIND_UNIT);

                uint32_t else_label = gen_label(cctx);
                uint32_t end_label = gen_label(cctx);

                uint32_t cond_reg = cctx->stack_offset++;
                const Type *cond_type = compile(cctx, cond_form, cond_reg);
                --cctx->stack_offset;
                emit_jfalse(cctx, cond_reg, else_label);

                const Type *then_type = compile(cctx, then_form, dst_reg);
                emit_jump(cctx, end_label);
                emit_label(cctx, else_label);
                const Type *else_type = compile(cctx, else_form, dst_reg);
                emit_label(cctx, end_label);

                assert(then_type == else_type);
                return then_type;
            }

            if (head.val.symbol_ptr == symbol_assign) {
                Any temp = cdr(form);
                Any sym_form = car(temp);
                temp = cdr(temp);
                Any val_form = car(temp);
                temp = cdr(temp);
                assert(ANY_KIND(temp) == KIND_UNIT);
                assert(ANY_TYPE(sym_form) == type_ptr_symbol);

                Binding *binding;
                if (!BindingMap_get(&cctx->binding_map, sym_form.val.symbol_ptr, &binding)) {
                    assert(0 && "not found");
                }


                uint32_t reg = cctx->stack_offset++;
                const Type *val_type = compile(cctx, val_form, reg);
                --cctx->stack_offset;

                emit_op2(cctx, OP_MOVE, binding->reg, reg);

                return type_unit;
            }

            if (head.val.symbol_ptr == symbol_plus) {
                Any temp = cdr(form);
                Any a = car(temp);
                temp = cdr(temp);
                Any b = car(temp);

                uint32_t a_reg = cctx->stack_offset++;
                uint32_t b_reg = cctx->stack_offset++;
                const Type *a_type = compile(cctx, a, a_reg);
                const Type *b_type = compile(cctx, b, b_reg);
                cctx->stack_offset -= 2;

                assert(a_type == b_type);
                assert(a_type->kind >= KIND_U8 && a_type->kind <= KIND_F64);
                emit_op3(cctx, OP_ADD_U8 + (a_type->kind - KIND_U8), dst_reg, a_reg, b_reg);
                return a_type;
            }

            if (head.val.symbol_ptr == symbol_print) {
                Any temp = cdr(form);
                Any val_form = car(temp);

                uint32_t reg = cctx->stack_offset++;
                const Type *val_type = compile(cctx, val_form, reg);
                --cctx->stack_offset;

                assert(val_type->kind >= KIND_BOOL && val_type->kind <= KIND_PTR);
                emit_op1(cctx, OP_PRINT_BOOL + (val_type->kind - KIND_BOOL), reg);
                return type_unit;
            }
        }

        /* call */

        uint32_t form_count = list_length(form);
        uint32_t base_reg = cctx->stack_offset;
        cctx->stack_offset += form_count;
        
        uint32_t callable_reg = base_reg;
        const Type *callable_type = compile(cctx, head, callable_reg);

        Any temp = cdr(form);
        for (uint32_t i = 1; i < form_count; ++i) {
            Any arg_form = car(temp);
            temp = cdr(temp);

            uint32_t arg_reg = base_reg + i;
            const Type *arg_type = compile(cctx, arg_form, arg_reg);
        }

        emit_call(cctx, callable_reg, dst_reg);
        cctx->stack_offset -= form_count;
    }

    switch (form_type->kind) {
    case KIND_UNIT: return form_type;
    case KIND_BOOL: emit_lit_b32(cctx, dst_reg, form.val.b32); return form_type;

    case KIND_U8: emit_lit_u8(cctx, dst_reg, form.val.u8); return form_type;
    case KIND_U16: emit_lit_u16(cctx, dst_reg, form.val.u16); return form_type;
    case KIND_U32: emit_lit_u32(cctx, dst_reg, form.val.u32); return form_type;
    case KIND_U64: emit_lit_u64(cctx, dst_reg, form.val.u64); return form_type;

    case KIND_I8: emit_lit_i8(cctx, dst_reg, form.val.i8); return form_type;
    case KIND_I16: emit_lit_i16(cctx, dst_reg, form.val.i16); return form_type;
    case KIND_I32: emit_lit_i32(cctx, dst_reg, form.val.i32); return form_type;
    case KIND_I64: emit_lit_i64(cctx, dst_reg, form.val.i64); return form_type;

    case KIND_F32: emit_lit_f32(cctx, dst_reg, form.val.f32); return form_type;
    case KIND_F64: emit_lit_f64(cctx, dst_reg, form.val.f64); return form_type;

    case KIND_PTR:
    case KIND_REF: emit_lit_ptr(cctx, dst_reg, form.val.ptr); return form_type;
    }

    assert(0 && "bad form");
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
    Symbol *sym = malloc(sizeof(Symbol));
    sym->data = malloc(length + 1);
    sym->hash = StrSlice_hash(slice);
    sym->length = length;
    memcpy(sym->data, str, length);
    sym->data[length] = 0;
    slice.str = sym->data;
    SymMap_put(&symbolmap, slice, sym);
    return sym;
}

const Symbol *intern_symbol_cstr(const char *str) {
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
            char *ptr = malloc(sizeof(Type) + (1 << ANY_KIND_BITS));
            while (((intptr_t)ptr % (1 << ANY_KIND_BITS)) != 0) {
                ++ptr;
            }
            Type *new_type = (Type *)ptr;
            *new_type = *type;
            TypeMap_put(&typemap, new_type->hash, new_type);
            assert(((intptr_t)new_type & ANY_KIND_MASK) == 0);
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
    Type type = { .kind = kind, .size = size };
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
    Type type = { .kind = KIND_STRUCT, .size = size, .fields = field_array };
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
        { .name = intern_symbol_cstr("car"), .type = type_any, .offset = offsetof(Cons, car) },
        { .name = intern_symbol_cstr("cdr"), .type = type_any, .offset = offsetof(Cons, cdr) },
    });
    type_ref_cons = ref_type(type_cons);

    /* complete Type type */
    const Type *type_struct_field = struct_type(sizeof(StructField), 3, (StructField[]) {
        { .name = intern_symbol_cstr("name"), .type = type_ptr_symbol, .offset = offsetof(StructField, name) },
        { .name = intern_symbol_cstr("type"), .type = type_ptr_type, .offset = offsetof(StructField, type) },
        { .name = intern_symbol_cstr("offset"), .type = type_u32, .offset = offsetof(StructField, offset) },
    });
    type->fields = struct_field_array(5, (StructField[]) {
        { .name = intern_symbol_cstr("kind"), .type = type_u32, .offset = offsetof(Type, kind) },
        { .name = intern_symbol_cstr("flags"), .type = type_u32, .offset = offsetof(Type, flags) },
        { .name = intern_symbol_cstr("size"), .type = type_u32, .offset = offsetof(Type, size) },
        { .name = intern_symbol_cstr("target"), .type = type_ptr_type, .offset = offsetof(Type, target) },
        { .name = intern_symbol_cstr("fields"), .type = ptr_type(array_type(type_struct_field)), .offset = offsetof(Type, fields) },
    });

    symbol_if       = intern_symbol_cstr("if");
    symbol_let      = intern_symbol_cstr("let");
    symbol_quote    = intern_symbol_cstr("quote");
    symbol_fun      = intern_symbol_cstr("fun");
    symbol_def      = intern_symbol_cstr("def");
    symbol_do       = intern_symbol_cstr("do");
    symbol_assign   = intern_symbol_cstr("=");
    symbol_plus     = intern_symbol_cstr("+");
    symbol_minus    = intern_symbol_cstr("-");
    symbol_mul      = intern_symbol_cstr("*");
    symbol_div      = intern_symbol_cstr("/");
    symbol_not      = intern_symbol_cstr("not");
    symbol_eq       = intern_symbol_cstr("==");
    symbol_lt       = intern_symbol_cstr("<");
    symbol_gt       = intern_symbol_cstr(">");
    symbol_lteq     = intern_symbol_cstr("<=");
    symbol_gteq     = intern_symbol_cstr(">=");
    symbol_print    = intern_symbol_cstr("print");
    symbol_tagbody  = intern_symbol_cstr("tagbody");
    symbol_go       = intern_symbol_cstr("go");
}


#define sym(x) MAKE_ANY_SYM(intern_symbol_cstr(x))
#define i32(x) MAKE_ANY_I32(x)
#define u32(x) MAKE_ANY_U32(x)
#define nil ANY_UNIT

int main() {
    init_globals();

    printf("op count: %d\n\n", NUM_OPS);

    assert(ANY_KIND(ANY_UNIT) == KIND_UNIT);
    assert(ANY_KIND(ANY_TRUE) == KIND_BOOL);
    assert(ANY_KIND(ANY_FALSE) == KIND_BOOL);
    assert(ANY_TYPE(ANY_UNIT) == type_unit);
    assert(ANY_TYPE(ANY_TRUE) == type_b32);
    assert(ANY_TYPE(ANY_FALSE) == type_b32);
    assert(list_length(list(ANY_TRUE, ANY_FALSE, nil)) == 2);
    assert(intern_symbol_cstr("foo"));
    assert(intern_symbol_cstr("foo") == intern_symbol_cstr("foo"));
    assert(prim_type(KIND_U32, sizeof(uint32_t)));
    assert(prim_type(KIND_U32, sizeof(uint32_t)) == prim_type(KIND_U32, sizeof(uint32_t)));

    Any code =
        list(
            sym("let"),
            list(
                sym("x"),
                u32(44),
                sym("y"),
                u32(55),
                nil
            ),
            list(
                sym("tagbody"),
                sym("start"),
                list(
                    sym("="),
                    sym("y"),
                    list(
                        sym("+"),
                        sym("y"),
                        u32(1),
                        nil
                    ),
                    nil
                ),
                list(
                    sym("print"),
                    list(
                        sym("+"),
                        sym("x"),
                        sym("y"),
                        nil
                    ),
                    nil
                ),
                list(
                    sym("go"),
                    sym("start"),
                    nil
                ),
                nil
            ),
            nil
        )
        ;

    CompilerCtx *cctx = calloc(1, sizeof(CompilerCtx));
    LabelMap_init(&cctx->label_map, 32);
    BindingMap_init(&cctx->binding_map, 32);
    
    ++cctx->stack_offset; compile(cctx, code, 0);

    /*const uint32_t top_label = gen_label(cctx);
    const uint32_t I = 0;
    const uint32_t ONE = 1;
    const uint32_t MAX = 2;
    const uint32_t TEMP = 3;

    emit_lit_i32(cctx, I, 0);
    emit_lit_i32(cctx, ONE, 1);
    emit_lit_i32(cctx, MAX, 10);
    emit_label(cctx, top_label);
    emit_op1(cctx, OP_PRINT_I32, I);
    emit_op3(cctx, OP_ADD_I32, I, I, ONE);
    emit_op3(cctx, OP_LT_I32, TEMP, I, MAX);
    emit_op2(cctx, OP_NOT_BOOL, TEMP, TEMP);
    emit_jfalse(cctx, TEMP, top_label);*/
    emit_ret(cctx);

    printf("\nbefore strip:\n");
    print_code(cctx->code, cctx->code_used);

    strip_labels(cctx);

    printf("\nafter strip:\n");
    print_code(cctx->code, cctx->code_used);

    fgetc(stdin);
    uint64_t stack[1024]; interpret(cctx->code, stack);

    fgetc(stdin);
    return 0;
}
