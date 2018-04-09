#ifndef TYPES_H
#define TYPES_H

#include "symbol.h"


#define ASSERT_CONCAT_(a, b) a##b
#define ASSERT_CONCAT(a, b) ASSERT_CONCAT_(a, b)
/* These can't be used after statements in c89. */
#ifdef __COUNTER__
#define STATIC_ASSERT(e,m) \
    ;enum { ASSERT_CONCAT(static_assert_, __COUNTER__) = 1/(int)(!!(e)) }
#else
/* This can't be used twice on the same line so ensure if using in headers
* that the headers are not included twice (by wrapping in #ifndef...#endif)
* Note it doesn't cause an issue when used on same line of separate modules
* compiled with gcc -combine -fwhole-program.  */
#define STATIC_ASSERT(e,m) \
    ;enum { ASSERT_CONCAT(assert_line_, __LINE__) = 1/(int)(!!(e)) }
#endif


#define PROPS_UNIT(X)   X(UNIT, unit, uint32_t, "unit")
#define PROPS_BOOL(X)   X(BOOL,  b32, bool,     "%u")
#define PROPS_U8(X)     X(U8,     u8, uint8_t,  "%"PRIu8)
#define PROPS_U16(X)    X(U16,   u16, uint16_t, "%"PRIu16)
#define PROPS_U32(X)    X(U32,   u32, uint32_t, "%"PRIu32)
#define PROPS_U64(X)    X(U64,   u64, uint64_t, "%"PRIu64)
#define PROPS_I8(X)     X(I8,     i8, int8_t,   "%"PRId8)
#define PROPS_I16(X)    X(I16,   i16, int16_t,  "%"PRId16)
#define PROPS_I32(X)    X(I32,   i32, int32_t,  "%"PRId32)
#define PROPS_I64(X)    X(I64,   i64, int64_t,  "%"PRId64)
#define PROPS_F32(X)    X(F32,   f32, float,    "%.2f")
#define PROPS_F64(X)    X(F64,   f64, double,   "%.2f")
#define PROPS_PTR(X)    X(PTR,   ptr, void *,   "%p")

#define FOR_ALL_INT(X) \
    PROPS_U8(X) \
    PROPS_U16(X) \
    PROPS_U32(X) \
    PROPS_U64(X) \
    PROPS_I8(X) \
    PROPS_I16(X) \
    PROPS_I32(X) \
    PROPS_I64(X)

#define FOR_ALL_NUM(X) \
    FOR_ALL_INT(X) \
    PROPS_F32(X) \
    PROPS_F64(X)

#define FOR_ALL_BASIC(X) \
    PROPS_BOOL(X) \
    FOR_ALL_NUM(X)

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


typedef struct Any Any;
typedef struct Type Type;
typedef struct Cons Cons;
typedef U8Array String;

typedef Any (*FunPtr0)(void);
typedef Any (*FunPtr1)(Any a);
typedef Any (*FunPtr2)(Any a, Any b);
typedef Any (*FunPtr3)(Any a, Any b, Any c);

typedef void (*VoidFunPtr0)(void);
typedef void (*VoidFunPtr1)(Any a);
typedef void (*VoidFunPtr2)(Any a, Any b);
typedef void (*VoidFunPtr3)(Any a, Any b, Any c);

typedef union Word Word;
union Word {
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
    uintptr_t type;
    Word val;
};


#define ANY_KIND_BITS 5
#define ANY_KIND_MASK ((1 << ANY_KIND_BITS) - 1)
#define ANY_KIND(any) ((any).type & ANY_KIND_MASK)
#define ANY_TYPE(any) (ANY_KIND(any) != KIND_REF_SLICE ? (const Type *)((any).type & ~ANY_KIND_MASK) : NULL)
#define MK_ANY_TYPE(t) ((t)->kind | (uintptr_t)(t))

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

    NUM_TYPE_KINDS
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

    const Type *target; /* target type for PTR, REF. elem type for ARRAY. return type for FUN. */
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

const Type *intern_prim_type(uint32_t kind, uint32_t size);
const Type *intern_array_type(const Type *elem_type);
const Type *intern_array_type_sized(const Type *elem_type, uint32_t elem_count);
const Type *intern_ptr_type(const Type *elem_type);
const Type *intern_ref_type(const Type *elem_type);
const Type *intern_struct_type(uint32_t size, uint32_t field_count, StructField *fields);
const Type *intern_fun_type(const Type *ret_type, uint32_t param_count, FunParam *params);

void init_types(void);

#endif
