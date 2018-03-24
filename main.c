#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>
#include <assert.h>
#include "fnv.h"
#include "murmur3.h"


typedef struct Type Type;


typedef struct Symbol Symbol;
typedef struct String String;
typedef struct Cons Cons;
typedef struct Array Array;

typedef union Word Word;
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
    Symbol *symbol;

    void *ptr;
    String *string_ptr;
    Cons *cons_ptr;
    Array *array_ptr;
};


typedef struct Any Any;
struct Any {
    const Type *type;
    Word val;
};


typedef enum TypeKind TypeKind;
enum TypeKind {
    KIND_ANY,

    /* primitive types that can be passed directly as Any */
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
    KIND_SYMBOL,
    KIND_TYPE,
    KIND_PTR,
    KIND_REF, /* regular pointer, but the pointed to value is preceded by a 32 bit reference count */

    /* the following types must be wrapped in a PTR or RC_PTR to be passed as Any */
    KIND_STRING,
    KIND_CONS,
    KIND_ARRAY,
    KIND_STRUCT,
};

#define IS_UNSIGNED_KIND(kind) ((kind) >= KIND_U8 && (kind) <= KIND_U64)
#define IS_SIGNED_KIND(kind) ((kind) >= KIND_I8 && (kind) <= KIND_I64)
#define IS_INTEGRAL_KIND(kind) ((kind) >= KIND_U8 && (kind) <= KIND_I64)
#define IS_REAL_KIND(kind) ((kind) == KIND_F32 || (kind) == KIND_F64)
#define IS_PTR_KIND(kind) ((kind) == KIND_PTR || (kind) == KIND_REF)

typedef enum TypeFlags TypeFlags;
enum TypeFlags {
    FLAG_UNSIZED
};

struct Type {
    TypeKind kind;
    TypeFlags flags;
    size_t size;
    const Type *target;
};


struct Symbol {
    size_t length;
    char data[];
};

struct String {
    size_t length;
    char data[];
};

struct Cons {
    Any car;
    Any cdr;
};

struct Array {
    size_t length;
    char data[];
};


typedef Any(*FunPtr0)(void);
typedef Any(*FunPtr1)(Any a);
typedef Any(*FunPtr2)(Any a, Any b);
typedef Any(*FunPtr3)(Any a, Any b, Any c);
typedef Any(*FunPtr4)(Any a, Any b, Any c, Any d);


const Type type_any = { KIND_ANY, 0, sizeof(Any) };

const Type type_unit = { KIND_UNIT, 0, 0 };
const Type type_bool = { KIND_BOOL, 0, sizeof(bool) };

const Type type_u8 = { KIND_U8, 0, sizeof(uint8_t) };
const Type type_u16 = { KIND_U16, 0, sizeof(uint16_t) };
const Type type_u32 = { KIND_U32, 0, sizeof(uint32_t) };
const Type type_u64 = { KIND_U64, 0, sizeof(uint64_t) };

const Type type_i8 = { KIND_I8, 0, sizeof(int8_t) };
const Type type_i16 = { KIND_I16, 0, sizeof(int16_t) };
const Type type_i32 = { KIND_I32, 0, sizeof(int32_t) };
const Type type_i64 = { KIND_I64, 0, sizeof(int64_t) };

const Type type_f32 = { KIND_F32, 0, sizeof(float) };
const Type type_f64 = { KIND_F64, 0, sizeof(double) };

const Type type_type = { KIND_TYPE, 0, sizeof(void *) };

const Type type_string = { KIND_STRING, 0, FLAG_UNSIZED };
const Type type_ref_string = { KIND_REF, 0, sizeof(void *), &type_string };

const Type type_cons = { KIND_CONS, 0, sizeof(Cons) };
const Type type_ref_cons = { KIND_REF, 0, sizeof(void *), &type_cons };

const Type type_slice_any = { KIND_ARRAY, FLAG_UNSIZED, 0, &type_any };
const Type type_ref_slice_any = { KIND_REF, 0, sizeof(void *), &type_slice_any };


const Any UNIT = { .type = &type_unit };
const Any TRUE = { .type = &type_bool, .val.b32 = true };
const Any FALSE = { .type = &type_bool, .val.b32 = false };

#define U8(x) ((Any) { .type = &type_u8, .val.u8 = (x) })
#define U16(x) ((Any) { .type = &type_u16, .val.u16 = (x) })
#define U32(x) ((Any) { .type = &type_u32, .val.u32 = (x) })
#define U64(x) ((Any) { .type = &type_u64, .val.u64 = (x) })

#define I8(x) ((Any) { .type = &type_i8, .val.i8 = (x) })
#define I16(x) ((Any) { .type = &type_i16, .val.i16 = (x) })
#define I32(x) ((Any) { .type = &type_i32, .val.i32 = (x) })
#define I64(x) ((Any) { .type = &type_i64, .val.i64 = (x) })

#define F32(x) ((Any) { .type = &type_f32, .val.f32 = (x) })
#define F64(x) ((Any) { .type = &type_f64, .val.f64 = (x) })

#define TYPE(x) ((Any) { .type = &type_type, .val.type = (x) })

#define REFCOUNT(ptr) (((uint32_t *)(ptr))[-1])
#define MAYBE_ADDREF(any) ((any).type->kind == KIND_REF && ++REFCOUNT((any).val.ptr))

uint64_t to_u64(Any num) {
    switch (num.type->kind) {
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

size_t to_size(Any num) {
    switch (num.type->kind) {
    case KIND_U8: return num.val.u8;
    case KIND_U16: return num.val.u16;
    case KIND_U32: return num.val.u32;
    case KIND_U64: assert(num.val.u64 < SIZE_MAX); return (size_t)num.val.u64;
    case KIND_I8: assert(num.val.i8 >= 0); return num.val.i8;
    case KIND_I16: assert(num.val.i16 >= 0); return num.val.i16;
    case KIND_I32: assert(num.val.i32 >= 0); return num.val.i32;
    case KIND_I64: assert(num.val.i64 >= 0 && num.val.i64 < SIZE_MAX); return (size_t)num.val.i64;
    default: assert(0 && "non-integral value"); return 0;
    }
}

Any to_any(void *ptr, const Type *type) {
    switch (type->kind) {
    case KIND_ANY:  return *(Any *)ptr;
    case KIND_UNIT: return UNIT;
    case KIND_BOOL: return *(bool *)ptr ? TRUE : FALSE;
    case KIND_U8:   return U8(*(uint8_t *)ptr);
    case KIND_U16:  return U16(*(uint16_t *)ptr);
    case KIND_U32:  return U32(*(uint32_t *)ptr);
    case KIND_U64:  return U64(*(uint64_t *)ptr);
    case KIND_I8:   return I8(*(int8_t *)ptr);
    case KIND_I16:  return I16(*(int16_t *)ptr);
    case KIND_I32:  return I32(*(int32_t *)ptr);
    case KIND_I64:  return I64(*(int64_t *)ptr);
    case KIND_F32:  return F32(*(float *)ptr);
    case KIND_F64:  return F64(*(double *)ptr);
    case KIND_SYMBOL:
    case KIND_TYPE:
    case KIND_PTR:
    case KIND_REF:
        return (Any) { .type = type, .val.ptr = *(void **)ptr };
    }
}

void from_any(Any any, void *dst) {
    switch (any.type->kind) {
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
    case KIND_SYMBOL:
    case KIND_TYPE:
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
    String *s = rc_alloc(sizeof(String) + len + 1);
    s->length = len;
    memcpy(s->data, str, len + 1);
    return (Any) { .type = &type_ref_string, .val.string_ptr = s };
}

Any cons(Any car, Any cdr) {
    Cons *c = rc_alloc(sizeof(Cons));
    c->car = car;
    c->cdr = cdr;
    MAYBE_ADDREF(car);
    MAYBE_ADDREF(cdr);
    return (Any) { .type = &type_ref_cons, .val.cons_ptr = c };
}

Any car(Any cons) {
    assert(cons.type == &type_ref_cons);
    return cons.val.cons_ptr->car;
}

Any cdr(Any cons) {
    assert(cons.type == &type_ref_cons);
    return cons.val.cons_ptr->cdr;
}

Any array_length(Any arr) {
    assert(IS_PTR_KIND(arr.type->kind));
    
    const Type *arr_type = arr.type->target;
    assert(arr_type);
    assert(arr_type->kind == KIND_ARRAY);
    
    const Type *value_type = arr_type->target;
    assert(value_type);

    if (arr_type->size < 0) {
        return U64(arr.val.array_ptr->length);
    }
    else {
        return U64(arr_type->size / value_type->size);
    }
}

Any array_get(Any arr, Any idx) {
    size_t i = to_size(idx);

    assert(IS_PTR_KIND(arr.type->kind));
    
    const Type *arr_type = arr.type->target;
    assert(arr_type);
    assert(arr_type->kind == KIND_ARRAY);

    const Type *value_type = arr_type->target;
    assert(value_type);

    if (arr_type->flags & FLAG_UNSIZED) {
        size_t len = arr.val.array_ptr->length;
        assert(i < len);
        return to_any(arr.val.array_ptr->data + i * value_type->size, value_type);
    }
    else {
        size_t len = arr_type->size / value_type->size;
        assert(i < len);
        return to_any((char *)arr.val.ptr + i * value_type->size, value_type);
    }
}

Any array_set(Any arr, Any idx, Any val) {
    size_t i = to_size(idx);

    assert(IS_PTR_KIND(arr.type->kind));

    const Type *arr_type = arr.type->target;
    assert(arr_type);
    assert(arr_type->kind == KIND_ARRAY);

    const Type *value_type = arr_type->target;
    assert(value_type);

    if (arr_type->flags & FLAG_UNSIZED) {
        size_t len = arr.val.array_ptr->length;
        void *ptr = arr.val.array_ptr->data + i * value_type->size;
        assert(i < len);
        from_any(val, ptr);
    }
    else {
        size_t len = arr_type->size / value_type->size;
        void *ptr = (char *)arr.val.ptr + i * value_type->size;
        assert(i < len);
        from_any(val, ptr);
    }

    return UNIT;
}




typedef struct VMCtx VMCtx;
typedef struct Function Function;

enum {
    OP_LABEL, /* labels are eliminated during compilation */
    OP_JUMP_LABEL,
    OP_JFALSE_LABEL,
    OP_JTRUE_LABEL,

    OP_JUMP,
    OP_JFALSE,
    OP_JTRUE,
    OP_TCALL,
    OP_CALL,
    OP_RET,

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
    OP_ANY_TO_U32,
    OP_ANY_TO_U64,
    OP_ANY_TO_I32,
    OP_ANY_TO_I64,
    OP_ANY_TO_F32,
    OP_ANY_TO_F64,

    OP_ADD,
    OP_ADD_F32,
    OP_ADD_F64,

    OP_SUB,
    OP_SUB_F32,
    OP_SUB_F64,

    OP_MUL_U32,
    OP_MUL_U64,
    OP_MUL_I32,
    OP_MUL_I64,
    OP_MUL_F32,
    OP_MUL_F64,
};

#define INSTR(Op) ((Word) { .i.op = (Op) })
#define INSTR_WITH_EXTRA(Op, Extra) ((Word) { .i.op = (Op), .i.extra = (Extra) })
#define INSTR_OP(instr) ((instr).i.op)
#define INSTR_I32(instr) ((int32_t)(instr).i.extra)
#define INSTR_U32(instr) ((uint32_t)(instr).i.extra)
#define INSTR_U64(instr) ((instr).i.extra)

struct VMCtx {
    Word *stack;
    Word *sp;
};

struct Function {
    Word *code;
};

#define MAX_STACK 4096
#define STACK_START (vmcx->stack)
#define STACK_END (STACK_START + MAX_STACK)

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
            if (!sp[-1].b32) {
                ip += INSTR_I32(instr) - 1;
            }
            continue;
        case OP_JTRUE:
            assert(sp > STACK_START);
            if (sp[-1].b32) {
                ip += INSTR_I32(instr) - 1;
            }
            continue;
        case OP_TCALL: {
            Function *fun = *(Function **)(sp - 1);
            fp = ++sp;
            ip = fun->code; /* tail call means we just overwrite these */
            continue;
        }
        case OP_CALL: {
            Function *fun = *(Function **)(sp - 1);
            vmcx->sp = ++sp;
            call(vmcx, fun);
            sp = vmcx->sp;
            continue;
        }
        case OP_RET: vmcx->sp = sp; return;

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

        case OP_UNIT_TO_ANY:    *(Any *)(sp    ) = (Any) { .type = &type_unit }; ++sp; continue;
        case OP_BOOL_TO_ANY:    *(Any *)(sp - 1) = (Any) { .type = &type_bool,  .val.b32 = sp[-1].b32 }; ++sp; continue;
        case OP_U8_TO_ANY:      *(Any *)(sp - 1) = (Any) { .type = &type_u8,    .val.u8  = sp[-1].u8  }; ++sp; continue;
        case OP_U16_TO_ANY:     *(Any *)(sp - 1) = (Any) { .type = &type_u16,   .val.u16 = sp[-1].u16 }; ++sp; continue;
        case OP_U32_TO_ANY:     *(Any *)(sp - 1) = (Any) { .type = &type_u32,   .val.u32 = sp[-1].u32 }; ++sp; continue;
        case OP_U64_TO_ANY:     *(Any *)(sp - 1) = (Any) { .type = &type_u64,   .val.u64 = sp[-1].u64 }; ++sp; continue;
        case OP_I8_TO_ANY:      *(Any *)(sp - 1) = (Any) { .type = &type_i8,    .val.i8  = sp[-1].i8  }; ++sp; continue;
        case OP_I16_TO_ANY:     *(Any *)(sp - 1) = (Any) { .type = &type_i16,   .val.i16 = sp[-1].i16 }; ++sp; continue;
        case OP_I32_TO_ANY:     *(Any *)(sp - 1) = (Any) { .type = &type_i32,   .val.i32 = sp[-1].i32 }; ++sp; continue;
        case OP_I64_TO_ANY:     *(Any *)(sp - 1) = (Any) { .type = &type_i64,   .val.i64 = sp[-1].i64 }; ++sp; continue;
        case OP_F32_TO_ANY:     *(Any *)(sp - 1) = (Any) { .type = &type_f32,   .val.f32 = sp[-1].f32 }; ++sp; continue;
        case OP_F64_TO_ANY:     *(Any *)(sp - 1) = (Any) { .type = &type_f64,   .val.f64 = sp[-1].f64 }; ++sp; continue;

        case OP_ANY_TO_UNIT: continue;
        case OP_ANY_TO_BOOL: continue;
        case OP_ANY_TO_U32: continue;
        case OP_ANY_TO_U64: continue;
        case OP_ANY_TO_I32: continue;
        case OP_ANY_TO_I64: continue;
        case OP_ANY_TO_F32: continue;
        case OP_ANY_TO_F64: continue;

        case OP_ADD:     sp[-2].u64 += sp[-1].u64; --sp; continue;
        case OP_ADD_F32: sp[-2].f32 += sp[-1].f32; --sp; continue;
        case OP_ADD_F64: sp[-2].f64 += sp[-1].f64; --sp; continue;

        case OP_SUB:     sp[-2].u64 -= sp[-1].u64; --sp; continue;
        case OP_SUB_F32: sp[-2].f32 -= sp[-1].f32; --sp; continue;
        case OP_SUB_F64: sp[-2].f64 -= sp[-1].f64; --sp; continue;

        case OP_MUL_U64: sp[-2].u64 *= sp[-1].u64; --sp; continue;
        case OP_MUL_I64: sp[-2].i64 *= sp[-1].i64; --sp; continue;
        case OP_MUL_F32: sp[-2].f32 *= sp[-1].f32; --sp; continue;
        case OP_MUL_F64: sp[-2].f64 *= sp[-1].f64; --sp; continue;

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
        case OP_BOOL_TO_ANY: printf("bool_to_any\n"); break;
        case OP_U8_TO_ANY: printf("u8_to_any\n"); break;
        case OP_U16_TO_ANY: printf("u16_to_any\n"); break;
        case OP_U32_TO_ANY: printf("u32_to_any\n"); break;
        case OP_U64_TO_ANY: printf("u64_to_any\n"); break;
        case OP_I8_TO_ANY: printf("i8_to_any\n"); break;
        case OP_I16_TO_ANY: printf("i16_to_any\n"); break;
        case OP_I32_TO_ANY: printf("i32_to_any\n"); break;
        case OP_I64_TO_ANY: printf("i64_to_any\n"); break;
        case OP_F32_TO_ANY: printf("f32_to_any\n"); break;
        case OP_F64_TO_ANY: printf("f64_to_any\n"); break;

        case OP_ANY_TO_UNIT: printf("any_to_unit\n"); break;
        case OP_ANY_TO_BOOL: printf("any_to_bool\n"); break;
        case OP_ANY_TO_U32: printf("any_to_u32\n"); break;
        case OP_ANY_TO_U64: printf("any_to_u64\n"); break;
        case OP_ANY_TO_I32: printf("any_to_i32\n"); break;
        case OP_ANY_TO_I64: printf("any_to_i64\n"); break;
        case OP_ANY_TO_F32: printf("any_to_f32\n"); break;
        case OP_ANY_TO_F64: printf("any_to_f64\n"); break;

        case OP_ADD: printf("add32\n"); break;
        case OP_ADD_F32: printf("add32f\n"); break;
        case OP_ADD_F64: printf("add64f\n"); break;

        case OP_SUB: printf("sub\n"); break;
        case OP_SUB_F32: printf("sub32f\n"); break;
        case OP_SUB_F64: printf("sub64f\n"); break;

        case OP_MUL_U32: printf("mul32u\n"); break;
        case OP_MUL_U64: printf("mul64u\n"); break;
        case OP_MUL_I32: printf("mul32i\n"); break;
        case OP_MUL_I64: printf("mul64i\n"); break;
        case OP_MUL_F32: printf("mul32f\n"); break;
        case OP_MUL_F64: printf("mul64f\n"); break;

        }

        i += instr_word_count(instr);
    }
}


const Type *compile(CompilerCtx *cctx, Any form) {
    switch (form.type->kind) {
    case KIND_UNIT:
        return form.type;
    case KIND_BOOL:
        emit_lit_7_bytes(cctx, form.val.b32);
        return form.type;
    case KIND_U8:
    case KIND_I8:
        emit_lit_7_bytes(cctx, form.val.u8);
        return form.type;
    case KIND_U16:
    case KIND_I16:
        emit_lit_7_bytes(cctx, form.val.u16);
        return form.type;
    case KIND_U32:
    case KIND_I32:
    case KIND_F32:
        emit_lit_1_word(cctx, form.val.u32);
        return form.type;
    case KIND_U64:
    case KIND_I64:
    case KIND_F64:
        emit_lit_1_word(cctx, form.val.u64);
        return form.type;
    case KIND_CONS: {
        Any head = car(form);

        if (head.val.symbol /*== if*/) {
            uint32_t else_label = gen_label(cctx);
            uint32_t end_label = gen_label(cctx);

            Any temp = cdr(form);
            const Type *cond_type = compile(cctx, car(temp));
            if (cond_type->kind == KIND_ANY) {
                emit_u64(cctx, OP_ANY_TO_BOOL);
            }
            else {
                assert(cond_type->kind == KIND_BOOL);
            }
            emit_jfalse(cctx, else_label);

            temp = cdr(temp);
            const Type *then_type = compile(cctx, car(temp));
            emit_jump(cctx, end_label);

            emit_u64(cctx, else_label);
            temp = cdr(temp);
            const Type *else_type = compile(cctx, car(temp));

            emit_u64(cctx, end_label);

            temp = cdr(temp);
            assert(temp.type->kind == KIND_UNIT);
            assert(then_type == else_type);
            return then_type;
        }

        /* call */
        const Type *fun_type = compile(cctx, head);
        emit_u64(cctx, OP_CALL);
    }
        
    }
}



int main() {
    CompilerCtx *cctx = calloc(1, sizeof(CompilerCtx));

    emit_lit_7_bytes(cctx, 10);
    emit_label(cctx, 123);
    emit_u64(cctx, OP_DUP);
    emit_u64(cctx, OP_PRINT);
    emit_lit_7_bytes(cctx, 1);
    emit_u64(cctx, OP_SUB);
    emit_jtrue(cctx, 123);
    emit_u64(cctx, OP_RET);

    printf("\ncode:\n");
    print_code(cctx);

    strip_labels(cctx);

    printf("\nstripped code:\n");
    print_code(cctx);
    printf("\n");

    Function fun = { cctx->code };
    VMCtx vm = { malloc(sizeof(uint64_t) * MAX_STACK) };
    vm.sp = vm.stack;

    fgetc(stdin);
    call(&vm, &fun);

    fgetc(stdin);
    return 0;
}

