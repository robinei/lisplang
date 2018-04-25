#include "any.h"

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>

#define REFCOUNT(ptr) (((uint32_t *)(ptr))[-1])
#define MAYBE_ADDREF(any) (ANY_KIND(any) == KIND_REF && ++REFCOUNT((any).val.ptr))

#define IS_ANY_SYM(any, sym) (ANY_TYPE(any) == type_ptr_symbol && (any).val.symbol_ptr == sym)

bool nullp(Any any) {
    return ANY_KIND(any) == KIND_UNIT;
}
bool consp(Any any) {
    return ANY_TYPE(any) == type_ref_cons;
}
bool symbolp(Any any) {
    return ANY_TYPE(any) == type_ptr_symbol;
}
const Symbol *to_symbol(Any any) {
    assert(symbolp(any));
    return any.val.symbol_ptr;
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

static void *rc_alloc(uint32_t size) {
    uint32_t *rc_ptr = calloc(1, sizeof(uint32_t) + size);
    return rc_ptr + 1;
}

Any make_string(const char *str) {
    uint32_t len = (uint32_t)strlen(str);
    U8Array *s = rc_alloc(sizeof(U8Array) + len + 1);
    s->length = len;
    memcpy(s->data, str, len + 1);
    return (Any) { .type = MK_ANY_TYPE(type_ref_string), .val.u8_array_ptr = s };
}

Any make_symbol(const char *str) {
    return MAKE_ANY_SYM(intern_symbol_cstr(str));
}

Any cons(Any car, Any cdr) {
    assert(consp(cdr) || nullp(cdr));
    Cons *c = rc_alloc(sizeof(Cons));
    c->car = car;
    c->cdr = cdr;
    MAYBE_ADDREF(car);
    MAYBE_ADDREF(cdr);
    return (Any) { .type = MK_ANY_TYPE(type_ref_cons), .val.cons_ptr = c };
}

Any make_list(Any first, ...) {
    Any arr[128];
    arr[0] = first;
    int i = 0;

    va_list args;
    va_start(args, first);
    for (;;) {
        Any x = va_arg(args, Any);
        if (nullp(x)) {
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
    while (!nullp(lst)) {
        ++len;
        lst = cdr(lst);
    }
    return len;
}

Any list_nth(Any lst, uint32_t n) {
    if (n == 0) {
        return car(lst);
    }
    return list_nth(cdr(lst), n - 1);
}

Any list_take(Any lst, uint32_t n) {
    if (n == 0) {
        return ANY_UNIT;
    }
    return cons(car(lst), list_take(cdr(lst), n - 1));
}

Any list_drop(Any lst, uint32_t n) {
    if (n == 0) {
        return lst;
    }
    return list_drop(cdr(lst), n - 1);
}

Any list_append(Any a, Any b) {
    if (nullp(a)) {
        return b;
    }
    return cons(car(a), list_append(cdr(a), b));
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

#define DEF_PRINTER(UNAME, LNAME, TYPE, FMT) \
    case KIND_ ## UNAME: printf("printed: " FMT "\n", any.val.LNAME); break;

void print_any(Any any) {
    switch (ANY_KIND(any)) {
    FOR_ALL_PRIM(DEF_PRINTER)
    default: assert(0 && "unsupported type");
    }
}
