#ifndef ANY_H
#define ANY_H

#include "types.h"

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

bool nullp(Any any);
bool consp(Any any);
bool symbolp(Any any);
const Symbol *to_symbol(Any any);

uint32_t to_u32(Any num);
uint64_t to_u64(Any num);
Any to_any(void *ptr, const Type *type);
void from_any(Any any, void *dst);
Any make_string(const char *str);
Any make_symbol(const char *str);

Any cons(Any car, Any cdr);
Any make_list(Any first, ...);
Any car(Any cons);
Any cdr(Any cons);
uint32_t list_length(Any lst);
Any list_nth(Any lst, uint32_t n);
Any list_take(Any lst, uint32_t n);
Any list_drop(Any lst, uint32_t n);

Any array_length(Any arr);
Any array_get(Any arr, Any idx);
Any array_set(Any arr, Any idx, Any val);

void print_any(Any any);

#endif
