#include "any.h"
#include "fnv.h"

#include <string.h>

STATIC_ASSERT(NUM_TYPE_KINDS < (1 << ANY_KIND_BITS), "not enough bits to represent type kinds");

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

const Type *intern_prim_type(uint32_t kind, uint32_t size) {
    Type type = { .kind = kind, .size = size };
    return intern_type(&type);
}

const Type *intern_array_type(const Type *elem_type) {
    Type type = { .kind = KIND_ARRAY, .flags = TYPE_FLAG_UNSIZED, .target = elem_type };
    return intern_type(&type);
}

const Type *intern_array_type_sized(const Type *elem_type, uint32_t elem_count) {
    Type type = { .kind = KIND_ARRAY, .size = elem_type->size * elem_count, .target = elem_type };
    return intern_type(&type);
}

const Type *intern_ptr_type(const Type *elem_type) {
    Type type = { .kind = KIND_PTR, .size = sizeof(void *), .target = elem_type };
    return intern_type(&type);
}

const Type *intern_ref_type(const Type *elem_type) {
    Type type = { .kind = KIND_REF, .size = sizeof(void *), .target = elem_type };
    return intern_type(&type);
}

static StructFieldArray *struct_field_array(uint32_t field_count, StructField *fields) {
    StructFieldArray *field_array = malloc(sizeof(StructFieldArray) + sizeof(StructField) * field_count);
    field_array->length = field_count;
    memcpy(field_array->fields, fields, sizeof(StructField) * field_count);
    return field_array;
}

const Type *intern_struct_type(uint32_t size, uint32_t field_count, StructField *fields) {
    StructFieldArray *field_array = struct_field_array(field_count, fields);
    Type type = { .kind = KIND_STRUCT, .size = size, .fields = field_array };
    if (field_count > 0 && fields[field_count - 1].type->flags & TYPE_FLAG_UNSIZED) {
        type.flags |= TYPE_FLAG_UNSIZED;
    }
    return intern_type(&type);
}

static FunParamArray *fun_param_array(uint32_t param_count, FunParam *params) {
    FunParamArray *param_array = malloc(sizeof(FunParamArray) + sizeof(FunParam) * param_count);
    param_array->length = param_count;
    memcpy(param_array->params, params, sizeof(FunParam) * param_count);
    return param_array;
}

const Type *intern_fun_type(const Type *ret_type, uint32_t param_count, FunParam *params) {
    FunParamArray *param_array = fun_param_array(param_count, params);
    Type type = { .kind = KIND_FUN, .size = sizeof(void *), .target = ret_type, .params = param_array };
    return intern_type(&type);
}

void init_types(void) {
    Type *type = calloc(1, sizeof(Type));
    type->kind = KIND_STRUCT;
    type->size = sizeof(Type);
    type->hash = 0x12345678; /* we have to invent a hash here, since this type is self-recursive and can't be properly hashed */
    TypeMap_put(&typemap, type->hash, type);

    type_type = type;
    type_ptr_type = intern_ptr_type(type_type);

    type_any = intern_prim_type(KIND_ANY, sizeof(Any));

    type_unit = intern_prim_type(KIND_UNIT, 0);
    type_b32 = intern_prim_type(KIND_BOOL, sizeof(bool));

    type_u8 = intern_prim_type(KIND_U8, sizeof(uint8_t));
    type_u16 = intern_prim_type(KIND_U16, sizeof(uint16_t));
    type_u32 = intern_prim_type(KIND_U32, sizeof(uint32_t));
    type_u64 = intern_prim_type(KIND_U64, sizeof(uint64_t));

    type_i8 = intern_prim_type(KIND_I8, sizeof(int8_t));
    type_i16 = intern_prim_type(KIND_I16, sizeof(int16_t));
    type_i32 = intern_prim_type(KIND_I32, sizeof(int32_t));
    type_i64 = intern_prim_type(KIND_I64, sizeof(int64_t));

    type_f32 = intern_prim_type(KIND_F32, sizeof(float));
    type_f64 = intern_prim_type(KIND_F64, sizeof(double));

    type_ptr_symbol = intern_ptr_type(intern_array_type(type_u8));
    type_ref_string = intern_ref_type(intern_array_type(type_u8));

    const Type *type_cons = intern_struct_type(sizeof(Cons), 2, (StructField[]) {
        { .name = intern_symbol_cstr("car"), .type = type_any, .offset = offsetof(Cons, car) },
        { .name = intern_symbol_cstr("cdr"), .type = type_any, .offset = offsetof(Cons, cdr) },
    });
    type_ref_cons = intern_ref_type(type_cons);

    /* complete Type type */
    const Type *type_struct_field = intern_struct_type(sizeof(StructField), 3, (StructField[]) {
        { .name = intern_symbol_cstr("name"), .type = type_ptr_symbol, .offset = offsetof(StructField, name) },
        { .name = intern_symbol_cstr("type"), .type = type_ptr_type, .offset = offsetof(StructField, type) },
        { .name = intern_symbol_cstr("offset"), .type = type_u32, .offset = offsetof(StructField, offset) },
    });
    type->fields = struct_field_array(5, (StructField[]) {
        { .name = intern_symbol_cstr("kind"), .type = type_u32, .offset = offsetof(Type, kind) },
        { .name = intern_symbol_cstr("flags"), .type = type_u32, .offset = offsetof(Type, flags) },
        { .name = intern_symbol_cstr("size"), .type = type_u32, .offset = offsetof(Type, size) },
        { .name = intern_symbol_cstr("target"), .type = type_ptr_type, .offset = offsetof(Type, target) },
        { .name = intern_symbol_cstr("fields"), .type = intern_ptr_type(intern_array_type(type_struct_field)), .offset = offsetof(Type, fields) },
    });

    assert(intern_prim_type(KIND_U32, sizeof(uint32_t)));
    assert(intern_prim_type(KIND_U32, sizeof(uint32_t)) == intern_prim_type(KIND_U32, sizeof(uint32_t)));
}
