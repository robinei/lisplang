#include "symbol.h"
#include "murmur3.h"

#include <string.h>

typedef struct StrSlice StrSlice;
struct StrSlice {
    const uint8_t *str;
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
    StrSlice slice = { (const uint8_t *)str, length };
    {
        const Symbol *sym;
        if (SymMap_get(&symbolmap, slice, &sym)) {
            return sym;
        }
    }
    Symbol *sym = malloc(sizeof(Symbol));
    sym->name = malloc(sizeof(U8Array) + length + 1);
    sym->hash = StrSlice_hash(slice);
    memcpy(sym->name->data, str, length);
    sym->name->length = length;
    sym->name->data[length] = 0;
    slice.str = sym->name->data;
    SymMap_put(&symbolmap, slice, sym);
    return sym;
}

const Symbol *intern_symbol_cstr(const char *str) {
    return intern_symbol(str, (uint32_t)strlen(str));
}

void init_symbols(void) {
    symbol_if       = intern_symbol_cstr("if");
    symbol_when     = intern_symbol_cstr("when");
    symbol_unless   = intern_symbol_cstr("unless");
    symbol_let      = intern_symbol_cstr("let");
    symbol_quote    = intern_symbol_cstr("quote");
    symbol_fun      = intern_symbol_cstr("fun");
    symbol_def      = intern_symbol_cstr("def");
    symbol_macrodef = intern_symbol_cstr("macro-def");
    symbol_do       = intern_symbol_cstr("do");
    symbol_set      = intern_symbol_cstr("set!");
    symbol_inc      = intern_symbol_cstr("inc!");
    symbol_dec      = intern_symbol_cstr("dec!");
    symbol_plus     = intern_symbol_cstr("+");
    symbol_minus    = intern_symbol_cstr("-");
    symbol_mul      = intern_symbol_cstr("*");
    symbol_div      = intern_symbol_cstr("/");
    symbol_mod      = intern_symbol_cstr("/");
    symbol_not      = intern_symbol_cstr("not");
    symbol_eq       = intern_symbol_cstr("=");
    symbol_lt       = intern_symbol_cstr("<");
    symbol_gt       = intern_symbol_cstr(">");
    symbol_lteq     = intern_symbol_cstr("<=");
    symbol_gteq     = intern_symbol_cstr(">=");
    symbol_print    = intern_symbol_cstr("print");
    symbol_tagbody  = intern_symbol_cstr("tagbody");
    symbol_go       = intern_symbol_cstr("go");
    symbol_the      = intern_symbol_cstr("the");
    symbol_bool     = intern_symbol_cstr("bool");
    symbol_u8       = intern_symbol_cstr("u8");
    symbol_u16      = intern_symbol_cstr("u16");
    symbol_u32      = intern_symbol_cstr("u32");
    symbol_u64      = intern_symbol_cstr("u64");
    symbol_i8       = intern_symbol_cstr("i8");
    symbol_i16      = intern_symbol_cstr("i16");
    symbol_i32      = intern_symbol_cstr("i32");
    symbol_i64      = intern_symbol_cstr("i64");
    symbol_f32      = intern_symbol_cstr("f32");
    symbol_f64      = intern_symbol_cstr("f64");
    symbol_array    = intern_symbol_cstr("array");
    
    assert(symbol_if);
    assert(intern_symbol_cstr("if") == symbol_if);
}
