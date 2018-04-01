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
    sym->name->length = length;
    memcpy(sym->name->data, str, length);
    sym->name->data[length] = 0;
    slice.str = sym->name->data;
    SymMap_put(&symbolmap, slice, sym);
    return sym;
}

const Symbol *intern_symbol_cstr(const char *str) {
    return intern_symbol(str, (uint32_t)strlen(str));
}

void init_symbols(void) {
    SymMap_init(&symbolmap, 512);

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
    symbol_mod      = intern_symbol_cstr("/");
    symbol_not      = intern_symbol_cstr("not");
    symbol_eq       = intern_symbol_cstr("==");
    symbol_lt       = intern_symbol_cstr("<");
    symbol_gt       = intern_symbol_cstr(">");
    symbol_lteq     = intern_symbol_cstr("<=");
    symbol_gteq     = intern_symbol_cstr(">=");
    symbol_print    = intern_symbol_cstr("print");
    symbol_tagbody  = intern_symbol_cstr("tagbody");
    symbol_go       = intern_symbol_cstr("go");
    
    assert(intern_symbol_cstr("foo"));
    assert(intern_symbol_cstr("foo") == intern_symbol_cstr("foo"));
}

