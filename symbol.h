#ifndef SYMBOL_H
#define SYMBOL_H

#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>
#include <inttypes.h>
#include <assert.h>

typedef struct U8Array U8Array;
struct U8Array {
    uint32_t length;
    uint8_t data[];
};

typedef struct Symbol Symbol;
struct Symbol {
    uint32_t hash;
    U8Array *name;
};

const Symbol *symbol_if;
const Symbol *symbol_let;
const Symbol *symbol_quote;
const Symbol *symbol_fun;
const Symbol *symbol_def;
const Symbol *symbol_do;
const Symbol *symbol_assign;
const Symbol *symbol_inc;
const Symbol *symbol_dec;
const Symbol *symbol_plus;
const Symbol *symbol_minus;
const Symbol *symbol_mul;
const Symbol *symbol_div;
const Symbol *symbol_mod;
const Symbol *symbol_not;
const Symbol *symbol_eq;
const Symbol *symbol_lt;
const Symbol *symbol_gt;
const Symbol *symbol_lteq;
const Symbol *symbol_gteq;
const Symbol *symbol_print;
const Symbol *symbol_tagbody;
const Symbol *symbol_go;
const Symbol *symbol_the;
const Symbol *symbol_bool;
const Symbol *symbol_u8;
const Symbol *symbol_u16;
const Symbol *symbol_u32;
const Symbol *symbol_u64;
const Symbol *symbol_i8;
const Symbol *symbol_i16;
const Symbol *symbol_i32;
const Symbol *symbol_i64;
const Symbol *symbol_f32;
const Symbol *symbol_f64;

void init_symbols(void);
const Symbol *intern_symbol(const char *str, uint32_t length);
const Symbol *intern_symbol_cstr(const char *str);

#endif
