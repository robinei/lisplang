#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "types.h"
#include "any.h"
#include "compile.h"
#include "interpret.h"


static uint64_t native_fib(uint32_t n) {
    uint64_t a = 0;
    uint64_t b = 1;
    uint64_t temp;
start:
    --n;
    if (n > 0) {
        temp = b;
        b = a + b;
        a = temp;
        goto start;
    }
    else {
        goto end;
    }
end:
    return b;
}


#define sym(x) MAKE_ANY_SYM(intern_symbol_cstr(#x))
#define i32(x) MAKE_ANY_I32(x)
#define u32(x) MAKE_ANY_U32(x)
#define u64(x) MAKE_ANY_U64(x)
#define nil ANY_UNIT
#define list(...) make_list(__VA_ARGS__, nil)


int main(int argc, char *argv[]) {
    init_types();
    init_symbols();
    init_compile();

    uint32_t iters = 10000000;
    if (argc > 1) {
        iters = atoi(argv[1]);
    }
    printf("op count: %d\n\n", NUM_OPS);

    printf("fib(5) = %"SCNu64"\n", native_fib(5));

    clock_t before = clock();
    uint64_t res1 = native_fib(iters);
    printf("native_fib result = %"SCNu64"\n", res1);
    printf("native time: %u ms\n", (uint32_t)((clock() - before) * 1000 / CLOCKS_PER_SEC));

    assert(ANY_KIND(ANY_UNIT) == KIND_UNIT);
    assert(ANY_KIND(ANY_TRUE) == KIND_BOOL);
    assert(ANY_KIND(ANY_FALSE) == KIND_BOOL);
    assert(ANY_TYPE(ANY_UNIT) == type_unit);
    assert(ANY_TYPE(ANY_TRUE) == type_b32);
    assert(ANY_TYPE(ANY_FALSE) == type_b32);
    assert(list_length(make_list(ANY_TRUE, ANY_FALSE, nil)) == 2);

    Any sexpr =
        list(
            sym(let),
            list(
                sym(n), u32(iters),
                sym(a), u64(0),
                sym(b), u64(1),
                sym(temp), u64(0)
            ),
            list(
                sym(tagbody),
                sym(start),
                list(sym(=), sym(n), list(sym(-), sym(n), u32(1))),
                list(
                    sym(if),
                    list(sym(>), sym(n), u32(0)),
                    list(
                        sym(tagbody),
                        list(sym(=), sym(temp), sym(b)),
                        list(sym(=), sym(b), list(sym(+), sym(a), sym(b))),
                        list(sym(=), sym(a), sym(temp)),
                        list(sym(go), sym(start))
                    ),
                    list(sym(go), sym(end))
                ),
                sym(end),
                list(sym(print), sym(b))
            )
        );
    
    CodeBlock block = compile_block(sexpr);

    Word stack[1024];
    before = clock();
    interpret(block.code, stack);
    printf("interpreted time: %u ms\n", (uint32_t)((clock() - before) * 1000 / CLOCKS_PER_SEC));
    
    return 0;
}
