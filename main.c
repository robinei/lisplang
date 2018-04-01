#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "any.h"
#include "read.h"
#include "compile.h"
#include "interpret.h"


#define DEF_VAL_PRINTER(UNAME, LNAME, TYPE, FMT) case KIND_ ## UNAME: printf(FMT, form.val.LNAME); return;

void print_form(Any form) {
    const Type *type = ANY_TYPE(form);
    if (type == type_ref_string) {
        printf("\"%s\"", form.val.u8_array_ptr->data);
        return;
    }
    if (type == type_ptr_symbol) {
        printf("%s", form.val.symbol_ptr->name->data);
        return;
    }
    if (type == type_ref_cons) {
        printf("(");
        bool printed_one = false;
        while (ANY_KIND(form) != KIND_UNIT) {
            if (printed_one) {
                printf(" ");
            }
            print_form(car(form));
            form = cdr(form);
            printed_one = true;
        }
        printf(")");
        return;
    }
    switch (ANY_KIND(form)) {
    case KIND_UNIT: printf("()"); return;
    FOR_ALL_PRIM(DEF_VAL_PRINTER)
    }
    assert(0 && "missing printer");
}


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
    assert(list_length(make_list(ANY_TRUE, ANY_FALSE, ANY_UNIT)) == 2);

    const char *prog =
        "(let (n %u a 0 b 1 temp 0)\n"
        "  (tagbody\n"
        "    start\n"
        "    (= n (- n 1))\n"
        "    (if (> n 0)\n"
        "      (tagbody\n"
        "        (= temp b)\n"
        "        (= b (+ a b))\n"
        "        (= a temp)"
        "        (go start))\n"
        "      (go end))\n"
        "    end"
        "    (print b)))\n";
    char prog2[1000];
    sprintf(prog2, prog, iters);

    Any sexpr = read_cstr(prog2);

    print_form(sexpr);
    printf("\n");

    CodeBlock block = compile_block(sexpr);

    Word stack[1024];
    before = clock();
    interpret(block.code, stack);
    printf("interpreted time: %u ms\n", (uint32_t)((clock() - before) * 1000 / CLOCKS_PER_SEC));
    
    return 0;
}
