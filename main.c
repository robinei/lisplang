#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "read.h"
#include "expand.h"
#include "emit.h"
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
        while (!nullp(form)) {
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

static char *read_file(const char *filename) {
    char *str;
    uint32_t len;

    FILE *fp = fopen(filename, "rb");
    if (!fp) {
        return NULL;
    }

    fseek(fp, 0, SEEK_END);
    len = ftell(fp);
    fseek(fp, 0, SEEK_SET);
    str = malloc(len + 1);
    fread(str, 1, len, fp);
    str[len] = 0;

    return str;
}


typedef struct CodeBlock CodeBlock;
struct CodeBlock {
    uint64_t *code;
    uint32_t length;
};

CodeBlock compile_block(Any form) {
    BindingCtx bindings = {0};
    CompilerCtx cctx = { .bindings = &bindings };

    form = expand_form(&cctx, form);

    printf("\nexpanded program:\n");
    print_form(form);
    printf("\n\n");

    AstNode *ast = parse_form(&cctx, form, NULL, PARSE_FLAG_TOPLEVEL | PARSE_FLAG_TAILPOS, NULL);
    emit_code_block(&cctx, ast);
    
    printf("\nbefore strip:\n");
    print_code(cctx.code, cctx.code_used);

    strip_labels(&cctx);

    printf("\nafter strip:\n");
    print_code(cctx.code, cctx.code_used);

    return (CodeBlock) { cctx.code, cctx.code_used };
}

static uint64_t native_fib(int32_t n) {
    int32_t a = 0;
    int32_t b = 1;
    int32_t temp;
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

    int32_t iters = 10000000;
    if (argc > 1) {
        iters = atoi(argv[1]);
    }
    printf("op count: %d\n\n", NUM_OPS);

    printf("fib(5) = %"PRIu64"\n", native_fib(5));

    clock_t before = clock();
    uint64_t res1 = native_fib(iters);
    printf("native_fib result = %"PRIu64"\n", res1);
    printf("native time: %u ms\n", (uint32_t)((clock() - before) * 1000 / CLOCKS_PER_SEC));

    assert(ANY_KIND(ANY_UNIT) == KIND_UNIT);
    assert(ANY_KIND(ANY_TRUE) == KIND_BOOL);
    assert(ANY_KIND(ANY_FALSE) == KIND_BOOL);
    assert(ANY_TYPE(ANY_UNIT) == type_unit);
    assert(ANY_TYPE(ANY_TRUE) == type_b32);
    assert(ANY_TYPE(ANY_FALSE) == type_b32);
    assert(list_length(make_list(ANY_TRUE, ANY_FALSE, ANY_UNIT)) == 2);

    Any sexpr = read_cstr(read_file("test.lisp"));

    printf("\noriginal program:\n");
    print_form(sexpr);
    printf("\n");

    CodeBlock block = compile_block(sexpr);

#if 1
    printf("\nrunning:\n");
    uint8_t stack[4096];
    before = clock();
    interpret(block.code, stack);
    printf("interpreted time: %u ms\n", (uint32_t)((clock() - before) * 1000 / CLOCKS_PER_SEC));
#endif

    return 0;
}
