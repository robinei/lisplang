#include "pprint.h"
#include "any.h"

#include <stdio.h>
#include <string.h>

#define COLOR_NORMAL    "\x1B[0m"
#define COLOR_RED       "\x1B[31m"
#define COLOR_GREEN     "\x1B[32m"
#define COLOR_YELLOW    "\x1B[33m"
#define COLOR_BLUE      "\x1B[34m"
#define COLOR_MAGENTA   "\x1B[35m"
#define COLOR_CYAN      "\x1B[36m"
#define COLOR_WHITE     "\x1B[37m"

#define PAREN_COLOR     COLOR_CYAN
#define KEYWORD_COLOR   COLOR_YELLOW
#define DESTRUCTIVE_OP_COLOR COLOR_YELLOW
#define OPERATOR_COLOR  COLOR_YELLOW
#define STRING_COLOR    COLOR_RED
#define NUMBER_COLOR    COLOR_BLUE
#define BOOL_COLOR      COLOR_BLUE
#define TYPE_COLOR      COLOR_GREEN
#define SYMBOL_COLOR    COLOR_NORMAL

static const char *curr_color = COLOR_NORMAL;

static void set_color(const char *color) {
#ifndef _WIN32
    if (strcmp(curr_color, color)) {
        printf("%s", color);
        curr_color = color;
    }
#endif
}

static const char *color_for_symbol(const Symbol *sym) {
    if (sym == symbol_if || sym == symbol_when || sym == symbol_unless || sym == symbol_do ||
        sym == symbol_tagbody || sym == symbol_go || sym == symbol_fun || sym == symbol_let ||
        sym == symbol_quote || sym == symbol_the) {
        return KEYWORD_COLOR;
    }
    if (sym == symbol_set || sym == symbol_inc || sym == symbol_dec) {
        return DESTRUCTIVE_OP_COLOR;
    }
    if (sym == symbol_plus || sym == symbol_minus || sym == symbol_mul || sym == symbol_div ||
        sym == symbol_mod || sym == symbol_eq || sym == symbol_lt || sym == symbol_gt ||
        sym == symbol_lteq || sym == symbol_gteq || sym == symbol_not) {
        return OPERATOR_COLOR;
    }
    if (sym == symbol_i8 || sym == symbol_i16 || sym == symbol_i32 || sym == symbol_i64 ||
        sym == symbol_u8 || sym == symbol_u16 || sym == symbol_u32 || sym == symbol_u64 ||
        sym == symbol_f32 || sym == symbol_f64) {
        return TYPE_COLOR;
    }
    return SYMBOL_COLOR;
}

static void print_char(int ch, uint32_t *col) {
    printf("%c", ch);
    ++*col;
}

static void print_space(uint32_t *col) {
    print_char(' ', col);
}

static void print_newline(uint32_t *col) {
    putchar('\n');
    *col = 0;
}

static void print_indent(uint32_t *col, uint32_t indent) {
    for (uint32_t i = 0; i < indent; ++i) {
        putchar(' ');
    }
    *col += indent;
}

static void print_newline_and_indent(uint32_t *col, uint32_t indent) {
    print_newline(col);
    print_indent(col, indent);
}

static void print_form(Any form, uint32_t *col);

static void print_all_on_same_line(Any form, uint32_t *col) {
    print_form(car(form), col);
    form = cdr(form);
    while (!nullp(form)) {
        print_space(col);
        print_form(car(form), col);
        form = cdr(form);
    }
}

static void print_separated_by_newline_keeping_indent(Any form, uint32_t *col) {
    uint32_t indent = *col;
    print_form(car(form), col);
    form = cdr(form);
    while (!nullp(form)) {
        print_newline_and_indent(col, indent);
        print_form(car(form), col);
        form = cdr(form);
    }
}

static void print_two_then_rest_on_new_line_keeping_last_indent(Any form, uint32_t *col) {
    print_form(car(form), col);
    form = cdr(form);
    print_space(col);
    print_separated_by_newline_keeping_indent(form, col);
}

static void print_one_then_rest_on_new_line_keeping_orig_indent_plus(Any form, uint32_t *col) {
    uint32_t orig_indent = *col;
    print_form(car(form), col);
    form = cdr(form);
    if (!nullp(form)) {
        print_newline_and_indent(col, orig_indent + 1);
        print_separated_by_newline_keeping_indent(form, col);
    }
}

static void print_two_then_rest_on_new_line_keeping_orig_indent_plus(Any form, uint32_t *col) {
    uint32_t orig_indent = *col;
    print_form(car(form), col);
    form = cdr(form);
    print_space(col);
    print_form(car(form), col);
    form = cdr(form);
    if (!nullp(form)) {
        print_newline_and_indent(col, orig_indent + 1);
        print_separated_by_newline_keeping_indent(form, col);
    }
}

static Any print_two_and_two_then_newline_keeping_indent_returning_rest(Any form, uint32_t *col) {
    uint32_t indent = *col;
    while (!nullp(form) && !nullp(cdr(form))) {
        print_form(car(form), col);
        form = cdr(form);
        print_space(col);
        print_form(car(form), col);
        form = cdr(form);
        if (!nullp(form) && !nullp(cdr(form))) {
            print_newline_and_indent(col, indent);
        }
    }
    return form;
}

static void print_let(Any form, uint32_t *col) {
    uint32_t orig_indent = *col;
    print_form(car(form), col); /* let */
    print_space(col);
    form = cdr(form);

    if (symbolp(car(form))) {
        form = print_two_and_two_then_newline_keeping_indent_returning_rest(form, col);
    } else {
        set_color(PAREN_COLOR);
        print_char('(', col);
        print_two_and_two_then_newline_keeping_indent_returning_rest(car(form), col);
        set_color(PAREN_COLOR);
        print_char(')', col);
        form = cdr(form);
    }

    if (!nullp(form)) {
        print_newline_and_indent(col, orig_indent + 1);
        print_separated_by_newline_keeping_indent(form, col);
    }
}

static void print_form(Any form, uint32_t *col) {
    const Type *type = ANY_TYPE(form);
    if (type == type_ref_string) {
        set_color(STRING_COLOR);
        *col += printf("\"%s\"", form.val.u8_array_ptr->data);
        return;
    }
    if (type == type_ptr_symbol) {
        set_color(color_for_symbol(form.val.symbol_ptr));
        *col += printf("%s", form.val.symbol_ptr->name->data);
        return;
    }
    if (type == type_ref_cons) {
        Any head = car(form);
        if (symbolp(head) && head.val.symbol_ptr == symbol_the) {
            form = cdr(form);
            Any type = car(form);
            form = cdr(form);
            assert(nullp(cdr(form)));
            print_form(car(form), col);
            print_space(col);
            set_color(PAREN_COLOR);
            *col += printf("::");
            print_space(col);
            print_form(type, col);
            return;
        }
        set_color(PAREN_COLOR);
        print_char('(', col);
        if (!symbolp(head)) {
            print_all_on_same_line(form, col);
        } else {
            const Symbol *sym = head.val.symbol_ptr;
            if (sym == symbol_if || sym == symbol_do) {
                print_two_then_rest_on_new_line_keeping_last_indent(form, col);
            } else if (sym == symbol_tagbody) {
                print_one_then_rest_on_new_line_keeping_orig_indent_plus(form, col);
            } else if (sym == symbol_when || sym == symbol_unless || sym == symbol_fun) {
                print_two_then_rest_on_new_line_keeping_orig_indent_plus(form, col);
            } else if (sym == symbol_let) {
                print_let(form, col);
            } else {
                print_all_on_same_line(form, col);
            }
        }
        set_color(PAREN_COLOR);
        print_char(')', col);
        return;
    }
#define DEF_VAL_PRINTER(UNAME, LNAME, TYPE, FMT) case KIND_ ## UNAME: set_color(NUMBER_COLOR); *col += printf(FMT, form.val.LNAME); return;
    switch (ANY_KIND(form)) {
    case KIND_UNIT: set_color(PAREN_COLOR); *col += printf("()"); return;
    case KIND_BOOL: set_color(BOOL_COLOR); *col += printf("%s", form.val.b32 ? "#t" : "#f"); return;
    FOR_ALL_NUM(DEF_VAL_PRINTER)
    PROPS_PTR(DEF_VAL_PRINTER)
    }
    assert(0 && "missing printer");
}

void pretty_print(Any form) {
    uint32_t col = 0;
    print_form(form, &col);
    set_color(COLOR_NORMAL);
    print_newline(&col);
}
