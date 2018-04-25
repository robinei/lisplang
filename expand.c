#include "expand.h"
#include <stdio.h>

static Any expand_all(CompilerCtx *cctx, Any form) {
    if (nullp(form)) {
        return form;
    }
    Any head = expand_form(cctx, car(form));
    return cons(head, expand_all(cctx, cdr(form)));
}

static Any expand_let_bindings(CompilerCtx *cctx, Any form) {
    if (nullp(form)) {
        return form;
    }
    Any name_form = car(form);
    form = cdr(form);
    Any init_form = car(form);
    form = cdr(form);
    init_form = expand_form(cctx, init_form);
    return cons(name_form, cons(init_form, expand_let_bindings(cctx, form)));
}

static Any maybe_wrap_in_do(CompilerCtx *cctx, Any form);

static Any process_expr_seq(CompilerCtx *cctx, Any form) {
    if (!consp(form)) {
        return form;
    }
    Any part = expand_form(cctx, car(form));
    if (consp(part)) {
        Any head = car(part);
        if (symbolp(head)) {
            if (head.val.symbol_ptr == symbol_let) {
                Any args = cdr(part);
                if (nullp(cdr(args))) {
                    Any bindings = car(args);
                    Any body = maybe_wrap_in_do(cctx, cdr(form));
                    return cons(cons(MAKE_ANY_SYM(symbol_let), cons(bindings, cons(body, ANY_UNIT))), ANY_UNIT);
                }
            }
            else if (head.val.symbol_ptr == symbol_do) {
                return list_append(cdr(part), process_expr_seq(cctx, cdr(form)));
            }
        }
    }
    return cons(part, process_expr_seq(cctx, cdr(form)));
}

static Any maybe_wrap_in_do(CompilerCtx *cctx, Any form) {
    if (!consp(form)) {
        return form;
    }
    form = process_expr_seq(cctx, form);
    if (nullp(cdr(form))) {
        return car(form);
    }
    return cons(MAKE_ANY_SYM(symbol_do), form);
}

Any expand_form(CompilerCtx *cctx, Any form) {
    if (!consp(form)) {
        return form;
    }
    Any head = car(form);
    Any args = cdr(form);
    if (symbolp(head)) {
        const Symbol *sym = head.val.symbol_ptr;
        if (sym == symbol_quote) {
            return form;
        }
        if (sym == symbol_if) {
            args = expand_all(cctx, args);
            Any cond_form = car(args);
            args = cdr(args);
            Any then_form = car(args);
            args = cdr(args);
            if (!nullp(args)) {
                return cons(head, args);
            }
            return cons(head, cons(cond_form, cons(then_form, cons(ANY_UNIT, ANY_UNIT))));
        }
        /* TODO: replace "when" and "unless" expansion with macro */
        if (sym == symbol_when) {
            Any cond_form = expand_form(cctx, car(args));
            Any body = maybe_wrap_in_do(cctx, cdr(args));
            return cons(MAKE_ANY_SYM(symbol_if), cons(cond_form, cons(body, cons(ANY_UNIT, ANY_UNIT))));
        }
        if (sym == symbol_unless) {
            Any cond_form = expand_form(cctx, car(args));
            Any body = maybe_wrap_in_do(cctx, cdr(args));
            return cons(MAKE_ANY_SYM(symbol_if), cons(cond_form, cons(ANY_UNIT, cons(body, ANY_UNIT))));
        }
        if (sym == symbol_do) {
            Any body = process_expr_seq(cctx, args);
            if (consp(body) && nullp(cdr(body))) {
                return car(body);
            }
            return cons(MAKE_ANY_SYM(symbol_do), body);
        }
        if (sym == symbol_tagbody) {
            Any body = process_expr_seq(cctx, args);
            if (consp(body) && nullp(cdr(body))) {
                return car(body);
            }
            return cons(MAKE_ANY_SYM(symbol_tagbody), body);
        }
        if (sym == symbol_let) {
            Any bindings = car(args);
            Any body = cdr(args);
            if (symbolp(bindings)) {
                bindings = args;
                body = ANY_UNIT;
            }
            if (!nullp(body)) {
                body = cons(maybe_wrap_in_do(cctx, body), ANY_UNIT);
            }
            return cons(head, cons(expand_let_bindings(cctx, bindings), body));
        }
        if (sym == symbol_fun) {
            Any params = car(args);
            Any body = maybe_wrap_in_do(cctx, cdr(args));
            return cons(head, cons(params, cons(body, ANY_UNIT)));
        }
        /* TODO: if symbol is a macro then run the macro to expand the form */
    }
    return expand_all(cctx, form);
}
