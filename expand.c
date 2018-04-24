#include "expand.h"

static Any expand_all(CompilerCtx *cctx, Any form) {
    if (nullp(form)) {
        return form;
    }
    return cons(expand_form(cctx, car(form)), expand_all(cctx, cdr(form)));
}

static Any expand_let_bindings(CompilerCtx *cctx, Any form) {
    if (nullp(form)) {
        return form;
    }
    Any name_form = car(form);
    form = cdr(form);
    Any init_form = car(form);
    form = cdr(form);
    return cons(name_form, cons(expand_form(cctx, init_form), expand_let_bindings(cctx, form)));
}

static Any maybe_wrap_in_do(Any form) {
    if (!consp(form)) {
        return form;
    }
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
        /* TODO: replace "when" expansion with macro */
        if (sym == symbol_when) {
            args = expand_all(cctx, args);
            Any cond_form = car(args);
            Any body = cdr(args);
            return cons(MAKE_ANY_SYM(symbol_if), cons(cond_form, cons(maybe_wrap_in_do(body), cons(ANY_UNIT, ANY_UNIT))));
        }
        if (sym == symbol_let) {
            Any bindings = car(args);
            Any body = cdr(args);
            if (symbolp(bindings)) {
                bindings = cons(bindings, cons(car(body), ANY_UNIT));
                body = cdr(body);
            }
            body = expand_all(cctx, body);
            return cons(head, cons(expand_let_bindings(cctx, bindings), cons(maybe_wrap_in_do(body), ANY_UNIT)));
        }
        if (sym == symbol_fun) {
            Any params = car(args);
            Any body = cdr(args);
            body = expand_all(cctx, body);
            return cons(head, cons(params, cons(maybe_wrap_in_do(body), ANY_UNIT)));
        }
        /* TODO: if symbol is a macro then run the macro to expand the form */
    }
    return expand_all(cctx, form);
}
