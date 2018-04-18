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

static Any expand_and_maybe_wrap_in_do(CompilerCtx *cctx, Any form) {
    if (!consp(form)) {
        return form;
    }
    form = expand_all(cctx, form);
    uint32_t len = list_length(form);
    if (len == 1) {
        return form;
    }
    return cons(cons(MAKE_ANY_SYM(symbol_do), form), ANY_UNIT);
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
        if (sym == symbol_let) {
            Any bindings = car(args);
            Any body = cdr(args);
            if (symbolp(bindings)) {
                bindings = cons(bindings, cons(car(body), ANY_UNIT));
                body = cdr(body);
            }
            return cons(head, cons(expand_let_bindings(cctx, bindings), expand_and_maybe_wrap_in_do(cctx, body)));
        }
        if (sym == symbol_fun) {
            Any params = car(args);
            Any body = cdr(args);
            return cons(head, cons(params, expand_and_maybe_wrap_in_do(cctx, body)));
        }
        if (sym == symbol_do || sym == symbol_tagbody) {
            return cons(head, expand_all(cctx, args));
        }
        /* TODO: if symbol is a macro then run the macro to expand the form */
    }
    return expand_all(cctx, form);
}
