#include "any.h"

#include <stdarg.h>
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>


typedef struct SourceLoc SourceLoc;
struct SourceLoc {
    uint32_t line;
    uint32_t col;
};

#define EXPAND_INTERFACE
#define EXPAND_IMPLEMENTATION
#define NAME SourceMap
#define KEY_TYPE Cons *
#define VALUE_TYPE SourceLoc
#define HASH_FUNC(x) hashutil_uint32_mix((uint32_t)(uintptr_t)(x))
#define EQUAL_FUNC(x, y) ((x) == (y))
#include "hashtable.h"
typedef struct SourceMap SourceMap;



long long int my_strtoll(const char *nptr, const char **endptr, int base);
double my_strtod(const char *string, const char **endPtr);


static bool is_upper(char ch) { return ch >= 'A' && ch <= 'Z'; }

static bool is_lower(char ch) { return ch >= 'a' && ch <= 'z'; }

static bool is_alpha(char ch) { return is_upper(ch) || is_lower(ch); }

static bool is_digit(char ch) { return ch >= '0' && ch <= '9'; }

static bool is_alphanum(char ch) { return is_alpha(ch) || is_digit(ch); }

static bool is_symchar(char ch) {
    switch (ch) {
    case '_':
    case '-':
    case '=':
    case '+':
    case '*':
    case '/':
    case '?':
    case '!':
    case '&':
    case '%':
    case '^':
    case '~':
    case '<':
    case '>':
        return true;
    default:
        return false;
    }
}


#define SCRATCH_LEN 1024

typedef struct ReadState ReadState;
struct ReadState {
    const char *text;
    uint32_t pos;
    SourceLoc loc;

    char scratch[SCRATCH_LEN];
};


static void read_error(ReadState *state, const char *fmt, ...) {
    printf("line %d, col %d: ", state->loc.line + 1, state->loc.col + 1);
    va_list args;
    va_start(args, fmt);
    vprintf(fmt, args);
    va_end(args);
    printf("\n");
    exit(1);
}

static char peek(ReadState *state, int offset) {
    return state->text[state->pos + offset];
}

static void step(ReadState *state) {
    ++state->loc.col;
    ++state->pos;
}

static void spacestep(ReadState *state) {
    if (state->text[state->pos] == '\r') {
        if (state->text[state->pos + 1] != '\n') {
            ++state->loc.line;
            state->loc.col = 0;
            ++state->pos;
            return;
        }
    } else if (state->text[state->pos] == '\n') {
        ++state->loc.line;
        state->loc.col = 0;
        ++state->pos;
        return;
    }
    ++state->loc.col;
    ++state->pos;
}

static void skip_space(ReadState *state) {
    char ch;
    for (;;) {
        switch (peek(state, 0)) {
        case ' ':
        case '\t':
        case '\f':
        case '\v':
        case '\r':
        case '\n':
            spacestep(state);
            continue;
        case ';': /* line comment */
            do {
                spacestep(state);
                ch = peek(state, 0);
            } while (ch && ch != '\n' && ch != '\r');
            continue;
        default:
            return;
        }
    }
}

static void expect_delim(ReadState *state) {
    switch (peek(state, 0)) {
    case ' ':
    case '\t':
    case '\f':
    case '\v':
    case '\r':
    case '\n':
    case '.':
    case ':':
    case '(':
    case ')':
    case '[':
    case ']':
        return;
    }
    read_error(state, "expected delimiter after expression");
}

static Any read_string(ReadState *state) {
    char *scratch = state->scratch;
    uint32_t len = 0;
    for (;;) {
        char ch = peek(state, 0);
        if (ch == '"') {
            step(state);
            if (len >= SCRATCH_LEN) {
                read_error(state, "string is too long");
            }
            scratch[len] = '\0';
            return make_string(scratch);
        } else if (ch == '\\') {
            step(state);
            ch = peek(state, 0);
            if (ch == '\0') {
                read_error(state, "unexpected end of input while reading string");
            }
            if (len >= SCRATCH_LEN) {
                read_error(state, "string is too long");
            }
            switch (ch) {
            case '\'': scratch[len++] = '\''; break;
            case '"': scratch[len++] = '"'; break;
            case '?': scratch[len++] = '?'; break;
            case '\\': scratch[len++] = '\\'; break;
            case 'a': scratch[len++] = '\a'; break;
            case 'b': scratch[len++] = '\b'; break;
            case 'f': scratch[len++] = '\f'; break;
            case 'n': scratch[len++] = '\r'; break;
            case 'r': scratch[len++] = '\r'; break;
            case 't': scratch[len++] = '\t'; break;
            case 'v': scratch[len++] = '\v'; break;
            /* TODO: handle \nnn \xnn \unnnn \Unnnnnnnn */
            default: read_error(state, "unexpected escape char: %c", ch);
            }
            step(state);
        } else if (ch == '\0') {
            read_error(state, "unexpected end of input while reading string");
        } else {
            if (ch == '\r' || ch == '\n') {
                spacestep(state);
            } else {
                step(state);
            }
            if (len >= SCRATCH_LEN) {
                read_error(state, "string is too long");
            }
            scratch[len++] = ch;
        }
    }
    return ANY_UNIT;
}

static Any read_symbol(ReadState *state) {
    char *scratch = state->scratch;
    uint32_t len = 0;
    for (;;) {
        char ch = peek(state, 0);
        if (!is_alphanum(ch) && !is_symchar(ch)) {
            if (len == 0) {
                read_error(state, "expected a symbol");
            }
            if (len >= SCRATCH_LEN) {
                read_error(state, "string is too long");
            }
            scratch[len] = '\0';
            Any result = make_symbol(scratch);
            return result;
        }
        if (len >= SCRATCH_LEN) {
            read_error(state, "string is too long");
        }
        scratch[len++] = ch;
        step(state);
    }
}

static Any read_number(ReadState *state) {
    errno = 0;
    const char *end;
    const char *start = state->text + state->pos;
    /* TODO: handle unsigned numbers */
    int64_t llval = my_strtoll(start, &end, 0);
    if (start == end) {
        read_error(state, "error parsing number");
    }
    if (errno == ERANGE) {
        read_error(state, "number too large");
    }
    if (*end != '.') {
        state->pos += (int)(end - start);
        return MAKE_ANY_I32((int32_t)llval);
    }
    start = state->text + state->pos;
    double dval = my_strtod(start, &end);
    if (start == end) {
        read_error(state, "error parsing number");
    }
    if (errno == ERANGE) {
        read_error(state, "number too large");
    }
    state->pos += (int)(end - start);
    return MAKE_ANY_F32((float)dval);
}

static Any read_form(ReadState *state);

static Any read_list(ReadState *state, char end) {
    skip_space(state);
    if (peek(state, 0) == end) {
        step(state);
        return ANY_UNIT;
    }

    //SourceLoc loc_before_car = state->loc;
    Any form = read_form(state);
    //SourceLoc loc_after_car = state->loc;

    Any result = cons(form, read_list(state, end));

    /*if (state->mod) {
        rt_sourcemap_put(&state->mod->location_before_car, result.u.cons, loc_before_car);
        rt_sourcemap_put(&state->mod->location_after_car, result.u.cons, loc_after_car);
    }*/

    return result;
}

static Any read_form(ReadState *state) {
    Any result = ANY_UNIT;
    skip_space(state);
    char ch = peek(state, 0);
    if (ch == '(') {
        step(state);
        result = read_list(state, ')');
    } else if (ch == '#') {
        step(state);
        ch = peek(state, 0);
        if (ch == 't') {
            step(state);
            expect_delim(state);
            result = ANY_TRUE;
        } else if (ch == 'f') {
            step(state);
            expect_delim(state);
            result = ANY_FALSE;
        } else {
            read_error(state, "expected #t or #f");
        }
    } else if (ch == '\'') {
        step(state);
        Any form = read_form(state);
        result = cons(make_symbol("quote"), form);
    } else if (ch == '"') {
        step(state);
        result = read_string(state);
    } else if (is_alpha(ch) || is_symchar(ch)) {
        result = read_symbol(state);
    } else if (is_digit(ch) || ((ch == '+' || ch == '-') && is_digit(peek(state, 1)))) {
        result = read_number(state);
        expect_delim(state);
    } else {
        read_error(state, "expected an expression");
    }
    /*for (;;) {
        skip_space(state);
        ch = peek(state, 0);
        if (ch == '.') {
            step(state);
            skip_space(state);
            Any sym = read_symbol(state);
            result = cons(make_symbol("."), cons(sym, cons(result, ANY_UNIT)));
        } else if (ch == '[') {
            step(state);
            Any list = read_list(state, ']');
            result = cons(result, list);
        } else {
            break;
        }
    }*/
    skip_space(state);
    if (peek(state, 0) == ':' && peek(state, 1) == ':') {
        step(state);
        step(state);
        Any typeform = read_form(state);
        result = cons(make_symbol("the"), cons(typeform, cons(result, ANY_UNIT)));
    }
    return result;
}

Any read_cstr(const char *text) {
    ReadState state = {0};
    state.text = text;
    return read_form(&state);
}
