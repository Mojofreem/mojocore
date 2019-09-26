#ifndef _MOJOCORE_REGEX_HEADER_
#define _MOJOCORE_REGEX_HEADER_

#include <stdio.h>
#include <stdlib.h>

struct State {
    int c;
    State *out;
    State *out1;
    int lastlist;
};

struct Frag {
    State *start;
    Ptrlist *out
};

typedef enum {
    eCharLiteral,
    eMatch,
    eJmp,
    eSplit
} eRegexOpCode;

typedef struct Instruction_s Instruction_t;
struct Instruction_s {
    eRegexOpCode opcode;
    int c;
    Instruction_t *x;
    Instruction_t *y;
};

typedef int (*InstImplHandler)(Instruction_t *prog, char *input);

typedef struct Thread_s Thread_t;
struct Thread_s {
    Instruction_t *pc;
};

Thread thread(Instruction_t *pc);

int thompsonvm(Instruction_t *prog, char *input) {
    int len;
    ThreadList *clist, *nlist;
    Instruction_t *pc;
    char *sp;

    len = proglen(prog); /* # of instructions */
    clist = threadlist(len);
    nlist = threadlist(len);

    addthread(clist, thread(prog));
    for(sp = input; *sp; sp++) {
        for(i = 0; i < clist.n; i++) {
            pc = clist.t[i].pc;
            switch(pc.opcode) {
                case eCharLiteral:
                    if(*sp != pc->c) {
                        break;
                    }
                    addthread(nlist, thread(pc + 1));
                    break;

                case eMatch:
                    return 1;

                case eJmp:
                    addthread(clist, thread(pc->x));
                    break;

                case eSplit:
                    addthread(clist, thread(pc->x));
                    addthread(clist, thread(pc->y));
                    break;
            }
        }
        swap(clist, nlist);
        clear(nlist);
    }
}

/////////////////////////////////////////////////////////////////////////////////////////////////////////////////


typedef enum {
    eReNone,
    eReCaseInsensitive
} eRegexFlags;

typedef enum {
    eCharLiteral,
    eMatch,
    eJmp,
    eSplit,
    eSave,
    eCharClass,
    eStringLiteral,
    eCharAny
} eRegexOpCode;

typedef struct regex_instr_s regex_instr_t;
struct regex_instr_s {
    eRegexOpCode opcode;
    union {
        int c;
        char *str;
    };
    regex_instr_t *out_a;
    regex_instr_t *out_b;
};

typedef struct regex_thread_s regex_thread_t;
struct regex_thread_s {
    regex_instr_t *pc;
    int subexpr_count;
    char **subexpressions;
};

typedef struct regex_subexpr_entry_s regex_subexpr_entry_t;
struct regex_subexpr_entry_s {
    char *name;
    int idx;
    regex_subexpr_entry_t *next;
};

typedef struct regex_s regex_t;
struct regex_s {
    regex_instr_t *expression;
    int subexpr_count;
    regex_subexpr_entry_t *subexpr_names;
};

typedef struct re_ctx_s re_ctx_t;

struct re_ctx_s {
    regex_t *expression;
    int matched;
    char *text;
    regex_instr_t *pc;
    char **subexpressions;
};

regex_t *regexCompile(const char *pattern, unsigned int flags);
void regexFree(regex_t *expr);

re_ctx_t *regexContextInit(regex_t *expr);
void regexContextFree(re_ctx_t *ctx);

int regexSearch(re_ctx_t *ctx, const char *text);

int regexGetNamedSubexprIndex(re_ctx_t *ctx, const char *name);
int regexGetNamedSubexprBounds(re_ctx_t *ctx, const char *name, int *start, int *end);
int regexGetSubexprBounds(re_ctx_t *ctx, int subnum, int *start, int *end);
int regexGetSubexprLen(re_ctx_t *ctx, int subnum);
int regexGetNamedSubexprLen(re_ctx_t *ctx, int subnum);
void regexCopySubexprValue(re_ctx_t *ctx, int subnum, char *buf, size_t buflen);
void regexCopyNamedSubexprValue(re_ctx_t *ctx, const char *name, char *buf, size_t buflen);

/////////////////////////////////////////////////////////////////////////////

typedef struct text_buf_s text_buf_t;
struct text_buf_s {
    char *buffer;
    int pos;
    int len;
};

text_buf_t *regexTextBufCreate(char *text) {
    text_buf_t *buf;

    if((buf = malloc(sizeof(text_buf_t))) == NULL) {
        return NULL;
    }
    memset(buf, 0, sizeof(text_buf_t));
    buf->buffer = text;
    buf->pos = 0;
    buf->len = strlen(text);

    return buf;
}

void regexTextBufDestroy(text_buf_t *buf) {
    free(buf);
}

int regexTextBufCharGet(text_buf_t *buf) {
    int c = -1;

    if(buf->pos < buf->len) {
        c = buf->buffer[buf->pos];
        buf->pos++;
    }

    return c;
}

int regexTextBufCharPeek(text_buf_t *buf) {
    int c = -1;

    if(buf->pos < buf->len) {
        c = buf->buffer[buf->pos];
    }

    return c;
}

/*

\\
.
(
[
|



*/

int regexCharLookAhead(const char *str, int offset) {
    int k;
    for(k = 0; k < offset; k++) {
        if(str[k] != '\0') {
            continue;
        }
        return 0;
    }
    return str[k];
}

int regexIsAlnum(char c) {
    return (((c >= 'a') && (c <= 'z')) ||
            ((c >= 'A') && (c <= 'Z')) ||
            ((c >= '0') && (c <= '9')));
}

unsigned char char_class_bitmap[32];

void regexCharClassClear(unsigned char *bitmap) {
    memset(bitmap, 0, 32);
}

void regexCharClassSetChar(unsigned char *bitmap, char c) {
    bitmap[c / 8] |= c % 8;
}

void regexCharClassSetRange(unsigned char *bitmap, char low, char high) {
    int k;

    for(k = low; k <= high; k++) {
        bitmap[c / 8] |= c % 8;
    }
}

void regexCharClassInvert(unsigned char *bitmap) {
    for(int k = 0; k < 32; k++) {
        bitmap[k] ^= 0xff;
    }
}

int regexCharClassCheck(unsigned char *bitmap, char c) {
    return bitmap[c / 8] & (c % 8);
}

const char *regexParseCharClass(const char *p, unsigned char *bitmap) {
    int invert = 0;
    int last = 0;
    int next;

    regexCharClassClear(bitmap);

    p++;
    if(*p == '^') {
        invert = 1;
        p++;
    }

    for(; (*p != '\0' && *p != ']'); p++) {
        if(*p == '-') {
            if(last == 0) {
                regexCharClassSetChar(bitmap, '-');
            } else {
                next = regexCharLookAhead(p, 1);
                if(next == '\0' || next == ']') {
                    // TODO - incomplete char class range
                    return NULL;
                }
                regexCharClassSetRange(bitmap, last, next);
                p++;
            }
            last = 0;
        } else {
            last = *p;
            regexCharClassSetChar(bitmap, *p);
        }
    }
    if(*p == ']') {
        return p;
    }
    // TODO - incomplete character class
    return NULL;
}

regex_t *regexCompile(const char *pattern, unsigned int flags) {
    const char *p;
    regex_instr_t *s;
    unsigned char bitmap[32];
    int k;
    int invert;
    int range;
    int last;
    int next;

    for(p = pattern; *p; p++) {
        switch(*p) {
            case '//':
                if(regexCharLookAhead(p, 1) == -1) {
                    // TODO - report incomplete escape formatted character
                    // TODO - cleanup before bailout
                    return NULL;
                }
                // TODO - append escaped character literal
            case '.':
            case '|':
            case '?':
            case '*':
            case '+':
            case '(':
                if(regexCharLookAhead(p, 1) == '?') {
                    if(regexCharLookAhead(p, 2) == 'i') {
                        // Case insensitive matching for this subexpression
                        // TODO - set the case insensitive flag
                    } else if((regexCharLookAhead(p, 2) == 'P') &&
                              (regexCharLookAhead(p, 3) != '<')) {
                        // PCRE5 style named subexpression
                        for(k = 4; regexIsAlnum(p[k]); k++);
                        if((p[k] != '>') || (k < 5)) {
                            // Malformed named subexpression
                            // TODO - report error in expression
                            // TODO - cleanup before bailout
                            return NULL
                        }
                        // TODO - store the subexpression name for indexing
                    } else {
                        // Invalid extended markup (we only support i and P:
                        // case-insensitive and named subexpressions)
                        // TODO - cleanup before bailout
                        return NULL;
                    }
                }
            case ')':
            case '[':
                if((p = regexParseCharClass(p, bitmap)) == NULL) {
                    return NULL;
                }
                // TODO - store the character class node
                break;

            default:
        }
    }
}


void regexFree(regex_t *expr);

re_ctx_t *regexContextInit(regex_t *expr);
void regexContextFree(re_ctx_t *ctx);

int regexSearch(re_ctx_t *ctx, const char *text);



int regexGetNamedSubexprIndex(re_ctx_t *ctx, const char *name) {
    regex_subexpr_entry_t *walk;

    for(walk = ctx->expression->subexpr_names; walk != NULL; walk = walk->next) {
        if(!strcmp(walk->name, name)) {
            return walk->idx;
        }
    }
    return 0;
}

int regexGetNamedSubexprBounds(re_ctx_t *ctx, const char *name, int *start, int *end) {
    return regexGetSubexprBounds(ctx, regexGetNamedSubexprIndex(name), start, end);
}

int regexGetSubexprBounds(re_ctx_t *ctx, int subnum, int *start, int *end) {
    if(!ctx->matched) {
        return 0;
    }
    if(subnum == 0) {
        if(start != NULL) {
            *start = 0;
        }
        if(end != NULL) {
            *end = strlen(ctx->text);
        }
    }
    if(subnum > ctx->expression->subexpr_count) {
        return 0;
    }
    if(start != NULL) {
        *start = ctx->subexpressions[subnum * 2];
    }
    if(end != NULL) {
        *end = ctx->subexpressions[(subnum * 2) + 1];
    }
    return 1;
}

int regexGetNamedSubexprLen(re_ctx_t *ctx, const char *name) {
    return regexGetSubexprLen(ctx, regexGetNamedSubexprIndex(name));
}

int regexGetSubexprLen(re_ctx_t *ctx, int subnum) {
    int a, b;

    if(!regexGetSubexprBounds(ctx, subnum, &a, &b)) {
        return 0;
    }
    return (int)(b - a);
}

int regexCopySubexprValue(re_ctx_t *ctx, int subnum, char *buf, size_t buflen) {
    int a, b;

    if(!regexGetSubexprBounds(ctx, subnum, &a, &b)) {
        buf[0] = '\0';
        return 0;
    }

    if(buflen > (b - a)) {
        buflen = (b - a) + 1;
    }

    memcpy(buf, ctx->subexpressions[subnum * 2], buflen);
    return 1;
}

int regexCopyNamedSubexprValue(re_ctx_t *ctx, const char *name, char *buf, size_t buflen) {
    return regexCopySubexprValue(ctx, regexGetNamedSubexprIndex(name), buf, buflen);
}

regex_subexpr_entry_t *reSubexprEntryCreate(const char *name, int index) {
    regex_subexpr_entry_t *entry;

    if((entry = malloc(sizeof(regex_subexpr_entry_t))) == NULL) {
        return NULL;
    }
    memset(entry, 0, sizeof(regex_subexpr_entry_t));

    entry->idx = index;
    if((entry->name = strdup(name)) == NULL) {
        free(entry);
        return NULL;
    }

    return entry;
}

void reSubexprEntryDestroy(regex_subexpr_entry_t *entry) {
    if(entry->name != NULL) {
        free(entry->name);
    }

    if(entry->next != NULL) {
        reSubexprEntryDestroy(entry->next);
        entry->next = NULL;
    }

    free(entry);
}


#endif // _MOJOCORE_REGEX_HEADER_
