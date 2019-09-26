#ifndef _MOJOCORE_REGEX_HEADER_
#define _MOJOCORE_REGEX_HEADER_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define TOKEN_STACK_DEPTH   1000

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

typedef enum {
    eCompileOk,
    eCompileCharClassRangeIncomplete,
    eCompileCharClassIncomplete,
    eCompileEscapeCharIncomplete,
    eCompileMalformedSubExprName,
    eCompileUnsupportedMeta,
    eCompileOutOfMem
} eRegexCompileStatus;

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

unsigned char *regexCharClassBitmapCopy(unsigned char *bitmap) {
    unsigned char *copy;

    if((copy = malloc(32)) == NULL) {
        return NULL;
    }
    memcpy(copy, bitmap, 32);
    return copy;
}

regex_instr_t *regexInstructionCreate(eRegexOpCode opcode, int c, char *str) {
    regex_instr_t *instr;

    if((instr = malloc(sizeof(regex_instr_t))) == NULL) {
        return NULL;
    }
    memset(instr, 0, sizeof(regex_instr_t));
    instr->opcode = opcode;
    if(str != NULL) {
        instr->str = str;
    } else {
        instr->c = c;
    }
    return instr;
}

int regexCharClassCheck(unsigned char *bitmap, int c) {
    //printf("Check %d = byte %d, bit %d\n", c, c / 8, c % 8);
    return bitmap[c / 8] & (c % 8);
}

void regexInstructionPrint(regex_instr_t *instr) {
    int j;

    switch(instr->opcode) {
        case eCharLiteral: printf("CHAR('%c')\n", instr->c); break;
        case eMatch: printf("MATCH\n"); break;
        case eJmp: printf("JMP\n"); break;
        case eSplit: printf("SPLIT\n"); break;
        case eSave: printf("SAVE\n"); break;
        case eCharClass:
            printf("CLASS([");
            for(int k = 0; k < 256; k++) {
                if(regexCharClassCheck((unsigned char *)instr->str, k)) {
                    for(j = k + 1;
                        (j < 256) && regexCharClassCheck((unsigned char *)instr->str, j);
                        j++);
                    j--;
                    if((j - k) > 3) {
                        //printf("%c-%c", k, j);
                        printf("%d-%d,", k, j);
                        k = j;
                    } else {
                        //printf("%c", k);
                        printf("%d,", k);
                    }
                }
            }
            printf("])\n");
            break;
        case eStringLiteral: printf("STR(\"%s\")\n", instr->str); break;
        case eCharAny: printf("ANYCHAR\n"); break;
    }
}

void regexInstructionDestroy(regex_instr_t *instr) {

}

typedef struct regex_compile_ctx_s regex_compile_ctx_t;
struct regex_compile_ctx_s {
    eRegexCompileStatus status;
    const char *pattern;
    int position;
};

const char *regexGetCompileStatusStr(eRegexCompileStatus status) {
    switch(status) {
        case eCompileOk: return "compiled successfully";
        case eCompileCharClassRangeIncomplete: return "char class range is incomplete";
        case eCompileCharClassIncomplete: return "char class definition is incomplete";
        case eCompileEscapeCharIncomplete: return "escape character is incomplete";
        case eCompileMalformedSubExprName: return "subexpression name is malformed";
        case eCompileUnsupportedMeta: return "expression uses an unsupported meta character";
        case eCompileOutOfMem: return "out of memory";
        default: return "Unknown failure";
    }
}

int regexIsAlnum(char c) {
    return (((c >= 'a') && (c <= 'z')) ||
            ((c >= 'A') && (c <= 'Z')) ||
            ((c >= '0') && (c <= '9')));
}

int regexIsMeta(int c) {
    return ((c == '|') || (c == '?') || (c == '.') || (c == '*') ||
            (c == '+') || (c == '(') || (c == '['));
}

#define SET_RESULT(res,stat,base,ptr)  res.status = stat; res.position = ptr - base;

void regexCharClassClear(unsigned char *bitmap) {
    memset(bitmap, 0, 32);
}

void regexCharClassSetChar(unsigned char *bitmap, int c) {
    bitmap[c / 8] |= 1 << (c % 8);
}

void regexCharClassSetRange(unsigned char *bitmap, int low, int high) {
    int k;

    for(k = low; k <= high; k++) {
        bitmap[k / 8] |= 1 << (k % 8);
    }
}

void regexCharClassInvert(unsigned char *bitmap) {
    for(int k = 0; k < 32; k++) {
        bitmap[k] ^= 0xff;
    }
}

eRegexCompileStatus regexParseCharClass(const char *buffer, unsigned char *bitmap, int *increment) {
    int invert = 0;
    int last = 0;
    int next;
    int c;

    regexCharClassClear(bitmap);

    buffer++;
    *increment = 1;
    if(buffer[1] == '^') {
        invert = 1;
        buffer++;
        (*increment)++;
    }

    for(c = buffer[0];
        (c != ']') && (c != '\0');
        c = buffer[0]) {
        if(c == '-') {
            if(last == 0) {
                regexCharClassSetChar(bitmap, '-');
                last = '-';
                printf("Class: [-]\n");
            } else {
                next = buffer[1];
                if(next == '\0' || next == ']') {
                    return eCompileCharClassRangeIncomplete;
                }
                printf("Class: Range [%c] - [%c]\n", last, next);
                regexCharClassSetRange(bitmap, last, next);
                buffer++;
                (*increment)++;
                last = 0;
            }
        } else {
            last = c;
            printf("Class: [%c]\n", c);
            regexCharClassSetChar(bitmap, c);
        }
        buffer++;
        (*increment)++;
    }
    if(c == ']') {
        if(invert) {
            regexCharClassInvert(bitmap);
        }
        return eCompileOk;
    }
    return eCompileCharClassIncomplete;
}

typedef struct shunting_yard_s shunting_yard_t;
struct shunting_yard_s {
    regex_instr_t *operand_stack[TOKEN_STACK_DEPTH];
    regex_instr_t *operator_stack[TOKEN_STACK_DEPTH];
    int operands;
    int operators;
};

int shuntingYardPush(shunting_yard_t *yard, regex_instr_t *instr) {
    regex_instr_t *operator;

    if(instr == NULL) {
        return -2;
    }
    if((instr->opcode == eCharLiteral) ||
       (instr->opcode == eStringLiteral) ||
       (instr->opcode == eCharClass) ||
       (instr->opcode == eCharAny)) {
        // Operand
        if(yard->operands >= TOKEN_STACK_DEPTH) {
            return -1;
        }
        yard->operand_stack[yard->operands] = instr;
        yard->operands++;
        return 1;
    } else {
        // Operator
        if(yard->operators < 1) {
            return -3;
        }
        yard->operators--;
        operator = yard->operator_stack[yard->operators];
    }
}

// Return a compiled regex, or an error and position within the pattern
regex_compile_ctx_t regexCompile(const char *pattern, unsigned int flags) {
    regex_compile_ctx_t result = {
        .status = eCompileOk,
        .pattern = pattern,
        .position = 0
    };

    regex_instr_t *operand_stack[TOKEN_STACK_DEPTH], *operator_stack[TOKEN_STACK_DEPTH];
    int operands = 0, operators = 0;
    const char *p;
    const char *str;
    char *name;
    unsigned char bitmap[32];
    eRegexCompileStatus status;
    int offset;

    for(p = pattern; *p != '\0'; p++) {
        switch(*p) {
            case '\\':
                if(p[1] == '\0') {
                    printf("\nERROR: Incomplete meta char escape\n");
                    SET_RESULT(result, eCompileEscapeCharIncomplete, pattern, p);
                    return result;
                }
                p++;
                regexInstructionPrint(regexInstructionCreate(eCharLiteral, *p, NULL));
                //printf("ESC: \\%c\n", *p);
                break;
            case '.':
                printf("Any");
                break;
            case '|':
                printf("Alt");
                break;
            case '?':
                printf("ZeroOrOne");
                break;
            case '*':
                printf("ZeroOrMore");
                break;
            case '+':
                printf("OneOrMore");
                break;
            case '(':
                printf("(");
                if(p[1] == '?') {
                    if(p[2] == 'i') {
                        // Case insensitive matching for this subexpression
                        p += 2;
                        // TODO - set the case insensitive flag
                        printf("CaseIns");
                    } else if(p[2] == 'P' && p[3] == '<') {
                        // PCRE5 style named subexpression
                        p += 3;
                        str = p;
                        for(; regexIsAlnum(*p); p++);
                        if((*p != '>') || ((p - str) == 1)) {
                            // Malformed named subexpression
                            SET_RESULT(result, eCompileMalformedSubExprName, pattern, p);
                            return result;
                        }
                        if((name = malloc(p - str)) == NULL) {
                            SET_RESULT(result, eCompileOutOfMem, pattern, p);
                            return result;
                        }
                        memcpy(name, str, p - str);
                        name[p - str - 1] = '\0';
                        printf("<%s>", name);
                        free(name);
                    }
                }
                break;

            case ')':
                printf(")");
                break;

            case '[':
                if((status = regexParseCharClass(p, bitmap, &offset)) != eCompileOk) {
                    SET_RESULT(result, status, pattern, p);
                    return result;
                }
                p += offset;
                regexInstructionPrint(regexInstructionCreate(eCharClass, 0, (char *)regexCharClassBitmapCopy(bitmap)));
                //printf("[...]\n");
                break;

            default:
                str = p;
                for(; !regexIsMeta(p[1]) && p[1] != '\0'; p++);
                if(p - str > 1) {
                    if((name = malloc(p - str + 1)) == NULL) {
                        SET_RESULT(result, eCompileOutOfMem, pattern, p);
                        return result;
                    }
                    memcpy(name, str, p - str);
                    name[p - str] = '\0';
                    printf("STR \"%s\"\n", name);
                    free(name);
                    p--;
                } else {
                    printf("CHAR '%c'\n", *p);
                }
                break;
        }
    }
    return result;
}



#if 0





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

regex_compile_result_t regexCompile(const char *pattern, unsigned int flags);
void regexFree(regex_t *expr);

const char *regexCompileStatusString(regex_compile_result_t *result);

re_ctx_t *regexContextInit(regex_t *expr);
void regexContextFree(re_ctx_t *ctx);

int regexSearch(re_ctx_t *ctx, const char *text);

int regexGetNamedSubexprIndex(re_ctx_t *ctx, const char *name);
int regexGetNamedSubexprBounds(re_ctx_t *ctx, const char *name, char **start, char **end);
int regexGetSubexprBounds(re_ctx_t *ctx, int subnum, char **start, char **end);
int regexGetSubexprLen(re_ctx_t *ctx, int subnum);
int regexGetNamedSubexprLen(re_ctx_t *ctx, int subnum);
void regexCopySubexprValue(re_ctx_t *ctx, int subnum, char *buf, size_t buflen);
void regexCopyNamedSubexprValue(re_ctx_t *ctx, const char *name, char *buf, size_t buflen);


/////////////////////////////////////////////////////////////////////////////




regex_instr_t *regexInstructionCreate(eRegexOpCode opcode) {
    regex_instr_t *inst;

    if((inst = malloc(sizeof(regex_instr_t))) == NULL) {
        return NULL;
    }
    memset(inst, 0, sizeof(regex_instr_t));
    inst->opcode = opcode;
    return inst;
}

void regexInstructionDestroy(regex_instr_t *inst) {
    if((inst->opcode == eCharClass) || (inst->opcode == eStringLiteral)) {
        if(inst->str != NULL) {
            free(inst->str);
        }
    }
    free(inst);
}

typedef struct text_buf_s text_buf_t;
struct text_buf_s {
    const char *buffer;
    int pos;
    int len;
};

void regexTextBufInit(text_buf_t *buf, const char *text) {
    memset(buf, 0, sizeof(text_buf_t));
    buf->buffer = text;
    buf->pos = 0;
    buf->len = strlen(text);
}

int regexTextBufCharGet(text_buf_t *buf) {
    int c = -1;

    if(buf->pos < buf->len) {
        c = buf->buffer[buf->pos];
        buf->pos++;
    }

    return c;
}

int regexTextBufCharPeek(text_buf_t *buf, int offset) {
    int c = -1;

    if((buf->pos + offset) < buf->len) {
        c = buf->buffer[buf->pos + offset];
    }

    return c;
}

const char *regexTextBufPosGet(text_buf_t *buf) {
    return buf->buffer + buf->pos;
}

void regexTextBufAdvance(text_buf_t *buf, int pos) {
    buf->pos += pos;
}

int regexTextBufStrLen(text_buf_t *buf, const char *origin) {
    return (buf->buffer + buf->pos) - origin + 1;
}

char *regexTextBufStrCopy(text_buf_t *buf, const char *origin) {
    int len = regexTextBufStrLen(buf, origin);
    char *str;

    if((str = malloc(len)) == NULL) {
        return NULL;
    }
    memcpy(str, origin, len - 1);
    str[len - 1] = '\0';
    return str;
}

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
        bitmap[k / 8] |= k % 8;
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

int regexParseCharClass(text_buf_t *buffer, unsigned char *bitmap) {
    int invert = 0;
    int last = 0;
    int next;
    int c;

    regexCharClassClear(bitmap);

    if(regexTextBufCharPeek(buffer, 0) == '^') {
        invert = 1;
        regexTextBufAdvance(buffer, 1);
    }

    for(c = regexTextBufCharPeek(buffer, 0);
        (c != ']') && (c != '\0');
        c = regexTextBufCharPeek(buffer, 0)) {
        regexTextBufAdvance(buffer, 1);
        if(c == '-') {
            if(last == 0) {
                regexCharClassSetChar(bitmap, '-');
            } else {
                next = regexTextBufCharPeek(buffer, 1);
                if(next == '\0' || next == ']') {
                    // TODO - incomplete char class range
                    return 0;
                }
                regexCharClassSetRange(bitmap, last, next);
                regexTextBufAdvance(buffer, 1);
            }
            last = 0;
        } else {
            last = c;
            regexCharClassSetChar(bitmap, c);
        }
    }
    if(c == ']') {
        return 1;
    }
    // TODO - incomplete character class
    return 0;
}

regex_compile_result_t regexCompile(const char *pattern, unsigned int flags) {
    regex_compile_result_t result;
    text_buf_t buffer;
    const char *p;
    regex_instr_t *s;
    unsigned char bitmap[32];
    int k;
    int c;
    int invert;
    int range;
    int last;
    int next;
    const char *str;
    char *frag;
    int num;

    result.status = eCompileOk;

    for(regexTextBufInit(&buffer, pattern);
        (c = regexTextBufCharGet(&buffer)) != -1;
       ) {
        switch(c) {
            case '\\':
                if((c = regexTextBufCharGet(&buffer)) == -1) {
                    printf("ERROR: Incomplete meta char escape\n");
                    // TODO - report incomplete escape formatted character
                    // TODO - cleanup before bailout
                    return result;
                }
                printf("{%c}", c);
                break;
            case '.':
                printf("Any");
                break;
            case '|':
                printf("Alt");
                break;
            case '?':
                printf("ZeroOrOne");
                break;
            case '*':
                printf("ZeroOrMore");
                break;
            case '+':
                printf("OneOrMore");
                break;
            case '(':
                printf("(");
                if(regexTextBufCharPeek(&buffer, 1) == '?') {
                    if(regexTextBufCharPeek(&buffer, 2) == 'i') {
                        // Case insensitive matching for this subexpression
                        regexTextBufAdvance(&buffer, 2);
                        // TODO - set the case insensitive flag
                        printf("CaseIns");
                    } else if((regexTextBufCharPeek(&buffer, 2) == 'P') &&
                              (regexTextBufCharPeek(&buffer, 3) == '<')) {
                        // PCRE5 style named subexpression
                        regexTextBufAdvance(&buffer, 3);
                        str = regexTextBufPosGet(&buffer);
                        for(; regexIsAlnum(regexTextBufCharPeek(&buffer, 0));
                              regexTextBufAdvance(&buffer, 1));
                        if((regexTextBufCharGet(&buffer) != '>') ||
                           (regexTextBufStrLen(&buffer, str) <= 1)) {
                            // Malformed named subexpression
                            // TODO - report error in expression
                            // TODO - cleanup before bailout
                            return result;
                        }
                        frag = regexTextBufStrCopy(&buffer, str);
                        printf("(<%s>", frag);
                        free(frag);
                    }
                } else {
                    printf("(");
                }
                break;

            case ')':
                printf(")");
                break;

            case '[':
                if(!regexParseCharClass(&buffer, bitmap)) {
                    return result;
                }
                printf("[...]");
                break;

            default:
                str = regexTextBufPosGet(&buffer);
                for(num = 1;
                    !regexIsMeta(regexTextBufCharPeek(&buffer, 1));
                    regexTextBufCharGet(&buffer));
                if(num > 1) {
                    frag = regexTextBufStrCopy(&buffer, str);
                    printf("{%s}", frag);
                    free(frag);
                } else {
                    printf("{%c}", c);
                }
                break;
        }
    }
    return result;
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

int regexGetNamedSubexprBounds(re_ctx_t *ctx, const char *name, char **start, char **end) {
    return regexGetSubexprBounds(ctx, regexGetNamedSubexprIndex(ctx, name), start, end);
}

int regexGetSubexprBounds(re_ctx_t *ctx, int subnum, char **start, char **end) {
    if(!ctx->matched) {
        return 0;
    }
    if(subnum == 0) {
        if(start != NULL) {
            *start = ctx->text;
        }
        if(end != NULL) {
            *end = ctx->text + strlen(ctx->text);
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
    return regexGetSubexprLen(ctx, regexGetNamedSubexprIndex(ctx, name));
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
    return regexCopySubexprValue(ctx, regexGetNamedSubexprIndex(ctx, name), buf, buflen);
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

#endif

int main(int argc, char **argv) {
    regex_compile_ctx_t result;

    if(argc > 1) {
        result = regexCompile(argv[1], 0);
        if(result.status != eCompileOk) {
            printf("Compile failed: %s", regexGetCompileStatusStr(result.status));
        }
    }

    return 0;
}

#endif // _MOJOCORE_REGEX_HEADER_
