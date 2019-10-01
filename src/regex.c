#ifndef _MOJOCORE_REGEX_HEADER_
#define _MOJOCORE_REGEX_HEADER_

/*
    Task list:
        - parse tokens
        - build NFA (shunting yard)
        - build VM implementation
        - validate regex evaluator
        - clean up and refactor
*/

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
    eLexCharLiteral,
    eLexCharClass,
    eLexStringLiteral,
    eLexCharAny,
    eLexAlternative,
    eLexZeroOrOne,
    eLexZeroOrMany,
    eLexOneOrMany,
    eLexSubExprStart,
    eLexSubExprEnd
} eRegexLexeme;

typedef enum {
    eCompileOk,
    eCompileCharClassRangeIncomplete,
    eCompileCharClassIncomplete,
    eCompileEscapeCharIncomplete,
    eCompileMalformedSubExprName,
    eCompileUnsupportedMeta,
    eCompileOutOfMem
} eRegexCompileStatus;

typedef struct character_s character_t;
struct character_s {
    int c;
    int escaped;
};

character_t regexGetNextPatternChar(const char **pattern) {
    character_t result = {
        .c = -1,
        .escaped = 0
    };

    if(**pattern == '\0') {
        return result;
    }
    if(**pattern == '\\') {
        result.escaped = 1;
        (*pattern)++;
        if(**pattern == '\0') {
            return result;
        }
    }
    result.c = **pattern;
    (*pattern)++;
    return result;
}

int regexCheckNextPatternChar(const char **pattern, char c) {
    if(**pattern == '\0' || **pattern != c) {
        return 0;
    }
    (*pattern)++;
    return 1;
}

int regexGetActualChar(character_t c) {
    if(c.escaped) {
        switch(c.c) {
            case 'n': return '\n';
            case 'r': return '\r';
            case 't': return '\t';
            default: return c.c;
        }
    }
    return c.c;
}

int regexIsAlnum(char c) {
    return (((c >= 'a') && (c <= 'z')) ||
            ((c >= 'A') && (c <= 'Z')) ||
            ((c >= '0') && (c <= '9')));
}

int regexIsMeta(int c) {
    return ((c == '|') || (c == '?') || (c == '.') || (c == '*') ||
            (c == '+') || (c == '(') || (c == '[') || (c == ')'));
}

int regexGetPatternStrLen(const char **pattern) {
    const char *orig = *pattern;
    int count = 0;
    character_t c;

    for(;;) {
        if(**pattern == '\0') {
            *pattern = orig;
            return count;
        }
        c = regexGetNextPatternChar(pattern);
        if(c.c == -1) {
            // Incomplete escape char
            return -1;
        }
        if(!c.escaped && regexIsMeta(c.c)) {
            *pattern = orig;
            return count;
        }
        count++;
    }
}

char *regexGetPatternStr(const char **pattern, int len) {
    char *str, *ptr;
    character_t c;

    if((str = malloc(len + 1)) == NULL) {
        return NULL;
    }
    ptr = str;

    for(; len; len--) {
        if(**pattern == '\0') {
            return str;
        }
        c = regexGetNextPatternChar(pattern);
        if(c.escaped) {
            switch(c.c) {
                case 'r': *ptr = '\r'; break;
                case 'n': *ptr = '\n'; break;
                case 't': *ptr = '\t'; break;
                default: *ptr = c.c; break;
            }
        } else {
            *ptr = c.c;
        }
        ptr++;
    }
    *ptr = '\0';
    return str;
}

void mapClear(unsigned char *bitmap) {
    memset(bitmap, 0, 32);
}

void mapSet(unsigned char *bitmap, int pos) {
    unsigned int idx = pos / 8u;
    unsigned int bit = pos % 8u;

    bitmap[idx] |= (1u << bit);
}

int mapCheck(const unsigned char *bitmap, int pos) {
    unsigned int idx = pos / 8u;
    unsigned int bit = pos % 8u;

    return (int)(bitmap[idx] & (1u << bit));
}

void mapInvert(unsigned char *bitmap) {
    for(int k = 0; k < 32; k++) {
        bitmap[k] ^= (unsigned char)0xFF;
    }
}

unsigned char *mapCopy(unsigned char *bitmap) {
    unsigned char *copy;

    if((copy = malloc(32)) == NULL) {
        return NULL;
    }
    memcpy(copy, bitmap, 32);
    return copy;
}

eRegexCompileStatus regexParseCharClass(const char **pattern, unsigned char *bitmap) {
    character_t c;
    int invert = 0;
    int range = 0;
    int last;
    int next;
    int k;

    mapClear(bitmap);

    if(*pattern == '\0') {
        return eCompileCharClassIncomplete;
    }

    c = regexGetNextPatternChar(pattern);
    if(!c.escaped && c.c == '^') {
        invert = 1;
        c = regexGetNextPatternChar(pattern);
    }

    for(;;) {
        if(c.c == -1) {
            if(c.escaped) {
                return eCompileEscapeCharIncomplete;
            }
            return eCompileCharClassIncomplete;
        }
        if(!c.escaped && c.c == ']') {
            // End of the char class
            break;
        }
        if(range == 0) {
            last = regexGetActualChar(c);
            mapSet(bitmap, last);
            range = 1;
        } else if(range == 1) {
            if(!c.escaped && c.c == '-') {
                range = 2;
            } else {
                last = regexGetActualChar(c);
                mapSet(bitmap, last);
            }
        } else {
            next = regexGetActualChar(c);
            for(; last <= next; last++) {
                mapSet(bitmap, last);
            }
            range = 0;
        }
        if(*pattern == '\0') {
            if(range == 2) {
                return eCompileCharClassRangeIncomplete;
            }
            return eCompileCharClassIncomplete;
        }
        c = regexGetNextPatternChar(pattern);
    }
    if(range == 2) {
        return eCompileCharClassRangeIncomplete;
    }
    if(invert) {
        mapInvert(bitmap);
    }
    return eCompileOk;
}

int regexGetSubexpressionNameLen(const char **pattern) {
    int c;
    int k;

    for(k = 0; (*pattern)[k] != '>' && (*pattern)[k] != '\0'; k++) {
        if(!regexIsAlnum((*pattern)[k])) {
            return -1;
        }
    }
    if(k == 0) {
        return -1;
    }
    return k;
}

char *regexGetSubexpressionName(const char **pattern, int len) {
    char *str, *ptr;
    int k;

    if((str = malloc(len + 1)) == NULL) {
        return NULL;
    }
    ptr = str;
    for(; len; len--) {
        *ptr = **pattern;
        (*pattern)++;
        ptr++;
    }
    *ptr = '\0';
    return str;
}

void regexPrintCharClass(unsigned char *bitmap) {
    int k;
    int run;

    for(k = 0; k < 256; k++) {
        if(mapCheck(bitmap, k)) {
            for(run = k + 1; run < 256 && mapCheck(bitmap, run); run++);
            run--;
            printf("%c", ((k < 32) || (k > 127)) ? '.' : k);
            if(run - k > 3) {
                printf("-%c", ((run < 32) || (run > 127)) ? '.' : run);
                k = run;
            }
        }
    }
}

typedef struct regex_lexeme_s regex_lexeme_t;
struct regex_lexeme_s {
    eRegexLexeme lexType;
    union {
        int c;
        char *str;
    };
    regex_lexeme_t *next;
};

int regexLexemeCreate(regex_lexeme_t **list, eRegexLexeme lexType, int c, char *str) {
    regex_lexeme_t *lexeme, *walk;

    if((lexeme = malloc(sizeof(regex_lexeme_t))) == NULL) {
        return 0;
    }
    memset(lexeme, 0, sizeof(regex_lexeme_t));
    lexeme->lexType = lexType;
    if(str != NULL) {
        lexeme->str = str;
    } else {
        lexeme->c = c;
    }

    if(*list == NULL) {
        *list = lexeme;
    } else {
        for(walk = *list; walk->next != NULL; walk = walk->next);
        walk->next = lexeme;
    }

    return 1;
}

void regexLexemeDestroy(regex_lexeme_t *instr) {
    regex_lexeme_t *next;

    for(; instr != NULL; instr = next) {
        next = instr->next;
        switch(instr->lexType) {
            case eCharClass:
            case eStringLiteral:
                free(instr->str);
                break;
            default:
                break;
        }
        free(instr);
    }
}

typedef struct regex_compile_ctx_s regex_compile_ctx_t;
struct regex_compile_ctx_s {
    eRegexCompileStatus status;
    const char *pattern;
    int position;
};

#define SET_RESULT(res,stat,ptr)  res.status = stat; res.position = ptr - res.pattern;

// Return a compiled regex, or an error and position within the pattern
regex_compile_ctx_t regexCompile(const char *pattern, unsigned int flags) {
    regex_compile_ctx_t result = {
            .status = eCompileOk,
            .pattern = pattern,
            .position = 0
    };
    character_t c;
    int len;
    char *str;
    char character;
    unsigned char bitmap[32];
    eRegexCompileStatus status;
    int subexpr = 0;
    regex_lexeme_t *program = NULL;

    for(; *pattern != '\0';) {
        c = regexGetNextPatternChar(&pattern);
        if(c.c == -1) {
            SET_RESULT(result, eCompileEscapeCharIncomplete, pattern);
            return result;
        }

        if(!c.escaped) {
            switch(c.c) {
                case '.':
                    if(!regexLexemeCreate(&program, eLexCharAny, 0, 0)) {
                        SET_RESULT(result, eCompileOutOfMem, pattern);
                        return result;
                    }
                    printf("Any\n");
                    continue;
                case '|':
                    if(!regexLexemeCreate(&program, eLexAlternative, 0, 0)) {
                        SET_RESULT(result, eCompileOutOfMem, pattern);
                        return result;
                    }
                    printf("Alt\n");
                    continue;
                case '?':
                    if(!regexLexemeCreate(&program, eLexZeroOrOne, 0, 0)) {
                        SET_RESULT(result, eCompileOutOfMem, pattern);
                        return result;
                    }
                    printf("ZeroOrOne\n");
                    continue;
                case '*':
                    if(!regexLexemeCreate(&program, eLexZeroOrMany, 0, 0)) {
                        SET_RESULT(result, eCompileOutOfMem, pattern);
                        return result;
                    }
                    printf("ZeroOrMore\n");
                    continue;
                case '+':
                    if(!regexLexemeCreate(&program, eLexOneOrMany, 0, 0)) {
                        SET_RESULT(result, eCompileOutOfMem, pattern);
                        return result;
                    }
                    printf("OneOrMore\n");
                    continue;
                case '(':
                    subexpr++;
                    str = NULL;
                    if(regexCheckNextPatternChar(&pattern, '?')) {
                        // Meta character subexpression modifier
                        if(regexCheckNextPatternChar(&pattern, 'i')) {
                            // Case insensitive sub expression
                            // TODO
                        } else if(regexCheckNextPatternChar(&pattern, 'P')) {
                            // Named sub expression
                            if(!regexCheckNextPatternChar(&pattern, '<')) {
                                SET_RESULT(result, eCompileMalformedSubExprName, pattern);
                                return result;
                            }
                            len = regexGetSubexpressionNameLen(&pattern);
                            if(len == -1) {
                                SET_RESULT(result, eCompileMalformedSubExprName, pattern);
                                return result;
                            }
                            if((str = regexGetSubexpressionName(&pattern, len)) == NULL) {
                                SET_RESULT(result, eCompileOutOfMem, pattern);
                                return result;
                            }
                            printf("[%s]", str);
                        } else {
                            SET_RESULT(result, eCompileUnsupportedMeta, pattern);
                            return result;
                        }
                    }
                    if(!regexLexemeCreate(&program, eLexSubExprStart, 0, str)) {
                        SET_RESULT(result, eCompileOutOfMem, pattern);
                        return result;
                    }
                    printf("({%d}\n", subexpr);
                    continue;
                case ')':
                    if(!regexLexemeCreate(&program, eLexSubExprEnd, 0, 0)) {
                        SET_RESULT(result, eCompileOutOfMem, pattern);
                        return result;
                    }
                    printf(")\n");
                    continue;
                case '[':
                    if((status = regexParseCharClass(&pattern, bitmap)) != eCompileOk) {
                        SET_RESULT(result, status, pattern);
                        return result;
                    }
                    if((str = (char *)mapCopy(bitmap)) == NULL) {
                        SET_RESULT(result, eCompileOutOfMem, pattern);
                        return result;
                    }
                    if(!regexLexemeCreate(&program, eLexCharClass, 0, str)) {
                        SET_RESULT(result, eCompileOutOfMem, pattern);
                        return result;
                    }
                    printf("CharClass(");
                    regexPrintCharClass(bitmap);
                    printf(")\n");
                    continue;
                default:
                    // Unnecessary escaping
                    break;
            }
        }

        // Regular character
        switch(len = regexGetPatternStrLen(&pattern)) {
            case -1:
                SET_RESULT(result, eCompileEscapeCharIncomplete, pattern);
                return result;
            case 0:
                if(c.escaped) {
                    switch(c.c) {
                        case 'r': character = '\r'; break;
                        case 'n': character = '\n'; break;
                        case 't': character = '\t'; break;
                        default: character = c.c; break;
                    }
                    printf("CHAR(\\%c)\n", c.c);
                } else {
                    character = c.c;
                    printf("CHAR(%c)\n", c.c);
                }
                if(!regexLexemeCreate(&program, eLexCharLiteral, character, 0)) {
                    SET_RESULT(result, eCompileOutOfMem, pattern);
                    return result;
                }
                break;
            default:
                len++;
                if(c.escaped) {
                    pattern--;
                }
                pattern--;
                if((str = regexGetPatternStr(&pattern, len)) == NULL) {
                    SET_RESULT(result, eCompileOutOfMem, pattern);
                    return result;
                }
                if(!regexLexemeCreate(&program, eLexStringLiteral, 0, str)) {
                    SET_RESULT(result, eCompileOutOfMem, pattern);
                    return result;
                }
                printf("STRING(\"%s\")\n", str);
        }
    }
    return result;
}

/////////////////////////////////////////////////////////////////////////////

typedef struct regex_instr_s regex_instr_t;
struct regex_instr_s {
    eRegexOpCode opcode;
    union {
        int c;
        char *str;
    };
    regex_instr_t *out_a;
    regex_instr_t *out_b;
    regex_instr_t *next;
};

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

void regexInstructionDestroy(regex_instr_t *instr) {
    switch(instr->opcode) {
        case eCharClass:
        case eStringLiteral:
            free(instr->str);
            break;
        default:
            break;
    }
    free(instr);
}

void pushInstruction(regex_instr_t **list, regex_instr_t *instr) {
    regex_instr_t *walk;

    if(*list == NULL) {
        *list = instr;
    } else {
        for(walk = *list; walk->next != NULL; walk = walk->next);
        walk->next = instr;
    }
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
                if(mapCheck((unsigned char *)instr->str, k)) {
                    for(j = k + 1;
                        (j < 256) && mapCheck((unsigned char *)instr->str, j);
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
    return 0;
}

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
