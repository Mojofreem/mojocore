#ifndef _MOJO_REGEX_HEADER_
#define _MOJO_REGEX_HEADER_

#define MOJO_REGEX_COMPILE_IMPLEMENTATION
#define MOJO_REGEX_EVALUATE_IMPLEMENTATION
#define MOJO_REGEX_IMPLEMENTATION

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/*
Regex VM Bytecode (v1)

    Program stack is an array of 32 bit ints
    Operators and operands are represented in independent stack entries.
    Bytecode:
        eTokenCharLiteral       1
            1 operand: character literal to match
        eTokenCharClass         2
            1 operand: index of the character class to match
        eTokenStringLiteral     3
            1 operand: index of the string table entry to match
        eTokenCharAny           4
            0 operands
        eTokenMatch             5
            0 operands
        eTokenSplit             6
            2 operands: program counter indexes to continue evaluation from
        eTokenJmp               7
            1 operand: program counter index to transition to
        eTokenSave              8
            1 operand: the group index to save the current text position to

Regex VM Bytecode (v2) (REGEX_USE_COMPACT_VM)

    A more compact storage arrangement. Each operation is encoded into a single
    program stack entry.
    All operations are encoded as a single 32 bit int value:

         32    -     18 17     -     4 3- 0
        |--------------|--------------|----|
        |14 bits (op a)|14 bits (op b)|4bit|
        | Operand A    | Operand B    | Op |

    Each operation has the same opcode as v1, but now all opcodes have their
    operands encoded within a single instruction.

 */

#define REGEX_USE_COMPACT_VM

typedef struct regex_vm_s regex_vm_t;
struct regex_vm_s {
    unsigned int *program;
    int size;
    char **string_table;
    int string_tbl_size;
    unsigned char **class_table;
    int class_tbl_size;
    char **group_table;
    int group_tbl_size;
};

#ifdef MOJO_REGEX_COMPILE_IMPLEMENTATION

typedef enum {
    eCompileOk,
    eCompileCharClassRangeIncomplete,
    eCompileCharClassIncomplete,
    eCompileEscapeCharIncomplete,
    eCompileMalformedSubExprName,
    eCompileUnsupportedMeta,
    eCompileOutOfMem,
    eCompileMissingOperand,
    eCompileMissingSubexprStart,
    eCompileInternalError
} eRegexCompileStatus;

typedef struct regex_compile_ctx_s regex_compile_ctx_t;
struct regex_compile_ctx_s {
    eRegexCompileStatus status;
    const char *pattern;
    int position;
    regex_vm_t *vm;
};

const char *regexGetCompileStatusStr(eRegexCompileStatus status);
eRegexCompileStatus regexCompile(regex_compile_ctx_t *ctx, const char *pattern);

#endif // MOJO_REGEX_COMPILE_IMPLEMENTATION

#ifdef MOJO_REGEX_EVALUATE_IMPLEMENTATION

typedef struct regex_match_s regex_match_t;
struct regex_match_s {
    const char *text;
    regex_vm_t *vm;
    const char **subexprs;
};

//regex_match_t *regexMatch(regex_vm_t *vm, const char *text, int complete);
void regexMatchFree(regex_match_t *match);
const char *regexGroupValueGet(regex_match_t *match, int group, int *len);

#endif // MOJO_REGEX_EVALUATE_IMPLEMENTATION

void regexVMFree(regex_vm_t *vm);
void regexVMGenerateDeclaration(regex_vm_t *vm, const char *symbol, FILE *fp);
void regexVMGenerateDefinition(regex_vm_t *vm, const char *symbol, FILE *fp);
void regexVMPrintProgram(FILE *fp, regex_vm_t *vm);

int regexGroupCountGet(regex_vm_t *vm);
const char *regexGroupNameLookup(regex_vm_t *vm, int group);
int regexGroupIndexLookup(regex_vm_t *vm, const char *name);


#if defined(MOJO_REGEX_IMPLEMENTATION)

// Data structures and function declarations ////////////////////////////////

#define DEF_VM_SIZE_INC 1024
#define DEF_VM_STRTBL_INC 32

#define VM_PC_UNVISITED     ((int)-1)
#define VM_PC_PENDING_ASSIGNMENT    ((int)-2)

typedef enum {
    eTokenNone,
    eTokenCharLiteral,
    eTokenCharClass,
    eTokenStringLiteral,
    eTokenCharAny,
    eTokenMatch,
    eTokenSplit,
    eTokenJmp,
    eTokenSave,
    eTokenConcatenation,
    eTokenAlternative,
    eTokenZeroOrOne,
    eTokenZeroOrMany,
    eTokenOneOrMany,
    eTokenSubExprStart,
    eTokenSubExprEnd
} eRegexToken;

typedef enum {
    ePriorityNone,
    ePriorityLow,
    ePriorityMedium,
    ePriorityHigh
} eRegexTokenPriority;

typedef struct regex_token_s regex_token_t;
struct regex_token_s {
    eRegexToken tokenType;
    union {
        int c;
        char *str;
        unsigned char *bitmap;
        int group;
    };
    int pc;
    regex_token_t *out_a;
    regex_token_t *out_b;
    regex_token_t *next;
};

typedef struct regex_vm_build_s regex_vm_build_t;
struct regex_vm_build_s {
    regex_vm_t *vm;
    regex_token_t *tokens;
    int string_tbl_count;
    int class_tbl_count;
    int group_tbl_count;
    int groups;
    int pc;
};

int regexVMGroupTableEntryAdd(regex_vm_build_t *build, const char *group, int len, int index);

/////////////////////////////////////////////////////////////////////////////
// Character parsing, assists with escaped chars
/////////////////////////////////////////////////////////////////////////////

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
    int k, val;

    if(**pattern == '\0') {
        return result;
    }
    if(**pattern == '\\') {
        result.escaped = 1;
        (*pattern)++;
        if(**pattern == '\0') {
            return result;
        }
        switch(**pattern) {
            case 'a': result.c = '\a'; break;
            case 'b': result.c = '\b'; break;
            case 'e': result.c = '\x1B'; break;
            case 'f': result.c = '\f'; break;
            case 'n': result.c = '\n'; break;
            case 'r': result.c = '\r'; break;
            case 't': result.c = '\t'; break;
            case 'v': result.c = '\v'; break;
            case 'x':
                // Hexidecimal encoding
                (*pattern)++;
                val = 0;
                for(k = 0; k < 2; k++) {
                    val *= 16;
                    if((**pattern >= 0) && (**pattern <= 9)) {
                        val += **pattern - '0';
                    } else if((**pattern >= 'A') && (**pattern <= 'F')) {
                        val += (**pattern - 'A') + 10;
                    } else if((**pattern >= 'a') && (**pattern <= 'f')) {
                        val += (**pattern - 'a') + 10;
                    } else {
                        return result;
                    }
                }
                result.c = (char)val;
                break;
            case 'u':
                // Unicode is not supported in this iteration
                return result;
            default:
                if((**pattern >= '0') && (**pattern <= '9')) {
                    // Octal encoded escape character
                    val = 0;
                    for(k = 0; k < 3; k++) {
                        if((**pattern >= '0') && (**pattern <= '9')) {
                            val *= 8;
                            val += **pattern - '0';
                        } else {
                            return result;
                        }
                        (*pattern)++;
                    }
                    result.c = (char)(val);
                } else {
                    // Assume an overzealous escape regime
                    result.c = **pattern;
                }
                break;
        }
    } else {
        result.c = **pattern;
    }
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

int regexIsAlnum(char c) {
    return (((c >= 'a') && (c <= 'z')) ||
            ((c >= 'A') && (c <= 'Z')) ||
            ((c >= '0') && (c <= '9')));
}

int regexIsMeta(int c) {
    return ((c == '|') || (c == '?') || (c == '.') || (c == '*') ||
            (c == '+') || (c == '(') || (c == '[') || (c == ')'));
}

// String literal parsing handlers //////////////////////////////////////////

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
        *ptr = (char)(c.c);
        ptr++;
    }
    *ptr = '\0';
    return str;
}

// Character class parsing handlers /////////////////////////////////////////

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
    int last = 0;
    int next;

    memset(bitmap, 0, 32);

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
            last = c.c;
            mapSet(bitmap, last);
            range = 1;
        } else if(range == 1) {
            if(!c.escaped && c.c == '-') {
                range = 2;
            } else {
                last = c.c;
                mapSet(bitmap, last);
            }
        } else {
            next = c.c;
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

void regexPrintCharClassToFP(FILE *fp, unsigned char *bitmap) {
    int k;
    int run;

    for(k = 0; k < 256; k++) {
        if(mapCheck(bitmap, k)) {
            for(run = k + 1; run < 256 && mapCheck(bitmap, run); run++);
            run--;
            fprintf(fp, "%c", ((k < 32) || (k > 127)) ? '.' : k);
            if(run - k > 3) {
                fprintf(fp, "-%c", ((run < 32) || (run > 127)) ? '.' : run);
                k = run;
            }
        }
    }
}

void regexPrintCharClass(unsigned char *bitmap) {
    regexPrintCharClassToFP(stdout, bitmap);
}

// Subexpression name table /////////////////////////////////////////////////

// The subexpression group number is derived from it's pattern order. This
// group number can be used to derive the expression number, via the following
// utility functions.

int regexSubexprStartFromGroup(int group) {
    return (group - 1) * 2;
}

int regexSubexprEndFromGroup(int group) {
    return ((group - 1) * 2) + 1;
}

int regexGroupFromSubexpression(int subexpr) {
    if(subexpr % 2) {
        subexpr -= 1;
    }
    return subexpr / 2;
}

// Parses the subexpression name pointed to by pattern, creates a
// subexpression name lookup entry, and adds it to the subexpression name list.
// Returns 1 on success, 0 on out of memory, and -1 if the name is malformed.
int regexSubexprLookupEntryCreate(regex_vm_build_t *build, const char **pattern, int index) {
    int len;

    index--;
    for(len = 0; (*pattern)[len] != '>' && (*pattern)[len] != '\0'; len++) {
        if(!regexIsAlnum((*pattern)[len])) {
            return -1;
        }
    }
    if(len == 0) {
        return -1;
    }

    if(!regexVMGroupTableEntryAdd(build, *pattern, len, index)) {
        return 0;
    }
    *pattern += len;

    // Discard the trailing '>' delimiter
    (*pattern)++;

    return 1;
}

// Token management functions //////////////////////////////////////////////

int regexTokenIsTerminal(regex_token_t *token, int preceeding) {
    switch(token->tokenType) {
        case eTokenCharLiteral:
        case eTokenStringLiteral:
        case eTokenCharClass:
        case eTokenCharAny:
            return 1;
        case eTokenZeroOrOne:
        case eTokenZeroOrMany:
        case eTokenOneOrMany:
            return preceeding;
        case eTokenSubExprEnd:
            return preceeding;
        case eTokenSubExprStart:
            return !preceeding;
        default:
            return 0;
    }
}

regex_token_t *regexAllocToken(eRegexToken tokenType, int c, char *str) {
    regex_token_t *token;

    if((token = malloc(sizeof(regex_token_t))) == NULL) {
        return NULL;
    }
    memset(token, 0, sizeof(regex_token_t));
    token->tokenType = tokenType;
    if(str != NULL) {
        token->str = str;
    } else {
        token->c = c;
    }
    return token;
}

int regexTokenCreate(regex_token_t **list, eRegexToken tokenType, int c, char *str) {
    regex_token_t *token, *walk;

    if((token = regexAllocToken(tokenType, c, str)) == NULL) {
        return 0;
    }

    if(*list == NULL) {
        *list = token;
    } else {
        for(walk = *list; walk->next != NULL; walk = walk->next);
        if(regexTokenIsTerminal(token, 0) && regexTokenIsTerminal(walk, 1)) {
            // Two adjacent terminals have an implicit concatenation
            if((walk->next = regexAllocToken(eTokenConcatenation, 0, NULL)) == NULL) {
                free(token);
                return 0;
            }
            walk = walk->next;
        }
        walk->next = token;
    }

    return 1;
}

void regexTokenDestroy(regex_token_t *token, int stack) {
    regex_token_t *next;

    if(token == NULL) {
        return;
    }

    if(stack) {
        for(; token != NULL; token = next) {
            next = token->next;
            switch(token->tokenType) {
                case eTokenCharClass:
                case eTokenStringLiteral:
                    if(token->str != NULL) {
                        free(token->str);
                    }
                    break;
                default:
                    break;
            }
            free(token);
        }
    } else {
        if(token->pc != VM_PC_UNVISITED) {
            return;
        }
        token->pc = VM_PC_UNVISITED;
        regexTokenDestroy(token->out_a, 0);
        regexTokenDestroy(token->out_b, 0);
    }
}

/////////////////////////////////////////////////////////////////////////////

#define SET_RESULT(stat)  result = stat; goto compileFailure;

eRegexCompileStatus regexTokenizePattern(const char *pattern,
                                         int *pos,
                                         regex_token_t **tokens,
                                         regex_vm_build_t *build) {
    eRegexCompileStatus result = eCompileOk;
    const char *start = pattern;
    character_t c;
    int response;
    int len;
    char *str;
    unsigned char bitmap[32];
    eRegexCompileStatus status;
    int subexpr = 0;

    // Parse the regex pattern into a sequence of tokens (operators and operands)
    // The output of this stage is a sequence of lexical tokens in infix form

    // Loop through the pattern until we've handled it all
    for(; *pattern != '\0';) {
        // Get the next character in the pattern. The helper function assists
        // in disambiguating escaped characters.
        c = regexGetNextPatternChar(&pattern);
        if(c.c == -1) {
            SET_RESULT(eCompileEscapeCharIncomplete);
        }

        if(!c.escaped) {
            switch(c.c) {
                case '.':
                    // Operand, the meta "ANY" char
                    if(!regexTokenCreate(tokens, eTokenCharAny, 0, 0)) {
                        SET_RESULT(eCompileOutOfMem);
                    }
                    continue;

                case '[':
                    // Operand, character class
                    if((status = regexParseCharClass(&pattern, bitmap)) != eCompileOk) {
                        SET_RESULT(status);
                    }
                    if((str = (char *)mapCopy(bitmap)) == NULL) {
                        SET_RESULT(eCompileOutOfMem);
                    }
                    if(!regexTokenCreate(tokens, eTokenCharClass, 0, str)) {
                        SET_RESULT(eCompileOutOfMem);
                    }
                    continue;

                case '?':
                    if(!regexTokenCreate(tokens, eTokenZeroOrOne, 0, 0)) {
                        SET_RESULT(eCompileOutOfMem);
                    }
                    continue;

                case '*':
                    if(!regexTokenCreate(tokens, eTokenZeroOrMany, 0, 0)) {
                        SET_RESULT(eCompileOutOfMem);
                    }
                    continue;

                case '+':
                    if(!regexTokenCreate(tokens, eTokenOneOrMany, 0, 0)) {
                        SET_RESULT(eCompileOutOfMem);
                    }
                    continue;

                case '|':
                    if(!regexTokenCreate(tokens, eTokenAlternative, 0, 0)) {
                        SET_RESULT(eCompileOutOfMem);
                    }
                    continue;

                case '(':
                    // Grouped subexpression, complex operator, resolves to a compound operand
                    build->groups++;
                    subexpr = build->groups;
                    str = NULL;
                    if(regexCheckNextPatternChar(&pattern, '?')) {
                        // Meta character subexpression modifier
                        if(regexCheckNextPatternChar(&pattern, 'i')) {
                            // Case insensitive sub expression
                            // TODO
                        } else if(regexCheckNextPatternChar(&pattern, 'P')) {
                            // Named sub expression
                            if(!regexCheckNextPatternChar(&pattern, '<')) {
                                SET_RESULT(eCompileMalformedSubExprName);
                            }
                            if((response = regexSubexprLookupEntryCreate(build, &pattern, subexpr)) != 1) {
                                SET_RESULT((response == 0 ? eCompileOutOfMem : eCompileMalformedSubExprName));
                            }
                        } else {
                            SET_RESULT(eCompileUnsupportedMeta);
                        }
                    }
                    if(!regexTokenCreate(tokens, eTokenSubExprStart, subexpr, NULL)) {
                        SET_RESULT(eCompileOutOfMem);
                    }
                    continue;

                case ')':
                    // End of grouped subexpression
                    if(!regexTokenCreate(tokens, eTokenSubExprEnd, 0, 0)) {
                        SET_RESULT(eCompileOutOfMem);
                    }
                    continue;

                default:
                    // Operand, unnecessary escaping, fall through to "regular character"...
                    break;
            }
        }

        // Operand, either character literal or string literal
        switch(len = regexGetPatternStrLen(&pattern)) {
            case -1:
            SET_RESULT(eCompileEscapeCharIncomplete);
            case 0:
                if(!regexTokenCreate(tokens, eTokenCharLiteral, (char)(c.c), 0)) {
                    SET_RESULT(eCompileOutOfMem);
                }
                break;
            default:
                len++;
                if(c.escaped) {
                    pattern--;
                }
                pattern--;
                if((str = regexGetPatternStr(&pattern, len)) == NULL) {
                    SET_RESULT(eCompileOutOfMem);
                }
                if(!regexTokenCreate(tokens, eTokenStringLiteral, 0, str)) {
                    SET_RESULT(eCompileOutOfMem);
                }
        }
    }

    SET_RESULT(eCompileOk);

compileFailure:
    *pos = (int)(pattern - start);

    return result;
}

typedef struct regex_ptrlist_s regex_ptrlist_t;
struct regex_ptrlist_s {
    regex_token_t **out;
    regex_ptrlist_t *next;
};

typedef struct regex_fragment_s regex_fragment_t;
struct regex_fragment_s {
    regex_token_t *token;
    regex_ptrlist_t *ptrlist;
    regex_fragment_t *next;
};

typedef enum {
    OP_GREATER_OR_EQUAL,
    OP_ALL
} eRegexOpApply;

// Shunting yard implementation, to convert infix to postfix form ///////////

void regexTokenStackPush(regex_token_t **stack, regex_token_t *token) {
    if(*stack == NULL) {
        *stack = token;
        token->next = NULL;
        return;
    }
    token->next = *stack;
    *stack = token;
}

regex_token_t *regexTokenStackPop(regex_token_t **stack) {
    regex_token_t *entry;

    if(*stack == NULL) {
        return NULL;
    }
    entry = *stack;
    *stack = entry->next;
    entry->next = NULL;
    return entry;
}

eRegexToken regexTokenStackPeekType(regex_token_t *stack) {
    if(stack == NULL) {
        return eTokenNone;
    }
    return stack->tokenType;
}

eRegexTokenPriority regexGetTokenTypePriority(eRegexToken tokenType) {
    switch(tokenType) {
        case eTokenCharLiteral:
        case eTokenCharClass:
        case eTokenStringLiteral:
        case eTokenCharAny:
        case eTokenMatch:
        default:
            return ePriorityNone;

        case eTokenZeroOrOne:
        case eTokenZeroOrMany:
        case eTokenOneOrMany:
            return ePriorityHigh;

        case eTokenConcatenation:
        case eTokenAlternative:
            return ePriorityMedium;

        case eTokenSubExprStart:
        case eTokenSubExprEnd:
            return ePriorityLow;
    }
}

int regexStackTypeGreaterOrEqualToToken(regex_token_t *stack, eRegexToken tokenType) {
    eRegexTokenPriority stackPriority;

    stackPriority = regexGetTokenTypePriority(regexTokenStackPeekType(stack));
    return (stackPriority >= regexGetTokenTypePriority(tokenType));
}

int regexGetOperatorArity(regex_token_t *token) {
    switch(token->tokenType) {
        case eTokenZeroOrOne:
        case eTokenZeroOrMany:
        case eTokenOneOrMany:
            return 1;
        case eTokenConcatenation:
        case eTokenAlternative:
            return 2;
        default:
            return 0;
    }
}

// NFA form regex support ///////////////////////////////////////////////////

regex_fragment_t *regexFragmentCreate(regex_token_t *token, regex_ptrlist_t *list) {
    regex_fragment_t *fragment;

    if((fragment = malloc(sizeof(regex_fragment_t))) == NULL) {
        return NULL;
    }
    memset(fragment, 0, sizeof(regex_fragment_t));
    fragment->token = token;
    fragment->ptrlist = list;
    return fragment;
}

regex_ptrlist_t *regexPtrlistCreate(regex_token_t **state) {
    regex_ptrlist_t *entry;

    if((entry = malloc(sizeof(regex_ptrlist_t))) == NULL) {
        return NULL;
    }
    memset(entry, 0, sizeof(regex_ptrlist_t));
    entry->out = state;
    return entry;
}

regex_ptrlist_t *regexPtrlistAppend(regex_ptrlist_t *lista, regex_ptrlist_t *listb) {
    regex_ptrlist_t *walk;

    if(lista == NULL) {
        return listb;
    }
    if(listb == NULL) {
        return lista;
    }

    for(walk = lista; walk->next != NULL; walk = walk->next);
    walk->next = listb;
    return lista;
}

void regexPtrlistPatch(regex_ptrlist_t *list, regex_token_t *token) {
    for(; list != NULL; list = list->next) {
        *(list->out) = token;
    }
}

void regexPtrListFree(regex_ptrlist_t *list) {
    regex_ptrlist_t *next = NULL;
    for(; list != NULL; list = next) {
        next = list->next;
        free(list);
    }
}

void regexFragmentFree(regex_fragment_t *fragment) {
    regex_fragment_t *next;

    while(fragment != NULL) {
        next = fragment->next;
        if(fragment->token != NULL) {
            regexTokenDestroy(fragment->token, 0);
            fragment->token = NULL;
        }
        if(fragment->ptrlist != NULL) {
            regexPtrListFree(fragment->ptrlist);
            fragment->ptrlist = NULL;
        }
        free(fragment);
        fragment = next;
    }
}

void regexFragmentStackPush(regex_fragment_t **stack, regex_fragment_t *fragment) {
    fragment->next = *stack;
    *stack = fragment;
}

regex_fragment_t *regexFragmentStackPop(regex_fragment_t **stack) {
    regex_fragment_t *fragment;
    fragment = *stack;
    if(fragment != NULL) {
        *stack = fragment->next;
        fragment->next = NULL;
    }
    return fragment;
}

int regexOperatorLiteralCreate(regex_fragment_t **stack, regex_token_t *token) {
    regex_fragment_t *fragment;
    regex_ptrlist_t *list;

    if((list = regexPtrlistCreate(&(token->out_a))) == NULL) {
        return 0;
    }
    if((fragment = regexFragmentCreate(token, list)) == NULL) {
        regexPtrListFree(list);
        return 0;
    }
    regexFragmentStackPush(stack, fragment);
    return 1;
}

int regexOperatorSubexprCreate(regex_fragment_t **stack, regex_token_t *token) {
    regex_ptrlist_t *list;
    regex_fragment_t *fragment;

    token->tokenType = eTokenSave;
    if((list = regexPtrlistCreate(&(token->out_a))) == NULL) {
        return 0;
    }
    if((fragment = regexFragmentCreate(token, list)) == NULL) {
        regexPtrListFree(list);
        return 0;
    }
    regexFragmentStackPush(stack, fragment);
    return 1;
}

int regexOperatorConcatenationCreate(regex_fragment_t **stack, regex_token_t *token) {
    regex_fragment_t *e1, *e2;
    regex_fragment_t *fragment;

    regexTokenDestroy(token, 1);
    if(((e2 = regexFragmentStackPop(stack)) == NULL) ||
       ((e1 = regexFragmentStackPop(stack)) == NULL)) {
        return 0;
    }
    regexPtrlistPatch(e1->ptrlist, e2->token);
    if((fragment = regexFragmentCreate(e1->token, e2->ptrlist)) == NULL) {
        return 0;
    }
    regexFragmentStackPush(stack, fragment);

    regexPtrListFree(e1->ptrlist);
    e1->ptrlist = NULL;
    e1->token = NULL;
    regexFragmentFree(e1);
    e2->token = NULL;
    e2->ptrlist = NULL;
    regexFragmentFree(e2);

    return 1;
}

int regexOperatorAlternationCreate(regex_fragment_t **stack, regex_token_t *token) {
    regex_fragment_t *e1, *e2, *fragment;
    regex_ptrlist_t *list;
    regex_token_t *jmp = NULL;

    if(((e2 = regexFragmentStackPop(stack)) == NULL) ||
       ((e1 = regexFragmentStackPop(stack)) == NULL)) {
        return 0;
    }

    if(!regexTokenCreate(&jmp, eTokenJmp, 0, NULL)) {
        return 0;
    }
    regexPtrlistPatch(e1->ptrlist, jmp);
    if((list = regexPtrlistCreate(&(jmp->out_a))) == NULL) {
        return 0;
    }

    token->tokenType = eTokenSplit;
    token->out_a = e1->token;
    token->out_b = e2->token;
    if((fragment = regexFragmentCreate(token, regexPtrlistAppend(list, e2->ptrlist))) == NULL) {
        return 0;
    }
    regexFragmentStackPush(stack, fragment);

    regexPtrListFree(e1->ptrlist);
    e1->ptrlist = NULL;
    e1->token = NULL;
    regexFragmentFree(e1);
    e2->ptrlist = NULL;
    e2->token = NULL;
    regexFragmentFree(e2);

    return 1;
}

int regexOperatorZeroOrOneCreate(regex_fragment_t **stack, regex_token_t *token) {
    regex_fragment_t *e, *fragment;
    regex_ptrlist_t *list;

    if((e = regexFragmentStackPop(stack)) == NULL) {
        return 0;
    }
    token->tokenType = eTokenSplit;
    token->out_a = e->token;
    if((list = regexPtrlistCreate(&(token->out_b))) == NULL) {
        return 0;
    }
    if((fragment = regexFragmentCreate(token, regexPtrlistAppend(e->ptrlist, list))) == NULL) {
        return 0;
    }
    regexFragmentStackPush(stack, fragment);

    e->ptrlist = NULL;
    e->token = NULL;
    regexFragmentFree(e);

    return 1;
}

int regexOperatorZeroOrMoreCreate(regex_fragment_t **stack, regex_token_t *token) {
    regex_ptrlist_t *list;
    regex_fragment_t *fragment, *e;
    regex_token_t *jmp = NULL;

    if((e = regexFragmentStackPop(stack)) == NULL) {
        return 0;
    }
    token->tokenType = eTokenSplit;
    token->out_a = e->token;
    if((list = regexPtrlistCreate(&(token->out_b))) == NULL) {
        return 0;
    }
    if(!regexTokenCreate(&jmp, eTokenJmp, 0, NULL)) {
        return 0;
    }
    jmp->out_a = token;
    regexPtrlistPatch(e->ptrlist, jmp);
    if((fragment = regexFragmentCreate(token, list)) == NULL) {
        return 0;
    }
    regexFragmentStackPush(stack, fragment);

    e->ptrlist = NULL;
    e->token = NULL;
    regexFragmentFree(e);

    return 1;
}

int regexOperatorOneOrMoreCreate(regex_fragment_t **stack, regex_token_t *token) {
    regex_ptrlist_t *list;
    regex_fragment_t *fragment, *e;

    if((e = regexFragmentStackPop(stack)) == NULL) {
        return 0;
    }
    token->tokenType = eTokenSplit;
    token->out_a = e->token;
    regexPtrlistPatch(e->ptrlist, token);
    if((list = regexPtrlistCreate(&(token->out_b))) == NULL) {
        return 0;
    }
    if((fragment = regexFragmentCreate(e->token, list)) == NULL) {
        free(list);
        return 0;
    }
    regexFragmentStackPush(stack, fragment);
    return 1;
}

int regexOperatorMatchCreate(regex_fragment_t **stack) {
    regex_fragment_t *e;
    regex_token_t *token = NULL;

    if((e = regexFragmentStackPop(stack)) == NULL) {
        return 0;
    }
    if(!regexTokenCreate(&token, eTokenMatch, 0, NULL)) {
        return 0;
    }
    regexPtrlistPatch(e->ptrlist, token);
    regexFragmentStackPush(stack, e);

    return 1;
}

int regexHasSufficientOperands(regex_fragment_t *fragments, int arity) {
    for(; ((fragments != NULL) && (arity)); arity--, fragments = fragments->next);
    return arity == 0;
}

eRegexCompileStatus regexOperatorApply(regex_token_t **operators, eRegexOpApply apply, eRegexToken tokenType, regex_fragment_t **operands) {
    regex_token_t *operator;

    while((apply == OP_ALL && *operators != NULL) ||
          (apply == OP_GREATER_OR_EQUAL && regexStackTypeGreaterOrEqualToToken(*operators, tokenType))) {
        operator = regexTokenStackPop(operators);
        if(!regexHasSufficientOperands(*operands, regexGetOperatorArity(operator))) {
            return eCompileMissingOperand;
        }
        switch(operator->tokenType) {
            case eTokenZeroOrOne:
                if(!regexOperatorZeroOrOneCreate(operands, operator)) {
                    return eCompileOutOfMem;
                }
                break;

            case eTokenZeroOrMany:
                if(!regexOperatorZeroOrMoreCreate(operands, operator)) {
                    return eCompileOutOfMem;
                }
                break;

            case eTokenOneOrMany:
                if(!regexOperatorOneOrMoreCreate(operands, operator)) {
                    return eCompileOutOfMem;
                }
                break;

            case eTokenConcatenation:
                if(!regexOperatorConcatenationCreate(operands, operator)) {
                    return eCompileOutOfMem;
                }
                break;

            case eTokenAlternative:
                if(!regexOperatorAlternationCreate(operands, operator)) {
                    return eCompileOutOfMem;
                }
                break;

            default:
                return eCompileInternalError;
        }
    }
    return eCompileOk;
}

#define SET_YARD_RESULT(res)    status = res; goto ShuntingYardFailure;

eRegexCompileStatus regexShuntingYardFragment(regex_token_t **tokens, regex_fragment_t **root_stack, int sub_expression) {
    regex_token_t *token = NULL, *operators = NULL;
    regex_fragment_t *operands = NULL, *subexpr;
    eRegexCompileStatus status;
    int group_num;

    if(sub_expression >= 0) {
        operands = *root_stack;
        if(!regexTokenCreate(&token, eTokenConcatenation, 0, NULL)) {
            return eCompileOutOfMem;
        }
        regexTokenStackPush(&operators, token);
    }

    while((token = regexTokenStackPop(tokens)) != NULL) {
        switch(token->tokenType) {
            default:
                return eCompileInternalError;
            case eTokenCharLiteral:
            case eTokenCharClass:
            case eTokenStringLiteral:
            case eTokenCharAny:
            case eTokenMatch:
                if(!regexOperatorLiteralCreate(&operands, token)) {
                    SET_YARD_RESULT(eCompileOutOfMem);
                }
                break;

            case eTokenZeroOrOne:
            case eTokenZeroOrMany:
            case eTokenOneOrMany:
            case eTokenConcatenation:
            case eTokenAlternative:
                if((status = regexOperatorApply(&operators, OP_GREATER_OR_EQUAL, token->tokenType, &operands)) != eCompileOk) {
                    goto ShuntingYardFailure;
                }
                regexTokenStackPush(&operators, token);
                break;

            case eTokenSubExprStart:
                subexpr = NULL;
                group_num = token->group;
                token->group = regexSubexprStartFromGroup(token->group);
                if(!regexOperatorSubexprCreate(&subexpr, token)) {
                    SET_YARD_RESULT(eCompileOutOfMem);
                }
                if((status = regexShuntingYardFragment(tokens, &subexpr, group_num)) != eCompileOk) {
                    SET_YARD_RESULT(status);
                }
                regexFragmentStackPush(&operands, subexpr);
                break;

            case eTokenSubExprEnd:
                if(sub_expression == -1) {
                    SET_YARD_RESULT(eCompileMissingSubexprStart);
                }
                token->group = regexSubexprEndFromGroup(sub_expression);
                if(!regexOperatorSubexprCreate(&operands, token)) {
                    SET_YARD_RESULT(eCompileOutOfMem);
                }
                token = NULL;
                if(!regexTokenCreate(&token, eTokenConcatenation, 0, NULL)) {
                    return eCompileOutOfMem;
                }
                regexTokenStackPush(&operators, token);
                if((status = regexOperatorApply(&operators, OP_ALL, 0, &operands)) != eCompileOk) {
                    goto ShuntingYardFailure;
                }
                if(operands->next != NULL) {
                    SET_YARD_RESULT(eCompileInternalError);
                }
                *root_stack = operands;
                return eCompileOk;
        }
    }

    if((status = regexOperatorApply(&operators, OP_ALL, 0, &operands)) != eCompileOk) {
        goto ShuntingYardFailure;
    }

    if(sub_expression != -1) {
        SET_YARD_RESULT(eCompileInternalError);
    }

    // Complete the sequence with a match
    if(!regexOperatorMatchCreate(&operands)) {
        SET_YARD_RESULT(eCompileOutOfMem);
    }

    *root_stack = operands;

    return status;

ShuntingYardFailure:
    regexFragmentFree(operands);
    regexTokenDestroy(operators, 1);
    operands = NULL;

    return status;
}

eRegexCompileStatus regexShuntingYard(regex_token_t **tokens) {
    regex_fragment_t *stack = NULL;
    eRegexCompileStatus status;

    status = regexShuntingYardFragment(tokens, &stack, -1);
    if(status != eCompileOk) {
        return status;
    }

    *tokens = stack->token;
    stack->token = NULL;
    regexFragmentFree(stack);

    return status;
}

void regexVMStringTableFree(regex_vm_t *vm) {
    int k;
    if(vm->string_table != NULL) {
        for(k = 0; k < vm->string_tbl_size; k++) {
            if(vm->string_table[k] != NULL) {
                free(vm->string_table[k]);
            }
        }
        free(vm->string_table);
    }
    vm->string_table = NULL;
    vm->string_tbl_size = 0;
}

int regexVMStringTableEntryAdd(regex_vm_build_t *build, const char *str) {
    int index;

    for(index = 0; index < build->string_tbl_count; index++) {
        if(!strcmp(build->vm->string_table[index], str)) {
            return index;
        }
    }

    if(build->vm->string_tbl_size <= build->string_tbl_count) {
        if((build->vm->string_table = realloc(build->vm->string_table, (build->vm->string_tbl_size + DEF_VM_STRTBL_INC) * sizeof(char *))) == NULL) {
            return -1;
        }
        memset(build->vm->string_table + build->vm->string_tbl_size, 0, DEF_VM_STRTBL_INC * sizeof(char *));
        build->vm->string_tbl_size += DEF_VM_STRTBL_INC;
    }
    if((build->vm->string_table[build->string_tbl_count] = strdup(str)) == NULL) {
        return -1;
    }
    index = build->string_tbl_count;
    build->string_tbl_count++;
    return index;
}

const char *regexVMStringTableEntryGet(regex_vm_t *vm, int string_table_id) {
    if(string_table_id >= vm->string_tbl_size) {
        return NULL;
    }
    return vm->string_table[string_table_id];
}

void regexVMClassTableFree(regex_vm_t *vm) {
    int k;
    if(vm->class_table != NULL) {
        for(k = 0; k < vm->class_tbl_size; k++) {
            if(vm->class_table[k] != NULL) {
                free(vm->class_table[k]);
            }
        }
        free(vm->class_table);
    }
    vm->class_table = NULL;
    vm->class_tbl_size = 0;
}

int regexVMClassTableEntryAdd(regex_vm_build_t *build, const unsigned char *bitmap) {
    int index;

    for(index = 0; index < build->class_tbl_count; index++) {
        if(!memcmp(build->vm->class_table[index], bitmap, 32)) {
            return index;
        }
    }

    if(build->vm->class_tbl_size <= build->class_tbl_count ) {
        if((build->vm->class_table = realloc(build->vm->class_table, (build->vm->class_tbl_size + DEF_VM_STRTBL_INC) * sizeof(char *))) == NULL) {
            return -1;
        }
        memset(build->vm->class_table + build->vm->class_tbl_size, 0, DEF_VM_STRTBL_INC * sizeof(char *));
        build->vm->class_tbl_size += DEF_VM_STRTBL_INC;
    }

    if((build->vm->class_table[build->class_tbl_count] = malloc(32)) == NULL) {
        return -1;
    }
    memcpy(build->vm->class_table[build->class_tbl_count], bitmap, 32);
    index = build->class_tbl_count;
    build->class_tbl_count++;
    return index;
}

const unsigned char *regexVMClassTableEntryGet(regex_vm_t *vm, int class_table_id) {
    if(class_table_id >= vm->class_tbl_size) {
        return NULL;
    }
    return vm->class_table[class_table_id];
}

void regexVMGroupTableFree(regex_vm_t *vm) {
    int k;
    if(vm->group_table != NULL) {
        for(k = 0; k < vm->group_tbl_size; k++) {
            if(vm->group_table[k] != NULL) {
                free(vm->group_table[k]);
            }
        }
        free(vm->group_table);
    }
    vm->group_table = NULL;
    vm->group_tbl_size = 0;
}

int regexVMGroupTableEntryAdd(regex_vm_build_t *build, const char *group, int len, int index) {
    int add = DEF_VM_STRTBL_INC;

    if(build->vm->group_tbl_size <= index) {
        while((build->vm->group_tbl_size + add) <= index) {
            add += DEF_VM_STRTBL_INC;
        }
        if((build->vm->group_table = realloc(build->vm->group_table, (build->vm->group_tbl_size + add) * sizeof(char *))) == NULL) {
            return 0;
        }
        memset(build->vm->group_table + build->vm->group_tbl_size, 0, add * sizeof(char *));
        build->vm->group_tbl_size += add;
        build->group_tbl_count += add;
    }

    if((build->vm->group_table[index] = malloc(len + 1)) == NULL) {
        return 0;
    }
    strncpy(build->vm->group_table[index], group, len);
    build->vm->group_table[index][len] = '\0';
    build->vm->group_tbl_size += add;

    return 1;
}

const char *regexVMGroupNameFromIndex(regex_vm_t *vm, int index) {
    index = regexGroupFromSubexpression(index);
    if((index < vm->group_tbl_size) && (index >= 0)) {
        return vm->group_table[index];
    }
    return NULL;
}

// VM Patch list management functions ///////////////////////////////////////

#ifdef REGEX_USE_COMPACT_VM

#define REGEX_VM_INSTR_DECODE2(var,vm,pc)   (var)[0] = (vm)->program[(pc)] & 0xFU; \
                                            (var)[1] = ((vm)->program[(pc)] & 0x3FFF0U) >> 4U; \
                                            (var)[2] = ((vm)->program[(pc)] & 0xFFFC0000U) >> 18U;
#define REGEX_VM_INSTR_DECODE1(var,vm,pc)   (var)[0] = (vm)->program[(pc)] & 0xFU; \
                                            (var)[1] = ((vm)->program[(pc)] & 0x3FFF0U) >> 4U;
#define REGEX_VM_INSTR_DECODE0(var,vm,pc)   (var)[0] = (vm)->program[(pc)] & 0xFU;

#define REGEX_VM_INSTR_ENCODE2(vm,pc,instr,opa,opb) (vm)->program[(pc)] = (instr & 0xFU) | ((opa & 0x3FFFU) << 4U) | ((opb & 0x3FFFU) << 18U)
#define REGEX_VM_INSTR_ENCODE1(vm,pc,instr,opa)     (vm)->program[(pc)] = (instr & 0xFU) | ((opa & 0x3FFFU) << 4U)
#define REGEX_VM_INSTR_ENCODE0(vm,pc,instr)         (vm)->program[(pc)] = (instr & 0xFU)

typedef struct regex_vm_pc_patch_s regex_vm_pc_patch_t;
struct regex_vm_pc_patch_s {
    regex_token_t *token;
    int pc;
    int operand;
    regex_vm_pc_patch_t *next;
};

int regexAddPCPatchEntry(regex_vm_pc_patch_t **patch_list, regex_token_t *token, int index, int operand) {
    regex_vm_pc_patch_t *entry;

    if((entry = malloc(sizeof(regex_vm_pc_patch_t))) == NULL) {
        return 0;
    }
    memset(entry, 0, sizeof(regex_vm_pc_patch_t));
    entry->pc = index;
    entry->operand = operand;
    entry->token = token;
    entry->next = *patch_list;
    *patch_list = entry;
    return 1;
}

void regexVMPatchListFree(regex_vm_pc_patch_t *patch_list) {
    regex_vm_pc_patch_t *next;

    for(; patch_list != NULL; patch_list = next) {
        next = patch_list;
        free(patch_list);
    }
}

void regexVMPatchJumps(regex_vm_build_t *build, regex_vm_pc_patch_t **patch_list) {
    regex_vm_pc_patch_t *patch, *next;
    unsigned int instr[3];

    for(patch = *patch_list; patch != NULL; patch = next) {
        next = patch->next;
        REGEX_VM_INSTR_DECODE2(instr, build->vm, patch->pc);
        if(patch->operand == 1) {
            instr[1] = patch->token->pc;
        } else {
            instr[2] = patch->token->pc;
        }
        REGEX_VM_INSTR_ENCODE2(build->vm, patch->pc, instr[0], instr[1], instr[2]);
        free(patch);
    }
    *patch_list = NULL;
}

/////////////////////////////////////////////////////////////////////////////

int regexVMProgramAdd(regex_vm_build_t *build, eRegexToken opcode, unsigned int arg1, unsigned int arg2) {
    if((build->vm->size - build->pc) <= 0) {
        if((build->vm->program = realloc(build->vm->program, (build->vm->size + DEF_VM_SIZE_INC) * sizeof(int))) == NULL) {
            return 0;
        }
        memset(build->vm->program + build->vm->size, 0, DEF_VM_SIZE_INC * sizeof(int));
        build->vm->size += DEF_VM_SIZE_INC;
    }

    REGEX_VM_INSTR_ENCODE2(build->vm, build->pc, opcode, arg1, arg2);
    build->pc++;
    
    return 1;
}

void regexWalkAllTokens(regex_token_t *token, int value) {
    if(token != NULL) {
        if(token->pc == value) {
            return;
        }
        token->pc = value;
        if(token->out_a != NULL) {
            regexWalkAllTokens(token->out_a, value);
        }
        if(token->out_b != NULL) {
            regexWalkAllTokens(token->out_b, value);
        }
    }
}

// VM bytecode generator ////////////////////////////////////////////////////

int regexVMProgramGenerate(regex_vm_build_t *build, regex_vm_pc_patch_t **patch_list, regex_token_t *token) {
    int idx_a = 0, idx_b = 0;

    if(token == NULL) {
        return 1;
    }
    if(token->pc != VM_PC_UNVISITED) {
        return 1;
    }
    token->pc = build->pc;

    switch(token->tokenType) {
        case eTokenCharLiteral:
            idx_a = token->c;
            break;
        case eTokenStringLiteral:
            if((idx_a = regexVMStringTableEntryAdd(build, token->str)) == -1) {
                return 0;
            }
            break;
        case eTokenCharClass:
            if((idx_a = regexVMClassTableEntryAdd(build, token->bitmap)) == -1) {
                return 0;
            }
            break;
        case eTokenSave:
            idx_a = token->group;
            break;
        case eTokenSplit:
            if((idx_a = token->out_a->pc) == VM_PC_UNVISITED) {
                if(!regexAddPCPatchEntry(patch_list, token->out_a, build->pc, 1)) {
                    return 0;
                }
            }
            if((idx_b = token->out_b->pc) == VM_PC_UNVISITED) {
                if(!regexAddPCPatchEntry(patch_list, token->out_b, build->pc, 2)) {
                    return 0;
                }
            }
            break;
        case eTokenJmp:
            if((idx_a = token->out_a->pc) == VM_PC_UNVISITED) {
                if(!regexAddPCPatchEntry(patch_list, token->out_a, build->pc, 1)) {
                    return 0;
                }
            }
            break;
        default:
            // Fall through
            break;
    }

    if(!regexVMProgramAdd(build, token->tokenType, idx_a, idx_b)) {
        return 0;
    }

    if(token->tokenType == eTokenSplit) {
        if(!regexVMProgramGenerate(build, patch_list, token->out_a)) {
            return 0;
        }
        return regexVMProgramGenerate(build, patch_list, token->out_b);
    }

    return regexVMProgramGenerate(build, patch_list, token->out_a);

}

int regexVMBuildInit(regex_vm_build_t *build) {
    memset(build, 0, sizeof(regex_vm_build_t));
    if((build->vm = malloc(sizeof(regex_vm_t))) == NULL) {
        return 0;
    }
    memset(build->vm, 0, sizeof(regex_vm_t));
    return 1;
}

void regexVMFree(regex_vm_t *vm) {
    regexVMStringTableFree(vm);
    regexVMClassTableFree(vm);
    regexVMGroupTableFree(vm);
    free(vm);
}

void regexVMBuildDestroy(regex_vm_build_t *build) {
    regexVMFree(build->vm);
    free(build);
}

void regexVMGenerateDeclaration(regex_vm_t *vm, const char *symbol, FILE *fp) {
    fprintf(fp, "extern regex_vm_t *%s;\n", symbol);
}

#define MAX_VM_INSTR_PER_ROW    8

void regexEmitEscapedString(FILE *fp, const char *str) {
    for(; *str != '\0'; str++) {
        if((*str < 32) || (*str > 127)) {
            switch(*str) {
                case '\n': fprintf(fp, "\\n"); break;
                case '\r': fprintf(fp, "\\r"); break;
                case '\t': fprintf(fp, "\\t"); break;
                case '\f': fprintf(fp, "\\f"); break;
                case '\v': fprintf(fp, "\\v"); break;
                case '\a': fprintf(fp, "\\a"); break;
                default:
                    fprintf(fp, "\\x%2.2X\"\"", *str);
                    break;
            }
        } else {
            fputc(*str, fp);
        }
    }
}

void regexVMGenerateDefinition(regex_vm_t *vm, const char *symbol, FILE *fp) {
    int k, j;

    fprintf(fp, "unsigned int _%s_program[] = {", symbol);
    for(k = 0; k < vm->size; k++) {
        if(k != 0) {
            fputc(',', fp);
        }
        if(!(k % MAX_VM_INSTR_PER_ROW)) {
            fputs("\n    ", fp);
        } else {
            fputc(' ', fp);
        }
        fprintf(fp, "0x%.2X", vm->program[k]);
    }
    fprintf(fp, "\n};\n\n");

    fprintf(fp, "char *_%s_string_table[] = {\n", symbol);
    for(k = 0; k < vm->string_tbl_size; k++) {
        if(vm->string_table[k] == NULL) {
            fprintf(fp, "    NULL%s\n", (((k + 1) != vm->string_tbl_size) ? "," : ""));
        } else {
            fprintf(fp, "    \"");
            regexEmitEscapedString(fp, vm->string_table[k]);
            fprintf(fp, "\"%s\n", (((k + 1) != vm->string_tbl_size) ? "," : ""));
        }
    }
    fprintf(fp, "};\n\n");

    for(k = 0; k < vm->class_tbl_size; k++) {
        if(vm->class_table[k] == NULL) {
            continue;
        }
        fprintf(fp, "unsigned char _%s_class_entry_%d[] = {", symbol, k);
        for(j = 0; j < 32; j++) {
            if(j != 0) {
                fputc(',', fp);
            }
            if(!(j % 8)) {
                fputs("\n    ", fp);
            } else {
                fputc(' ', fp);
            }
            fprintf(fp, "0x%2.2X", vm->class_table[k][j]);
        }
        fprintf(fp, "\n};\n\n");
    }

    fprintf(fp, "unsigned char *_%s_class_table[] = {\n", symbol);
    for(k = 0; k < vm->class_tbl_size; k++) {
        if(vm->class_table[k] == NULL) {
            fprintf(fp, "    NULL,\n");
        } else {
            fprintf(fp, "    _%s_class_entry_%d,\n", symbol, k);
        }
    }
    fprintf(fp, "};\n\n");

    fprintf(fp, "char *_%s_group_table[] = {\n", symbol);
    for(k = 0; k < vm->group_tbl_size; k++) {
        if(vm->group_table[k] == NULL) {
            fprintf(fp, "    NULL,\n");
        } else {
            fprintf(fp, "    \"");
            regexEmitEscapedString(fp, vm->group_table[k]);
            fprintf(fp, "\",\n");
        }
    }
    fprintf(fp, "};\n\n");

    fprintf(fp, "regex_vm_t _%s = {\n", symbol);
    fprintf(fp, "    .program = _%s_program,\n", symbol);
    fprintf(fp, "    .size = %d,\n", vm->size);
    fprintf(fp, "    .string_table = _%s_string_table,\n", symbol);
    fprintf(fp, "    .string_tbl_size = %d,\n", vm->string_tbl_size);
    fprintf(fp, "    .class_table = _%s_class_table,\n", symbol);
    fprintf(fp, "    .class_tbl_size = %d,\n", vm->class_tbl_size);
    fprintf(fp, "    .group_table = _%s_group_table,\n", symbol);
    fprintf(fp, "    .group_tbl_size = %d\n", vm->group_tbl_size);
    fprintf(fp, "};\n\n");
    fprintf(fp, "regex_vm_t *%s = &_%s;\n", symbol, symbol);
}

void regexVMPrintProgram(FILE *fp, regex_vm_t *vm) {
    int pc;
    unsigned int instr[3];
    const char *name;

    for(pc = 0; pc < vm->size; pc++) {
        REGEX_VM_INSTR_DECODE2(instr, vm, pc);
        fprintf(fp, "%3.3d  ", pc);
        switch(instr[0]) {
            case eTokenCharLiteral:
                fprintf(fp, "char (%c:%d)\n", (instr[1] < 32 || instr[1] > 127) ? '-' : instr[1], instr[1]);
                break;
            case eTokenStringLiteral:
                fprintf(fp, "string(\"%s\")\n", vm->string_table[instr[1]]);
                break;
            case eTokenCharClass:
                fprintf(fp, "class([");
                regexPrintCharClassToFP(fp, vm->class_table[instr[1]]);
                fprintf(fp, "])\n");
                break;
            case eTokenCharAny:
                fprintf(fp, "anychar\n");
                break;
            case eTokenSave:
                fprintf(fp, "save %d", instr[1]);
                if((name = regexVMGroupNameFromIndex(vm, (int)instr[1])) != NULL) {
                    fprintf(fp, " [%s]\n", name);
                } else {
                    fprintf(fp, "\n");
                }
                break;
            case eTokenSplit:
                fprintf(fp, "split %d, %d\n", instr[1], instr[2]);
                break;
            case eTokenMatch:
                fprintf(fp, "match\n");
                break;
            case eTokenJmp:
                fprintf(fp, "jmp %d\n", instr[1]);
                break;
            default:
                fprintf(fp, "UNKNOWN [%d]!\n", instr[0]);
                return;        }
    }
}
#else
typedef struct regex_vm_pc_patch_s regex_vm_pc_patch_t;
struct regex_vm_pc_patch_s {
    regex_token_t *token;
    int pc;
    regex_vm_pc_patch_t *next;
};

int regexAddPCPatchEntry(regex_vm_pc_patch_t **patch_list, regex_token_t *token, int index) {
    regex_vm_pc_patch_t *entry;

    if((entry = malloc(sizeof(regex_vm_pc_patch_t))) == NULL) {
        return 0;
    }
    memset(entry, 0, sizeof(regex_vm_pc_patch_t));
    entry->pc = index;
    entry->token = token;
    entry->next = *patch_list;
    *patch_list = entry;
    return 1;
}

void regexVMPatchListFree(regex_vm_pc_patch_t *patch_list) {
    regex_vm_pc_patch_t *next;

    for(; patch_list != NULL; patch_list = next) {
        next = patch_list;
        free(patch_list);
    }
}

void regexVMPatchJumps(regex_vm_build_t *build, regex_vm_pc_patch_t **patch_list) {
    regex_vm_pc_patch_t *patch, *next;

    for(patch = *patch_list; patch != NULL; patch = next) {
        next = patch->next;
        build->vm->program[patch->pc] = patch->token->pc;
        free(patch);
    }
    *patch_list = NULL;
}

/////////////////////////////////////////////////////////////////////////////

int regexVMProgramAdd(regex_vm_build_t *build, eRegexToken opcode, int arg1, int arg2) {
    int size;

    switch(opcode) {
        case eTokenCharLiteral: size = 2; break;
        case eTokenCharClass: size = 2; break;
        case eTokenStringLiteral: size = 2; break;
        case eTokenCharAny: size = 1; break;
        case eTokenSplit: size = 3; break;
        case eTokenJmp: size = 2; break;
        case eTokenSave: size = 2; break;
        case eTokenMatch: size = 1; break;
        default: return 0;
    }

    if((build->vm->size - build->pc) <= size) {
        if((build->vm->program = realloc(build->vm->program, (build->vm->size + DEF_VM_SIZE_INC) * sizeof(int))) == NULL) {
            return 0;
        }
        memset(build->vm->program + build->vm->size, 0, DEF_VM_SIZE_INC * sizeof(int));
        build->vm->size += DEF_VM_SIZE_INC;
    }

    build->vm->program[build->pc] = opcode;
    build->pc++;
    if(size > 1) {
        build->vm->program[build->pc] = arg1;
        build->pc++;
        if(size > 2) {
            build->vm->program[build->pc] = arg2;
            build->pc++;
        }
    }
    return 1;
}

void regexWalkAllTokens(regex_token_t *token, int value) {
    if(token != NULL) {
        if(token->pc == value) {
            return;
        }
        token->pc = value;
        if(token->out_a != NULL) {
            regexWalkAllTokens(token->out_a, value);
        }
        if(token->out_b != NULL) {
            regexWalkAllTokens(token->out_b, value);
        }
    }
}

// VM bytecode generator ////////////////////////////////////////////////////

int regexVMProgramGenerate(regex_vm_build_t *build, regex_vm_pc_patch_t **patch_list, regex_token_t *token) {
    int idx_a = 0, idx_b = 0;

    if(token == NULL) {
        return 1;
    }
    if(token->pc != VM_PC_UNVISITED) {
        return 1;
    }
    token->pc = build->pc;

    switch(token->tokenType) {
        case eTokenCharLiteral:
            idx_a = token->c;
            break;
        case eTokenStringLiteral:
            if((idx_a = regexVMStringTableEntryAdd(build, token->str)) == -1) {
                return 0;
            }
            break;
        case eTokenCharClass:
            if((idx_a = regexVMClassTableEntryAdd(build, token->bitmap)) == -1) {
                return 0;
            }
            break;
        case eTokenSave:
            idx_a = token->group;
            break;
        case eTokenSplit:
            if((idx_a = token->out_a->pc) == VM_PC_UNVISITED) {
                if(!regexAddPCPatchEntry(patch_list, token->out_a, build->pc + 1)) {
                    return 0;
                }
            }
            if((idx_b = token->out_b->pc) == VM_PC_UNVISITED) {
                if(!regexAddPCPatchEntry(patch_list, token->out_b, build->pc + 2)) {
                    return 0;
                }
            }
            break;
        case eTokenJmp:
            if((idx_a = token->out_a->pc) == VM_PC_UNVISITED) {
                if(!regexAddPCPatchEntry(patch_list, token->out_a, build->pc + 1)) {
                    return 0;
                }
            }
            break;
        default:
            // Fall through
            break;
    }

    if(!regexVMProgramAdd(build, token->tokenType, idx_a, idx_b)) {
        return 0;
    }

    if(token->tokenType == eTokenSplit) {
        if(!regexVMProgramGenerate(build, patch_list, token->out_a)) {
            return 0;
        }
        return regexVMProgramGenerate(build, patch_list, token->out_b);
    }

    return regexVMProgramGenerate(build, patch_list, token->out_a);

}

int regexVMBuildInit(regex_vm_build_t *build) {
    memset(build, 0, sizeof(regex_vm_build_t));
    if((build->vm = malloc(sizeof(regex_vm_t))) == NULL) {
        return 0;
    }
    memset(build->vm, 0, sizeof(regex_vm_t));
    return 1;
}

void regexVMFree(regex_vm_t *vm) {
    regexVMStringTableFree(vm);
    regexVMClassTableFree(vm);
    regexVMGroupTableFree(vm);
    free(vm);
}

void regexVMBuildDestroy(regex_vm_build_t *build) {
    regexVMFree(build->vm);
    free(build);
}

void regexVMGenerateDeclaration(regex_vm_t *vm, const char *symbol, FILE *fp) {
    fprintf(fp, "extern regex_vm_t *%s;\n", symbol);
}

#define MAX_VM_INSTR_PER_ROW    8

void regexEmitEscapedString(FILE *fp, const char *str) {
    for(; *str != '\0'; str++) {
        if((*str < 32) || (*str > 127)) {
            switch(*str) {
                case '\n': fprintf(fp, "\\n"); break;
                case '\r': fprintf(fp, "\\r"); break;
                case '\t': fprintf(fp, "\\t"); break;
                case '\f': fprintf(fp, "\\f"); break;
                case '\v': fprintf(fp, "\\v"); break;
                case '\a': fprintf(fp, "\\a"); break;
                default:
                    fprintf(fp, "\\x%2.2X\"\"", *str);
                    break;
            }
        } else {
            fputc(*str, fp);
        }
    }
}

void regexVMGenerateDefinition(regex_vm_t *vm, const char *symbol, FILE *fp) {
    int k, j;

    fprintf(fp, "int _%s_program[] = {", symbol);
    for(k = 0; k < vm->size; k++) {
        if(k != 0) {
            fputc(',', fp);
        }
        if(!(k % MAX_VM_INSTR_PER_ROW)) {
            fputs("\n    ", fp);
        } else {
            fputc(' ', fp);
        }
        fprintf(fp, "0x%.2X", vm->program[k]);
    }
    fprintf(fp, "\n};\n\n");

    fprintf(fp, "char *_%s_string_table[] = {\n", symbol);
    for(k = 0; k < vm->string_tbl_size; k++) {
        if(vm->string_table[k] == NULL) {
            fprintf(fp, "    NULL%s\n", (((k + 1) != vm->string_tbl_size) ? "," : ""));
        } else {
            fprintf(fp, "    \"");
            regexEmitEscapedString(fp, vm->string_table[k]);
            fprintf(fp, "\"%s\n", (((k + 1) != vm->string_tbl_size) ? "," : ""));
        }
    }
    fprintf(fp, "};\n\n");

    for(k = 0; k < vm->class_tbl_size; k++) {
        if(vm->class_table[k] == NULL) {
            continue;
        }
        fprintf(fp, "unsigned char _%s_class_entry_%d[] = {", symbol, k);
        for(j = 0; j < 32; j++) {
            if(j != 0) {
                fputc(',', fp);
            }
            if(!(j % 8)) {
                fputs("\n    ", fp);
            } else {
                fputc(' ', fp);
            }
            fprintf(fp, "0x%2.2X", vm->class_table[k][j]);
        }
        fprintf(fp, "\n};\n\n");
    }

    fprintf(fp, "unsigned char *_%s_class_table[] = {\n", symbol);
    for(k = 0; k < vm->class_tbl_size; k++) {
        if(vm->class_table[k] == NULL) {
            fprintf(fp, "    NULL,\n");
        } else {
            fprintf(fp, "    _%s_class_entry_%d,\n", symbol, k);
        }
    }
    fprintf(fp, "};\n\n");

    fprintf(fp, "char *_%s_group_table[] = {\n", symbol);
    for(k = 0; k < vm->group_tbl_size; k++) {
        if(vm->group_table[k] == NULL) {
            fprintf(fp, "    NULL,\n");
        } else {
            fprintf(fp, "    \"");
            regexEmitEscapedString(fp, vm->group_table[k]);
            fprintf(fp, "\",\n");
        }
    }
    fprintf(fp, "};\n\n");

    fprintf(fp, "regex_vm_t _%s = {\n", symbol);
    fprintf(fp, "    .program = _%s_program,\n", symbol);
    fprintf(fp, "    .size = %d,\n", vm->size);
    fprintf(fp, "    .string_table = _%s_string_table,\n", symbol);
    fprintf(fp, "    .string_tbl_size = %d,\n", vm->string_tbl_size);
    fprintf(fp, "    .class_table = _%s_class_table,\n", symbol);
    fprintf(fp, "    .class_tbl_size = %d,\n", vm->class_tbl_size);
    fprintf(fp, "    .group_table = _%s_group_table,\n", symbol);
    fprintf(fp, "    .group_tbl_size = %d\n", vm->group_tbl_size);
    fprintf(fp, "};\n\n");
    fprintf(fp, "regex_vm_t *%s = &_%s;\n", symbol, symbol);
}

void regexVMPrintProgram(FILE *fp, regex_vm_t *vm) {
    int pc;
    int value;
    const char *name;

    for(pc = 0; pc < vm->size; pc++) {
        fprintf(fp, "%3.3d  ", pc);
        switch(vm->program[pc]) {
            case eTokenCharLiteral:
                pc++;
                value = vm->program[pc];
                fprintf(fp, "char (%c:%d)\n", (value < 32 || value > 127) ? '-' : value, value);
                break;
            case eTokenStringLiteral:
                pc++;
                fprintf(fp, "string(\"%s\")\n", vm->string_table[vm->program[pc]]);
                break;
            case eTokenCharClass:
                pc++;
                fprintf(fp, "class([");
                regexPrintCharClassToFP(fp, vm->class_table[vm->program[pc]]);
                fprintf(fp, "])\n");
                break;
            case eTokenCharAny:
                fprintf(fp, "anychar\n");
                break;
            case eTokenSave:
                pc++;
                fprintf(fp, "save %d", vm->program[pc]);
                value = vm->program[pc];
                if((name = regexVMGroupNameFromIndex(vm, value)) != NULL) {
                    fprintf(fp, " [%s]\n", name);
                } else {
                    fprintf(fp, "\n");
                }
                break;
            case eTokenSplit:
                pc++;
                fprintf(fp, "split %d, %d\n", vm->program[pc], vm->program[pc + 1]);
                pc++;
                break;
            case eTokenMatch:
                fprintf(fp, "match\n");
                break;
            case eTokenJmp:
                pc++;
                fprintf(fp, "jmp %d\n", vm->program[pc]);
                break;
            default:
                fprintf(fp, "UNKNOWN [%d]!\n", vm->program[pc]);
                return;        }
    }
}
#endif

const char *regexGetCompileStatusStr(eRegexCompileStatus status) {
    switch(status) {
        case eCompileOk: return "compiled successfully";
        case eCompileCharClassRangeIncomplete: return "char class range is incomplete";
        case eCompileCharClassIncomplete: return "char class definition is incomplete";
        case eCompileEscapeCharIncomplete: return "escape character is incomplete";
        case eCompileMalformedSubExprName: return "subexpression name is malformed";
        case eCompileUnsupportedMeta: return "expression uses an unsupported meta character";
        case eCompileOutOfMem: return "out of memory";
        case eCompileMissingOperand: return "missing operand during postfix transform";
        case eCompileMissingSubexprStart: return "missing subexpr start \"(\"";
        case eCompileInternalError: return "unknown internal error token";
        default: return "Unknown failure";
    }
}

// Return a compiled regex, or an error and position within the pattern
eRegexCompileStatus regexCompile(regex_compile_ctx_t *ctx, const char *pattern) {
    regex_vm_build_t build;
    regex_vm_pc_patch_t *patch_list = NULL;

    memset(ctx, 0, sizeof(regex_compile_ctx_t));
    ctx->pattern = pattern;
    ctx->status = eCompileOk;

    // VM allocated
    if(!regexVMBuildInit(&build)) {
        ctx->status = eCompileOutOfMem;
        return ctx->status;
    }

    // Parse the regex pattern into a sequence of tokens (operators and operands)
    // The output of this stage is a sequence of lexical tokens in infix form

    if((ctx->status = regexTokenizePattern(ctx->pattern, &(ctx->position),
                                           &(build.tokens), &build)) != eCompileOk) {
        regexTokenDestroy(build.tokens, 1);
        regexVMBuildDestroy(&build);
        ctx->vm = NULL;
        return ctx->status;
    }

    // Next, convert the infix form of the regular expression to postfix form,
    // and derive an NFA representation. We accomplish this using the shunting
    // yard algorithm. This is then converted into a VM bytecode sequence, for
    // runtime evaluation.

    if((ctx->status = regexShuntingYard(&(build.tokens))) != eCompileOk) {
        regexWalkAllTokens(build.tokens, VM_PC_UNVISITED);
        regexTokenDestroy(build.tokens, 0);
        regexVMBuildDestroy(&build);
        ctx->vm = NULL;
        return ctx->status;
    }

    regexWalkAllTokens(build.tokens, VM_PC_PENDING_ASSIGNMENT);
    regexWalkAllTokens(build.tokens, VM_PC_UNVISITED);

    if(!regexVMProgramGenerate(&build, &patch_list, build.tokens)) {
        regexWalkAllTokens(build.tokens, VM_PC_PENDING_ASSIGNMENT);
        regexWalkAllTokens(build.tokens, VM_PC_UNVISITED);
        regexTokenDestroy(build.tokens, 0);
        regexVMBuildDestroy(&build);
        regexVMPatchListFree(patch_list);
        ctx->vm = NULL;
        ctx->status = eCompileOutOfMem;
        return ctx->status;
    }

    regexVMPatchJumps(&build, &patch_list);
    regexVMPatchListFree(patch_list);

    // Fixup the lookup tables
    build.vm->string_tbl_size = build.string_tbl_count;
    build.vm->class_tbl_size = build.class_tbl_count;
    build.vm->group_tbl_size = build.groups;

    build.vm->size = build.pc;
    ctx->vm = build.vm;

    return eCompileOk;
}

#if 0
// Each branch of the evaluation follows a thread. The pc keeps track of the
// program counter for the thread. Pos is used to track submatches for string
// literals. subexprs is an array of group start/stop markers.
typedef struct regex_thread_s regex_thread_t;
struct regex_thread_s {
    int pc;
    int pos;
    regex_thread_t *next;
    const char *subexprs[0];
};

typedef struct regex_eval_s regex_eval_t;
struct regex_eval_s {
    const char *sp;
    regex_thread_t *thread;
    regex_thread_t *queue;
    regex_vm_t *vm;
    const char **subexprs;
    regex_thread_t *pool;
};

void regexThreadCopySubexprs(int count, regex_thread_t *dest, regex_thread_t *src) {
    int k;

    for(k = 0; k < count; k++) {
        src->subexprs[k] = dest->subexprs[k];
    }
}

regex_eval_t *regexEvalCreate(regex_vm_t *vm) {
    regex_eval_t *eval;

    if((eval = malloc(sizeof(regex_eval_t))) == NULL) {
        return NULL;
    }
    memset(eval, 0, sizeof(regex_eval_t));

    if((eval->subexprs = malloc(sizeof(char *) * vm->group_tbl_size)) == NULL) {
        free(eval);
        return NULL;
    }
    memset(eval->subexprs, 0, sizeof(char *) * vm->group_tbl_size);

    eval->vm = vm;

    return eval;
}

int regexThreadCreate(regex_eval_t *eval, regex_thread_t *parent, int pc) {
    regex_thread_t *thread, *walk;

    if(eval->pool != NULL) {
        thread = eval->pool;
        eval->pool = thread->next;
        thread->next = NULL;
    } else {
        if((thread = malloc(sizeof(regex_thread_t) + (eval->vm->group_tbl_size * sizeof(char *)))) == NULL) {
            return 0;
        }
    }
    memset(thread, 0, sizeof(regex_thread_t) + (eval->vm->group_tbl_size * sizeof(char *)));
    if(parent != NULL) {
        regexThreadCopySubexprs(eval->vm->group_tbl_size, thread, parent);
    }
    thread->pc = pc;
    thread->pos = -1;

    thread->next = eval->queue;
    eval->queue = thread;

    return 1;
}

void regexThreadFree(regex_eval_t *eval, regex_thread_t *thread) {
    regex_thread_t *next;

    if(thread == NULL) {
        return;
    }
    if(eval != NULL) {
        thread->next = eval->pool;
        eval->pool = thread;
        return;
    }
    for(; thread != NULL; thread = next) {
        next = thread->next;
        free(thread);
    }
}

void regexEvalFree(regex_eval_t *eval) {
    regexThreadFree(NULL, eval->thread);
    regexThreadFree(NULL, eval->queue);
    regexThreadFree(NULL, eval->pool);
    if(eval->subexprs != NULL) {
        free(eval->subexprs);
        eval->subexprs = NULL;
    }
    free(eval);
}

int regexVMNoTextAdvance(int instr) {
    switch(instr) {
        case eTokenMatch:
        case eTokenJmp:
        case eTokenSplit:
        case eTokenSave:
            return 1;
        default:
            return 0;
    }
}

typedef enum {
    eEvalInternalError = -2,
    eEvalOutOfMem = -1,
    eEvalNoMatch = 0,
    eEvalMatch = 1,
    eEvalContinue = 2
} eRegexEvalResult;

eRegexEvalResult regexThreadProcess(regex_eval_t *eval, regex_thread_t *thread) {
    const unsigned char *bitmap;
    const char *str;
    int value, pc_a, pc_b;

    while(regexVMNoTextAdvance(eval->vm->program[thread->pc])) {
        switch(eval->vm->program[thread->pc]) {
            case eTokenSave:
                thread->pc++;
                value = eval->vm->program[thread->pc];
                printf("save %d\n", value);
                if(value % 2) {
                    // Group end
                    thread->subexprs[value] = eval->sp;
                } else {
                    // Group start
                    if(thread->subexprs[value] == NULL) {
                        thread->subexprs[value] = eval->sp;
                    }
                }
                thread->pc++;
                break;
            case eTokenSplit:
                printf("split\n");
                pc_a = eval->vm->program[thread->pc + 1];
                pc_b = eval->vm->program[thread->pc + 2];
                if(!regexThreadCreate(eval, thread, pc_a)) {
                    return eEvalOutOfMem;
                }
                thread->pc = pc_b;
                break;
            case eTokenMatch:
                printf("match\n");
                if(*eval->sp == '\0') {
                    return eEvalMatch;
                }
                return eEvalNoMatch;
            case eTokenJmp:
                printf("jmp\n");
                thread->pc = eval->vm->program[thread->pc + 1];
                break;
            default:
                return eEvalInternalError;
        }
    }
    switch(eval->vm->program[thread->pc]) {
        case eTokenCharLiteral:
            printf("char\n");
            thread->pc++;
            if(eval->vm->program[thread->pc] != *(eval->sp)) {
                return eEvalNoMatch;
            } else {
                thread->pc++;
            }
            return eEvalMatch;
        case eTokenStringLiteral:
            if((str = regexVMStringTableEntryGet(eval->vm, eval->vm->program[thread->pc + 1])) == NULL) {
                return eEvalInternalError;
            }
            if(thread->pos == -1) {
                thread->pos = 0;
            }
            printf("str[%d]\n", thread->pos);
            if(str[thread->pos] == *(eval->sp)) {
                thread->pos++;
                if(str[thread->pos] == '\0') {
                    thread->pc += 2;
                    thread->pos = -1;
                    return eEvalMatch;
                }
                return eEvalContinue;
            } else {
                return eEvalNoMatch;
            }
            break;
        case eTokenCharClass:
            printf("class\n");
            thread->pc++;
            if((bitmap = regexVMClassTableEntryGet(eval->vm, eval->vm->program[thread->pc])) == NULL) {
                return eEvalInternalError;
            }
            if(!mapCheck(bitmap, *(eval->sp))) {
                return eEvalNoMatch;
            } else {
                thread->pc++;
            }
            return eEvalMatch;
        case eTokenCharAny:
            printf("any\n");
            thread->pc++;
            return eEvalMatch;
        default:
            return eEvalInternalError;
    }
}

int regexMatch(regex_vm_t *vm, const char *text, const char ***subexprs) {
    regex_eval_t *eval;
    regex_thread_t *thread;

    if((eval = regexEvalCreate(vm)) == NULL) {
        return 0;
    }

    if(!regexThreadCreate(eval, NULL, 0)) {
        regexEvalFree(eval);
        return 0;
    }

    for(eval->sp = text; (*eval->sp != '\0') && (eval->queue != NULL);) {
        eval->thread = eval->queue;
        eval->queue = NULL;
        printf("[%c:%d]\n", *eval->sp, *eval->sp);
        for(thread = eval->thread; thread != NULL && thread->pc < vm->size; thread = eval->thread) {
            eval->thread = thread->next;
            thread->next = NULL;
            switch(regexThreadProcess(eval, thread)) {
                default:
                case eEvalInternalError:
                case eEvalOutOfMem:
                    printf("DEATH!\n");
                    return -1;
                case eEvalMatch:
                case eEvalContinue:
                    printf("good\n");
                    if((eval->vm->program[thread->pc] == eTokenMatch) && (*eval->sp == '\0')) {
                        goto FoundMatch;
                    }
                    thread->next = eval->queue;
                    eval->queue = thread;
                    break;
                case eEvalNoMatch:
                    printf("nope\n");
                    regexThreadFree(eval, thread);
                    continue;
            }
        }
        eval->sp++;
    }

    regexEvalFree(eval);
    return 0;

FoundMatch:

    // TODO - copy subexpression markers
    regexEvalFree(eval);
    return 1;
}

#endif

int main(int argc, char **argv) {
    regex_compile_ctx_t result;

    if(argc > 1) {
        if(regexCompile(&result, argv[1]) != eCompileOk) {
            printf("Compile failed: %s", regexGetCompileStatusStr(result.status));
            return 1;
        }
        if(argc > 2) {
            regexVMPrintProgram(stdout, result.vm);
            /*
            if(regexMatch(result.vm, argv[2], NULL)) {
                printf("Match OK\n");
            } else {
                printf("Match FAIL\n");
            }
            */
            regexVMGenerateDeclaration(result.vm, "myparser", stdout);
            regexVMGenerateDefinition(result.vm, "myparser", stdout);
        }
    }

    return 0;
}


#endif // defined(MOJO_REGEX_IMPLEMENTATION)
#endif // _MOJO_REGEX_HEADER_

