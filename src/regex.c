#include <stdio.h>
#include <stdlib.h>
#include <string.h>


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

typedef struct regex_subexpr_name_s regex_subexpr_name_t;
struct regex_subexpr_name_s {
    int index;
    regex_subexpr_name_t *next;
    char name[0];
};

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


void regexTokenDestroy(regex_token_t *token);
void regexSubexprLookupFree(regex_subexpr_name_t *list);
void regexTokenChainPrint(regex_token_t *tokens, regex_subexpr_name_t *subexpr);
void regexTokenPrint(regex_token_t *token, regex_subexpr_name_t *subexpr, int newlines);
int regexTokenCreate(regex_token_t **list, eRegexToken tokenType, int c, char *str);
void regexPrintCharClass(unsigned char *bitmap);
void regexPrintCharClassToFP(FILE *fp, unsigned char *bitmap);

eRegexCompileStatus regexTokenizePattern(const char *pattern,
                                         int *pos,
                                         regex_token_t **tokens,
                                         regex_subexpr_name_t **subexpr_list);




typedef struct regex_compile_ctx_s regex_compile_ctx_t;
struct regex_compile_ctx_s {
    eRegexCompileStatus status;
    const char *pattern;
    int position;
    regex_token_t *tokens;
//    regex_state_t *state_tree;
    regex_subexpr_name_t *subexpr_list;
};


void regexCompileCtxInit(regex_compile_ctx_t *ctx, const char *pattern);
void regexCompileCtxCleanup(regex_compile_ctx_t *ctx);
const char *regexGetCompileStatusStr(eRegexCompileStatus status);

eRegexCompileStatus regexCompile(regex_compile_ctx_t *ctx);

typedef struct regex_vm_s regex_vm_t;
struct regex_vm_s {
    int *program;
    char **string_table;
    int string_tbl_size;
    //int string_tbl_count;
    unsigned char **class_table;
    int class_tbl_size;
    //int class_tbl_count;
    char **group_table;
    int group_tbl_size;
    int size;
    //int pc;
};

typedef struct regex_vm_build_s regex_vm_build_t;
struct regex_vm_build_s {
    regex_vm_t *vm;
    int string_tbl_count;
    int class_tbl_count;
    int group_tbl_count;
    int pc;
};

regex_vm_t *regexVMCreate(regex_token_t *tokens);
void regexVMDestroy(regex_vm_build_t *vm);

void regexVMGenerateDeclaration(regex_vm_t *vm, const char *symbol, FILE *fp);
void regexVMGenerateSource(regex_vm_t *vm, const char *symbol, FILE *fp);
void regexVMPrintProgram(FILE *fp, regex_vm_t *vm);


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

// Note: character classes are stored as 32 byte bitmaps (unsigned char bitmap[32];)

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
    int last = 0;
    int next;

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

// Parses the subexpression name pointed to by pattern, creates a
// subexpression name lookup entry, and adds it to the subexpression name list.
// Returns 1 on success, 0 on out of memory, and -1 if the name is malformed.
int regexSubexprLookupEntryCreate(regex_subexpr_name_t **list, const char **pattern, int index) {
    regex_subexpr_name_t *entry;
    char *ptr;
    int len;

    for(len = 0; (*pattern)[len] != '>' && (*pattern)[len] != '\0'; len++) {
        if(!regexIsAlnum((*pattern)[len])) {
            return -1;
        }
    }
    if(len == 0) {
        return -1;
    }

    if((entry = malloc(sizeof(regex_subexpr_name_t) + len + 1)) == NULL) {
        return 0;
    }
    memset(entry, 0, sizeof(regex_subexpr_name_t));

    ptr = entry->name;
    for(; len; len--) {
        *ptr = **pattern;
        (*pattern)++;
        ptr++;
    }
    *ptr = '\0';

    // Discard the trailing '>' delimiter
    (*pattern)++;

    entry->index = index;
    entry->next = *list;
    *list = entry;

    return 1;
}

void regexSubexprLookupFree(regex_subexpr_name_t *list) {
    regex_subexpr_name_t *next;

    for(; list != NULL; list = next) {
        next = list->next;
        free(list);
    }
}

int regexSubexprLookupIndex(regex_subexpr_name_t *list, const char *name) {
    for(; list != NULL; list = list->next) {
        if(!strcasecmp(list->name, name)) {
            return list->index;
        }
    }
    return 0;
}

const char *regexSubexprLookupName(regex_subexpr_name_t *list, int index) {
    for(; list != NULL; list = list->next) {
        if(list->index == index) {
            return list->name;
        }
    }
    return NULL;
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

void regexTokenPrint(regex_token_t *token, regex_subexpr_name_t *subexpr, int newlines) {
    const char *str = NULL;

#define HAS_NEWLINE()   (newlines ? "\n" : "")

    switch(token->tokenType) {
        case eTokenCharLiteral:
            printf("CHAR(%c)%s", token->c, HAS_NEWLINE());
            break;
        case eTokenCharClass:
            printf("CLASS[");
            regexPrintCharClass((unsigned char *)(token->str));
            printf("]%s", HAS_NEWLINE());
            break;
        case eTokenStringLiteral:
            printf("STRING(\"%s\")%s", token->str, HAS_NEWLINE());
            break;
        case eTokenCharAny:
            printf("ANY%s", HAS_NEWLINE());
            break;
        case eTokenConcatenation:
            printf("CONCAT%s", HAS_NEWLINE());
            break;
        case eTokenAlternative:
            printf("ALTERNATIVE%s", HAS_NEWLINE());
            break;
        case eTokenZeroOrOne:
            printf("ZERO_OR_ONE%s", HAS_NEWLINE());
            break;
        case eTokenZeroOrMany:
            printf("ZERO_OR_MANY%s", HAS_NEWLINE());
            break;
        case eTokenOneOrMany:
            printf("ONE_OR_MANY%s", HAS_NEWLINE());
            break;
        case eTokenSubExprStart:
            if(subexpr != NULL) {
                str = regexSubexprLookupName(subexpr, token->c);
            }
            if(str != NULL) {
                printf("SUBEXPR #%d <%s>%s", token->c, str, HAS_NEWLINE());
            } else {
                printf("SUBEXPR #%d%s", token->c, HAS_NEWLINE());
            }
            break;
        case eTokenSubExprEnd:
            printf("SUBEXPR #%d END%s", token->c, HAS_NEWLINE());
            break;
        case eTokenMatch:
            printf("MATCH!%s", HAS_NEWLINE());
            break;
        default:
            printf("UNKNOWN! <%d>%s", token->tokenType, HAS_NEWLINE());
            break;
    }
#undef HAS_NEWLINE
}

void regexTokenChainPrint(regex_token_t *tokens, regex_subexpr_name_t *subexpr) {
    for(; tokens != NULL; tokens = tokens->next) {
        regexTokenPrint(tokens, subexpr, 1);
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

void regexTokenDestroy(regex_token_t *token) {
    regex_token_t *next;

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
}

/////////////////////////////////////////////////////////////////////////////

#define SET_RESULT(stat)  result = stat; goto compileFailure;

eRegexCompileStatus regexTokenizePattern(const char *pattern,
                                         int *pos,
                                         regex_token_t **tokens,
                                         regex_subexpr_name_t **subexpr_list) {
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
                                SET_RESULT(eCompileMalformedSubExprName);
                            }
                            if((response = regexSubexprLookupEntryCreate(subexpr_list, &pattern, subexpr)) != 1) {
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

    if(result != eCompileOk) {
        if(*tokens != NULL) {
            regexTokenDestroy(*tokens);
            *tokens = NULL;
        }
        if(*subexpr_list != NULL) {
            regexSubexprLookupFree(*subexpr_list);
            *subexpr_list = NULL;
        }
    }

    return result;
}

typedef enum {
    ePriorityNone,
    ePriorityLow,
    ePriorityMedium,
    ePriorityHigh
} eRegexTokenPriority;

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

int regexTokenStackSize(regex_token_t *stack) {
    int count = 0;

    for(; stack != NULL; stack = stack->next, count++);
    return count;
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
    //printf("Priority s[%d] t[%d]\n", stackPriority, regexGetTokenTypePriority(tokenType));
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
            regexTokenDestroy(fragment->token);
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

    regexTokenDestroy(token);
    if(((e2 = regexFragmentStackPop(stack)) == NULL) ||
       ((e1 = regexFragmentStackPop(stack)) == NULL)) {
        return 0;
    }
    regexPtrlistPatch(e1->ptrlist, e2->token);
    if((fragment = regexFragmentCreate(e1->token, e2->ptrlist)) == NULL) {
        return 0;
    }
    regexFragmentStackPush(stack, fragment);
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

int regexFragmentStackSize(regex_fragment_t *fragments) {
    int depth = 0;

    for(; fragments != NULL; depth++, fragments = fragments->next);
    return depth;
}

int regexHasSufficientOperands(regex_fragment_t *fragments, int arity) {
    for(; ((fragments != NULL) && (arity)); arity--, fragments = fragments->next);
    return arity == 0;
}

void regexPrintOperatorStack(regex_token_t *operators) {
    printf("Operator stack: ");
    for(;operators != NULL; operators = operators->next) {
        switch(operators->tokenType) {
            case eTokenAlternative: printf("| "); break;
            case eTokenConcatenation: printf(". "); break;
            case eTokenZeroOrOne: printf("? "); break;
            case eTokenZeroOrMany: printf("* "); break;
            case eTokenOneOrMany: printf("+ "); break;
            default: printf("%% "); break;
        }
    }
    printf("\n");
}

eRegexCompileStatus regexOperatorApply(regex_token_t **operators, eRegexOpApply apply, eRegexToken tokenType, regex_fragment_t **operands) {
    regex_token_t *operator;

    while((apply == OP_ALL && *operators != NULL) ||
          (apply == OP_GREATER_OR_EQUAL && regexStackTypeGreaterOrEqualToToken(*operators, tokenType))) {
        operator = regexTokenStackPop(operators);
        if(!regexHasSufficientOperands(*operands, regexGetOperatorArity(operator))) {
            printf("Expected %d operands\n", regexGetOperatorArity(operator));
            return eCompileMissingOperand;
        }
        switch(operator->tokenType) {
            case eTokenZeroOrOne:
                printf("Apply ?\n");
                if(!regexOperatorZeroOrOneCreate(operands, operator)) {
                    return eCompileOutOfMem;
                }
                break;

            case eTokenZeroOrMany:
                printf("Apply *\n");
                if(!regexOperatorZeroOrMoreCreate(operands, operator)) {
                    return eCompileOutOfMem;
                }
                break;

            case eTokenOneOrMany:
                printf("Apply +\n");
                if(!regexOperatorOneOrMoreCreate(operands, operator)) {
                    return eCompileOutOfMem;
                }
                break;

            case eTokenConcatenation:
                printf("Apply #\n");
                if(!regexOperatorConcatenationCreate(operands, operator)) {
                    return eCompileOutOfMem;
                }
                break;

            case eTokenAlternative:
                printf("Apply |\n");
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

int regexGroupFromSubexpression(int subexpr) {
    return (subexpr - 1) * 2;
}

#define SET_YARD_RESULT(res)    status = res; goto ShuntingYardFailure;

void regexNFANodePrint(regex_token_t *state);

eRegexCompileStatus regexShuntingYardFragment(regex_token_t **tokens, regex_fragment_t **root_stack, int sub_expression) {
    regex_token_t *token = NULL, *operators = NULL;
    regex_fragment_t *operands = NULL, *subexpr;
    eRegexCompileStatus status = eCompileOk;
    int group_num;

    if(sub_expression >= 0) {
        operands = *root_stack;
        if(!regexTokenCreate(&token, eTokenConcatenation, 0, NULL)) {
            return eCompileOutOfMem;
        }
        regexTokenStackPush(&operators, token);
    }

    while((token = regexTokenStackPop(tokens)) != NULL) {
        regexTokenPrint(token, NULL, 1);
        switch(token->tokenType) {
            default:
                printf("ERROR! Unrecognized token type [%d]\n", token->tokenType);
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
                token->group = regexGroupFromSubexpression(token->group);
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
                token->group = regexGroupFromSubexpression(sub_expression) + 1;
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
    operands = NULL;

    return status;
}

void regexNFANodeClear(regex_token_t *token, int value) {
    if(token != NULL) {
        if(token->pc == value) {
            return;
        }
        token->pc = value;
        if(token->out_a != NULL) {
            regexNFANodeClear(token->out_a, value);
        }
        if(token->out_b != NULL) {
            regexNFANodeClear(token->out_b, value);
        }
    }
}

void regexNFANodePrint(regex_token_t *token) {
    if(token != NULL && token->pc == -2) {
        return;
    }
    printf("%p  ", token);
    if(token == NULL) {
        printf("NULL\n");
        return;
    }
    if(token->pc == -1) {
        token->pc = -2;
    }
    switch(token->tokenType) {
        case eTokenCharLiteral:
            printf("char(%c:%d)\n", (token->c < 32 || token->c > 127) ? '-' : token->c, token->c);
            regexNFANodePrint(token->out_a);
            break;
        case eTokenStringLiteral:
            printf("string(\"%s\")\n", token->str);
            regexNFANodePrint(token->out_a);
            break;
        case eTokenCharClass:
            printf("class(");
            regexPrintCharClass(token->bitmap);
            printf(")\n");
            regexNFANodePrint(token->out_a);
            break;
        case eTokenCharAny:
            printf("anychar\n");
            regexNFANodePrint(token->out_a);
            break;
        case eTokenSave:
            printf("save(%d)\n", token->group);
            regexNFANodePrint(token->out_a);
            break;
        case eTokenSplit:
            printf("split %p, %p\n", token->out_a, token->out_b);
            regexNFANodePrint(token->out_a);
            regexNFANodePrint(token->out_b);
            break;
        case eTokenMatch:
            printf("match\n");
            return;
        case eTokenJmp:
            printf("jmp %p\n", token->out_a);
            return;
        default:
            printf("UNKNOWN [%d]!\n", token->tokenType);
            return;
    }
}


eRegexCompileStatus regexShuntingYard(regex_token_t **tokens /*, regex_state_t **nfa_stack*/) {
    regex_fragment_t *stack = NULL;
    eRegexCompileStatus status = eCompileOk;
    regex_vm_t *vm;

    printf("\n== Shunting yard ==============================\n\n");

#if 1
    status = regexShuntingYardFragment(tokens, &stack, -1);
    if(status != eCompileOk) {
        return status;
    }

    printf("\n== Generating VM program ======================\n\n");

    if((vm = regexVMCreate(stack->token)) == NULL) {
        return eCompileOutOfMem;
    }
    printf("Printing program...\n");
    regexVMPrintProgram(stdout, vm);
    //regexVMDestroy(vm);

    printf("-------------------------------------------------\n");

#if 0
    regexNFANodeClear(stack->token, -3);
    regexNFANodeClear(stack->token, -1);
    printf("--------------------\n");
    regexNFANodePrint(stack->token);
    printf("--------------------\n");
#endif
#endif

    return status;
}

void regexVMStringTableFree(regex_vm_build_t *build) {
    int k;
    if(build->vm->string_table != NULL) {
        for(k = 0; k < build->vm->string_tbl_size; k++) {
            if(build->vm->string_table[k] != NULL) {
                free(build->vm->string_table[k]);
            }
        }
        free(build->vm->string_table);
    }
    build->vm->string_table = NULL;
    build->vm->string_tbl_size = 0;
    build->string_tbl_count = 0;
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

void regexVMClassTableFree(regex_vm_build_t *build) {
    int k;
    if(build->vm->class_table != NULL) {
        for(k = 0; k < build->class_tbl_count; k++) {
            if(build->vm->class_table[k] != NULL) {
                free(build->vm->class_table[k]);
            }
        }
        free(build->vm->class_table);
    }
    build->vm->class_table = NULL;
    build->class_tbl_count = 0;
    build->vm->class_tbl_size = 0;
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

void regexVMGroupTableFree(regex_vm_build_t *build) {
    int k;
    if(build->vm->group_table != NULL) {
        for(k = 0; k < build->vm->group_tbl_size; k++) {
            if(build->vm->group_table[k] != NULL) {
                free(build->vm->group_table[k]);
            }
        }
        free(build->vm->group_table);
    }
    build->vm->group_table = NULL;
    build->vm->group_tbl_size = 0;
}

int regexVMGroupTableEntryAdd(regex_vm_t *vm, const char *group, int index) {
    int add = DEF_VM_STRTBL_INC;

    if(vm->group_tbl_size <= index) {
        while((vm->group_tbl_size + add) <= index) {
            add += DEF_VM_STRTBL_INC;
        }
        if((vm->group_table = realloc(vm->group_table, (vm->group_tbl_size + add) * sizeof(char *))) == NULL) {
            return -1;
        }
        memset(vm->group_table + vm->group_tbl_size, 0, add * sizeof(char *));
        vm->group_tbl_size += add;
    }

    if((vm->group_table[index] = strdup(group)) == NULL) {
        return -1;
    }
    vm->group_tbl_size += add;
    return index;
}

/////////////////////////////////////////////////////////////////////////////

int regexVMProgramGrow(regex_vm_build_t *build, int size) {
    if((build->vm->size - build->pc) > size) {
        return 1;
    }
    if((build->vm->program = realloc(build->vm->program, (build->vm->size + DEF_VM_SIZE_INC) * sizeof(int))) == NULL) {
        return 0;
    }
    memset(build->vm->program + build->vm->size, 0, DEF_VM_SIZE_INC * sizeof(int));
    build->vm->size += DEF_VM_SIZE_INC;
    return 1;
}

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
    if(!regexVMProgramGrow(build, size)) {
        return 0;
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

int regexVMInstrCharLiteralCreate(regex_vm_build_t *build, int c) {
    return regexVMProgramAdd(build, eTokenCharLiteral, c, 0);
}

int regexVMInstrStringLiteralCreate(regex_vm_build_t *build, const char *str) {
    int index;

    if((index = regexVMStringTableEntryAdd(build, str)) == -1) {
        return 0;
    }

    return regexVMProgramAdd(build, eTokenStringLiteral, index, 0);
}

int regexVMInstrClassLiteralCreate(regex_vm_build_t *build, const unsigned char *bitmap) {
    int index;

    if((index = regexVMClassTableEntryAdd(build, bitmap)) == -1) {
        return 0;
    }

    return regexVMProgramAdd(build, eTokenCharClass, index, 0);
}

int regexVMInstrCharAnyLiteralCreate(regex_vm_build_t *build) {
    return regexVMProgramAdd(build, eTokenCharAny, 0, 0);
}

int regexVMInstrSplitCreate(regex_vm_build_t *build, int idx_a, int idx_b) {
    return regexVMProgramAdd(build, eTokenSplit, idx_a, idx_b);
}

int regexVMInstrSaveCreate(regex_vm_build_t *build, int group) {
    return regexVMProgramAdd(build, eTokenSave, group, 0);
}

int regexVMInstrMatchCreate(regex_vm_build_t *build) {
    return regexVMProgramAdd(build, eTokenMatch, 0, 0);
}

int regexVMInstrJmpCreate(regex_vm_build_t *build, int idx) {
    return regexVMProgramAdd(build, eTokenJmp, idx, 0);
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

int regexVMProgramGenerate(regex_vm_build_t *build, regex_vm_pc_patch_t **patch_list, regex_token_t *token) {
    int idx_a, idx_b;

    if(token == NULL) {
        return 1;
    }
    if(token->pc != VM_PC_UNVISITED) {
        return 1;
    }
    switch(token->tokenType) {
        case eTokenCharLiteral:
            token->pc = build->pc;
            if(!regexVMInstrCharLiteralCreate(build, token->c)) {
                return 0;
            }
            return regexVMProgramGenerate(build, patch_list, token->out_a);
        case eTokenStringLiteral:
            token->pc = build->pc;
            if(!regexVMInstrStringLiteralCreate(build, token->str)) {
                return 0;
            }
            return regexVMProgramGenerate(build, patch_list, token->out_a);
        case eTokenCharClass:
            token->pc = build->pc;
            if(!regexVMInstrClassLiteralCreate(build, token->bitmap)) {
                return 0;
            }
            return regexVMProgramGenerate(build, patch_list, token->out_a);
        case eTokenCharAny:
            token->pc = build->pc;
            if(!regexVMInstrCharAnyLiteralCreate(build)) {
                return 0;
            }
            return regexVMProgramGenerate(build, patch_list, token->out_a);
        case eTokenSave:
            token->pc = build->pc;
            if(!regexVMInstrSaveCreate(build, token->group)) {
                return 0;
            }
            return regexVMProgramGenerate(build, patch_list, token->out_a);
        case eTokenSplit:
            token->pc = build->pc;
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
            if(!regexVMInstrSplitCreate(build, idx_a, idx_b)) {
                return 0;
            }
            if(!regexVMProgramGenerate(build, patch_list, token->out_a)) {
                return 0;
            }
            return regexVMProgramGenerate(build, patch_list, token->out_b);
        case eTokenMatch:
            token->pc = build->pc;
            if(!regexVMInstrMatchCreate(build)) {
                return 0;
            }
            return 1;
        case eTokenJmp:
            token->pc = build->pc;
            if((idx_a = token->out_a->pc) == VM_PC_UNVISITED) {
                if(!regexAddPCPatchEntry(patch_list, token->out_a, build->pc + 1)) {
                    return 0;
                }
            }
            if(!regexVMInstrJmpCreate(build, idx_a)) {
                return 0;
            }
            return regexVMProgramGenerate(build, patch_list, token->out_b);
        default:
            printf("UNKNOWN [%d]!\n", token->tokenType);
            return 0;
    }
}

void regexVMPatchJumps(regex_vm_build_t *build, regex_vm_pc_patch_t **patch_list) {
    regex_vm_pc_patch_t *patch, *next;

    for(patch = *patch_list; patch != NULL; patch = next) {
        next = patch->next;
        printf("Patch %d (of %d) to %d\n", patch->pc, build->pc, patch->token->pc);
        build->vm->program[patch->pc] = patch->token->pc;
        free(patch);
    }
    *patch_list = NULL;
}

regex_vm_t *regexVMCreate(regex_token_t *tokens) {
    regex_vm_t *vm;
    regex_vm_build_t build;
    regex_vm_pc_patch_t *patch_list = NULL;

    memset(&build, 0, sizeof(regex_vm_build_t));
    if((vm = malloc(sizeof(regex_vm_t))) == NULL) {
        return NULL;
    }
    memset(vm, 0, sizeof(regex_vm_t));
    build.vm = vm;

    regexWalkAllTokens(tokens, VM_PC_PENDING_ASSIGNMENT);
    regexWalkAllTokens(tokens, VM_PC_UNVISITED);

    if(!regexVMProgramGenerate(&build, &patch_list, tokens)) {
        // TODO - cleanup
        return NULL;
    }

    printf("Patching jumps...\n");

    regexVMPatchJumps(&build, &patch_list);

    vm->size = build.pc;
    
    return vm;
}

void regexVMDestroy(regex_vm_build_t *build) {
    regexVMStringTableFree(build);
    regexVMClassTableFree(build);
    regexVMGroupTableFree(build);
    free(build);
}

void regexVMGenerateDeclaration(regex_vm_t *vm, const char *symbol, FILE *fp) {
}

void regexVMGenerateSource(regex_vm_t *vm, const char *symbol, FILE *fp) {
}

void regexVMPrintProgram(FILE *fp, regex_vm_t *vm) {
    int pc;
    int value;

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
                fprintf(fp, "save %d\n", vm->program[pc]);
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
                printf("UNKNOWN [%d]!\n", vm->program[pc]);
                return;        }
    }
}

void regexCompileCtxInit(regex_compile_ctx_t *ctx, const char *pattern) {
    memset(ctx, 0, sizeof(regex_compile_ctx_t));
    ctx->pattern = pattern;
    ctx->status = eCompileOk;
}

void regexCompileCtxCleanup(regex_compile_ctx_t *ctx) {
    if(ctx->tokens != NULL) {
        regexTokenDestroy(ctx->tokens);
        ctx->tokens = NULL;
    }
    if(ctx->subexpr_list != NULL) {
        regexSubexprLookupFree(ctx->subexpr_list);
        ctx->subexpr_list = NULL;
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
        case eCompileMissingOperand: return "missing operand during postfix transform";
        case eCompileMissingSubexprStart: return "missing subexpr start \"(\"";
        case eCompileInternalError: return "unknown internal error token";
        default: return "Unknown failure";
    }
}

// Return a compiled regex, or an error and position within the pattern
eRegexCompileStatus regexCompile(regex_compile_ctx_t *ctx) {
    // Parse the regex pattern into a sequence of tokens (operators and operands)
    // The output of this stage is a sequence of lexical tokens in infix form
    if((ctx->status = regexTokenizePattern(ctx->pattern, &(ctx->position),
                                           &(ctx->tokens), &(ctx->subexpr_list))) != eCompileOk) {
        regexCompileCtxCleanup(ctx);
        return ctx->status;
    }

    regexTokenChainPrint(ctx->tokens, ctx->subexpr_list);

    // Next, convert the infix form of the regular expression to postfix form,
    // and derive an NFA representation. We accomplish this using the shunting
    // yard algorithm. This is then converted into a VM bytecode sequence, for
    // runtime evaluation.

    if((ctx->status = regexShuntingYard(&(ctx->tokens))) != eCompileOk) {
        regexCompileCtxCleanup(ctx);
        return ctx->status;
    }

    return ctx->status;
}

int main(int argc, char **argv) {
    regex_compile_ctx_t result;

    regexCompileCtxInit(&result, argv[1]);
    if(argc > 1) {
        if(regexCompile(&result) != eCompileOk) {
            printf("Compile failed: %s", regexGetCompileStatusStr(result.status));
        }
    }

    regexCompileCtxCleanup(&result);

    return 0;
}
