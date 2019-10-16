#ifndef _MOJOCORE_REGEX_HEADER_
#define _MOJOCORE_REGEX_HEADER_

/*
    Task list:
        - generate instruction set
        - build VM implementation
        - validate regex evaluator
        - clean up and refactor

        - compiler
        - parser
        - code generator (compiler -> c)

The regular expression:

    a(bcd(?iefg[hijk]foo)?)[0-9]+cat

Should be converted into the conceptual regex VM program:

1       0  char a
(       1  save 2
bcd     2  str bcd
(       3  save 4
?       4  split 5, 9
efg     5  str efg
[hijk]  6  class [hijk]
foo     7  str foo
)       8  save 5
)       9  save 3
[0-9]   10 class [0-9]
+       11 split 10, 12
cat     12 str cat
        13 match

if current is
    c,s,cl,.,ss
and previous was
    c,s,cl,.,se

For the expression:

    a(bcd(?iefg[hijk]foo)?)[0-9]+cat

The base token stream is (infix):

    c(a) ( s(bcd) ( s(efg) cl(hijk) s(foo) ) ? ) cl(0-9) + s(cat)

c(a) -> save(2) -> split -> s(bcd) -> save(4) -> s(efg) -> cl(hijk) -> s(foo) -> save(5) -> save(3) -> cl(0-9) -> split -> s(cat)
                     |                                                                                    ^          |
                     +----------------------------------------------------------------------------------->|<---------+

With implied concatenation (#) operators:

    c(a) # ( s(bcd) # ( s(efg) # cl(hijk) # s(foo) ) ? ) # cl(0-9) + # s(cat)

    1   c(a)        c(a)
    2   #           c(a)                                                                            #
    3   (           c(a)                                                                            # (
    4   s(bcd)      c(a) s(bcd)                                                                     # (
    5   #           c(a) s(bcd)                                                                     # ( #
    6   (           c(a) s(bcd)                                                                     # ( # (
    7   s(efg)      c(a) s(bcd) s(efg)                                                              # ( # (
    8   #           c(a) s(bcd) s(efg)                                                              # ( # ( #
    9   cl(hijk)    c(a) s(bcd) s(efg) cl(hijk)                                                     # ( # ( #
    10  #           c(a) s(bcd) s(efg) cl(hijk)                                                     # ( # ( # #
    11  s(foo)      c(a) s(bcd) s(efg) cl(hijk) s(foo)                                              # ( # ( # #
    12  )           c(a) s(bcd) g([s(efg) [cl(hijk) s(foo)]])                                       # ( #
    13  ?           c(a) s(bcd) g([s(efg) [cl(hijk) s(foo)]])                                       # ( # ?
    14  )           c(a) g([s(bcd) opt(g([s(efg) [cl(hijk) s(foo)]]))])                             #
    15  #           c(a) g([s(bcd) opt(g([s(efg) [cl(hijk) s(foo)]]))])                             # #
    16  cl(0-9)     c(a) g([s(bcd) opt(g([s(efg) [cl(hijk) s(foo)]]))]) cl(0-9)                     # #
    17  +           c(a) g([s(bcd) opt(g([s(efg) [cl(hijk) s(foo)]]))]) cl(0-9)                     # # +
    18  #           c(a) g([s(bcd) opt(g([s(efg) [cl(hijk) s(foo)]]))]) many(cl(0-9))               # # #
    19  s(cat)      c(a) g([s(bcd) opt(g([s(efg) [cl(hijk) s(foo)]]))]) s(cat) many(cl(0-9))        # # #
    20  <DONE>      [c(a) [g([s(bcd) opt(g([s(efg) [cl(hijk) s(foo)]]))]) [s(cat) many(cl(0-9))]]]

    1   c(a)        char a
    2   g(1)        save 2
    3   s(bcd)      string "bcd"
    4   opt         split 5, 11
    5   g(2)        save 4
    6   s(efg)      string "efg"
    7   cl(hijk)    class "hijk"
    8   s(foo)      string "foo"
    9   <g(2)>      save 5
    10  <g(1)>      save 3
    11  s(cat)      string "cat"
    12  many        class "0-9"
    13  cl[0-9]     split 12, 14
    14  match

For the expression:

    a(?P<foolio>bcd(?iefg*[hijk]foo)?)[0-9]+cat

The base token stream is (infix):

    c(a) ( s(bcd) ( s(efg) * cl(hijk) s(foo) ) ? ) cl(0-9) + s(cat)

With implied concatenation (#) operators:

    c(a) # ( s(bcd) # ( s(efg) * # cl(hijk) # s(foo) ) ? ) # cl(0-9) + # s(cat)

    1   c(a)        c(a)
    2   #           c(a)                                                                                    #
    3   (           c(a)                                                                                    # (
    4   s(bcd)      c(a) s(bcd)                                                                             # (
    5   #           c(a) s(bcd)                                                                             # ( #
    6   (           c(a) s(bcd)                                                                             # ( # (
    7   s(efg)      c(a) s(bcd) s(efg)                                                                      # ( # (
    8   *           c(a) s(bcd) s(efg)                                                                      # ( # ( *
    9   #           c(a) s(bcd) kleene(s(efg))                                                              # ( # ( #
    10   cl(hijk)    c(a) s(bcd) kleene(s(efg)) cl(hijk)                                                    # ( # ( #
    11  #           c(a) s(bcd) kleene(s(efg)) cl(hijk)                                                     # ( # ( # #
    12  s(foo)      c(a) s(bcd) kleene(s(efg)) cl(hijk) s(foo)                                              # ( # ( # #
    13  )           c(a) s(bcd) g([kleene(s(efg)) [cl(hijk) s(foo)]])                                       # ( #
    14  ?           c(a) s(bcd) g([kleene(s(efg)) [cl(hijk) s(foo)]])                                       # ( # ?
    15  )           c(a) g([s(bcd) opt(g([kleene(s(efg)) [cl(hijk) s(foo)]]))])                             #
    16  #           c(a) g([s(bcd) opt(g([kleene(s(efg)) [cl(hijk) s(foo)]]))])                             # #
    17  cl(0-9)     c(a) g([s(bcd) opt(g([kleene(s(efg)) [cl(hijk) s(foo)]]))]) cl(0-9)                     # #
    18  +           c(a) g([s(bcd) opt(g([kleene(s(efg)) [cl(hijk) s(foo)]]))]) cl(0-9)                     # # +
    19  #           c(a) g([s(bcd) opt(g([kleene(s(efg)) [cl(hijk) s(foo)]]))]) many(cl(0-9))               # # #
    20  s(cat)      c(a) g([s(bcd) opt(g([kleene(s(efg)) [cl(hijk) s(foo)]]))]) s(cat) many(cl(0-9))        # # #
    21  #           c(a) g([s(bcd) opt(g([kleene(s(efg)) [cl(hijk) s(foo)]]))]) s(cat) many(cl(0-9))        # # # #
    21  <DONE>      c(a) g([s(bcd) opt(g([kleene(s(efg)) [cl(hijk) s(foo)]]))]) s(cat) many(cl(0-9)) <DONE> # # # #
    22              [c(a) [g([s(bcd) opt(g([kleene(s(efg)) [cl(hijk) s(foo)]]))]) [s(cat) [many(cl(0-9)) <DONE>]]]]

    1   c(a)        char a
    2   g(1)        save 2
    3   s(bcd)      string "bcd"
    4   opt         split 5, 11
    5   g(2)        save 4
    6   kleene      split 6, 7
    7   s(efg)      string "efg"
    8   cl(hijk)    class "hijk"
    9   s(foo)      string "foo"
    10  <g(2)>      save 5
    11  <g(1)>      save 3
    12  s(cat)      string "cat"
    13  many        class "0-9"
    14  cl[0-9]     split 12, 14
    15  match

    1   c(a)        c(a)
    2   #           c(a)                                                                                    #
    3   (           c(a)                                                                                    # (
    4   s(bcd)      c(a) s(bcd)                                                                             # (
    5   #           c(a) s(bcd)                                                                             # ( #
    6   (           c(a) s(bcd)                                                                             # ( # (
    7   s(efg)      c(a) s(bcd) s(efg)                                                                      # ( # (
    8   *           c(a) s(bcd) s(efg)                                                                      # ( # ( *
    9   #           c(a) s(bcd) s(efg) kleene                                                               # ( # ( #
    10  cl(hijk)    c(a) s(bcd) s(efg) kleene cl(hijk)                                                    # ( # ( #
    11  #           c(a) s(bcd) s(efg) kleene cl(hijk)                                                     # ( # ( # #
    12  s(foo)      c(a) s(bcd) s(efg) kleene cl(hijk) s(foo)                                              # ( # ( # #
    13  )           c(a) s(bcd) g([kleene(s(efg)) [cl(hijk) s(foo)]])                                       # ( #
    14  ?           c(a) s(bcd) g([kleene(s(efg)) [cl(hijk) s(foo)]])                                       # ( # ?
    15  )           c(a) g([s(bcd) opt(g([kleene(s(efg)) [cl(hijk) s(foo)]]))])                             #
    16  #           c(a) g([s(bcd) opt(g([kleene(s(efg)) [cl(hijk) s(foo)]]))])                             # #
    17  cl(0-9)     c(a) g([s(bcd) opt(g([kleene(s(efg)) [cl(hijk) s(foo)]]))]) cl(0-9)                     # #
    18  +           c(a) g([s(bcd) opt(g([kleene(s(efg)) [cl(hijk) s(foo)]]))]) cl(0-9)                     # # +
    19  #           c(a) g([s(bcd) opt(g([kleene(s(efg)) [cl(hijk) s(foo)]]))]) many(cl(0-9))               # # #
    20  s(cat)      c(a) g([s(bcd) opt(g([kleene(s(efg)) [cl(hijk) s(foo)]]))]) s(cat) many(cl(0-9))        # # #
    21  #           c(a) g([s(bcd) opt(g([kleene(s(efg)) [cl(hijk) s(foo)]]))]) s(cat) many(cl(0-9))        # # # #
    21  <DONE>      c(a) g([s(bcd) opt(g([kleene(s(efg)) [cl(hijk) s(foo)]]))]) s(cat) many(cl(0-9)) <DONE> # # # #
    22              [c(a) [g([s(bcd) opt(g([kleene(s(efg)) [cl(hijk) s(foo)]]))]) [s(cat) [many(cl(0-9)) <DONE>]]]]

 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>


typedef enum {
    eTokenNone,
    eTokenCharLiteral,
    eTokenCharClass,
    eTokenStringLiteral,
    eTokenCharAny,
    eTokenConcatenation,
    eTokenAlternative,
    eTokenZeroOrOne,
    eTokenZeroOrMany,
    eTokenOneOrMany,
    eTokenSubExprStart,
    eTokenSubExprEnd,
    eTokenMatch
} eRegexToken;

typedef enum {
    ePriorityNone,
    ePriorityLow,
    ePriorityMedium,
    ePriorityHigh
} eRegexTokenPriority;

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
        case eCompileInternalError: return "unknown internal error state";
        default: return "Unknown failure";
    }
}

// Character parsing, assists with escaped chars ////////////////////////////

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

// Subexpression name table /////////////////////////////////////////////////

typedef struct regex_subexpr_name_s regex_subexpr_name_t;
struct regex_subexpr_name_s {
    int index;
    regex_subexpr_name_t *next;
    char name[0];
};

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

typedef struct regex_token_s regex_token_t;
struct regex_token_s {
    eRegexToken tokenType;
    union {
        int c;
        char *str;
    };
    regex_token_t *next;
};

int regexTokenIsTerminal(regex_token_t *token, int preceeding) {
    switch(token->tokenType) {
        case eTokenCharLiteral:
        case eTokenStringLiteral:
        case eTokenCharClass:
        case eTokenCharAny:
            return 1;
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

typedef struct regex_compile_ctx_s regex_compile_ctx_t;
struct regex_compile_ctx_s {
    eRegexCompileStatus status;
    const char *pattern;
    int position;
    regex_token_t *tokens;
    regex_subexpr_name_t *subexpr_list;
};

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

#define SET_RESULT(stat)  result.status = stat; goto compileFailure;

regex_compile_ctx_t regexTokenizePattern(const char *pattern) {
    regex_compile_ctx_t result = {
            .status = eCompileOk,
            .pattern = pattern,
            .position = 0,
            .tokens = NULL,
            .subexpr_list = NULL
    };
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
                    if(!regexTokenCreate(&(result.tokens), eTokenCharAny, 0, 0)) {
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
                    if(!regexTokenCreate(&(result.tokens), eTokenCharClass, 0, str)) {
                        SET_RESULT(eCompileOutOfMem);
                    }
                    continue;

                case '?':
                    if(!regexTokenCreate(&(result.tokens), eTokenZeroOrOne, 0, 0)) {
                        SET_RESULT(eCompileOutOfMem);
                    }
                    continue;

                case '*':
                    if(!regexTokenCreate(&(result.tokens), eTokenZeroOrMany, 0, 0)) {
                        SET_RESULT(eCompileOutOfMem);
                    }
                    continue;

                case '+':
                    if(!regexTokenCreate(&(result.tokens), eTokenOneOrMany, 0, 0)) {
                        SET_RESULT(eCompileOutOfMem);
                    }
                    continue;

                case '|':
                    if(!regexTokenCreate(&(result.tokens), eTokenAlternative, 0, 0)) {
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
                            if((response = regexSubexprLookupEntryCreate(&(result.subexpr_list), &pattern, subexpr)) != 1) {
                                SET_RESULT((response == 0 ? eCompileOutOfMem : eCompileMalformedSubExprName));
                            }
                        } else {
                            SET_RESULT(eCompileUnsupportedMeta);
                        }
                    }
                    if(!regexTokenCreate(&(result.tokens), eTokenSubExprStart, subexpr, NULL)) {
                        SET_RESULT(eCompileOutOfMem);
                    }
                    continue;

                case ')':
                    // End of grouped subexpression
                    if(!regexTokenCreate(&(result.tokens), eTokenSubExprEnd, 0, 0)) {
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
                if(!regexTokenCreate(&(result.tokens), eTokenCharLiteral, (char)(c.c), 0)) {
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
                if(!regexTokenCreate(&(result.tokens), eTokenStringLiteral, 0, str)) {
                    SET_RESULT(eCompileOutOfMem);
                }
        }
    }

    SET_RESULT(eCompileOk);

compileFailure:
    result.position = (int)(pattern - result.pattern);

    if(result.status != eCompileOk) {
        regexCompileCtxCleanup(&result);
    }

    return result;
}

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

// NFA form regex support ///////////////////////////////////////////////////

typedef struct regex_state_s regex_state_t;
struct regex_state_s {
    eRegexOpCode opcode;
    union {
        int c;
        char *str;
        unsigned char *bitmap;
        int group;
    };
    regex_state_t *out_a;
    regex_state_t *out_b;
};

typedef struct regex_ptrlist_s regex_ptrlist_t;
struct regex_ptrlist_s {
    regex_state_t **out;
    regex_ptrlist_t *next;
};

typedef struct regex_fragment_s regex_fragment_t;
struct regex_fragment_s {
    regex_state_t *state;
    regex_ptrlist_t *ptrlist;
    regex_fragment_t *next;
};

regex_state_t *regexStateCreate(eRegexOpCode opcode, int c, char *str, regex_state_t *out_a, regex_state_t *out_b) {
    regex_state_t *state;

    if((state = malloc(sizeof(regex_state_t))) == NULL) {
        return NULL;
    }
    memset(state, 0, sizeof(regex_state_t));
    state->opcode = opcode;
    if(str != NULL) {
        state->str = str;
    } else {
        state->c = c;
    }
    state->out_a = out_a;
    state->out_b = out_b;
    return state;
}

void regexStateFree(regex_state_t *state) {
    if((state->opcode == eCharClass) || (state->opcode == eStringLiteral)) {
        if(state->str != NULL) {
            free(state->str);
        }
    }
    free(state);
}

regex_state_t *regexStateCharCreate(int c) {
    return regexStateCreate(eCharLiteral, c, NULL, NULL, NULL);
}

regex_state_t *regexStateCharClassCreate(unsigned char *bitmap) {
    return regexStateCreate(eCharClass, 0, (char *)bitmap, NULL, NULL);
}

regex_state_t *regexStateStringCreate(char *str) {
    return regexStateCreate(eStringLiteral, 0, str, NULL, NULL);
}

regex_state_t *regexStateAnyCreate(void) {
    return regexStateCreate(eCharAny, 0, NULL, NULL, NULL);
}

regex_state_t *regexStateSplitCreate(regex_state_t *out_a, regex_state_t *out_b) {
    return regexStateCreate(eSplit, 0, NULL, out_a, out_b);
}

regex_state_t *regexStateSaveCreate(int group) {
    return regexStateCreate(eSave, group, NULL, NULL, NULL);
}

regex_state_t *regexStateMatchCreate(void) {
    return regexStateCreate(eMatch, 0, NULL, NULL, NULL);
}

regex_fragment_t *regexFragmentCreate(regex_state_t *state, regex_ptrlist_t *list) {
    regex_fragment_t *fragment;

    if((fragment = malloc(sizeof(regex_fragment_t))) == NULL) {
        return NULL;
    }
    memset(fragment, 0, sizeof(regex_fragment_t));
    fragment->state = state;
    fragment->ptrlist = list;
    return fragment;
}

regex_ptrlist_t *regexPtrlistCreate(regex_state_t **state) {
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

void regexPtrlistPatch(regex_ptrlist_t *list, regex_state_t *state) {
    for(; list != NULL; list = list->next) {
        *(list->out) = state;
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
        if(fragment->state != NULL) {
            regexStateFree(fragment->state);
            fragment->state = NULL;
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
    }
    return fragment;
}

int regexOperatorLiteralCreate(regex_fragment_t **stack, eRegexToken token, int c, char *str) {
    regex_state_t *state;
    regex_fragment_t *fragment;
    regex_ptrlist_t *list;

    switch(token) {
        case eTokenCharLiteral:
            state = regexStateCharCreate(c);
            break;
        case eTokenCharClass:
            state = regexStateCharClassCreate((unsigned char *)str);
            break;
        case eTokenStringLiteral:
            state = regexStateStringCreate(str);
            break;
        case eTokenCharAny:
            state = regexStateAnyCreate();
            break;
        default:
            printf("Unexpected default? [%d]\n", token);
            return 0;
    }
    if(state == NULL) {
        return 0;
    }
    if((list = regexPtrlistCreate(&(state->out_a))) == NULL) {
        regexStateFree(state);
        return 0;
    }
    if((fragment = regexFragmentCreate(state, list)) == NULL) {
        regexPtrListFree(list);
        return 0;
    }
    regexFragmentStackPush(stack, fragment);
    return 1;
}

int regexOperatorSubexprCreate(regex_fragment_t **stack, int group) {
    regex_state_t *state;
    regex_ptrlist_t *list;
    regex_fragment_t *fragment;

    if((state = regexStateSaveCreate(group)) == NULL) {
        return 0;
    }
    if((list = regexPtrlistCreate(&(state->out_a))) == NULL) {
        regexStateFree(state);
        return 0;
    }
    if((fragment = regexFragmentCreate(state, list)) == NULL) {
        regexPtrListFree(list);
        return 0;
    }
    regexFragmentStackPush(stack, fragment);
    return 1;
}

int regexOperatorConcatenationCreate(regex_fragment_t **stack) {
    regex_fragment_t *e1, *e2;
    regex_fragment_t *fragment;

    if(((e2 = regexFragmentStackPop(stack)) == NULL) ||
       ((e1 = regexFragmentStackPop(stack)) == NULL)) {
        return 0;
    }
    regexPtrlistPatch(e1->ptrlist, e2->state);
    if((fragment = regexFragmentCreate(e1->state, e2->ptrlist)) == NULL) {
        return 0;
    }
    regexFragmentStackPush(stack, fragment);
    return 1;
}

int regexOperatorAlternationCreate(regex_fragment_t **stack) {
    regex_fragment_t *e1, *e2, *fragment;
    regex_state_t *state;

    if(((e2 = regexFragmentStackPop(stack)) == NULL) ||
       ((e1 = regexFragmentStackPop(stack)) == NULL)) {
        return 0;
    }
    if((state = regexStateSplitCreate(e1->state, e2->state)) == NULL) {
        return 0;
    }
    if((fragment = regexFragmentCreate(state, regexPtrlistAppend(e1->ptrlist, e2->ptrlist))) == NULL) {
        return 0;
    }
    regexFragmentStackPush(stack, fragment);
    return 1;
}

int regexOperatorZeroOrOneCreate(regex_fragment_t **stack) {
    regex_fragment_t *e, *fragment;
    regex_state_t *state;
    regex_ptrlist_t *list;

    if((e = regexFragmentStackPop(stack)) == NULL) {
        return 0;
    }
    if((state = regexStateSplitCreate(e->state, NULL)) == NULL) {
        return 0;
    }
    if((list = regexPtrlistCreate(&(state->out_b))) == NULL) {
        return 0;
    }
    if((fragment = regexFragmentCreate(state, regexPtrlistAppend(e->ptrlist, list))) == NULL) {
        return 0;
    }
    regexFragmentStackPush(stack, fragment);
    return 1;
}

int regexOperatorZeroOrMoreCreate(regex_fragment_t **stack) {
    regex_state_t *state;
    regex_ptrlist_t *list;
    regex_fragment_t *fragment, *e;

    if((e = regexFragmentStackPop(stack)) == NULL) {
        return 0;
    }
    if((state = regexStateSplitCreate(e->state, NULL)) == NULL) {
        regexFragmentStackPush(stack, e);
        return 0;
    }
    regexPtrlistPatch(e->ptrlist, state);
    if((list = regexPtrlistCreate(&(state->out_b))) == NULL) {
        return 0;
    }
    if((fragment = regexFragmentCreate(state, list)) == NULL) {
        return 0;
    }
    regexFragmentStackPush(stack, fragment);
    return 1;
}

int regexOperatorOneOrMoreCreate(regex_fragment_t **stack) {
    regex_state_t *state;
    regex_ptrlist_t *list;
    regex_fragment_t *fragment, *e;

    if((e = regexFragmentStackPop(stack)) == NULL) {
        return 0;
    }
    if((state = regexStateSplitCreate(e->state, NULL)) == NULL) {
        regexFragmentStackPush(stack, e);
        return 0;
    }
    regexPtrlistPatch(e->ptrlist, state);
    if((list = regexPtrlistCreate(&(state->out_b))) == NULL) {
        free(state);
        return 0;
    }
    if((fragment = regexFragmentCreate(e->state, list)) == NULL) {
        free(list);
        return 0;
    }
    regexFragmentStackPush(stack, fragment);
    return 1;
}

int regexOperatorMatchCreate(regex_fragment_t **stack) {
    regex_fragment_t *e;
    regex_state_t *state;

    if((e = regexFragmentStackPop(stack)) == NULL) {
        return 0;
    }
    if((state = regexStateMatchCreate()) == NULL) {
        return 0;
    }
    regexPtrlistPatch(e->ptrlist, state);
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

typedef enum {
    OP_GREATER_OR_EQUAL,
    OP_ALL
} eRegexOpApply;

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
                if(!regexOperatorZeroOrOneCreate(operands)) {
                    return eCompileOutOfMem;
                }
                break;

            case eTokenZeroOrMany:
                if(!regexOperatorZeroOrMoreCreate(operands)) {
                    return eCompileOutOfMem;
                }
                break;

            case eTokenOneOrMany:
                if(!regexOperatorOneOrMoreCreate(operands)) {
                    return eCompileOutOfMem;
                }
                break;

            case eTokenConcatenation:
                if(!regexOperatorConcatenationCreate(operands)) {
                    return eCompileOutOfMem;
                }
                break;

            case eTokenAlternative:
                if(!regexOperatorAlternationCreate(operands)) {
                    return eCompileOutOfMem;
                }
                break;

            default:
                return eCompileInternalError;
        }
        regexTokenDestroy(operator);
    }
    return eCompileOk;
}

#define SET_YARD_RESULT(res)    status = res; goto ShuntingYardFailure;

eRegexCompileStatus regexShuntingYardFragment(regex_token_t **tokens, regex_fragment_t **root_stack, int sub_expression) {
    regex_token_t *token, *operators = NULL;
    regex_fragment_t *operands = NULL, *subexpr, *sidestack;
    eRegexCompileStatus status = eCompileOk;

    while((token = regexTokenStackPop(tokens)) != NULL) {
        regexFragmentStackSize(operands);
        //printf("=> Operands: %d,  Operators: %d\n", regexFragmentStackSize(operands), regexTokenStackSize(operators));
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
                if(!regexOperatorLiteralCreate(&operands, token->tokenType, token->c, token->str)) {
                    SET_YARD_RESULT(eCompileOutOfMem);
                }
                token->str = NULL;
                regexTokenDestroy(token);
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
                if((status = regexShuntingYardFragment(tokens, &subexpr, token->c)) != eCompileOk) {
                    goto ShuntingYardFailure;
                }
                regexFragmentStackPush(&operands, subexpr);
                break;

            case eTokenSubExprEnd:
                regexTokenDestroy(token);
                if(sub_expression == -1) {
                    SET_YARD_RESULT(eCompileMissingSubexprStart);
                }
                if((status = regexOperatorApply(&operators, OP_ALL, 0, &operands)) != eCompileOk) {
                    goto ShuntingYardFailure;
                }
                if(operands->next != NULL) {
                    SET_YARD_RESULT(eCompileInternalError);
                }
                sidestack = regexFragmentStackPop(&operands);
                // Prefix/suffix the group operators
                if(!regexOperatorSubexprCreate(&operands, token->c)) {
                    SET_YARD_RESULT(eCompileOutOfMem);
                }
                regexFragmentStackPush(&operands, sidestack);
                if(!regexOperatorSubexprCreate(&operands, sub_expression + 1)) {
                    SET_YARD_RESULT(eCompileOutOfMem);
                }
                if(!regexTokenCreate(&operators, eTokenConcatenation, 0, NULL)) {
                    SET_YARD_RESULT(eCompileOutOfMem);
                }
                if(!regexTokenCreate(&operators, eTokenConcatenation, 0, NULL)) {
                    SET_YARD_RESULT(eCompileOutOfMem);
                }
                if((status = regexOperatorApply(&operators, OP_ALL, 0, &operands)) != eCompileOk) {
                    goto ShuntingYardFailure;
                }
                regexFragmentStackPush(root_stack, operands);
                return eCompileOk;
        }
    }

    if(sub_expression != -1) {
        return eCompileInternalError;
    }

    return status;

ShuntingYardFailure:
    regexFragmentFree(operands);

    return status;
}

eRegexCompileStatus regexShuntingYard(regex_token_t **tokens) {
    regex_fragment_t *stack = NULL;
    eRegexCompileStatus status;

    printf("\n== Shunting yard ==============================\n\n");

    if((status = regexShuntingYardFragment(tokens, &stack, -1)) != eCompileOk) {
        return status;
    }
    return eCompileOk;
}

/////////////////////////////////////////////////////////////////////////////

// Return a compiled regex, or an error and position within the pattern
regex_compile_ctx_t regexCompile(const char *pattern, unsigned int flags) {
    regex_compile_ctx_t result;

    // Parse the regex pattern into a sequence of tokens (operators and operands)
    // The output of this stage is a sequence of lexical tokens in infix form

    result = regexTokenizePattern(pattern);
    if(result.status != eCompileOk) {
        regexCompileCtxCleanup(&result);
        return result;
    }

    regexTokenChainPrint(result.tokens, result.subexpr_list);

    // Next, convert the infix form of the regular expression to postfix form,
    // and derive an NFA representation. We accomplish this using the shunting
    // yard algorithm.

    if((result.status = regexShuntingYard(&(result.tokens))) != eCompileOk) {
        regexCompileCtxCleanup(&result);
        return result;
    }

    // Finally, convert the infix form of the postfix expression into an ordered
    // NFA. This final form is used to perform the actual expression matching.

    // TODO
    /*
    if(!regexBuildNFAFromPostfixForm(result.tokens)) {
        result.status = eCompileInternalError;
        return result;
    }
    */

    // Return the completed NFA and associated meta-data to the caller.

    // TODO

    // Attempt to convert to infix form

    regexCompileCtxCleanup(&result);

    return result;
}

#undef SET_RESULT

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
