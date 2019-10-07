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
    eCompileOutOfMem
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
        if(c.escaped) {
            switch(c.c) {
                case 'r': *ptr = '\r'; break;
                case 'n': *ptr = '\n'; break;
                case 't': *ptr = '\t'; break;
                default: *ptr = (char)(c.c); break;
            }
        } else {
            *ptr = (char)(c.c);
        }
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
        //free(list->name);
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

// Lexeme management functions //////////////////////////////////////////////

typedef struct regex_token_s regex_token_t;
struct regex_token_s {
    eRegexToken tokenType;
    union {
        int c;
        char *str;
    };
    regex_token_t *outl;
    regex_token_t *outr;
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
                free(token->str);
                break;
            default:
                break;
        }
        free(token);
    }
}

void regexTokenPrint(regex_token_t *token, regex_subexpr_name_t *subexpr) {
    const char *str;

    switch(token->tokenType) {
        case eTokenCharLiteral:
            printf("CHAR(%c)\n", token->c);
            break;
        case eTokenCharClass:
            printf("CLASS[");
            regexPrintCharClass((unsigned char *)(token->str));
            printf("]\n");
            break;
        case eTokenStringLiteral:
            printf("STRING(\"%s\")\n", token->str);
            break;
        case eTokenCharAny:
            printf("ANY\n");
            break;
        case eTokenConcatenation:
            printf("CONCAT\n");
            break;
        case eTokenAlternative:
            printf("ALTERNATIVE\n");
            break;
        case eTokenZeroOrOne:
            printf("ZERO_OR_ONE\n");
            break;
        case eTokenZeroOrMany:
            printf("ZERO_OR_MANY\n");
            break;
        case eTokenOneOrMany:
            printf("ONE_OR_MANY\n");
            break;
        case eTokenSubExprStart:
            str = regexSubexprLookupName(subexpr, token->c);
            if(str != NULL) {
                printf("SUBEXPR #%d <%s>\n", token->c, str);
            } else {
                printf("SUBEXPR #%d\n", token->c);
            }
            break;
        case eTokenSubExprEnd:
            printf("SUBEXPR #%d END\n", token->c);
            break;
        case eTokenMatch:
            printf("MATCH!\n");
            break;
        default:
            printf("UNKNOWN! <%d>\n", token->tokenType);
            break;
    }
}

void regexTokenChainPrint(regex_token_t *token, regex_subexpr_name_t *subexpr) {
    for(; token != NULL; token = token->next) {
        regexTokenPrint(token, subexpr);
    }
}

void regexProgramStepPrint(regex_token_t *token, regex_subexpr_name_t *subexpr) {
    const char *str;

    while(token != NULL) {
        switch(token->tokenType) {
            case eTokenCharLiteral:
                printf("CHAR(%c)", token->c);
                break;
            case eTokenCharClass:
                printf("CLASS(");
                regexPrintCharClass((unsigned char *)(token->str));
                printf(")");
                break;
            case eTokenStringLiteral:
                printf("STRING(\"%s\")", token->str);
                break;
            case eTokenCharAny:
                printf("ANY");
                break;
            case eTokenConcatenation:
                printf("CONCAT(");
                regexProgramStepPrint(token->outl, subexpr);
                printf(",");
                regexProgramStepPrint(token->outr, subexpr);
                printf(")");
                break;
            case eTokenAlternative:
                printf("ALTERNATIVE\n");
                regexProgramStepPrint(token->outl, subexpr);
                printf("|");
                regexProgramStepPrint(token->outr, subexpr);
                printf(")");
                break;
            case eTokenZeroOrOne:
                printf("ZERO_OR_ONE");
                break;
            case eTokenZeroOrMany:
                printf("ZERO_OR_MANY");
                break;
            case eTokenOneOrMany:
                printf("ONE_OR_MANY");
                break;
            case eTokenSubExprStart:
                str = regexSubexprLookupName(subexpr, token->c);
                if(str != NULL) {
                    printf("SUBEXPR #%d <%s>\n", token->c, str);
                } else {
                    printf("SUBEXPR #%d\n", token->c);
                }
                break;
            case eTokenSubExprEnd:
                printf("SUBEXPR #%d END\n", token->c);
                break;
            case eTokenMatch:
                printf("MATCH\n");
                break;
            default:
                printf("UNKNOWN! <%d>\n", token->tokenType);
                break;
        }
    }
}

void stackPush(regex_token_t **stack, regex_token_t *token) {
    if(*stack == NULL) {
        *stack = token;
        token->next = NULL;
        return;
    }
    token->next = *stack;
    *stack = token;
}

regex_token_t *stackPop(regex_token_t **stack) {
    regex_token_t *entry;

    if(*stack == NULL) {
        return NULL;
    }
    entry = *stack;
    *stack = entry->next;
    return entry;
}

eRegexToken stackPeekType(regex_token_t *stack) {
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

int regexStackTypeGreaterOrEqualToToken(regex_token_t *stack, regex_token_t *token) {
    eRegexTokenPriority stackPriority;

    stackPriority = regexGetTokenTypePriority(stackPeekType(stack));
    return (stackPriority >= regexGetTokenTypePriority(token->tokenType));
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

int regexApplyOperator(regex_token_t **operands, regex_token_t *operator) {
    int arity = regexGetOperatorArity(operator);
    operator->next = NULL;
    if((operator->outl = stackPop(operands)) == NULL) {
        printf("ERROR: Missing operand 1 for operator\n");
        return 0;
    }
    if(arity > 1) {
        operator->outr = operator->outl;
        if((operator->outl = stackPop(operands)) == NULL) {
            printf("ERROR: Missing operand 2 for operator\n");
            return 0;
        }
    }
    stackPush(operands, operator);
    return 1;
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

typedef struct regex_instr_s regex_instr_t;
struct regex_instr_s {
    eRegexOpCode opcode;
    union {
        int c;
        int group;
        unsigned char *bitmap;
        char *str;
    };
    regex_instr_t *out_left;
    regex_instr_t *out_right;
    regex_instr_t *next;
};

regex_instr_t *regexInstrCreate(eRegexOpCode opcode, int c, char *str) {
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

eRegexOpCode regexGetOpcodeFromToken(eRegexToken tokenType) {
    switch(tokenType) {
        case eTokenCharLiteral: return eCharLiteral;
        case eTokenCharClass: return eCharClass;
        case eTokenStringLiteral: return eStringLiteral;
        case eTokenCharAny: return eCharAny;
        case eTokenMatch: return eMatch;
        default: return 0;
    }
}

regex_instr_t *regexInstrCharLiteralCreate(int c) {
    return regexInstrCreate(eCharLiteral, c, NULL);
}

regex_instr_t *regexInstrStringLiteralCreate(char *str) {
    return regexInstrCreate(eStringLiteral, 0, str);
}

regex_instr_t *regexInstrCharClassCreate(unsigned char *bitmap) {
    return regexInstrCreate(eCharClass, 0, (char *)bitmap);
}

regex_instr_t *regexInstrCharAnyCreate() {
    return regexInstrCreate(eCharAny, 0, NULL);
}

void regexInstrStackPush(regex_instr_t **stack, regex_instr_t *instr) {
    instr->next = *stack;
    *stack = instr;
}

regex_instr_t *regexInstrStackPop(regex_instr_t **stack) {
    regex_instr_t *instr;

    if(*stack == NULL) {
        return NULL;
    }
    instr = *stack;
    *stack = instr->next;
    return instr;
}

void regexPrintProgramElement(regex_token_t *token) {

}

void regexPrintProgram(regex_token_t *token) {
    switch(token->tokenType) {
        case eTokenNone: printf("!"); break;
        case eTokenCharLiteral:
            if((token->c < 32) || (token->c > 127)) {
                printf("\\x%2.2x", token->c);
            } else {
                printf("%c", token->c);
            }
            break;
        case eTokenCharClass:
            printf("[");
            regexPrintCharClass((unsigned char *)token->str);
            printf("]");
            if(token->outl != NULL) {
                regexPrintProgram(token->outl);
            }
            break;
        case eTokenStringLiteral:
            printf("%s", token->str);
            break;
        case eTokenCharAny:
            printf(".");
            break;
        case eTokenAlternative:
            regexPrintProgram(token->outl);
            printf("|");
            regexPrintProgram(token->outr);
            break;
        case eTokenZeroOrOne:
            regexPrintProgram(token->outl);
            printf("?");
            break;
        case eTokenZeroOrMany:
            regexPrintProgram(token->outl);
            printf("*");
            break;
        case eTokenOneOrMany:
            regexPrintProgram(token->outl);
            printf("+");
            break;
        case eTokenSubExprStart:
            printf("(");
            regexPrintProgram(token->outl);
            printf(")");

        case eTokenSubExprEnd:
        case eTokenMatch:
        case eTokenConcatenation:
        default:
            break;
    }
}

int regexShuntingYard(regex_token_t **program, regex_subexpr_name_t *list) {
    printf("----------\n");

    regexTokenChainPrint(*program, list);

    printf("----------\n");

    regex_token_t *lex_operands = NULL, *lex_operators = NULL, *token, *next, *lex_operator;
    regex_instr_t *operands = NULL, *operators = NULL, *instr, *operator;
    eRegexOpCode opcode;

    for(token = *program; token != NULL; token = next) {
        next = token->next;
        token->next = NULL;

        switch(token->tokenType) {
            case eTokenCharLiteral:
            case eTokenCharClass:
            case eTokenStringLiteral:
            case eTokenCharAny:
            case eTokenMatch:
                if((instr = regexInstrCreate(regexGetOpcodeFromToken(token->tokenType), token->c, token->str)) == NULL) {
                    return 0;
                }
                regexInstrStackPush(&operands, instr);
                stackPush(&lex_operands, token);
                break;

            case eTokenZeroOrOne:
            case eTokenZeroOrMany:
            case eTokenOneOrMany:
            case eTokenAlternative:
            case eTokenConcatenation:
                while(regexStackTypeGreaterOrEqualToToken(lex_operators, token)) {
                    lex_operator = stackPop(&lex_operators);
                    if(!regexApplyOperator(&lex_operands, lex_operator)) {
                        return 0;
                    }
                }
                stackPush(&lex_operators, token);
                break;

            case eTokenSubExprStart:
                stackPush(&lex_operators, token);
                break;

            case eTokenSubExprEnd:
                while(stackPeekType(lex_operators) != eTokenSubExprStart) {
                    if((lex_operator = stackPop(&lex_operators)) == NULL) {
                        printf("ERROR: Unable to find subexpression start token!\n");
                        return 0;
                    }
                    if(!regexApplyOperator(&lex_operands, lex_operator)) {
                        return 0;
                    }
                }
                // Pop and discard the closing subexpr end token
                stackPop(&lex_operators);
                break;

            default:
                printf("ERROR: Unknown token token [%d]\n", token->tokenType);
                return 0;
        }
    }

    while((lex_operator = stackPop(&lex_operators)) != NULL) {
        if(!regexApplyOperator(&lex_operands, lex_operator)) {
            return 0;
        }
    }

    printf("INFO: Apparent success\n");

    regexPrintProgram(lex_operands);

    return 1;
}

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

regex_state_t *regexBuildNFAFromPostfixForm(regex_token_t *token) {
    regex_fragment_t *stack = NULL, *e1, *e2;
    regex_state_t *state;

    for(; token != NULL; token = token->next) {
        switch(token->tokenType) {
            case eTokenCharLiteral:
                state = regexStateCharCreate(token->c);
                regexFragmentStackPush(&stack, regexFragmentCreate(state, regexPtrlistCreate(&(state->out_a))));
                break;

            case eTokenCharClass:
                state = regexStateCharClassCreate((unsigned char *)token->str);
                regexFragmentStackPush(&stack, regexFragmentCreate(state, regexPtrlistCreate(&(state->out_a))));
                break;

            case eTokenStringLiteral:
                state = regexStateStringCreate(token->str);
                regexFragmentStackPush(&stack, regexFragmentCreate(state, regexPtrlistCreate(&(state->out_a))));
                break;

            case eTokenCharAny:
                state = regexStateAnyCreate();
                regexFragmentStackPush(&stack, regexFragmentCreate(state, regexPtrlistCreate(&(state->out_a))));
                break;

            case eTokenZeroOrOne:
                e1 = regexFragmentStackPop(&stack);
                state = regexStateSplitCreate(e1->state, NULL);
                regexFragmentStackPush(&stack, regexFragmentCreate(state, regexPtrlistAppend(e1->ptrlist, regexPtrlistCreate(&state->out_b))));
                break;

            case eTokenZeroOrMany:
                e1 = regexFragmentStackPop(&stack);
                state = regexStateSplitCreate(e1->state, NULL);
                regexPtrlistPatch(e1->ptrlist, state);
                regexFragmentStackPush(&stack, regexFragmentCreate(state, regexPtrlistCreate(&(state->out_b))));
                break;

            case eTokenOneOrMany:
                e1 = regexFragmentStackPop(&stack);
                state = regexStateSplitCreate(e1->state, NULL);
                regexPtrlistPatch(e1->ptrlist, state);
                regexFragmentStackPush(&stack, regexFragmentCreate(e1->state, regexPtrlistCreate(&(state->out_b))));
                break;

            case eTokenAlternative:
                e2 = regexFragmentStackPop(&stack);
                e1 = regexFragmentStackPop(&stack);
                state = regexStateSplitCreate(e1->state, e2->state);
                regexFragmentStackPush(&stack, regexFragmentCreate(state, regexPtrlistAppend(e1->ptrlist, e2->ptrlist)));
                break;

            case eTokenConcatenation:
                e2 = regexFragmentStackPop(&stack);
                e1 = regexFragmentStackPop(&stack);
                regexPtrlistPatch(e1->ptrlist, e2->state);
                regexFragmentStackPush(&stack, regexFragmentCreate(e1->state, e2->ptrlist));
                break;

            case eTokenSubExprStart:
                break;

            case eTokenSubExprEnd:
                break;

            default:
                printf("ERROR: Unknown token token [%d]\n", token->tokenType);
                return NULL;
        }
        e1 = regexFragmentStackPop(&stack);
        regexPtrlistPatch(e1->ptrlist, regexStateCreate(eMatch, 0, NULL, NULL, NULL));
        return e1->state;
    }
    return state;
}

#if 0

int regexInfixToPostfix(regex_token_t **program, regex_subexpr_name_t *list) {
    printf("----------\n");

    regex_token_t *operands = NULL, *operators = NULL, *lexeme, *next, *operator;

    for(lexeme = *program; lexeme != NULL; lexeme = next) {
        next = lexeme->next;
        lexeme->next = NULL;

        switch(lexeme->tokenType) {
            case eTokenCharLiteral:
            case eTokenCharClass:
            case eTokenStringLiteral:
            case eTokenCharAny:
                stackPush(&operands, lexeme);
                break;

            case eTokenZeroOrOne:
            case eTokenZeroOrMany:
            case eTokenOneOrMany:
            case eTokenAlternative:
            case eTokenConcatenation:
                while(regexStackTypeGreaterOrEqualToLexeme(operators, lexeme)) {
                    operator = stackPop(&operators);
                    if(!regexApplyOperator(&operands, operator)) {
                        return 0;
                    }
                }
                stackPush(&operators, lexeme);
                break;

            case eTokenSubExprStart:
                stackPush(&lex_operators, lexeme);
                break;

            case eTokenSubExprEnd:
                while(stackPeekType(lex_operators) != eTokenSubExprStart) {
                    if((lex_operator = stackPop(&lex_operators)) == NULL) {
                        printf("ERROR: Unable to find subexpression start token!\n");
                        return 0;
                    }
                    if(!regexApplyOperator(&lex_operands, lex_operator)) {
                        return 0;
                    }
                }
                // Pop and discard the closing subexpr end token
                stackPop(&lex_operators);
                break;

            default:
                printf("ERROR: Unknown lexeme token [%d]\n", lexeme->tokenType);
                return 0;
        }
    }

    while((lex_operator = stackPop(&lex_operators)) != NULL) {
        if(!regexApplyOperator(&lex_operands, lex_operator)) {
            return 0;
        }
    }

    printf("INFO: Apparent success\n");

    regexPrintProgram(lex_operands);

    return 1;
}
#endif

/////////////////////////////////////////////////////////////////////////////

typedef struct regex_compile_ctx_s regex_compile_ctx_t;
struct regex_compile_ctx_s {
    eRegexCompileStatus status;
    const char *pattern;
    int position;
};

#define SET_RESULT(stat)  result.status = stat; goto compileFailure;

// Return a compiled regex, or an error and position within the pattern
regex_compile_ctx_t regexCompile(const char *pattern, unsigned int flags) {
    regex_compile_ctx_t result = {
            .status = eCompileOk,
            .pattern = pattern,
            .position = 0
    };
    character_t c;
    int response;
    int len;
    char *str;
    char character;
    unsigned char bitmap[32];
    eRegexCompileStatus status;
    int subexpr = 0;
    regex_token_t *program = NULL;
    regex_subexpr_name_t *subexpr_list = NULL;
    regex_token_t *operands = NULL, *operators = NULL, *operator;


    for(; *pattern != '\0';) {
        c = regexGetNextPatternChar(&pattern);
        if(c.c == -1) {
            SET_RESULT(eCompileEscapeCharIncomplete);
        }

        if(!c.escaped) {
            switch(c.c) {
                case '.':
                    if(!regexTokenCreate(&program, eTokenCharAny, 0, 0)) {
                        SET_RESULT(eCompileOutOfMem);
                    }
                    printf("Any\n");
                    continue;
                case '|':
                    if(!regexTokenCreate(&program, eTokenAlternative, 0, 0)) {
                        SET_RESULT(eCompileOutOfMem);
                    }
                    printf("Alt\n");
                    continue;
                case '?':
                    if(!regexTokenCreate(&program, eTokenZeroOrOne, 0, 0)) {
                        SET_RESULT(eCompileOutOfMem);
                    }
                    printf("ZeroOrOne\n");
                    continue;
                case '*':
                    if(!regexTokenCreate(&program, eTokenZeroOrMany, 0, 0)) {
                        SET_RESULT(eCompileOutOfMem);
                    }
                    printf("ZeroOrMore\n");
                    continue;
                case '+':
                    if(!regexTokenCreate(&program, eTokenOneOrMany, 0, 0)) {
                        SET_RESULT(eCompileOutOfMem);
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
                                SET_RESULT(eCompileMalformedSubExprName);
                            }
                            if((response = regexSubexprLookupEntryCreate(&subexpr_list, &pattern, subexpr)) != 1) {
                                SET_RESULT((response == 0 ? eCompileOutOfMem : eCompileMalformedSubExprName));
                            }
                            printf("[%s]", subexpr_list->name);
                        } else {
                            SET_RESULT(eCompileUnsupportedMeta);
                        }
                    }
                    if(!regexTokenCreate(&program, eTokenSubExprStart, subexpr, NULL)) {
                        SET_RESULT(eCompileOutOfMem);
                    }
                    printf("({%d%s%s}\n", subexpr, ((subexpr_list != NULL && subexpr_list->index == subexpr) ? ":" : ""), ((subexpr_list != NULL && subexpr_list->index == subexpr) ? subexpr_list->name : ""));
                    continue;
                case ')':
                    if(!regexTokenCreate(&program, eTokenSubExprEnd, 0, 0)) {
                        SET_RESULT(eCompileOutOfMem);
                    }
                    printf(")\n");
                    continue;
                case '[':
                    if((status = regexParseCharClass(&pattern, bitmap)) != eCompileOk) {
                        SET_RESULT(status);
                    }
                    if((str = (char *)mapCopy(bitmap)) == NULL) {
                        SET_RESULT(eCompileOutOfMem);
                    }
                    if(!regexTokenCreate(&program, eTokenCharClass, 0, str)) {
                        SET_RESULT(eCompileOutOfMem);
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
                SET_RESULT(eCompileEscapeCharIncomplete);
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
                if(!regexTokenCreate(&program, eTokenCharLiteral, character, 0)) {
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
                if(!regexTokenCreate(&program, eTokenStringLiteral, 0, str)) {
                    SET_RESULT(eCompileOutOfMem);
                }
                printf("STRING(\"%s\")\n", str);
        }
    }

    // The final implicit lexeme is the "match"

    /*
    if(!regexTokenCreate(&program, eTokenMatch, 0, 0)) {
        SET_RESULT(eCompileOutOfMem);
    }
    */

    // Attempt to convert to infix form

    regexShuntingYard(&program, subexpr_list);

    regexTokenDestroy(program);
    regexSubexprLookupFree(subexpr_list);
    program = NULL;

    return result;

compileFailure:
    result.position = (int)(pattern - result.pattern);

    // TODO - cleanup

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
