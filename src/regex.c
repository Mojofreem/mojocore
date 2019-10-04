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
    9   #           c(a) s(bcd) kleene(l->s(efg) r->0)                                                              # ( # ( #
    10   cl(hijk)   c(a) s(bcd) kleene(l->s(efg) r->0) cl(hijk)                                                    # ( # ( #
    11  #           c(a) s(bcd) kleene(l->s(efg) r->0) cl(hijk)                                                     # ( # ( # #
    12  s(foo)      c(a) s(bcd) kleene(l->s(efg) r->0) cl(hijk) s(foo)                                              # ( # ( # #
    13  )           c(a) s(bcd) g([kleene(l->s(efg) r->0) [cl(hijk) s(foo)]])                                       # ( #
    14  ?           c(a) s(bcd) g([kleene(l->s(efg) r->0) [cl(hijk) s(foo)]])                                       # ( # ?
    15  )           c(a) g([s(bcd) opt(g([kleene(s(efg)) [cl(hijk) s(foo)]]))])                             #
    16  #           c(a) g([s(bcd) opt(g([kleene(s(efg)) [cl(hijk) s(foo)]]))])                             # #
    17  cl(0-9)     c(a) g([s(bcd) opt(g([kleene(s(efg)) [cl(hijk) s(foo)]]))]) cl(0-9)                     # #
    18  +           c(a) g([s(bcd) opt(g([kleene(s(efg)) [cl(hijk) s(foo)]]))]) cl(0-9)                     # # +
    19  #           c(a) g([s(bcd) opt(g([kleene(s(efg)) [cl(hijk) s(foo)]]))]) many(cl(0-9))               # # #
    20  s(cat)      c(a) g([s(bcd) opt(g([kleene(s(efg)) [cl(hijk) s(foo)]]))]) s(cat) many(cl(0-9))        # # #
    21  <DONE>      [c(a) [g([s(bcd) opt(g([kleene(s(efg)) [cl(hijk) s(foo)]]))]) [s(cat) many(cl(0-9))]]]

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
    eLexNone,
    eLexCharLiteral,
    eLexCharClass,
    eLexStringLiteral,
    eLexCharAny,
    eLexConcatenation,
    eLexAlternative,
    eLexZeroOrOne,
    eLexZeroOrMany,
    eLexOneOrMany,
    eLexSubExprStart,
    eLexSubExprEnd,
    eLexMatch
} eRegexLexeme;

typedef enum {
    ePriorityNone,
    ePriorityLow,
    ePriorityMedium,
    ePriorityHigh
} eRegexLexemePriority;

typedef enum {
    eOpArityUnary,
    eOpArityBinary
} eRegexOpArity;

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

    // Discard the trailing '>' delimiter
    (*pattern)++;

    return str;
}

typedef struct regex_subexpr_name_s regex_subexpr_name_t;
struct regex_subexpr_name_s {
    char *name;
    int index;
    regex_subexpr_name_t *next;
};

int regexSubexprLookupEntryAdd(regex_subexpr_name_t **list, char *name, int index) {
    regex_subexpr_name_t *entry;

    if((entry = malloc(sizeof(regex_subexpr_name_t))) == NULL) {
        return 0;
    }
    memset(entry, 0, sizeof(regex_subexpr_name_t));
    entry->name = name;
    entry->index = index;
    entry->next = *list;
    *list = entry;
    return 1;
}

void regexSubexprLookupFree(regex_subexpr_name_t *list) {
    regex_subexpr_name_t *next;

    for(; list != NULL; list = next) {
        next = list->next;
        free(list->name);
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
    regex_lexeme_t *outl;
    regex_lexeme_t *outr;
    regex_lexeme_t *next;
};

int regexLexemeIsTerminal(regex_lexeme_t *lexeme, int preceeding) {
    switch(lexeme->lexType) {
        case eLexCharLiteral:
        case eLexStringLiteral:
        case eLexCharClass:
        case eLexCharAny:
            return 1;
        case eLexSubExprEnd:
            return preceeding;
        case eLexSubExprStart:
            return !preceeding;
        default:
            return 0;
    }
}

regex_lexeme_t *regexAllocLexeme(eRegexLexeme lexType, int c, char *str) {
    regex_lexeme_t *lexeme;

    if((lexeme = malloc(sizeof(regex_lexeme_t))) == NULL) {
        return NULL;
    }
    memset(lexeme, 0, sizeof(regex_lexeme_t));
    lexeme->lexType = lexType;
    if(str != NULL) {
        lexeme->str = str;
    } else {
        lexeme->c = c;
    }
    return lexeme;
}

int regexLexemeCreate(regex_lexeme_t **list, eRegexLexeme lexType, int c, char *str) {
    regex_lexeme_t *lexeme, *walk;

    if((lexeme = regexAllocLexeme(lexType, c, str)) == NULL) {
        return 0;
    }

    if(*list == NULL) {
        *list = lexeme;
    } else {
        for(walk = *list; walk->next != NULL; walk = walk->next);
        if(regexLexemeIsTerminal(lexeme, 0) && regexLexemeIsTerminal(walk, 1)) {
            // Two adjacent terminals have an implicit concatenation
            if((walk->next = regexAllocLexeme(eLexConcatenation, 0, NULL)) == NULL) {
                return 0;
            }
            walk = walk->next;
        }
        walk->next = lexeme;
    }

    return 1;
}

void regexLexemeDestroy(regex_lexeme_t *instr) {
    regex_lexeme_t *next;

    for(; instr != NULL; instr = next) {
        next = instr->next;
        switch(instr->lexType) {
            case eLexCharClass:
            case eLexStringLiteral:
                free(instr->str);
                break;
            default:
                break;
        }
        free(instr);
    }
}

void regexLexemePrint(regex_lexeme_t *lexeme, regex_subexpr_name_t *subexpr) {
    const char *str;

    switch(lexeme->lexType) {
        case eLexCharLiteral:
            printf("CHAR(%c)\n", lexeme->c);
            break;
        case eLexCharClass:
            printf("CLASS[");
            regexPrintCharClass((unsigned char *)(lexeme->str));
            printf("]\n");
            break;
        case eLexStringLiteral:
            printf("STRING(\"%s\")\n", lexeme->str);
            break;
        case eLexCharAny:
            printf("ANY\n");
            break;
        case eLexConcatenation:
            printf("CONCAT\n");
            break;
        case eLexAlternative:
            printf("ALTERNATIVE\n");
            break;
        case eLexZeroOrOne:
            printf("ZERO_OR_ONE\n");
            break;
        case eLexZeroOrMany:
            printf("ZERO_OR_MANY\n");
            break;
        case eLexOneOrMany:
            printf("ONE_OR_MANY\n");
            break;
        case eLexSubExprStart:
            str = regexSubexprLookupName(subexpr, lexeme->c);
            if(str != NULL) {
                printf("SUBEXPR #%d <%s>\n", lexeme->c, str);
            } else {
                printf("SUBEXPR #%d\n", lexeme->c);
            }
            break;
        case eLexSubExprEnd:
            printf("SUBEXPR #%d END\n", lexeme->c);
            break;
        case eLexMatch:
            printf("MATCH!\n");
            break;
        default:
            printf("UNKNOWN! <%d>\n", lexeme->lexType);
            break;
    }
}

void regexLexemeChainPrint(regex_lexeme_t *lexeme, regex_subexpr_name_t *subexpr) {
    for(; lexeme != NULL; lexeme = lexeme->next) {
        regexLexemePrint(lexeme, subexpr);
    }
}

void regexProgramStepPrint(regex_lexeme_t *lexeme, regex_subexpr_name_t *subexpr) {
    const char *str;

    while(lexeme != NULL) {
        switch(lexeme->lexType) {
            case eLexCharLiteral:
                printf("CHAR(%c)", lexeme->c);
                break;
            case eLexCharClass:
                printf("CLASS(");
                regexPrintCharClass((unsigned char *)(lexeme->str));
                printf(")");
                break;
            case eLexStringLiteral:
                printf("STRING(\"%s\")", lexeme->str);
                break;
            case eLexCharAny:
                printf("ANY");
                break;
            case eLexConcatenation:
                printf("CONCAT(");
                regexProgramStepPrint(lexeme->outl, subexpr);
                printf(",");
                regexProgramStepPrint(lexeme->outr, subexpr);
                printf(")");
                break;
            case eLexAlternative:
                printf("ALTERNATIVE\n");
                regexProgramStepPrint(lexeme->outl, subexpr);
                printf("|");
                regexProgramStepPrint(lexeme->outr, subexpr);
                printf(")");
                break;
            case eLexZeroOrOne:
                printf("ZERO_OR_ONE");
                break;
            case eLexZeroOrMany:
                printf("ZERO_OR_MANY");
                break;
            case eLexOneOrMany:
                printf("ONE_OR_MANY");
                break;
            case eLexSubExprStart:
                str = regexSubexprLookupName(subexpr, lexeme->c);
                if(str != NULL) {
                    printf("SUBEXPR #%d <%s>\n", lexeme->c, str);
                } else {
                    printf("SUBEXPR #%d\n", lexeme->c);
                }
                break;
            case eLexSubExprEnd:
                printf("SUBEXPR #%d END\n", lexeme->c);
                break;
            case eLexMatch:
                printf("MATCH\n");
                break;
            default:
                printf("UNKNOWN! <%d>\n", lexeme->lexType);
                break;
        }
    }
}

void stackPush(regex_lexeme_t **stack, regex_lexeme_t *instr) {
    if(*stack == NULL) {
        *stack = instr;
        instr->next = NULL;
        return;
    }
    instr->next = *stack;
    *stack = instr;
}

regex_lexeme_t *stackPop(regex_lexeme_t **stack) {
    regex_lexeme_t *entry;

    if(*stack == NULL) {
        return NULL;
    }
    entry = *stack;
    *stack = entry->next;
    return entry;
}

eRegexLexeme stackPeekType(regex_lexeme_t *stack) {
    if(stack == NULL) {
        return eLexNone;
    }
    return stack->lexType;
}

eRegexLexemePriority regexGetLexemeTypePriority(eRegexLexeme lexType) {
    switch(lexType) {
        case eLexCharLiteral:
        case eLexCharClass:
        case eLexStringLiteral:
        case eLexCharAny:
        case eLexMatch:
        default:
            return ePriorityNone;

        case eLexZeroOrOne:
        case eLexZeroOrMany:
        case eLexOneOrMany:
            return ePriorityHigh;

        case eLexConcatenation:
        case eLexAlternative:
            return ePriorityMedium;

        case eLexSubExprStart:
        case eLexSubExprEnd:
            return ePriorityLow;
    }
}

int regexStackTypeGreaterOrEqualToLexeme(regex_lexeme_t *stack, regex_lexeme_t *lexeme) {
    eRegexLexemePriority stackPriority;

    stackPriority = regexGetLexemeTypePriority(stackPeekType(stack));
    return (stackPriority >= regexGetLexemeTypePriority(lexeme->lexType));
}

int regexGetOperatorArity(regex_lexeme_t *lexeme) {
    switch(lexeme->lexType) {
        case eLexZeroOrOne:
        case eLexZeroOrMany:
        case eLexOneOrMany:
            return 1;
        case eLexConcatenation:
        case eLexAlternative:
            return 2;
        default:
            return 0;
    }
}

int regexApplyOperator(regex_lexeme_t **operands, regex_lexeme_t *operator) {
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

eRegexOpCode regexGetOpcodeFromLexeme(eRegexLexeme lexType) {
    switch(lexType) {
        case eLexCharLiteral: return eCharLiteral;
        case eLexCharClass: return eCharClass;
        case eLexStringLiteral: return eStringLiteral;
        case eLexCharAny: return eCharAny;
        case eLexMatch: return eMatch;
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

void regexPrintProgramElement(regex_lexeme_t *lexeme) {

}

void regexPrintProgram(regex_lexeme_t *lexeme) {
    switch(lexeme->lexType) {
        case eLexNone: printf("!"); break;
        case eLexCharLiteral:
            if((lexeme->c < 32) || (lexeme->c > 127)) {
                printf("\\x%2.2x", lexeme->c);
            } else {
                printf("%c", lexeme->c);
            }
            break;
        case eLexCharClass:
            printf("[");
            regexPrintCharClass((unsigned char *)lexeme->str);
            printf("]");
            if(lexeme->outl != NULL) {
                regexPrintProgram(lexeme->outl);
            }
            break;
        case eLexStringLiteral:
            printf("%s", lexeme->str);
            break;
        case eLexCharAny:
            printf(".");
            break;
        case eLexAlternative:
            regexPrintProgram(lexeme->outl);
            printf("|");
            regexPrintProgram(lexeme->outr);
            break;
        case eLexZeroOrOne:
            regexPrintProgram(lexeme->outl);
            printf("?");
            break;
        case eLexZeroOrMany:
            regexPrintProgram(lexeme->outl);
            printf("*");
            break;
        case eLexOneOrMany:
            regexPrintProgram(lexeme->outl);
            printf("+");
            break;
        case eLexSubExprStart:
            printf("(");
            regexPrintProgram(lexeme->outl);
            printf(")");

        case eLexSubExprEnd:
        case eLexMatch:
        case eLexConcatenation:
        default:
            break;
    }
}

int regexShuntingYard(regex_lexeme_t **program, regex_subexpr_name_t *list) {
    printf("----------\n");

    regexLexemeChainPrint(*program, list);

    printf("----------\n");

    regex_lexeme_t *lex_operands = NULL, *lex_operators = NULL, *lexeme, *next, *lex_operator;
    regex_instr_t *operands = NULL, *operators = NULL, *instr, *operator;
    eRegexOpCode opcode;

    for(lexeme = *program; lexeme != NULL; lexeme = next) {
        next = lexeme->next;
        lexeme->next = NULL;

        switch(lexeme->lexType) {
            case eLexCharLiteral:
            case eLexCharClass:
            case eLexStringLiteral:
            case eLexCharAny:
            case eLexMatch:
                if((instr = regexInstrCreate(regexGetOpcodeFromLexeme(lexeme->lexType), lexeme->c, lexeme->str)) == NULL) {
                    return 0;
                }
                regexInstrStackPush(&operands, instr);
                stackPush(&lex_operands, lexeme);
                break;

            case eLexZeroOrOne:
            case eLexZeroOrMany:
            case eLexOneOrMany:
            case eLexAlternative:
            case eLexConcatenation:
                while(regexStackTypeGreaterOrEqualToLexeme(lex_operators, lexeme)) {
                    lex_operator = stackPop(&lex_operators);
                    if(!regexApplyOperator(&lex_operands, lex_operator)) {
                        return 0;
                    }
                }
                stackPush(&lex_operators, lexeme);
                break;

            case eLexSubExprStart:
                stackPush(&lex_operators, lexeme);
                break;

            case eLexSubExprEnd:
                while(stackPeekType(lex_operators) != eLexSubExprStart) {
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
                printf("ERROR: Unknown lexeme token [%d]\n", lexeme->lexType);
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

typedef struct regex_compile_ctx_s regex_compile_ctx_t;
struct regex_compile_ctx_s {
    eRegexCompileStatus status;
    const char *pattern;
    int position;
};

//#define SET_RESULT(res,stat,ptr)  res.status = stat; res.position = ptr - res.pattern;

#define SET_RESULT(stat)  result.status = stat; result.position = pattern - result.pattern; goto compileFailure;

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
    regex_subexpr_name_t *subexpr_list = NULL;

    for(; *pattern != '\0';) {
        c = regexGetNextPatternChar(&pattern);
        if(c.c == -1) {
            SET_RESULT(eCompileEscapeCharIncomplete);
        }

        if(!c.escaped) {
            switch(c.c) {
                case '.':
                    if(!regexLexemeCreate(&program, eLexCharAny, 0, 0)) {
                        SET_RESULT(eCompileOutOfMem);
                    }
                    printf("Any\n");
                    continue;
                case '|':
                    if(!regexLexemeCreate(&program, eLexAlternative, 0, 0)) {
                        SET_RESULT(eCompileOutOfMem);
                    }
                    printf("Alt\n");
                    continue;
                case '?':
                    if(!regexLexemeCreate(&program, eLexZeroOrOne, 0, 0)) {
                        SET_RESULT(eCompileOutOfMem);
                    }
                    printf("ZeroOrOne\n");
                    continue;
                case '*':
                    if(!regexLexemeCreate(&program, eLexZeroOrMany, 0, 0)) {
                        SET_RESULT(eCompileOutOfMem);
                    }
                    printf("ZeroOrMore\n");
                    continue;
                case '+':
                    if(!regexLexemeCreate(&program, eLexOneOrMany, 0, 0)) {
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
                            len = regexGetSubexpressionNameLen(&pattern);
                            if(len == -1) {
                                SET_RESULT(eCompileMalformedSubExprName);
                            }
                            if((str = regexGetSubexpressionName(&pattern, len)) == NULL) {
                                SET_RESULT(eCompileOutOfMem);
                            }
                            printf("[%s]", str);
                        } else {
                            SET_RESULT(eCompileUnsupportedMeta);
                        }
                    }
                    if(str != NULL) {
                        regexSubexprLookupEntryAdd(&subexpr_list, str, subexpr);
                    }
                    if(!regexLexemeCreate(&program, eLexSubExprStart, subexpr, NULL)) {
                        SET_RESULT(eCompileOutOfMem);
                    }
                    printf("({%d%s%s}\n", subexpr, (str != NULL ? ":" : ""), (str != NULL ? str : ""));
                    continue;
                case ')':
                    if(!regexLexemeCreate(&program, eLexSubExprEnd, 0, 0)) {
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
                    if(!regexLexemeCreate(&program, eLexCharClass, 0, str)) {
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
                if(!regexLexemeCreate(&program, eLexCharLiteral, character, 0)) {
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
                if(!regexLexemeCreate(&program, eLexStringLiteral, 0, str)) {
                    SET_RESULT(eCompileOutOfMem);
                }
                printf("STRING(\"%s\")\n", str);
        }
    }

    // The final implicit lexeme is the "match"

    if(!regexLexemeCreate(&program, eLexMatch, 0, 0)) {
        SET_RESULT(eCompileOutOfMem);
    }

    // Attempt to convert to infix form

    regexShuntingYard(&program, subexpr_list);

    regexLexemeDestroy(program);
    regexSubexprLookupFree(subexpr_list);
    program = NULL;

    return result;

compileFailure:
    // TODO - cleanup

    return result;
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
