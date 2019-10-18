#include "re_token.h"


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
