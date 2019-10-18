#ifndef _RE_PARSE_HEADER_
#define _RE_PARSE_HEADER_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>


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

eRegexCompileStatus regexTokenizePattern(const char *pattern,
                                         int *pos,
                                         regex_token_t **tokens,
                                         regex_subexpr_name_t **subexpr_list);


#endif // _RE_PARSE_HEADER_
