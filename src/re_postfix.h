#ifndef _RE_POSTFIX_HEADER_
#define _RE_POSTFIX_HEADER_


#include "re_token.h"


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

/*
typedef struct regex_state_s regex_state_t;
struct regex_state_s {
    eRegexOpCode opcode;
    union {
        int c;
        char *str;
        unsigned char *bitmap;
        int group;
    };
    int pc;
    regex_state_t *out_a;
    regex_state_t *out_b;
};
*/

eRegexCompileStatus regexShuntingYard(regex_token_t **tokens /*, regex_state_t **nfa_stack*/);


#endif // _RE_POSTFIX_HEADER_
