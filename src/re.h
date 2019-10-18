#ifndef _RE_HEADER_
#define _RE_HEADER_


#include "re_token.h"
#include "re_postfix.h"


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


#endif // _RE_HEADER_
