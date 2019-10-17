#include "re.h"


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
        case eCompileInternalError: return "unknown internal error state";
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
    // yard algorithm.

    if((ctx->status = regexShuntingYard(&(ctx->tokens), &(ctx->state_tree))) != eCompileOk) {
        regexCompileCtxCleanup(ctx);
        return ctx->status;
    }

    // At this point, we have a valid NFA form of the regular expression. We
    // can use it directly, or transform it into the compact VM form.

    /*
    if((result.status= regexNFAToVMBytecode(&result)) != eCompileOk) {
        regexCompileCtxCleanup(&result);
        return result;
    }
     */

    regexCompileCtxCleanup(ctx);

    return ctx->status;
}
