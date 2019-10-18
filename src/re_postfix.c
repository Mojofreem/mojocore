#include <stdio.h>
#include <stdlib.h>
#include <string.h>


#include "re_postfix.h"
#include "re_vm.h"


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
    regexVMDestroy(vm);

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
