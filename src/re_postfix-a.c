#include <stdio.h>
#include <stdlib.h>
#include <string.h>


#include "re_postfix.h"


typedef enum {
    ePriorityNone,
    ePriorityLow,
    ePriorityMedium,
    ePriorityHigh
} eRegexTokenPriority;

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
    state->pc = -1;
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
        fragment->next = NULL;
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
                if(!regexOperatorZeroOrOneCreate(operands)) {
                    return eCompileOutOfMem;
                }
                break;

            case eTokenZeroOrMany:
                printf("Apply *\n");
                if(!regexOperatorZeroOrMoreCreate(operands)) {
                    return eCompileOutOfMem;
                }
                break;

            case eTokenOneOrMany:
                printf("Apply +\n");
                if(!regexOperatorOneOrMoreCreate(operands)) {
                    return eCompileOutOfMem;
                }
                break;

            case eTokenConcatenation:
                printf("Apply #\n");
                if(!regexOperatorConcatenationCreate(operands)) {
                    return eCompileOutOfMem;
                }
                break;

            case eTokenAlternative:
                printf("Apply |\n");
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

void regexNFANodePrint(regex_state_t *state, int pad);

eRegexCompileStatus regexShuntingYardFragment(regex_token_t **tokens, regex_fragment_t **root_stack, int sub_expression) {
    regex_token_t *token, *operators = NULL;
    regex_fragment_t *operands = NULL, *subexpr, *sidestack;
    eRegexCompileStatus status = eCompileOk;

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
                if(!regexOperatorSubexprCreate(&operands, (sub_expression - 1) * 2)) {
                    SET_YARD_RESULT(eCompileOutOfMem);
                }
                regexFragmentStackPush(&operands, sidestack);
                if(!regexOperatorSubexprCreate(&operands, ((sub_expression - 1) * 2) + 1)) {
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
                printf("Group close:\n");
                printf("    base: %d  %p\n", operands->state->opcode, operands->state);
                regexNFANodePrint(operands->state, 1);
                regexFragmentStackPush(root_stack, operands);
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

    return status;
}

void indent(int pad) {
    int k;

    for(k = 0; k < pad; k++) {
        printf("    ");
    }
}

void regexNFANodeClear(regex_state_t *state, int value) {
    if(state != NULL) {
        if(state->pc == value) {
            return;
        }
        state->pc = value;
        if(state->out_a != NULL) {
            regexNFANodeClear(state->out_a, value);
        }
        if(state->out_b != NULL) {
            regexNFANodeClear(state->out_b, value);
        }
    }
}

void regexNFANodePrint(regex_state_t *state, int pad) {
    if(state != NULL && state->pc == -2) {
        return;
    }
    indent(pad);
    printf("%p  ", state);
    if(state == NULL) {
        printf("NULL\n");
        return;
    }
    if(state->pc == -1) {
        state->pc = -2;
    }
    switch(state->opcode) {
        case eCharLiteral:
            printf("char(%c)\n", (state->c < 32 || state->c > 127) ? '-' : state->c);
            regexNFANodePrint(state->out_a, pad);
            break;
        case eStringLiteral:
            printf("string(\"%s\")\n", state->str);
            regexNFANodePrint(state->out_a, pad);
            break;
        case eCharClass:
            printf("class(");
            regexPrintCharClass(state->bitmap);
            printf(")\n");
            regexNFANodePrint(state->out_a, pad);
            break;
        case eCharAny:
            printf("anychar\n");
            regexNFANodePrint(state->out_a, pad);
            break;
        case eSave:
            printf("save(%d)\n", state->group);
            regexNFANodePrint(state->out_a, pad);
            break;
        case eSplit:
            printf("split %p, %p\n", state->out_a, state->out_b);
            regexNFANodePrint(state->out_a, pad + 1);
            regexNFANodePrint(state->out_b, pad + 1);
            break;
        case eMatch:
            printf("match\n");
            return;
        default:
            printf("UNKNOWN!\n");
            return;
    }
}

eRegexCompileStatus regexShuntingYard(regex_token_t **tokens, regex_state_t **nfa_stack) {
    regex_fragment_t *stack = NULL;
    eRegexCompileStatus status;

    printf("\n== Shunting yard ==============================\n\n");

#if 0
    status = regexShuntingYardFragment(tokens, &stack, -1);
    *nfa_stack = stack->state;

    regexNFANodeClear(stack->state, -3);
    regexNFANodeClear(stack->state, -1);
    printf("--------------------\n");
    regexNFANodePrint(stack->state, 0);
    printf("--------------------\n");
#endif

    return status;
}
