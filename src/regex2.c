#ifndef _MOJO_REGEX_HEADER_
#define _MOJO_REGEX_HEADER_

#define MOJO_REGEX_TEST_MAIN
#define MOJO_REGEX_COMPILE_IMPLEMENTATION
#define MOJO_REGEX_VM_SOURCE_GENERATION
#define MOJO_REGEX_VM_DEBUG
#define MOJO_REGEX_EVALUATE_IMPLEMENTATION
#define MOJO_REGEX_UNICODE
#define MOJO_REGEX_IMPLEMENTATION

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MOJO_REGEX_DEBUG_INTERNAL_ERRORS

#ifdef MOJO_REGEX_DEBUG_INTERNAL_ERRORS
#   define INTERNAL_ERROR(msg)  fprintf(stderr, "ERROR [%s] in %s @ %d\n", msg, __FUNCTION__, __LINE__)
#else
#   define INTERNAL_ERROR(msg)
#endif

/////////////////////////////////////////////////////////////////////////////
// Macro definition sanity checks
/////////////////////////////////////////////////////////////////////////////

#if defined(MOJO_REGEX_VM_SOURCE_GENERATION) && !defined(MOJO_REGEX_COMPILE_IMPLEMENTATION)
#   define MOJO_REGEX_COMPILE_IMPLEMENTATION
#endif

#if defined(MOJO_REGEX_COMPILE_IMPLEMENTATION) || defined(MOJO_REGEX_VM_DEBUG)
#   define MOJO_REGEX_COMMON_DEBUG
#endif

/////////////////////////////////////////////////////////////////////////////
// Declarations
/////////////////////////////////////////////////////////////////////////////

typedef struct regex_vm_s regex_vm_t;
struct regex_vm_s {
    // TODO
};

// Global memory allocator prototypes. If you change these, be sure and do so
// BEFORE calling any API functions, or memory management may end up in an
// unstable state. Any caller provided functions are expected to clear any
// allocated memory to 0.
typedef void *(*regexMemAllocator_t)(size_t size, void *ctx);
typedef void (*regexMemDeallocator_t)(void *ptr, void *ctx);
typedef void *(*regexMemReallocator_t)(void *ptr, size_t old_size, size_t new_size, void *ctx);

void regexSetMemoryAllocators(regexMemAllocator_t alloc,
                              regexMemDeallocator_t dealloc,
                              regexMemReallocator_t re_alloc,
                              void *context);

typedef enum {
    // The first tokens map directly to VM opcodes
    eTokenCharLiteral,      // 0
    eTokenCharClass,
    eTokenStringLiteral,
    eTokenCharAny,
    eTokenMatch,
    eTokenSplit,
    eTokenJmp,
    eTokenSave,
    eTokenUtf8Class,
    eTokenCall,
    eTokenReturn,           // 10
    eTokenByte,
    eTokenAssertion,
#ifdef MOJO_REGEX_COMPILE_IMPLEMENTATION
    // The remaining tokens are abstractions, and reduce down to the above VM opcodes
    eTokenConcatenation,
    eTokenAlternative,
    eTokenZeroOrOne,
    eTokenZeroOrMany,
    eTokenOneOrMany,
    eTokenRange,
    eTokenSubExprStart,
    eTokenSubExprEnd,       // 20
#endif // MOJO_REGEX_COMPILE_IMPLEMENTATION
    eTokenUnknown // <-- this should always be the last token enum
} eRegexToken_t;

#ifdef MOJO_REGEX_COMPILE_IMPLEMENTATION

typedef struct regex_token_s regex_token_t;
struct regex_token_s {
    eRegexToken_t tokenType;
};

typedef struct regex_build_ctx_s regex_build_ctx_t;
struct regex_build_ctx_s {
    // TODO
};

typedef enum {
    ePriorityNone,
    ePriorityLow,
    ePriorityMedium,
    ePriorityHigh
} eRegexTokenPriority_t;

typedef enum {
    eReTokNotTerminal = 0,
    eReTokTerminal,
    eReTokPreceeding,
    eReTokNotPreceeding
} eRegexTokenTerminality_t;

typedef void (*regexMetaTokenEmit_t)(FILE *fp, regex_build_ctx_t *build_ctx, regex_token_t *token);
typedef void (*regexMetaPatternEmit_t)(FILE *fp, regex_build_ctx_t *build_ctx, regex_token_t *token);
#endif // MOJO_REGEX_COMPILE_IMPLEMENTATION
#ifdef MOJO_REGEX_COMMON_DEBUG
typedef void (*regexMetaVMInstrEmit_t)(FILE *fp, regex_vm_t *vm, int opcode,
                                       unsigned int operand_a, unsigned int operand_b);

typedef struct regex_token_detail_s regex_token_detail_t;
struct regex_token_detail_s {
    eRegexToken_t token;
    const char *name;
    const char *instr;
    regexMetaVMInstrEmit_t emitVMInstr;
#endif // MOJO_REGEX_COMMON_DEBUG
#ifdef MOJO_REGEX_COMPILE_IMPLEMENTATION
    regexMetaTokenEmit_t emitToken;
    regexMetaPatternEmit_t emitPattern;
    eRegexTokenPriority_t priority;
    eRegexTokenTerminality_t terminal;
    int arity;
};

void _reMetaTokEmit_eTokenCharLiteral(FILE __unused *fp, regex_build_ctx_t __unused *build_ctx,
                                      regex_token_t __unused *token) {}


#define RE_META_TOK_EMIT_N(token)   NULL
#define RE_META_TOK_EMIT_Y(token)   _reMetaTokEmit_ ## token
#define RE_META_TOK_EMIT(unique,token)  RE_META_TOK_EMIT_ ## unique(token)

#define RE_META_PAT_EMIT_N(token)   NULL
#define RE_META_PAT_EMIT_Y(token)   _reMetaPatEmit_ ## token
#define RE_META_PAT_EMIT(unique,token)  RE_META_PAT_EMIT_ ## unique(token)

#define RE_META_VM_EMIT_N(token)    NULL
#define RE_META_VM_EMIT_Y(token)    _reMetaVMEmit_ ## token
#define RE_META_VM_EMIT(unique,token)  RE_META_VM_EMIT_ ## unique(token)

#define RE_TOK_DETAIL(token,instr,emitT,emitP,emitV,priority,terminal,arity) \
    {token, #token, \
    instr, \
    RE_META_TOK_EMIT(emitT,token), RE_META_PAT_EMIT(emitP,token), \
    RE_META_VM_EMIT(emitV,token), ePriority ## priority, terminal, arity}

#define RE_TOK_DETAIL_END RE_TOK_DETAIL(eTokenUnknown, "<unknown>", N, N, N, None, eReTokNotTerminal, 0)

//  Detail        Token                 VM Instr    Tok   Pat   VM    Priority  Terminal            Arity
//  Table         --------------------  ----------  ----  ----  ----  --------  --------            -----
regex_token_detail_t _regexTokenDetails[] = {
    RE_TOK_DETAIL(eTokenCharLiteral, "char", Y, N, N, None, eReTokTerminal, 1),
    RE_TOK_DETAIL(eTokenCharClass, "", N, N, N, None, eReTokNotTerminal, 0),
    RE_TOK_DETAIL(eTokenStringLiteral, "", N, N, N, None, eReTokNotTerminal, 0),
    RE_TOK_DETAIL(eTokenCharAny, "", N, N, N, None, eReTokNotTerminal, 0),
    RE_TOK_DETAIL(eTokenMatch, "", N, N, N, None, eReTokNotTerminal, 0),
    RE_TOK_DETAIL(eTokenSplit, "", N, N, N, None, eReTokNotTerminal, 0),
    RE_TOK_DETAIL(eTokenJmp, "", N, N, N, None, eReTokNotTerminal, 0),
    RE_TOK_DETAIL(eTokenSave, "", N, N, N, None, eReTokNotTerminal, 0),
    RE_TOK_DETAIL(eTokenUtf8Class, "", N, N, N, None, eReTokNotTerminal, 0),
    RE_TOK_DETAIL(eTokenCall, "", N, N, N, None, eReTokNotTerminal, 0),
    RE_TOK_DETAIL(eTokenReturn, "", N, N, N, None, eReTokNotTerminal, 0),
    RE_TOK_DETAIL(eTokenByte, "", N, N, N, None, eReTokNotTerminal, 0),
    RE_TOK_DETAIL(eTokenAssertion, "", N, N, N, None, eReTokNotTerminal, 0),
#ifdef MOJO_REGEX_COMPILE_IMPLEMENTATION
    RE_TOK_DETAIL(eTokenConcatenation, "", N, N, N, None, eReTokNotTerminal, 0),
    RE_TOK_DETAIL(eTokenAlternative, "", N, N, N, None, eReTokNotTerminal, 0),
    RE_TOK_DETAIL(eTokenZeroOrOne, "", N, N, N, None, eReTokNotTerminal, 0),
    RE_TOK_DETAIL(eTokenZeroOrMany, "", N, N, N, None, eReTokNotTerminal, 0),
    RE_TOK_DETAIL(eTokenOneOrMany, "", N, N, N, None, eReTokNotTerminal, 0),
    RE_TOK_DETAIL(eTokenRange, "", N, N, N, None, eReTokNotTerminal, 0),
    RE_TOK_DETAIL(eTokenSubExprStart, "", N, N, N, None, eReTokNotTerminal, 0),
    RE_TOK_DETAIL(eTokenSubExprEnd, "", N, N, N, None, eReTokNotTerminal, 0),
#endif // MOJO_REGEX_COMPILE_IMPLEMENTATION
    RE_TOK_DETAIL_END
};

#ifdef MOJO_REGEX_VM_SOURCE_GENERATION
#endif // MOJO_REGEX_VM_SOURCE_GENERATION

#endif // MOJO_REGEX_COMPILE_IMPLEMENTATION

#ifdef MOJO_REGEX_EVALUATE_IMPLEMENTATION
#endif // MOJO_REGEX_EVALUATE_IMPLEMENTATION

/////////////////////////////////////////////////////////////////////////////
// Implementation definition
/////////////////////////////////////////////////////////////////////////////

#ifdef MOJO_REGEX_IMPLEMENTATION

/////////////////////////////////////////////////////////////////////////////
// Delegated memory allocation handlers
/////////////////////////////////////////////////////////////////////////////

static void *_regexDefMemAllocator(size_t size, void __unused *ctx) {
    void *ptr = malloc(size);
    memset(ptr, 0, size);
    return ptr;
}

static void _regexDefMemDeallocator(void *ptr, void __unused *ctx) {
    free(ptr);
}

static void *_regexDefMemReallocator(void *ptr, size_t old_size,
                                     size_t new_size, void __unused *ctx) {
    ptr = realloc(ptr, new_size);
    if(new_size > old_size) {
        memset(((unsigned char *)ptr) + old_size, 0, new_size - old_size);
    }
    return ptr;
}

regexMemAllocator_t _regexAlloc = _regexDefMemAllocator;
regexMemDeallocator_t _regexDealloc = _regexDefMemDeallocator;
regexMemReallocator_t _regexRealloc = _regexDefMemReallocator;
void *_regexMemContext = NULL;

void regexSetMemoryAllocators(regexMemAllocator_t alloc,
                              regexMemDeallocator_t dealloc,
                              regexMemReallocator_t re_alloc,
                              void *context) {
    _regexAlloc = alloc;
    _regexDealloc = dealloc;
    _regexRealloc = re_alloc;
    _regexMemContext = context;
}

#define RE_STR_NULL_TERM -1

// Duplicate the str. If str == NULL, returns NULL. If len == RE_STR_NULL_TERM
// (-1), then the length is determined by strlen(). An additional byte, and
// a trailing null '\0' is applied to all duplicates.
static char *_regexStrdup(const char *str, int len) {
    char *ptr;

    if(len == RE_STR_NULL_TERM) {
        len = (int)strlen(str);
    }

    if((ptr = _regexAlloc(len + 1, _regexMemContext)) == NULL) {
        return NULL;
    }
    memcpy(ptr, str, len);
    ptr[len] = '\0';
    return ptr;
}

// Duplicate the str, including the prefix in the new string. If prefix is NULL,
// behaves identically to _regexStrdup(). If len == RE_STR_NULL_TERM (-1), then
// the length of str is detemined by strlen(). Note that prefix is expected to
// be null terminated, and len has no bearing on prefix. Space will be allocated
// for prefix, str, and an additional null terminator.
static char *_regexPrefixedStrdup(const char *prefix, const char *str, int len) {
    char *ptr;
    size_t pre_len;

    if(prefix == NULL) {
        return _regexStrdup(str, len);
    }
    if(len == RE_STR_NULL_TERM) {
        len = (int)strlen(str);
    }
    pre_len = strlen(prefix);
    if((ptr = _regexAlloc(len + pre_len + 1, _regexMemContext)) == NULL) {
        return NULL;
    }
    memcpy(ptr, prefix, pre_len);
    memcpy(ptr + pre_len, str, len);
    ptr[len + pre_len] = '\0';
    return ptr;
}

// Performs a comparison of str_a against str_b, with optionally specified lengths.
// If the string lengths are unequal, then the comparison will never much, but
// may still be used to order the strings. If either length == RE_STR_NULL_TERM
// (-1), then the associated string will be measured using strlen(). Comparison
// is performed using memcmp, allowing inline null characters in the comparison.
static int _regexStrncmp(const unsigned char *str_a, int len_a,
                         const unsigned char *str_b, int len_b) {
    if((str_a == NULL) || (str_b == NULL)) {
        if(str_a == str_b) {
            return 0;
        }
        return (str_a == NULL ? -1 : 1);
    }
    if(len_a == RE_STR_NULL_TERM) {
        len_a = (int)strlen((char *)str_a);
    }
    if(len_b == RE_STR_NULL_TERM) {
        len_b = (int)strlen((char *)str_b);
    }
    if(len_a != len_b) {
        return memcmp(str_a, str_b, ((len_a < len_b) ? len_a : len_b));
    }
    return memcmp(str_a, str_b, len_a);
}

// Functionally similar to _regexStrncmp, but prepends prefix to str_a. If
// prefix is NULL, behaves identically to _regexStrncmp.
static int _regexPrefixedStrncmp(const unsigned char *prefix,
                                 const unsigned char *str_a, int len_a,
                                 const unsigned char *str_b, int len_b) {
    size_t pre_len;

    if(prefix == NULL) {
        return _regexStrncmp(str_a, len_a, str_b, len_b);
    }
    if((str_a == NULL) || (str_b == NULL)) {
        if(str_a == str_b) {
            return 0;
        }
        return (str_a == NULL ? -1 : 1);
    }

    if(len_a == RE_STR_NULL_TERM) {
        len_a = (int)strlen((char *)str_a);
    }
    if(len_b == RE_STR_NULL_TERM) {
        len_b = (int)strlen((char *)str_b);
    }

    pre_len = strlen((char *)prefix);
    if((len_a + pre_len) != len_b) {
        return -1;
    }
    if(memcmp(prefix, str_b, pre_len) != 0) {
        return -1;
    }
    return memcmp(str_a, str_b + pre_len, len_a);
}

#ifdef MOJO_REGEX_COMPILE_IMPLEMENTATION
#endif // MOJO_REGEX_COMPILE_IMPLEMENTATION

#ifdef MOJO_REGEX_VM_SOURCE_GENERATION
#endif // MOJO_REGEX_VM_SOURCE_GENERATION

#ifdef MOJO_REGEX_EVALUATE_IMPLEMENTATION
#endif // MOJO_REGEX_EVALUATE_IMPLEMENTATION

#endif // MOJO_REGEX_IMPLEMENTATION

/////////////////////////////////////////////////////////////////////////////
// Main test stub
/////////////////////////////////////////////////////////////////////////////

#ifdef MOJO_REGEX_TEST_MAIN

int main(int argc, char *argv[]) {
    //const char *pattern = "";
    //const char *test = "";

    printf("Regex2 validation test stub\n");
    printf("===========================\n");

    // To silence the "unused function" editor warnings. Not neccessary for
    // normal usage, as the default have already been set.
    regexSetMemoryAllocators(_regexDefMemAllocator, _regexDefMemDeallocator,
                             _regexDefMemReallocator, NULL);

    _regexTokenDetails[0].emitToken(stdout,NULL,NULL);
    _regexTokenDetails[1].emitToken(stdout,NULL,NULL);
    return 0;
}

#endif // MOJO_REGEX_TEST_MAIN

#endif // _MOJO_REGEX_HEADER_
