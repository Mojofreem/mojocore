#ifndef _MOJO_REGEX_HEADER_
#define _MOJO_REGEX_HEADER_

/*///////////////////////////////////////////////////////////////////////////

Regex VM Bytecode (v8 - in development)

    Each operation is encoded into a single 32 bit int value:

       +-------------------------------+-------------------------------+
       |  3                   2        '          1                    |
       |1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6'5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0|
       +-------------------------------+-------------------------------+
       |                              1'                               |
       |F E D C B A 9 8 7 6 5 4 3 2 1 0'F E D C B A 9 8 7 6 5 4 3 2 1 0|
       +---------------------------+---+-----------------------+-------+
       |         31 ... 18         |          17 ... 4         | 3...0 |
       +---------------------------+---------------------------+-------+
       |    14 bits (operand A)    |     14 bits (operand B)   |4bit op|
       +---------------------------+---------------------------+-------+

    Operators:               opcode     Operand A               Operand B
        eTokenCharLiteral       0       char to match           program counter
                                        inverse flag
        eTokenCharClass         1       class idx to match
        eTokenStringLiteral     2       str idx to match
        eTokenCharAny           3       dotall flag
        eTokenMatch             4       end of input flag
        eTokenSplit             5       program counter         program counter
        eTokenJmp               6       program counter
        eTokenSave              7       subexpression number    compound flag
        eTokenUtf8Class         8       leading low bytes       utf8 idx to match
                                        low byte count          inverse flag
        eTokenCall              9       program counter
        eTokenReturn            A
        eTokenByte              B
        eTokenAssertion         C       type specifier
        eTokenUtf8Literal       D       encoding, bits spread across operands
        <reserved>              E
        <reserved>              F

    Note about eTokenCharLiteral:

       +-------------------------------+-------------------------------+
       |  3                   2        '          1                    |
       |1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6'5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0|
       +-------------------------------+-------------------------------+
       |                              1'                               |
       |F E D C B A 9 8 7 6 5 4 3 2 1 0'F E D C B A 9 8 7 6 5 4 3 2 1 0|
       +-+---------+---------------+---+-----------------------+-------+
       | |         |   25 ... 18   |          17 ... 4         | 3...0 |
       +-+---------+---------------+---------------------------+-------+
       |I| unused  |      char     |   (opt) program counter   |4bit op|
       +-+---------+---------------+---------------------------+-------+

        Matches the character specified. If the inverse flag (I) is set, the
        logic is inverted, and the instruction matches any character EXCEPT the
        character specified. Operand B is optional, and, if specified, is the
        program counter to jmp to in the case of not match. If the value is
        0x3FFF, then the instruction reverts to default no match behaviour.
        Note that this instruction matches a single BYTE. For utf8 literals,
        use the eTokenUtf8Literal.

    Note about eTokenCharAny:
        In ASCII mode, this instruction matches any character _except_
        newline. In Unicode mode, this instruction matches a single unicode
        codepoint, which MAY match multiple bytes, but again, does NOT match
        newline. If the Operand A dotall flag is set, the instruction will ALSO
        match newline.

    Note about eTokenMatch:
        If the end of input flag is not set, the pattern matches at this
        instruction. If the end of input flag IS set, this ONLY matches if the
        evaluated text is at the end of input ('$' assertion operator)

    Note about eTokenSave:
        By default, this instruction captures the widest matching range for the
        internal expression, ie., "(fo.)+" evaluating "fooforfob" would capture
        "fooforfob" as a single value for the subexpression. If the compound
        flag is set, then in addition to this widest match behaviour, each
        individual occurrence of a match would be captured as well. For example,
        the pattern "(?*fo.)+" evaluating "fooforfob" would capture "fooforfob",
        as well as "foo", "for", and "fob" (1 primary, 3 sub captures)

    Note about eTokenUtf8Class:

       +-------------------------------+-------------------------------+
       |  3                   2        '          1                    |
       |1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6'5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0|
       +-------------------------------+-------------------------------+
       |                              1'                               |
       |F E D C B A 9 8 7 6 5 4 3 2 1 0'F E D C B A 9 8 7 6 5 4 3 2 1 0|
       +---+-----------+-----------+-+-+-----------------------+-------+
       |   |  29...24  |  23...18  | |        16 ... 4         | 3...0 |
       +---+-----------+-----------+-+-------------------------+-------+
       |Num|  midhigh  |  midlow   |I|     utf8 class index    |4bit op|
       +---+-----------+-----------+-+-------------------------+-------+

        This instruction represents the low bytes of a utf8 encoding (for 2, 3,
        and 4 byte encodings). For single byte encodings, a standard char class
        is used. num is the number of low bytes to match, with the lowest
        being the class bitmap range specified in operand B, and the midlow,
        and midhigh bytes stored in operand A. The low byte encoding is 6 bits
        per byte, with the 2 bit 0x80 prefix removed. If the inverse bit (I) is
        set, then the match logic is inverted for all bytes present, including
        the class bitmap (it is NOT pre-inverted).

        00 - 2 byte encoding (midlow, low)
        01 - 3 byte encoding (midhigh, midlow, low)
        10 - 4 byte encoding, seperate leading high byte (midhigh, midlow, low)
        11 - unused

    Note about eTokenCall:
        This instruction allows common repeating sequences to be factored out
        into a subroutine. The evaluation implementation is limited to an
        effective call depth of 5 (to limit thread overhead), so nested calls
        are limited. The initial design consideration for this feature was
        to reduce overhead when using unicode character classes, as they
        consist of a generated sub-pattern in and of themselves, due to the
        large and distributed range of branching byte sequence dependencies.

    Note about eTokenReturn:
        This instruction returns to the next consecutive instruction following
        a previously issued eTokenCall instruction. If no previous eTokenCall
        was issued, the instruction is considered a non match.

    Note about eTokenByte:
        This instruction explicitly matches any single byte. This differs
        from both eTokenCharAny in that it always matches newline, and always
        matches a single byte.

    Note about eTokenAssertion:
        This instruction is non-consuming, and merely checks the current
        evaluation state to ensure the specified condition is met. The type
        specifier details what condition is being checked:

        0 - start of line assertion, true if this is the first position in the
            line, either the start of the evaluated text, or immediately
            following a newline
        1 - end of line assertion, true if this is the last position in the line,
            either the end of the evaluated text, or immediately preceeding a
            newline. Note that this is a delayed evaluation, as the evaluator
            does not read ahead, so the actual assertion will be assessed on the
            next character read.
        2 - start of word assertion, true if this is the start of the evaluated
            text or the last character read was not a word character, and the
            next character read is a word character. Note that this is a 2 phase
            evaluation: the last character check is performed immediately, and
            the next character check is deferred to the next read.
        3 - end of word assertion, true if the last character read was a word
            character, and the next character read is not a word character or
            is the end of the evaluated text. Note that this is a 2 phase
            evaluation: the last character check is performed immediately, and
            the next character check is deferred to the next read.

    Note about eTokenUtf8Literal:

       +-------------------------------+-------------------------------+
       |  3                   2        '          1                    |
       |1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6'5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0|
       +-------------------------------+-------------------------------+
       |                              1'                               |
       |F E D C B A 9 8 7 6 5 4 3 2 1 0'F E D C B A 9 8 7 6 5 4 3 2 1 0|
       +---+-+-------+-----------------+-----------------------+-------+
       |   | |       |                24 ... 4                 | 3...0 |
       +---+-+-------+-----------------------------------------+-------+
       |Num|I|unused |                codepoint                |4bit op|
       +---+-+-------+-----------------------------------------+-------+

        This instruction explicitly matches the encoded utf8 codepoint. The
        high two bits (Num) represent the byte encoding (0 - 1 byte, 1 - 2
        byte, 2 - 3 byte, and 3 - 4 byte). The inverse bit (I) will match any
        character other than the one specified.

///////////////////////////////////////////////////////////////////////////*/

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
#   define MOJO_REGEX_COMMON_IMPLEMENTATION
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
    eTokenUtf8Literal,
#ifdef MOJO_REGEX_COMPILE_IMPLEMENTATION
    // The remaining tokens are abstractions, and reduce down to the above VM opcodes
    eTokenConcatenation,
    eTokenAlternative,
    eTokenZeroOrOne,
    eTokenZeroOrMany,
    eTokenOneOrMany,
    eTokenRange,
    eTokenSubExprStart,     // 20
    eTokenSubExprEnd,
#endif // MOJO_REGEX_COMPILE_IMPLEMENTATION
    eTokenUnknown // <-- this should always be the last token enum
} eRegexToken_t;

#ifdef MOJO_REGEX_COMPILE_IMPLEMENTATION

typedef enum {
    eCompileOk,
    eCompileCharClassRangeIncomplete,
    eCompileCharClassIncomplete,
    eCompileEscapeCharIncomplete,
    eCompileUnknownUnicodeClass,
    eCompileInvalidEscapeChar,
    eCompileMalformedSubExprName,
    eCompileUnsupportedMeta,
    eCompileOutOfMem,
    eCompileMissingOperand,
    eCompileMissingSubexprStart,
    eCompileConflictingAttrs,
    eCompileUnknownSubroutine,
    eCompileSubroutineNameCollision,
    eCompileInternalError
} eRegexCompileStatus_t;

typedef enum {
    ePhaseTokenize,
    ePhaseNFAGraph,
    ePhaseVMGen,
    ePhaseComplete
} eRegexCompilePhase_t;

int regexEscapeStrLen(const char *str, int len);
void regexEscapedStrEmit(FILE *fp, const char *str, int len);
void regexCharLiteralEmit(FILE *fp, int c);

#define RE_FLAG_DOT_ALL     0x1u

// Token flags //////////////////////////////////////////////////////////////

#define RE_TOK_FLAG_INVERT                  0x01u   // eTokenCharLiteral, eTokenUtf8Class, eTokenUtf8Literal
#define RE_TOK_FLAG_DOT_ALL                 0x01u   // eTokenCharAny
#define RE_TOK_FLAG_END_OF_INPUT            0x01u   // eTokenMatch
#define RE_TOK_FLAG_COMPOUND                0x01u   // eTokenSubExprStart, eTokenSave
#define RE_TOK_FLAG_NO_CAPTURE              0x02u   // eTokenSubExprStart
#define RE_TOK_FLAG_SUBROUTINE              0x04u   // eTokenSubExprStart

#define RE_TOK_FLAG_ASSERT_START_OF_LINE    0x01u   // eTokenAssertion
#define RE_TOK_FLAG_ASSERT_END_OF_LINE      0x02u   // eTokenAssertion
#define RE_TOK_FLAG_ASSERT_START_OF_WORD    0x03u   // eTokenAssertion
#define RE_TOK_FLAG_ASSERT_END_OF_WORD      0x04u   // eTokenAssertion

#define RE_TOK_FLAG_UTF8_MASK               0x06u   // eTokenUtf8Class
#define RE_TOK_FLAG_UTF8_2_BYTES            0x02u   // eTokenUtf8Class
#define RE_TOK_FLAG_UTF8_3_BYTES            0x04u   // eTokenUtf8Class
#define RE_TOK_FLAG_UTF8_4_BYTES            0x06u   // eTokenUtf8Class

// VM instruction flags /////////////////////////////////////////////////////

// Map instruction bits to operand bit positions
#define VM_OP_A_BIT(bit)    ((0x1u << (bit)) >> 18u)
#define VM_OP_B_BIT(bit)    ((0x1u << (bit)) >> 4u)

#define RE_VM_FLAG_CHAR_INVERT          VM_OP_A_BIT(31)
#define RE_VM_FLAG_CHARANY_DOTALL       0x1
#define RE_VM_FLAG_MATCH_END_OF_INPUT   0x1
#define RE_VM_FLAG_SAVE_COMPOUND        0x1
#define RE_VM_FLAG_UTF8_CLASS_INVERT    VM_OP_B_BIT(17)
#define RE_VM_FLAG_ASSERT_START_OF_LINE 0x0
#define RE_VM_FLAG_ASSERT_END_OF_LINE   0x1
#define RE_VM_FLAG_ASSERT_START_OF_WORD 0x2
#define RE_VM_FLAG_ASSERT_END_OF_WORD   0x3
#define RE_VM_FLAG_UTF8_LITERAL_INVERT  VM_OP_A_BIT(29)

typedef enum {
    eRePtrOutA,
    eRePtrOutB
} eRePtrListType_t;

typedef struct regex_token_s regex_token_t;

typedef struct regex_ptrlist_s regex_ptrlist_t;
struct regex_ptrlist_s {
    eRePtrListType_t type;
    regex_token_t *token;
    regex_ptrlist_t *next;
};

struct regex_token_s {
    eRegexToken_t tokenType;
    int c; // eTokenCharLiteral, eTokenUtf8Literal, eTokenUtf8Class
    union {
        char *str; // eTokenStrLiteral
        unsigned int *bitmap; // eTokenCharClass, eTokenUtf8Class
        int group; // eTokenSubExprStart, eTokenSave
        int min; // eTokenRange
    };
    union {
        int len; // eTokenStrLiteral - str length, to allow embedded \0 chars
        int max; // eTokenRange
        struct {
            unsigned short flags; // eTokenChar, eTokenCharAny, eTokenUtf8Class,
                // eTokenUtf8Literal, eTokenMatch, eTokenSubExprStart, eTokenSave,
                // eTokenAssertion
            union {
                short sub_index; // eTokenCall, eTokenSubExprStart
                short lead_count; // eTokenUtf8Class
            };
            unsigned short lead_bits; // eTokenUtf8Class
        };
    };
    int pc; // program counter
    regex_ptrlist_t *ptrlist;
    regex_token_t *out_a;
    regex_token_t *out_b;
    regex_token_t *next;
};

typedef struct regex_subroutine_s regex_subroutine_t;
struct regex_subroutine_s {
    int id;
    char *name;
    char *alias;
    char *pattern;
    size_t len;
    regex_token_t *tokens;
    int infixed;
    int program_counter;
    regex_subroutine_t *next;
};

typedef struct regex_subroutine_index_s regex_subroutine_index_t;
struct regex_subroutine_index_s {
    regex_subroutine_t *subroutines;
    int next_id;
};

typedef struct regex_str_entry_s regex_str_entry_t;
struct regex_str_entry_s {
    unsigned char *str;
    int len;
    regex_str_entry_t *next;
};

typedef struct regex_str_table_s regex_str_table_t;
struct regex_str_table_s {
    regex_str_entry_t *entries;
    int size;
};

typedef struct regex_utf8_range_s regex_utf8_range_t;
struct regex_utf8_range_s {
    int start;
    int end;
    regex_utf8_range_t *next;
};

typedef struct regex_build_ctx_s regex_build_ctx_t;
struct regex_build_ctx_s {
    regex_token_t *token_pool;
    regex_ptrlist_t *ptrlist_pool;
    regex_utf8_range_t *utf8_range_pool;

    regex_str_table_t string_table;
    regex_str_table_t class_table;
    regex_str_table_t utf8_table;
    regex_str_table_t group_table;

    regex_subroutine_index_t subroutine_index;

    unsigned int flags;
    int group_count;
};

typedef enum {
    eReBuildSrcAPI,
    eReBuildSrcInternal,
    eReBuildSrcClass,
    eReBuildSrcSub
} eRegexBuildSource_t;

typedef struct regex_pattern_s regex_pattern_t;
struct regex_pattern_s {
    eRegexBuildSource_t source;
    const char *pattern;
    const char *base_pattern;
    int pos;
    int len;
    regex_pattern_t *next;
};

typedef struct regex_build_s regex_build_t;
struct regex_build_s {
    regex_build_ctx_t *build_ctx;
    regex_token_t *tokens;
    eRegexCompileStatus_t status;
    regex_pattern_t *pattern;
};

regex_build_ctx_t *regexBuildContextCreate(void);
regex_build_ctx_t *regexBuildContextInit(regex_build_ctx_t *build_ctx);
void regexBuildContextDestroy(regex_build_ctx_t *build_ctx);

regex_build_t *regexBuilderCreate(regex_build_ctx_t *build_ctx);
regex_build_t *regexBuilderInit(regex_build_ctx_t *build_ctx, regex_build_t *build);
void regexBuilderDestroy(regex_build_t *build);

regex_pattern_t *regexPatternCreate(eRegexBuildSource_t source, const char *pat_str, int pat_len);
regex_pattern_t *regexPatternInit(regex_pattern_t *pattern, eRegexBuildSource_t source, const char *pat_str, int pat_len);
void regexPatternDestroy(regex_pattern_t *pattern);

int regexBuildPatternPush(regex_build_t *build, regex_pattern_t *pattern);
regex_pattern_t *regexBuildPatternPop(regex_build_t *build);

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
#endif // MOJO_REGEX_COMPILE_IMPLEMENTATION
#ifdef MOJO_REGEX_COMMON_IMPLEMENTATION
typedef void (*regexMetaVMInstrEmit_t)(FILE *fp, regex_vm_t *vm, int opcode,
                                       unsigned int operand_a, unsigned int operand_b);

typedef struct regex_token_detail_s regex_token_detail_t;
struct regex_token_detail_s {
    eRegexToken_t token;
    const char *name;
    const char *instr;
    regexMetaVMInstrEmit_t emitVMInstr;
#endif // MOJO_REGEX_COMMON_IMPLEMENTATION
#ifdef MOJO_REGEX_COMPILE_IMPLEMENTATION
    regexMetaTokenEmit_t emitToken;
    eRegexTokenPriority_t priority;
    eRegexTokenTerminality_t terminal;
    int arity;
#endif // MOJO_REGEX_COMPILE_IMPLEMENTATION
#ifdef MOJO_REGEX_COMMON_IMPLEMENTATION
};
#endif // MOJO_REGEX_COMMON_IMPLEMENTATION
#ifdef MOJO_REGEX_COMPILE_IMPLEMENTATION

#define RE_META_TOK_EMIT_N(token)   NULL
#define RE_META_TOK_EMIT_Y(token)   _reMetaTokEmit_ ## token
#define RE_META_TOK_EMIT(unique,token)  RE_META_TOK_EMIT_ ## unique(token)

// token emitters ///////////////////////////////////////////////////////////

void _reMetaTokEmit_eTokenCharLiteral(FILE *fp, regex_build_ctx_t *build_ctx, regex_token_t *token) {

}

void _reMetaTokEmit_eTokenCharClass(FILE *fp, regex_build_ctx_t *build_ctx, regex_token_t *token) {

}

void _reMetaTokEmit_eTokenStringLiteral(FILE *fp, regex_build_ctx_t *build_ctx, regex_token_t *token) {

}

void _reMetaTokEmit_eTokenCharAny(FILE *fp, regex_build_ctx_t *build_ctx, regex_token_t *token) {

}

void _reMetaTokEmit_eTokenMatch(FILE *fp, regex_build_ctx_t *build_ctx, regex_token_t *token) {

}

void _reMetaTokEmit_eTokenSplit(FILE *fp, regex_build_ctx_t *build_ctx, regex_token_t *token) {

}

void _reMetaTokEmit_eTokenJmp(FILE *fp, regex_build_ctx_t *build_ctx, regex_token_t *token) {

}

void _reMetaTokEmit_eTokenSave(FILE *fp, regex_build_ctx_t *build_ctx, regex_token_t *token) {

}

void _reMetaTokEmit_eTokenUtf8Class(FILE *fp, regex_build_ctx_t *build_ctx, regex_token_t *token) {

}

void _reMetaTokEmit_eTokenCall(FILE *fp, regex_build_ctx_t *build_ctx, regex_token_t *token) {

}

void _reMetaTokEmit_eTokenReturn(FILE *fp, regex_build_ctx_t *build_ctx, regex_token_t *token) {

}

void _reMetaTokEmit_eTokenByte(FILE *fp, regex_build_ctx_t *build_ctx, regex_token_t *token) {

}

void _reMetaTokEmit_eTokenAssertion(FILE *fp, regex_build_ctx_t *build_ctx, regex_token_t *token) {

}

void _reMetaTokEmit_eTokenUtf8Literal(FILE *fp, regex_build_ctx_t *build_ctx, regex_token_t *token) {

}

void _reMetaTokEmit_eTokenAlternative(FILE *fp, regex_build_ctx_t *build_ctx, regex_token_t *token) {

}

void _reMetaTokEmit_eTokenZeroOrOne(FILE *fp, regex_build_ctx_t *build_ctx, regex_token_t *token) {

}

void _reMetaTokEmit_eTokenZeroOrMany(FILE *fp, regex_build_ctx_t *build_ctx, regex_token_t *token) {

}

void _reMetaTokEmit_eTokenOneOrMany(FILE *fp, regex_build_ctx_t *build_ctx, regex_token_t *token) {

}

void _reMetaTokEmit_eTokenRange(FILE *fp, regex_build_ctx_t *build_ctx, regex_token_t *token) {

}

void _reMetaTokEmit_eTokenSubExprStart(FILE *fp, regex_build_ctx_t *build_ctx, regex_token_t *token) {

}

void _reMetaTokEmit_eTokenSubExprEnd(FILE *fp, regex_build_ctx_t *build_ctx, regex_token_t *token) {

}

#endif // MOJO_REGEX_COMPILE_IMPLEMENTATION
#ifdef MOJO_REGEX_COMMON_IMPLEMENTATION

#define RE_META_VM_EMIT_N(token)    NULL
#define RE_META_VM_EMIT_Y(token)    _reMetaVMEmit_ ## token
#define RE_META_VM_EMIT(unique,token)  RE_META_VM_EMIT_ ## unique(token)

void _reMetaVMEmit_eTokenCharLiteral(FILE *fp, regex_vm_t *vm, int opcode, unsigned int operand_a, unsigned int operand_b) {

}

void _reMetaVMEmit_eTokenCharClass(FILE *fp, regex_vm_t *vm, int opcode, unsigned int operand_a, unsigned int operand_b) {

}

void _reMetaVMEmit_eTokenStringLiteral(FILE *fp, regex_vm_t *vm, int opcode, unsigned int operand_a, unsigned int operand_b) {

}

void _reMetaVMEmit_eTokenCharAny(FILE *fp, regex_vm_t *vm, int opcode, unsigned int operand_a, unsigned int operand_b) {

}

void _reMetaVMEmit_eTokenMatch(FILE *fp, regex_vm_t *vm, int opcode, unsigned int operand_a, unsigned int operand_b) {

}

void _reMetaVMEmit_eTokenSplit(FILE *fp, regex_vm_t *vm, int opcode, unsigned int operand_a, unsigned int operand_b) {

}

void _reMetaVMEmit_eTokenJmp(FILE *fp, regex_vm_t *vm, int opcode, unsigned int operand_a, unsigned int operand_b) {

}

void _reMetaVMEmit_eTokenSave(FILE *fp, regex_vm_t *vm, int opcode, unsigned int operand_a, unsigned int operand_b) {

}

void _reMetaVMEmit_eTokenUtf8Class(FILE *fp, regex_vm_t *vm, int opcode, unsigned int operand_a, unsigned int operand_b) {

}

void _reMetaVMEmit_eTokenCall(FILE *fp, regex_vm_t *vm, int opcode, unsigned int operand_a, unsigned int operand_b) {

}

void _reMetaVMEmit_eTokenAssertion(FILE *fp, regex_vm_t *vm, int opcode, unsigned int operand_a, unsigned int operand_b) {

}

void _reMetaVMEmit_eTokenUtf8Literal(FILE *fp, regex_vm_t *vm, int opcode, unsigned int operand_a, unsigned int operand_b) {

}


// eTokenReturn and eTokenByte have no additional VM metadata to display

#ifndef MOJO_REGEX_COMPILE_IMPLEMENTATION
#   define RE_TOK_DETAIL(token,instr,emitV,emitT,priority,terminal,arity) \
        {token, #token, instr, RE_META_VM_EMIT(emitV,token)}
#else // MOJO_REGEX_COMPILE_IMPLEMENTATION
#   define RE_TOK_DETAIL(token,instr,emitV,emitT,priority,terminal,arity) \
        {token, #token, instr, RE_META_VM_EMIT(emitV,token), \
        RE_META_TOK_EMIT(emitT,token), \
        ePriority ## priority, terminal, arity}
#endif // MOJO_REGEX_COMPILE_IMPLEMENTATION

#define RE_TOK_DETAIL_END RE_TOK_DETAIL(eTokenUnknown, "<unknown>", N, N, None, eReTokNotTerminal, 0)

//  Detail        Token                 VM Instr      VM    Tok   Priority  Terminal            Arity
//  Table         --------------------  ------------  ----  ----  --------  --------            -----
regex_token_detail_t _regexTokenDetails[] = {
    RE_TOK_DETAIL(eTokenCharLiteral,    "char",       Y,    Y,    None,     eReTokTerminal,     1),
    RE_TOK_DETAIL(eTokenCharClass,      "class",      Y,    Y,    None,     eReTokTerminal,     0),
    RE_TOK_DETAIL(eTokenStringLiteral,  "string",     Y,    Y,    None,     eReTokTerminal,     0),
    RE_TOK_DETAIL(eTokenCharAny,        "anychar",    Y,    Y,    None,     eReTokTerminal,     0),
    RE_TOK_DETAIL(eTokenMatch,          "match",      Y,    Y,    None,     eReTokNotTerminal,  0),
    RE_TOK_DETAIL(eTokenSplit,          "split",      Y,    N,    None,     eReTokNotTerminal,  0),
    RE_TOK_DETAIL(eTokenJmp,            "jmp",        Y,    N,    None,     eReTokNotTerminal,  0),
    RE_TOK_DETAIL(eTokenSave,           "save",       Y,    N,    None,     eReTokNotTerminal,  0),
    RE_TOK_DETAIL(eTokenUtf8Class,      "utf8class",  Y,    Y,    None,     eReTokTerminal,     0),
    RE_TOK_DETAIL(eTokenCall,           "call",       Y,    Y,    None,     eReTokTerminal,     0),
    RE_TOK_DETAIL(eTokenReturn,         "return",     N,    N,    None,     eReTokTerminal,     0),
    RE_TOK_DETAIL(eTokenByte,           "byte",       N,    Y,    None,     eReTokTerminal,     0),
    RE_TOK_DETAIL(eTokenAssertion,      "assertion",  Y,    Y,    None,     eReTokTerminal,     0),
    RE_TOK_DETAIL(eTokenUtf8Literal,    "utf8literal",Y,    Y,    None,     eReTokTerminal,     0),
#endif // MOJO_REGEX_COMMON_IMPLEMENTATION
#ifdef MOJO_REGEX_COMPILE_IMPLEMENTATION
    RE_TOK_DETAIL(eTokenConcatenation,  NULL,         N,    N,    Medium,   eReTokNotTerminal,  2),
    RE_TOK_DETAIL(eTokenAlternative,    NULL,         N,    Y,    Medium,   eReTokNotTerminal,  2),
    RE_TOK_DETAIL(eTokenZeroOrOne,      NULL,         N,    Y,    High,     eReTokPreceeding,   1),
    RE_TOK_DETAIL(eTokenZeroOrMany,     NULL,         N,    Y,    High,     eReTokPreceeding,   1),
    RE_TOK_DETAIL(eTokenOneOrMany,      NULL,         N,    Y,    High,     eReTokPreceeding,   1),
    RE_TOK_DETAIL(eTokenRange,          NULL,         N,    Y,    High,     eReTokPreceeding,   1),
    RE_TOK_DETAIL(eTokenSubExprStart,   NULL,         N,    Y,    Low,      eReTokNotPreceeding,0),
    RE_TOK_DETAIL(eTokenSubExprEnd,     NULL,         N,    Y,    Low,      eReTokPreceeding,   0),
#endif // MOJO_REGEX_COMPILE_IMPLEMENTATION
#ifdef MOJO_REGEX_COMMON_IMPLEMENTATION
    RE_TOK_DETAIL_END
};
#endif // MOJO_REGEX_COMMON_IMPLEMENTATION
#ifdef MOJO_REGEX_COMPILE_IMPLEMENTATION

// Determines whether a given token is a terminal operand. Used when comparing
// two adjacent tokens, the preceeding flag indicates whether the token being
// checked is leftmost.
int regexTokenIsTerminal(regex_token_t *token, int preceeding) {
    return ((_regexTokenDetails[token->tokenType].terminal == eReTokTerminal) ||
            (preceeding && (_regexTokenDetails[token->tokenType].terminal == eReTokPreceeding)) ||
            (!preceeding && (_regexTokenDetails[token->tokenType].terminal == eReTokNotPreceeding)));
}

// Token priority when applying operators to operands in the shunting yard
eRegexTokenPriority_t regexGetTokenTypePriority(eRegexToken_t tokenType) {
    return _regexTokenDetails[tokenType].priority;
}

// Operator arity (number of tokens that the operator functions on) in the
// shunting yard
int regexGetOperatorArity(regex_token_t *token) {
    return _regexTokenDetails[token->tokenType].arity;
}

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

/////////////////////////////////////////////////////////////////////////////
// String table management and data reproduction
/////////////////////////////////////////////////////////////////////////////

// Ensure that only a single entry for the data exists. When adding, if the
// data already exists in the table, then do not add a new entry.
#define RE_STR_TABLE_DEDUPE      0x01u

// Retain the data provided to the table. The table retains ownership of the
// pointer, and will free it upon removal.
#define RE_STR_TABLE_RETAIN_PTR    0x02u

static int _regexStrTableEntryAdd(regex_str_table_t *table, size_t entry_size,
                                  unsigned char *entry_data, unsigned int flags,
                                  int *deduped) {
    regex_str_entry_t *entry, *walk;
    int index;

    if(entry_size == RE_STR_NULL_TERM) {
        entry_size = ((entry_data != NULL) ? strlen((char *)entry_data) : 0);
    }
    if(flags & RE_STR_TABLE_DEDUPE) {
        for(index = 0, entry = table->entries; entry != NULL; entry = entry->next, index++) {
            if(!_regexStrncmp(entry_data, entry->len, entry->str, entry_size)) {
                break;
            }
        }
        if(entry != NULL) {
            if(deduped != NULL) {
                *deduped = 1;
            }
            return index;
        }
    }
    if((entry = _regexAlloc(sizeof(regex_str_entry_t), _regexMemContext)) == NULL) {
        return -1;
    }
    entry->len = entry_size;
    if((flags & RE_STR_TABLE_RETAIN_PTR) || (entry_data == NULL)) {
        entry->str = entry_data;
    } else {
        if((entry->str = (unsigned char *)_regexStrdup((char *)entry_data, entry_size)) == NULL) {
            return -1;
        }
    }
    if(table->entries == NULL) {
        table->entries = entry;
    } else {
        for(walk = table->entries; walk->next != NULL; walk = walk->next);
        walk->next = entry;
    }
    table->size++;
    return table->size - 1;
}

static const unsigned char *_regexStrTableEntryGet(regex_str_table_t *table, int index, int *len) {
    regex_str_entry_t *walk;

    if((index < 0) || (index >= table->size)) {
        return NULL;
    }
    for(walk = table->entries; index > 0; walk = walk->next, index--);
    if(len != NULL) {
        *len = walk->len;
    }
    return walk->str;
}

static void _regexStrTableFreeAll(regex_str_table_t *table) {
    regex_str_entry_t *walk, *next;

    for(walk = table->entries; walk != NULL; walk = next) {
        next = walk->next;
        if(walk->str != NULL) {
            _regexDealloc(walk->str, _regexMemContext);
        }
        _regexDealloc(walk, _regexMemContext);
    }
    table->entries = NULL;
    table->size = 0;
}

/////////////////////////////////////////////////////////////////////////////
// Character class registration functions
/////////////////////////////////////////////////////////////////////////////

// Forward declarations of the unicode property classes
const char _uax_db_Mark[];
const char _uax_db_Number[];
const char _uax_db_Punctuation[];
const char _uax_db_Separator[];
const char _uax_db_Uppercase_Letter[];
const char _uax_db_Lowercase_Letter[];
const char _uax_db_Letter[];

typedef struct regex_char_class_s regex_char_class_t;
struct regex_char_class_s {
    const char *alias;
    const char *name;
    const char *class_string;
    regex_char_class_t *next;
};

regex_char_class_t _regex_char_class_default_import_table[] = {
        {"M", "Mark", _uax_db_Mark, NULL},
        {"N", "Number", _uax_db_Number, NULL},
        {"P", "Punctuation", _uax_db_Punctuation, NULL},
        {"Z", "Separator", _uax_db_Separator, NULL},
        {"Lu", "Uppercase_Letter", _uax_db_Uppercase_Letter, NULL},
        {"Ll", "Lowercase_Letter", _uax_db_Lowercase_Letter, NULL},
        {"L", "Letter", _uax_db_Letter, NULL},
        {NULL, NULL, NULL, NULL}
};

regex_char_class_t _subroutine_test = {
        NULL, "test",
        "A-Za-z\\uA000-\\uA0FF",
        //"A-Za-z\\u0123-\\u0138\\uA000-\\uA0FF",
        NULL
};

static int _regex_unicode_table_initialized = 0;
regex_char_class_t *_regex_unicode_charclass_table = NULL;

int regexRegUnicodeCharClassAdd(regex_char_class_t *utf8class) {
    regex_char_class_t *walk;

    if((utf8class == NULL) || (utf8class->class_string == NULL)) {
        return 0;
    }

    for(walk = _regex_unicode_charclass_table; walk != NULL; walk = walk->next) {
        if((utf8class->alias != NULL) && (walk->alias != NULL) &&
           (!strcmp(utf8class->alias, walk->alias))) {
            // A class with the same abbreviation has already been registered
            return 0;
        }
        if((utf8class->name != NULL) && (walk->name != NULL) &&
           (!strcmp(utf8class->name, walk->name))) {
            // A class with the same property has already been registered
            return 0;
        }
    }

    utf8class->next = _regex_unicode_charclass_table;
    _regex_unicode_charclass_table = utf8class;
    return 1;
}

int regexRegUnicodeCharClassAddTable(regex_char_class_t *utf8class) {
    int failed = 0;

    for(; (utf8class != NULL) && (utf8class->class_string != NULL); utf8class++) {
        if(!regexRegUnicodeCharClassAdd(utf8class)) {
            failed++;
        }
    }
    return failed ? 0 : 1;
}

static int _regexRegUnicodeInitializeTable(void) {
    if(_regex_unicode_table_initialized) {
        return 1;
    }
    return regexRegUnicodeCharClassAddTable(_regex_char_class_default_import_table);
}

static const char *_regexRegUnicodeCharClassGet(const char *classId, int len) {
    regex_char_class_t *walk;

    if(len <= 0) {
        len = (int)strlen(classId);
    }

    for(walk = _regex_unicode_charclass_table; walk != NULL; walk = walk->next) {
        if((walk->alias != NULL) && (strlen(walk->alias) == len) &&
           (!strncmp(walk->alias, classId, len))) {
            return walk->class_string;
        }
        if((strlen(walk->name) == len) && (!strncmp(walk->name, classId, len))) {
            return walk->class_string;
        }
    }
    return NULL;
}

static int _regexRegUnicodeCharClassLookup(const char *classId, int len, const char **name, const char **alias) {
    regex_char_class_t *walk;

    if(len <= 0) {
        len = (int)strlen(classId);
    }

    for(walk = _regex_unicode_charclass_table; walk != NULL; walk = walk->next) {
        if((walk->alias != NULL) && (strlen(walk->alias) == len) &&
           (!strncmp(walk->alias, classId, len))) {
            *name = walk->name;
            *alias = walk->alias;
            return 1;
        }
        if((strlen(walk->name) == len) && (!strncmp(walk->name, classId, len))) {
            *name = walk->name;
            *alias = walk->alias;
            return 1;
        }
    }
    return 0;
}

void regexRegUnicodeCharClassRemove(const char *classId) {
    regex_char_class_t *entry, *last = NULL;

    for(entry = _regex_unicode_charclass_table; entry != NULL; entry = entry->next) {
        if(((entry->alias != NULL) && (!strcmp(entry->alias, classId))) ||
           (!strcmp(entry->name, classId))) {
            break;
        }
        last = entry;
    }
    if(entry == NULL) {
        return;
    }
    if(last == NULL) {
        _regex_unicode_charclass_table = entry->next;
    } else {
        last->next = entry->next;
    }
}

/////////////////////////////////////////////////////////////////////////////
// Subroutine management data structures and support functions
/////////////////////////////////////////////////////////////////////////////

static void _regexTokenDestroy(regex_build_t *build, regex_token_t *token, int full_stack);

static void _regexSubroutineIndexFreeAll(regex_subroutine_index_t *index) {
    regex_subroutine_t *entry, *next;

    for(entry = index->subroutines; entry != NULL; entry = next) {
        next = entry->next;
        if(entry->name != NULL) {
            _regexDealloc(entry->name, _regexMemContext);
        }
        if(entry->alias != NULL) {
            _regexDealloc(entry->alias, _regexMemContext);
        }
        _regexTokenDestroy(NULL, entry->tokens, 1);
        _regexDealloc(entry, _regexMemContext);
    }
    index->subroutines = NULL;
    index->next_id = 0;
}
static regex_subroutine_t *_regexSubroutineIndexEntryCreate(regex_subroutine_index_t *index,
                                                            const char *prefix,
                                                            const char *name, size_t name_len,
                                                            const char *alias, size_t alias_len) {
    regex_subroutine_t *entry;

    if((entry = _regexAlloc(sizeof(regex_subroutine_t), _regexMemContext)) == NULL) {
        return NULL;
    }
    if(name != NULL) {
        if((entry->name = _regexPrefixedStrdup(prefix, name, name_len)) == NULL) {
            _regexDealloc(entry, _regexMemContext);
            return NULL;
        }
    }
    if((alias != NULL) && (_regexPrefixedStrncmp((unsigned char *)prefix, (unsigned char *)name,
                                                 name_len, (unsigned char *)alias, alias_len))) {
        if((entry->alias = _regexPrefixedStrdup(prefix, alias, alias_len)) == NULL) {
            if(entry->name != NULL) {
                _regexDealloc(entry->name, _regexMemContext);
            }
            _regexDealloc(entry, _regexMemContext);
            return NULL;
        }
    }
    entry->id = index->next_id;
    index->next_id++;
    entry->next = index->subroutines;
    index->subroutines = entry;
    return entry;
}

static regex_subroutine_t *_regexSubroutineIndexEntryGet(regex_subroutine_index_t *index,
                                                         const char *prefix,
                                                         const char *name, size_t len,
                                                         int id) {
    regex_subroutine_t *entry;

    if(name != NULL) {
        for(entry = index->subroutines; entry != NULL; entry = entry->next) {
            if(!_regexPrefixedStrncmp((unsigned char *)prefix,
                                      (unsigned char *)name, len,
                                      (unsigned char *)(entry->name), RE_STR_NULL_TERM)) {
                return entry;
            }
            if(!_regexPrefixedStrncmp((unsigned char *)prefix,
                                      (unsigned char *)name, len,
                                      (unsigned char *)(entry->alias), RE_STR_NULL_TERM)) {
                return entry;
            }
        }
    }
    if(id >= 0) {
        for(entry = index->subroutines; entry != NULL; entry = entry->next) {
            if(id == entry->id) {
                return entry;
            }
        }
    }
    return NULL;
}

#define RE_SUBROUTINE_NOTFOUND 0
#define RE_SUBROUTINE_COLLISION -1
#define RE_SUBROUTINE_OUTOFMEM -2
#define RE_SUBROUTINE_INVALID_CALL -3

// The subroutine index registration functions return a subroutine ID (# >= 0)
// on success, or a negative value on failure: -2 = out of mem, -1 name/alias
// collision, -3 parameter errors (invalid function input)

// Register a pattern with a name and/or alias
static int _regexSubroutineIndexRegisterPattern(regex_subroutine_index_t *index,
                                                const char *prefix, const char *name,
                                                const char *alias, const char *pattern) {
    regex_subroutine_t *entry;

    if(pattern == NULL) {
        return RE_SUBROUTINE_INVALID_CALL;
    }

    if((_regexSubroutineIndexEntryGet(index, prefix, name, RE_STR_NULL_TERM, -1) != NULL) ||
       (_regexSubroutineIndexEntryGet(index, prefix, alias, RE_STR_NULL_TERM, -1) != NULL)) {
        return RE_SUBROUTINE_COLLISION;
    }

    if((entry = _regexSubroutineIndexEntryCreate(index, prefix, name, RE_STR_NULL_TERM, alias, RE_STR_NULL_TERM)) == NULL) {
        return RE_SUBROUTINE_OUTOFMEM;
    }

    entry->pattern = (char *)pattern;
    entry->len = strlen(pattern);

    return entry->id;
}

// Register a token stream with a name and/or alias
static int _regexSubroutineIndexRegisterTokens(regex_subroutine_index_t *index,
                                               const char *prefix, const char *name,
                                               const char *alias, regex_token_t *tokens) {
    regex_subroutine_t *entry;

    if(tokens == NULL) {
        return RE_SUBROUTINE_INVALID_CALL;
    }

    if((_regexSubroutineIndexEntryGet(index, prefix, name, RE_STR_NULL_TERM, -1) != NULL) ||
       (_regexSubroutineIndexEntryGet(index, prefix, alias, RE_STR_NULL_TERM, -1) != NULL)) {
        return RE_SUBROUTINE_COLLISION;
    }

    if((entry = _regexSubroutineIndexEntryCreate(index, prefix, name, RE_STR_NULL_TERM, alias, RE_STR_NULL_TERM)) == NULL) {
        return RE_SUBROUTINE_OUTOFMEM;
    }

    entry->tokens = tokens;

    return entry->id;
}

// Register the intent to create a subroutine with a name and/or alias. Once the
// intended pattern or token stream is created, the subroutine index must be
// updated. Attempting to reference the subroutine without registering a
// pattern or token stream will result in an error.
static int _regexSubroutineIndexRegisterIntent(regex_subroutine_index_t *index, const char *prefix,
                                               const char *name, int name_len,
                                               const char *alias, int alias_len) {
    regex_subroutine_t *entry;

    if((_regexSubroutineIndexEntryGet(index, prefix, name, name_len, -1) != NULL) ||
       (_regexSubroutineIndexEntryGet(index, prefix, alias, alias_len, -1) != NULL)) {
        return RE_SUBROUTINE_COLLISION;
    }

    if((entry = _regexSubroutineIndexEntryCreate(index, NULL, name, name_len, alias, alias_len)) == NULL) {
        return RE_SUBROUTINE_OUTOFMEM;
    }

    return entry->id;
}

/////////////////////////////////////////////////////////////////////////////
// Build context management functions
/////////////////////////////////////////////////////////////////////////////

static void _regexPtrListFree(regex_build_t *build, regex_ptrlist_t *list);

regex_build_ctx_t *regexBuildContextCreate(void) {
    regex_build_ctx_t *build_ctx;

    if((build_ctx = _regexAlloc(sizeof(regex_build_ctx_t), _regexMemContext)) == NULL) {
        return NULL;
    }
    return regexBuildContextInit(build_ctx);
}

regex_build_ctx_t *regexBuildContextInit(regex_build_ctx_t *build_ctx) {
    if(build_ctx == NULL) {
        return NULL;
    }
    memset(build_ctx, 0, sizeof(regex_build_ctx_t));
    return build_ctx;
}

void regexBuildContextDestroy(regex_build_ctx_t *build_ctx) {
    if(build_ctx != NULL) {
        _regexTokenDestroy(NULL, build_ctx->token_pool, 1);
        _regexPtrListFree(NULL, build_ctx->ptrlist_pool);
        _regexStrTableFreeAll(&(build_ctx->string_table));
        _regexStrTableFreeAll(&(build_ctx->class_table));
        _regexStrTableFreeAll(&(build_ctx->utf8_table));
        _regexStrTableFreeAll(&(build_ctx->group_table));
        _regexSubroutineIndexFreeAll(&(build_ctx->subroutine_index));
        _regexDealloc(build_ctx, _regexMemContext);
    }
}

regex_build_t *regexBuilderCreate(regex_build_ctx_t *build_ctx) {
    regex_build_t *build;

    if(build_ctx == NULL) {
        return NULL;
    }
    if((build = _regexAlloc(sizeof(regex_build_t), _regexMemContext)) == NULL) {
        return NULL;
    }
    return regexBuilderInit(build_ctx, build);
}

regex_build_t *regexBuilderInit(regex_build_ctx_t *build_ctx, regex_build_t *build) {
    if((build_ctx == NULL) || (build == NULL)) {
        return NULL;
    }
    memset(build, 0, sizeof(regex_build_t));
    build->build_ctx = build_ctx;
    return build;
}

void regexBuilderDestroy(regex_build_t *build) {
    if(build != NULL) {
        if(build->tokens != NULL) {
            _regexTokenDestroy(build, build->tokens, 1);
        }
        if(build->pattern != NULL) {
            INTERNAL_ERROR("pattern left in build");
        }
        _regexDealloc(build, _regexMemContext);
    }
}

regex_pattern_t *regexPatternCreate(eRegexBuildSource_t source, const char *pat_str, int pat_len) {
    regex_pattern_t *pattern;

    if((pattern = _regexAlloc(sizeof(regex_pattern_t), _regexMemContext)) == NULL) {
        return NULL;
    }
    return regexPatternInit(pattern, source, pat_str, pat_len);
}

regex_pattern_t *regexPatternInit(regex_pattern_t *pattern, eRegexBuildSource_t source,
                                  const char *pat_str, int pat_len) {
    if(pattern == NULL) {
        return NULL;
    }
    memset(pattern, 0, sizeof(regex_pattern_t));
    pattern->source = source;
    pattern->pattern = pat_str;
    pattern->base_pattern = pat_str;
    pattern->len = pat_len;
    return pattern;
}

void regexPatternDestroy(regex_pattern_t *pattern) {
    if(pattern != NULL) {
        _regexDealloc(pattern, _regexMemContext);
    }
}

int regexBuildPatternPush(regex_build_t *build, regex_pattern_t *pattern) {
    if(pattern == NULL) {
        return 0;
    }
    pattern->next = build->pattern;
    build->pattern = pattern;
    return 1;
}

regex_pattern_t *regexBuildPatternPop(regex_build_t *build) {
    regex_pattern_t *pattern;

    if((pattern = build->pattern) != NULL) {
        build->pattern = pattern->next;
        pattern->next = NULL;
    }
    return pattern;
}

/////////////////////////////////////////////////////////////////////////////
// Token management functions
/////////////////////////////////////////////////////////////////////////////

static regex_ptrlist_t *_regexPtrlistCreate(regex_build_t *build,
                                            regex_token_t *token,
                                            eRePtrListType_t type) {
    regex_ptrlist_t *entry;

    if((build != NULL) && (build->build_ctx->ptrlist_pool != NULL)) {
        entry = build->build_ctx->ptrlist_pool;
        build->build_ctx->ptrlist_pool = entry->next;
        memset(entry, 0, sizeof(regex_ptrlist_t));
    } else if((entry = _regexAlloc(sizeof(regex_ptrlist_t), _regexMemContext)) == NULL) {
        return NULL;
    }
    entry->token = token;
    entry->type = type;
    return entry;
}

static regex_ptrlist_t *_regexPtrlistAppend(regex_ptrlist_t *lista, regex_ptrlist_t *listb) {
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

// If noJmps is TRUE, then any Jump tokens in list will NOT be patched. This is
// to prevent redundant chained jumps. Caller should ensure that the remaining
// entries in list are retained, as they may not all be patched. Returns the
// number of entries that were actually patched.
static int _regexPtrlistPatch(regex_build_t *build, regex_ptrlist_t **list, regex_token_t *token, int no_jumps) {
    regex_ptrlist_t *jmps = NULL, *next, *walk;
    int patched = 0;

    for(walk = *list; walk != NULL; walk = next) {
        next = walk->next;
        if(no_jumps && (walk->token->tokenType = eTokenJmp)) {
            walk->next = jmps;
            jmps = walk;
            continue;
        }
        if(walk->type == eRePtrOutA) {
            walk->token->out_a = token;
        } else {
            walk->token->out_b = token;
        }
        if(build != NULL) {
            walk->next = build->build_ctx->ptrlist_pool;
            build->build_ctx->ptrlist_pool = walk;
        } else {
            _regexDealloc(walk, _regexMemContext);
        }
        patched++;
    }
    *list = jmps;
    return patched;
}

static void _regexPtrListFree(regex_build_t *build, regex_ptrlist_t *list) {
    regex_ptrlist_t *next = NULL;

    for(; list != NULL; list = next) {
        next = list->next;
        if(build != NULL) {
            list->next = build->build_ctx->ptrlist_pool;
            build->build_ctx->ptrlist_pool = list;
        } else {
            _regexDealloc(list, _regexMemContext);
        }
    }
}

static regex_token_t *_regexAllocToken(regex_build_t *build, eRegexToken_t tokenType,
                                       const char *str, const unsigned int *bitmap,
                                       int len, int retain_ptr) {
    regex_token_t *token;

    if(build->build_ctx->token_pool != NULL) {
        token = build->build_ctx->token_pool;
        build->build_ctx->token_pool = token->next;
        memset(token, 0, sizeof(regex_token_t));
    } else if((token = _regexAlloc(sizeof(regex_token_t), _regexMemContext)) == NULL) {
        return NULL;
    }
    token->tokenType = tokenType;
    token->pc = -1;
    if(str != NULL) {
        if(len == RE_STR_NULL_TERM) {
            len = (int)strlen(str);
        }
        token->len = len;
        if(retain_ptr) {
            token->str = (char *)str;
        } else {
            if((token->str = (char *)_regexStrdup(str, len)) == NULL) {
                _regexDealloc(token, _regexMemContext);
                return NULL;
            }
        }
    } else if(bitmap != NULL) {
        if(retain_ptr) {
            token->bitmap = (unsigned int *)bitmap;
        } else if((token->bitmap = _regexAlloc(len, _regexMemContext)) == NULL) {
            _regexDealloc(token, _regexMemContext);
            return NULL;
        } else {
            memcpy(token->bitmap, bitmap, len);
        }
        token->len = len;
    }
    return token;
}

static void _regexTokenDestroy(regex_build_t *build, regex_token_t *token, int full_stack) {
    regex_token_t *next;

    if(token == NULL) {
        return;
    }

    for(; token != NULL; token = next) {
        next = token->next;
        switch(token->tokenType) {
            case eTokenCharClass:
            case eTokenStringLiteral:
            case eTokenUtf8Class:
                if(token->str != NULL) {
                    _regexDealloc((void *)(token->str), _regexMemContext);
                }
                break;
            default:
                break;
        }
        _regexPtrListFree(NULL, token->ptrlist);
        if(build != NULL) {
            token->next = build->build_ctx->token_pool;
            build->build_ctx->token_pool = token;
        } else {
            _regexDealloc(token, _regexMemContext);
        }
        if(!full_stack) {
            break;
        }
    }
}

static regex_token_t *_regexTokenBaseCreate(regex_build_t *build,
                                            eRegexToken_t tokenType,
                                            const void *str, const void *ptr,
                                            int retain_ptr, int len) {
    regex_token_t *token, *walk;

    if((token = _regexAllocToken(build, tokenType, str, ptr, len, retain_ptr)) == NULL) {
        build->status = eCompileOutOfMem;
        return NULL;
    }

    if(build->tokens == NULL) {
        build->tokens = token;
    } else {
        for(walk = build->tokens; walk->next != NULL; walk = walk->next);
        if(regexTokenIsTerminal(token, 0) && regexTokenIsTerminal(walk, 1)) {
            // Two adjacent terminals have an implicit concatenation
            if((walk->next = _regexAllocToken(build, eTokenConcatenation, NULL, NULL, 0, 0)) == NULL) {
                _regexTokenDestroy(build, token, 0);
                build->status = eCompileOutOfMem;
                return NULL;
            }
            walk = walk->next;
        }
        walk->next = token;
    }
    return token;
}

static int _regexCreateTokenCharLiteral(regex_build_t *build, int c, int invert) {
    regex_token_t *token;

    if((token = _regexTokenBaseCreate(build, eTokenCharLiteral, NULL, NULL, 0, 0)) == NULL) {
        return 0;
    }
    token->c = c;
    token->flags = (invert ? RE_TOK_FLAG_INVERT : 0);
    return 1;
}

static int _regexCreateTokenUtf8Literal(regex_build_t *build, int c, int invert) {
    regex_token_t *token;

    if((token = _regexTokenBaseCreate(build, eTokenUtf8Literal, NULL, NULL, 0, 0)) == NULL) {
        return 0;
    }
    token->c = c;
    token->flags = (invert ? RE_TOK_FLAG_INVERT : 0);
    return 1;
}

static int _regexCreateTokenCharClass(regex_build_t *build, unsigned int *bitmap) {
    return _regexTokenBaseCreate(build, eTokenCharClass, NULL, bitmap, 1, 0) != NULL;
}

static int _regexCreateTokenStringLiteral(regex_build_t *build, const char *str, int len) {
    return _regexTokenBaseCreate(build, eTokenStringLiteral, str, NULL, 0, len) != NULL;
}

static int _regexCreateTokenCharAny(regex_build_t *build, int dot_all) {
    regex_token_t *token;

    if((token = _regexTokenBaseCreate(build, eTokenCharAny, NULL, NULL, 0, 0)) == NULL) {
        return 0;
    }
    token->flags = (dot_all ? RE_TOK_FLAG_DOT_ALL : 0);
    return 1;
}

static int _regexCreateTokenMatch(regex_build_t *build, regex_token_t **tokens, int dot_all) {
    regex_token_t *stack, *token;

    stack = build->tokens;
    build->tokens = NULL;
    if((token = _regexTokenBaseCreate(build, eTokenMatch, NULL, NULL, 0, 0)) == NULL) {
        return 0;
    }
    token->flags = (dot_all ? RE_TOK_FLAG_DOT_ALL : 0);
    *tokens = build->tokens;
    build->tokens = stack;
    return 1;
}

static int _regexCreateTokenJmp(regex_build_t *build, regex_token_t **tokens) {
    regex_token_t *stack;

    stack = build->tokens;
    build->tokens = NULL;
    if(_regexTokenBaseCreate(build, eTokenJmp, NULL, NULL, 0, 0) == NULL) {
        return 0;
    }
    *tokens = build->tokens;
    build->tokens = stack;
    return 1;
}

static int _regexCreateTokenUtf8Class(regex_build_t *build,
                                      unsigned int *bitmap, int lead_bytes, int midhigh, int midlow, int invert) {
    regex_token_t *token;
    short lead_bits = (short)(((unsigned int)midhigh << 6u) | (unsigned int)midlow);

    if((token = _regexTokenBaseCreate(build, eTokenUtf8Class, NULL, bitmap, 0, 64)) == NULL) {
        return 0;
    }
    token->flags = (invert ? RE_TOK_FLAG_INVERT : 0);
    token->lead_bits = (short)lead_bits;
    token->lead_count = (short)lead_bytes;
    return 1;
}

static int _regexCreateTokenCall(regex_build_t *build, int sub_index) {
    regex_token_t *token;

    if((token = _regexTokenBaseCreate(build, eTokenCall, NULL, NULL, 0, 0)) == NULL) {
        return 0;
    }
    token->sub_index = (short)sub_index;
    return 1;
}

static int _regexCreateTokenReturn(regex_build_t *build, regex_token_t **tokens) {
    regex_token_t *stack;

    stack = build->tokens;
    build->tokens = NULL;
    if(_regexTokenBaseCreate(build, eTokenReturn, NULL, NULL, 0, 0) == NULL) {
        return 0;
    }
    *tokens = build->tokens;
    build->tokens = stack;
    return 1;
}

static int _regexCreateTokenByte(regex_build_t *build) {
    return _regexTokenBaseCreate(build, eTokenByte, NULL, NULL, 0, 0) != NULL;
}

static int _regexCreateTokenAssertion(regex_build_t *build, int assertion) {
    regex_token_t *token;

    if((token = _regexTokenBaseCreate(build, eTokenAssertion, NULL, NULL, 0, 0)) == NULL) {
        return 0;
    }
    token->flags = (unsigned int)assertion;
    return 1;
}

static int _regexCreateTokenConcatenation(regex_build_t *build) {
    return _regexTokenBaseCreate(build, eTokenConcatenation, NULL, NULL, 0, 0) != NULL;
}

static int _regexCreateTokenAlternative(regex_build_t *build) {
    return _regexTokenBaseCreate(build, eTokenAlternative, NULL, NULL, 0, 0) != NULL;
}

static int _regexCreateTokenZeroOrOne(regex_build_t *build) {
    return _regexTokenBaseCreate(build, eTokenZeroOrOne, NULL, NULL, 0, 0) != NULL;
}

static int _regexCreateTokenZeroOrMany(regex_build_t *build) {
    return _regexTokenBaseCreate(build, eTokenZeroOrMany, NULL, NULL, 0, 0) != NULL;
}

static int _regexCreateTokenOneOrMany(regex_build_t *build) {
    return _regexTokenBaseCreate(build, eTokenOneOrMany, NULL, NULL, 0, 0) != NULL;
}

static int _regexCreateTokenRange(regex_build_t *build, int min, int max) {
    regex_token_t *token;

    if((token = _regexTokenBaseCreate(build, eTokenRange, NULL, NULL, 0, 0)) == NULL) {
        return 0;
    }
    token->min = min;
    token->max = max;
    return 1;
}

static int _regexCreateTokenSubExprStart(regex_build_t *build, int group, int sub_index, int compound, int no_capture) {
    unsigned int flags = 0;

    if(sub_index >= 0) {
        flags |= RE_TOK_FLAG_SUBROUTINE;
    }
    if(compound) {
        flags |= RE_TOK_FLAG_COMPOUND;
    }
    if(no_capture) {
        flags |= RE_TOK_FLAG_NO_CAPTURE;
    }
    regex_token_t *token;

    if((token = _regexTokenBaseCreate(build, eTokenSubExprStart, NULL, NULL, 0, 0)) == NULL) {
        return 0;
    }
    token->flags = flags;
    token->group = group;
    token->sub_index = (short)sub_index;
    return 1;
}

static int _regexCreateTokenSubExprEnd(regex_build_t *build) {
    return _regexTokenBaseCreate(build, eTokenSubExprEnd, NULL, NULL, 0, 0) != NULL;
}

/////////////////////////////////////////////////////////////////////////////
// Character parsing, assists with escaped chars
/////////////////////////////////////////////////////////////////////////////

static int _parseIsHexDigit(char c) {
    return (((c >= 'a') && (c <= 'f')) ||   // a - f
            ((c >= 'A') && (c <= 'F')) ||   // A - F
            ((c >= '0') && (c <= '9')));    // 0 - 9
}

static int _parseHexValueGet(char c) {
    if ((c >= 'a') && (c <= 'f')) {
        return c - 'a' + 10;
    } else if((c >= 'A') && (c <= 'F')) {
        return c - 'A' + 10;
    } else if((c >= '0') && (c <= '9')) {
        return c - '0';
    }
    return 0; // This should never be reached if c is a hex digit
}

static int _parseIsIdChar(char c) {
    return (((c >= 'a') && (c <= 'z')) ||   // a - z
            ((c >= 'A') && (c <= 'Z')) ||   // A - Z
            ((c >= '0') && (c <= '9')) ||   // 0 - 9
            (c == '_') || (c == '-'));      // _, -

}

static int _parseHexValueStr(const char *str, int len, unsigned int *value) {
    unsigned int val = 0;
    int cnt;

    for(cnt = 0; len && _parseIsHexDigit(*str); len--, str++, cnt++) {
        val *= 16;
        val += _parseHexValueGet(*str);
    }
    *value = val;
    return !len;
}

// Validates that pattern points to a valid range quantifier (ie., "{3,6}").
// Returns 0 if false, or the length of the quantifier (excluding delimiters)
// if true.
static int _parseRangeQuantifier(const char *pattern, int *min, int *max) {
    int k, val;

    if(*pattern != '{') {
        return 0;
    }
    for(k = 1, val = 0; ((pattern[k] != '\0') && (pattern[k] != ',') && (pattern[k] != '}') &&
                         ((pattern[k] >= '0') && (pattern[k] <= '9'))); k++) {
        val *= 10;
        val += pattern[k] - '0';
    }
    if(pattern[k] != ',') {
        return 0;
    }
    if(min != NULL) {
        *min = val;
    }
    for(val = 0; ((pattern[k] != '\0') && (pattern[k] != '}') &&
                  ((pattern[k] >= '0') && (pattern[k] <= '9'))); k++) {
        val *= 10;
        val += pattern[k] - '0';
    }
    if(pattern[k] != '}') {
        return 0;
    }
    if(max != NULL) {
        *max = val;
    }
    return k;
}

typedef enum {
    eRegexPatternEnd = 0,           // Reached the end of the pattern string '\0'
    eRegexPatternChar,              // A valid pattern character (0-256)
    eRegexPatternUnicode,           // A unicode utf8 escaped character
    eRegexPatternInvalid,           // An unknown escape value
    eRegexPatternInvalidEscape,     // An invalid escape value (malformed)
    eRegexPatternMetaChar,          // A metacharacter
    eRegexPatternMetaClass,         // A meta-class (ie., \d, \s, \w)
    eRegexPatternUnicodeMetaClass,  // A utf8 meta-class (ie., \p{...})
    eRegexPatternEscapedChar        // An escaped character (literal metachar, C escapes)
} eRegexPatternCharState_t;

typedef struct parseChar_s parseChar_t;
struct parseChar_s {
    eRegexPatternCharState_t state; // identity of the parsed value
    const char *ptr;    // pointer to the position in the pattern that the char was parsed from
    int len;    // length of the parse value (for position advance)
    int c;  // parsed character value
};

static int _parsePatternIsValid(parseChar_t *pc) {
    switch(pc->state) {
        case eRegexPatternEnd:
        case eRegexPatternInvalid:
        case eRegexPatternInvalidEscape:
            return 0;
        default:
            return 1;
    }
}

static void _parsePatternCharAdvance(const char **pattern, parseChar_t *pc) {
    if(_parsePatternIsValid(pc)) {
        (*pattern) += pc->len;
    }
}

static void _parsePatternCharBacktrack(const char **pattern, parseChar_t *pc) {
    if(_parsePatternIsValid(pc)) {
        (*pattern) -= pc->len;
    }
}

static parseChar_t _parseGetNextPatternChar(const char **pattern, const char **id, int *len);

static int _parseCheckNextPatternChar(const char **pattern, char c) {
    parseChar_t pc;

    pc = _parseGetNextPatternChar(pattern, NULL, NULL);
    if(!_parsePatternIsValid(&pc)) {
        return 0;
    }
    if((pc.c == '\0') || (pc.c != c)) {
        return 0;
    }
    _parsePatternCharAdvance(pattern, &pc);
    return 1;
}

typedef enum {
    eRegexPatternIdOk,
    eRegexPatternIdMalformed,
    eRegexPatternIdMissing
} eRegexPatternId_t;

// Attempts to parse an identifier delimited by start and end. On success, sets
// id to the position of the start of the identifier, and len to the total length
// of the identifier. Note that len does NOT include the delimiters. Only
// characters within the char class [A-Za-z0-9_-] are valid for the identifier.
// Returns:
//     eRegexPatternIdOk - an id was found
//     eRegexPatternIdMissing - the start delimiter was NOT found
//     eRegexPatternIdMalformed - the id contained invalid characters (not in
//                      [A-Za-z0-9_-]), the end delimiter was missing, or the
//                      delimiters were empty
static eRegexPatternId_t _parseCheckIdentifier(const char *pattern,
                                               int start, int end, const char **id, int *len) {
    int k;
    const char *ptr = pattern;

    if(!_parseCheckNextPatternChar(&ptr, (char)start)) {
        return eRegexPatternIdMissing;
    }
    for(k = 0; _parseIsIdChar(ptr[k]) && (ptr[k] != end); k++);
    if((k == 0) || (ptr[k] != end)) {
        return eRegexPatternIdMalformed;
    }
    if(id != NULL) {
        *id = ptr;
    }
    if(len != NULL) {
        *len = k;
    }
    return eRegexPatternIdOk;
}

static parseChar_t _parseGetNextPatternChar(const char **pattern, const char **id, int *len) {
    parseChar_t result = {
            .c = 0,
            .ptr = *pattern,
            .state = eRegexPatternInvalid,
            .len = 1
    };
    eRegexPatternId_t id_status;
    int size;
    const char *ptr;

    if(**pattern == '\0') {
        result.state = eRegexPatternEnd;
        return result;
    }

    if(**pattern == '\\') {
        result.len = 2;
        switch(*(*pattern + 1)) {
            case '\0': // End of pattern string
                result.state = eRegexPatternInvalid;
                return result;

            case 'p': // unicode class
            case 'P': // unicode class (inverted)
                if(_parseCheckIdentifier(*pattern + 2, '{', '}', id, &size) != eRegexPatternIdOk) {
                    result.state = eRegexPatternInvalidEscape;
                    return result;
                }
                if(len != NULL) {
                    *len = size;
                }
                result.len = size + 4; // \p{size}
                result.c = *(*pattern + 1);
                result.state = eRegexPatternUnicodeMetaClass;
                return result;

            case 'd': // digit
            case 'D': // non digit
            case 's': // whitespace
            case 'S': // non whitespace
            case 'w': // word character
            case 'W': // non word character
            case 'B': // explicit byte match
            case 'X': // full unicode glyph (base + markers)
                result.state = eRegexPatternMetaClass;
                result.c = *(*pattern + 1);
                return result;

            case 'R': // Subroutine call
                if(_parseCheckIdentifier(*pattern + 2, '{', '}', id, &size) != eRegexPatternIdOk) {
                    result.state = eRegexPatternInvalidEscape;
                    return result;
                }
                if(len != NULL) {
                    *len = size;
                }
                result.state = eRegexPatternMetaClass;
                result.len = size + 4; // \R{size}
                result.c = *(*pattern + 1);
                return result;

            case '0': result.c = '\0'; // null
                result.state = eRegexPatternEscapedChar;
                return result;
            case 'a': result.c = '\a'; // alarm (bell)
                result.state = eRegexPatternEscapedChar;
                return result;
            case 'b': result.c = '\b'; // backspace
                result.state = eRegexPatternEscapedChar;
                return result;
            case 'e': result.c = '\x1B'; // escape
                result.state = eRegexPatternEscapedChar;
                return result;
            case 'f': result.c = '\f'; // formfeed
                result.state = eRegexPatternEscapedChar;
                return result;
            case 'n': result.c = '\n'; // newline
                result.state = eRegexPatternEscapedChar;
                return result;
            case 'r': result.c = '\r'; // carraige return
                result.state = eRegexPatternEscapedChar;
                return result;
            case 't': result.c = '\t'; // tab
                result.state = eRegexPatternEscapedChar;
                return result;
            case '-': result.c = '-';  // escaped dash for character class ranges
                result.state = eRegexPatternEscapedChar;
                return result;
            case 'v': result.c = '\v'; // vertical tab
                result.state = eRegexPatternEscapedChar;
                return result;

            case 'u': // unicode codepoint (0 - 0x10FFFF)
                if((id_status = _parseCheckIdentifier(*pattern + 2, '{', '}', &ptr, &size)) == eRegexPatternIdMalformed) {
                    result.state = eRegexPatternInvalidEscape;
                    return result;
                } else if(id_status == eRegexPatternIdMissing) {
                    if(!_parseHexValueStr(*pattern + 2, 4, (unsigned int *)(&result.c))) {
                        result.state = eRegexPatternInvalidEscape;
                        return result;
                    }
                    result.len = 6;
                } else {
                    if(!_parseHexValueStr(ptr, size, (unsigned int *)(&result.c))) {
                        result.state = eRegexPatternInvalidEscape;
                        return result;
                    }
                    result.len = size + 4;
                }
                result.state = eRegexPatternUnicode;
                return result;

            case 'x': // hexidecimal encoded byte
                if(!_parseHexValueStr(*pattern + 2, 2, (unsigned int *)(&result.c))) {
                    result.state = eRegexPatternInvalidEscape;
                } else {
                    result.len = 4;
                    result.state = eRegexPatternEscapedChar;
                }
                return result;

            case '\\':  // literal backslash
            case '|':   // literal pipe
            case '?':   // literal question mark
            case '.':   // literal period
            case '*':   // literal asterisk
            case '^':   // literal caret
            case '+':   // literal plus
            case '(':   // literal open parenthesis
            case '[':   // literal open bracket
            case ')':   // literal close parenthesis
            case '{':   // literal open brace
            case '$':   // literal dollar sign
                result.state = eRegexPatternEscapedChar;
                result.c = *(*pattern + 1);
                return result;

            case '<': // start of word assertion
            case '>': // end of word assertion
                result.state = eRegexPatternMetaChar;
                result.c = *(*pattern + 1);
                return result;

            default:
                result.state = eRegexPatternInvalid;
                return result;
        }
    }
    switch(**pattern) {
        case '|':   // alternation
        case '?':   // zero or one
        case '.':   // character (may be multi byte for UTF8)
        case '*':   // zero or more (kleene)
        case '^':   // start of line assertion
        case '+':   // one or more
        case '(':   // subexpression start
        case '[':   // character class start
        case ')':   // subexpression end
        case '$':   // end of line assertion
            result.c = **pattern;
            result.state = eRegexPatternMetaChar;
            return result;

        case '{':   // range
            result.c = **pattern;
            if((size = _parseRangeQuantifier(*pattern + 1, NULL, NULL)) == 0) {
                result.state = eRegexPatternInvalidEscape;
                return result;
            }
            size += 2; // include the delimiters, since we re-parse in the tokenizer
            if(len != NULL) {
                *len = size;
            }
            if(id != NULL) {
                *id = *pattern + 1;
            }
            result.state = eRegexPatternMetaChar;
            result.len = size;
            return result;

        default:
            result.c = **pattern;
            result.state = eRegexPatternChar;
            return result;
    }
}

/////////////////////////////////////////////////////////////////////////////
// Utf8 and unicode functions
/////////////////////////////////////////////////////////////////////////////

#define UTF8_FOUR_BYTE_PREFIX   0xF0u
#define UTF8_FOUR_BYTE_MASK     0xF8u
#define UTF8_FOUR_BYTE_BITMASK  0x07u
#define UTF8_THREE_BYTE_PREFIX  0xE0u
#define UTF8_THREE_BYTE_MASK    0xF0u
#define UTF8_THREE_BYTE_BITMASK 0x0Fu
#define UTF8_TWO_BYTE_PREFIX    0xC0u
#define UTF8_TWO_BYTE_MASK      0xE0u
#define UTF8_TWO_BYTE_BITMASK   0x1Fu
#define UTF8_LOW_BYTE_PREFIX    0x80u
#define UTF8_LOW_BYTE_MASK      0xC0u
#define UTF8_LOW_BYTE_BITMASK   0x3Fu

// Returns the number of bytes required to utf8 encode the codepoint c
static int _parseUtf8EncodingByteLen(int c) {
    if(c > 65535) {
        return 4;
    } else if(c >2047) {
        return 3;
    } else if(c > 127) {
        return 2;
    }
    return 1;
}

// Returns the number of bytes required to utf8 encode the codepoint that
// begins the utf8 encoded sequence starting with byte 'c'
int parseUtf8EncodedHighByte(char c) {
    if(((unsigned)c & UTF8_TWO_BYTE_MASK) == UTF8_TWO_BYTE_PREFIX) {
        return 2;
    }
    if(((unsigned)c & UTF8_THREE_BYTE_MASK) == UTF8_THREE_BYTE_PREFIX) {
        return 3;
    }
    if(((unsigned)c & UTF8_FOUR_BYTE_MASK) == UTF8_FOUR_BYTE_PREFIX) {
        return 4;
    }
    return 1;
}

// Parses a utf8 encoded string sequence, and returns the codepoint value.
// Returns -1 if the str is not a multibyte utf8 encoded sequence.
int parseUtf8DecodeSequence(const char *str) {
    if(((unsigned)str[0] & UTF8_TWO_BYTE_MASK) == UTF8_TWO_BYTE_PREFIX) {
        if(((unsigned)str[1] & UTF8_LOW_BYTE_MASK) == UTF8_LOW_BYTE_PREFIX) {
            // two byte utf8
            return (int)((((unsigned)str[0] & UTF8_TWO_BYTE_BITMASK) << 6u) |
                         ((unsigned)str[1] & UTF8_LOW_BYTE_BITMASK));
        }
    } else if(((unsigned)str[0] & UTF8_THREE_BYTE_MASK) == UTF8_THREE_BYTE_PREFIX) {
        if(((unsigned)str[1] & UTF8_LOW_BYTE_MASK) == UTF8_LOW_BYTE_PREFIX) {
            if(((unsigned)str[2] & UTF8_LOW_BYTE_MASK) == UTF8_LOW_BYTE_PREFIX) {
                // three byte utf8
                return (int)((((unsigned)str[0] & UTF8_THREE_BYTE_BITMASK) << 12u) |
                             (((unsigned)str[1] & UTF8_LOW_BYTE_BITMASK) << 6u) |
                             ((unsigned)str[2] & UTF8_LOW_BYTE_BITMASK));
            }
        }
    } else if(((unsigned)str[0] & UTF8_FOUR_BYTE_MASK) == UTF8_FOUR_BYTE_PREFIX) {
        if(((unsigned)str[1] & UTF8_LOW_BYTE_MASK) == UTF8_LOW_BYTE_PREFIX) {
            if(((unsigned)str[2] & UTF8_LOW_BYTE_MASK) == UTF8_LOW_BYTE_PREFIX) {
                if(((unsigned)str[3] & UTF8_LOW_BYTE_MASK) == UTF8_LOW_BYTE_PREFIX) {
                    // four byte utf8
                    return (int)((((unsigned)str[0] & UTF8_FOUR_BYTE_BITMASK) << 18u) |
                                 (((unsigned)str[1] & UTF8_LOW_BYTE_BITMASK) << 12u) |
                                 (((unsigned)str[2] & UTF8_LOW_BYTE_BITMASK) << 6u) |
                                 ((unsigned)str[3] & UTF8_LOW_BYTE_BITMASK));
                }
            }
        }
    }
    return -1;
}

/////////////////////////////////////////////////////////////////////////////
// String parsing and escape functions
/////////////////////////////////////////////////////////////////////////////

typedef struct parse_str_s parse_str_t;
struct parse_str_s {
    int characters;
    int bytes;
};

// _parseGetPatternStrLen determines the length in characters AND bytes to
// represent a string in the pattern, including the number of additional bytes
// needed to accommodate any utf8 encoded characters within the string. If the
// string contains an invalid escape, both the character and byte counts are
// set to -1.
static parse_str_t _parseGetPatternStrLen(const char **pattern) {
    const char *ptr = *pattern;
    parse_str_t count = {.characters = 0, .bytes = 0};
    parseChar_t c;

    for(;;) {
        c = _parseGetNextPatternChar(&ptr, NULL, NULL);
        switch(c.state) {
            case eRegexPatternEnd:
            case eRegexPatternMetaChar:
            case eRegexPatternUnicodeMetaClass:
            case eRegexPatternMetaClass:
                return count;

            case eRegexPatternChar:
            case eRegexPatternEscapedChar:
                count.characters++;
                break;

            case eRegexPatternUnicode:
                count.characters++;
                count.bytes += _parseUtf8EncodingByteLen(c.c) - 1;
                break;

            case eRegexPatternInvalid:
            case eRegexPatternInvalidEscape:
                count.characters = -1;
                count.bytes = -1;
                return count;

        }
        _parsePatternCharAdvance(&ptr, &c);
    }
}

// Allocates size + 1 bytes and decodes a string from pattern into the buffer.
// Note: this function expects the len to have been pre-validated, and does NOT
// recognize the null character, to allow it to be used as an actual value
// within the string.
static char *_parseGetPatternStr(const char **pattern, int len, int size) {
    char *str, *ptr;
    int k;
    parseChar_t c;

    if((str = _regexAlloc(size + 1, _regexMemContext)) == NULL) {
        return NULL;
    }
    ptr = str;

    for(; len; len--) {
        c = _parseGetNextPatternChar(pattern, NULL, NULL);
        _parsePatternCharAdvance(pattern, &c);
        if(c.state == eRegexPatternUnicode) {
            for(k = _parseUtf8EncodingByteLen(c.c); k; k--) {
                *ptr = (char)(((unsigned int)c.c >> (unsigned int)((k - 1) * 8)) & 0xFFu);
                ptr++;
            }
        } else {
            *ptr = (char)(c.c);
            ptr++;
        }
    }
    *ptr = '\0';
    return str;
}

// Returns the escaped size of the character in str[0]. If stride is not NULL,
// stride is set to the number bytes consumed (for multibyte utf8 encodings)
int regexEscapeCharLen(const char *str, int *stride) {
    int k, out = 0, codepoint;

    if((codepoint = parseUtf8DecodeSequence(str)) != -1) {
        // utf8 encoding
        if(codepoint > 0xFFFFF) {
            if(stride != NULL) { *stride = 4; }
            return 10; // \u{######}
        } else if(codepoint > 0xFFFF) {
            if(stride != NULL) { *stride = 4; }
            return 9; // \u{#####}
        } else if(codepoint > 2047) {
            if(stride != NULL) { *stride = 3; }
            return 6; // \u####
        } else if(codepoint > 127) {
            if(stride != NULL) { *stride = 2; }
            return 6; // \u####
        }
    }
    if(stride != NULL) { *stride = 1; }
    switch(*str) {
        case '\0':
        case '\a':
        case '\b':
        case '\x1b':
        case '\f':
        case '\n':
        case '\r':
        case '\t':
        case '\v':
            return 2;

        default:
            if((*str >= ' ') && (*str <= 127)) {
                return 1;
            }
            return 4; // \x##
    }
}

int regexEscapeStrLen(const char *str, int len) {
    int k, stride, out = 0;

    for(k = 0; k < len; k += stride) {
        out += regexEscapeCharLen(str + k, &stride);
    }
    return out;
}

void regexEscapedStrEmit(FILE *fp, const char *str, int len) {
    int k;
    int codepoint;

    if(str == NULL) {
        fputs("NULL", fp);
        return;
    }
    for(k = 0; k < len; k++) {
        // Check for utf8 encoding
        if((codepoint = parseUtf8DecodeSequence(str + k)) != -1) {
            if(codepoint > 0xFFFF) {
                k += 3;
                fprintf(fp, "\\u{%04X}", codepoint);
            } else if(codepoint > 2047) {
                k += 2;
                fprintf(fp, "\\u%4.4X", codepoint);
            } else if(codepoint > 127) {
                k += 1;
                fprintf(fp, "\\u%4.4X", codepoint);
            }
            if(codepoint > 127) {
                continue;
            }
        }
        switch(str[k]) {
            case '\0': fputs("\\0", fp); break;
            case '\a': fputs("\\a", fp); break;
            case '\b': fputs("\\b", fp); break;
            case '\x1b': fputs("\\e", fp); break;
            case '\f': fputs("\\f", fp); break;
            case '\n': fputs("\\n", fp); break;
            case '\r': fputs("\\r", fp); break;
            case '\t': fputs("\\t", fp); break;
            case '\v': fputs("\\v", fp); break;
            default:
                if((str[k] >= ' ') && (str[k] <= 127)) {
                    fputc(str[k], fp);
                } else {
                    fprintf(fp, "\\x%2.2X", str[k]);
                }
                break;
        }
    }
}

void regexCharLiteralEmit(FILE *fp, int c) {
    switch(c) {
        case '\0': fputs("\\0", fp); break;
        case '\a': fputs("\\a", fp); break;
        case '\b': fputs("\\b", fp); break;
        case '\x1b': fputs("\\e", fp); break;
        case '\f': fputs("\\f", fp); break;
        case '\n': fputs("\\n", fp); break;
        case '\r': fputs("\\r", fp); break;
        case '\t': fputs("\\t", fp); break;
        case '\v': fputs("\\v", fp); break;
        case '\\': fputs("\\\\", fp); break;
        case '|': fputs("\\|", fp); break;
        case '?': fputs("\\?", fp); break;
        case '.': fputs("\\.", fp); break;
        case '*': fputs("\\*", fp); break;
        case '^': fputs("\\^", fp); break;
        case '+': fputs("\\+", fp); break;
        case '(': fputs("\\(", fp); break;
        case ')': fputs("\\)", fp); break;
        case '[': fputs("\\[", fp); break;
        case '{': fputs("\\{", fp); break;
        case '$': fputs("\\$", fp); break;
        default:
            if((c >= ' ') && (c <= 127)) {
                fputc(c, fp);
            } else {
                fprintf(fp, "\\x%2.2X", c);
            }
            break;
    }
}

/////////////////////////////////////////////////////////////////////////////
// Character class parsing handlers
/////////////////////////////////////////////////////////////////////////////

// Unicode char class support

// For utf8 encoded characters, trailing bytes have 6 bits of character data,
// as the leading bits identify the byte as part of a multibyte codepoint.
// This simplifies the unicode char class table entries to only need 64 bits
// (8 bytes) to represent each byte.

// |  high byte  | midhigh byte| midlow byte | low byte |
// | 4 byte lead | 3 byte lead | 2 byte lead | low byte |
//                             |<---- 2 byte group ---->|
//               |<------------ 3 byte group ---------->|
// |<------------------ 4 byte group ------------------>|

void _regexUtf8RangeFreeAll(regex_build_t *build, regex_utf8_range_t *range) {
    regex_utf8_range_t *next;

    for(; range != NULL; range = next) {
        next = range->next;
        if(build != NULL) {
            range->next = build->build_ctx->utf8_range_pool;
            build->build_ctx->utf8_range_pool = range;
        } else {
            _regexDealloc(range, _regexMemContext);
        }
    }
}

int _parseCreateUtf8Range(regex_build_t *build, regex_utf8_range_t **sequence, int start, int end) {
    regex_utf8_range_t *range, *walk, *last, *next;
    int swap;

    if(start > end) {
        swap = start;
        start = end;
        end = swap;
    }
    if(build->build_ctx->utf8_range_pool != NULL) {
        range = build->build_ctx->utf8_range_pool;
        build->build_ctx->utf8_range_pool = range->next;
    } else {
        if((range = _regexAlloc(sizeof(regex_utf8_range_t), _regexMemContext)) == NULL) {
            return 0;
        }
    }
    range->next = NULL;
    range->start = start;
    range->end = end;

    if(*sequence == NULL) {
        *sequence = range;
    } else {
        if(end < (*sequence)->start) {
            range->next = *sequence;
            *sequence = range;
            return 1;
        }

        for(walk = *sequence, last = NULL; walk != NULL; walk = walk->next) {
            if(start <= walk->end) {
                if(end >= start) {
                    // The ranges overlap, merge them
                    if(start < walk->start) {
                        walk->start = start;
                    }
                    if(end > walk->end) {
                        walk->end = end;
                    }

                    range->next = build->build_ctx->utf8_range_pool;
                    build->build_ctx->utf8_range_pool = range;

                    // Resolve any resultant range bridges
                    range = walk;
                    for(; walk != NULL; walk = next) {
                        next = walk->next;
                        if(range->end >= walk->start) {
                            if(walk->end > range->end) {
                                range->end = walk->end;
                            }
                            range->next = walk->next;
                            walk->next = build->build_ctx->utf8_range_pool;
                            build->build_ctx->utf8_range_pool = walk;
                        }
                    }
                    return 1;
                }
            } else {
                if(last == NULL) {
                    INTERNAL_ERROR("utf8 ranges out of sequence");
                    return 0;
                }
                range->next = last->next;
                last->next = range;
                return 1;
            }
            last = walk;
        }
        last->next = range;
        return 1;
    }
}

typedef enum {
    eReUtf8SegDoneRangeRemaining,
    eReUtf8SegDoneNextRange,
    eReUtf8SegKeepNextRange,
} eReUtf8RangeSegment_t;

typedef struct regex_utf8_segment_s regex_utf8_segment_t;
struct regex_utf8_segment_s {
    int bytes;
    int high;
    int midhigh;
    int midlow;
    unsigned int bitmap[2];
};

static int _regexUtf8CalculateSegmentBoundary(regex_utf8_segment_t *segment) {
    switch(segment->bytes) {
        case 2:
            return (int)((((unsigned)(segment->midlow) & UTF8_TWO_BYTE_BITMASK) << 6u) | UTF8_LOW_BYTE_BITMASK);
        case 3:
            return (int)((((unsigned)(segment->midhigh) & UTF8_THREE_BYTE_BITMASK) << 12u) |
                         (((unsigned)(segment->midlow) & UTF8_LOW_BYTE_BITMASK) << 6u) |
                         UTF8_LOW_BYTE_BITMASK);
        case 4:
            return (int)((((unsigned)(segment->high) & UTF8_FOUR_BYTE_BITMASK) << 18u) |
                         (((unsigned)(segment->midhigh) & UTF8_LOW_BYTE_BITMASK) << 12u) |
                         (((unsigned)(segment->midlow) & UTF8_LOW_BYTE_BITMASK) << 6u) |
                         UTF8_LOW_BYTE_BITMASK);
        default:
            INTERNAL_ERROR("utf8 segment contains single byte encoding");
            return -1;
    }
}

static int _regexUtf8SegmentInBounds(regex_utf8_segment_t *segment, int start, int end) {
    int seg_start, seg_end;

    seg_start = (int)((unsigned)_regexUtf8CalculateSegmentBoundary(segment) & 0xFFFFFFC0u);
    seg_end = _regexUtf8CalculateSegmentBoundary(segment);
    return ((seg_start <= end) && (seg_end >= start));
}

static int _regexUtf8SegmentInit(regex_utf8_segment_t *segment, int codepoint) {
    segment->bytes = _parseUtf8EncodingByteLen(codepoint);
    switch(segment->bytes) {
        case 2:
            segment->midlow = (int)((((unsigned)codepoint) >> 6u) & UTF8_TWO_BYTE_BITMASK);
            break;
        case 3:
            segment->midhigh = (int)((((unsigned)codepoint) >> 12u) & UTF8_THREE_BYTE_BITMASK);
            segment->midlow = (int)((((unsigned)codepoint) >> 6u) & UTF8_LOW_BYTE_BITMASK);
            break;
        case 4:
            segment->high = (int)((((unsigned)codepoint) >> 18u) & UTF8_FOUR_BYTE_BITMASK);
            segment->midhigh = (int)((((unsigned)codepoint) >> 12u) & UTF8_LOW_BYTE_BITMASK);
            segment->midlow = (int)((((unsigned)codepoint) >> 6u) & UTF8_LOW_BYTE_BITMASK);
            break;
        default:
            INTERNAL_ERROR("utf8 codepoint is not multibyte");
            return 0;
    }
    return 1;
}

static int _regexUtf8SegmentGenerate(regex_build_t *build, regex_utf8_segment_t *segment) {

}

// Returns a segment of a utf8 codepoint range for conversion into a char class.
static eReUtf8RangeSegment_t _regexUtf8RangeSegment(int *next_codepoint, int start, int end,
                                                    regex_utf8_segment_t *segment) {
    int seg_end;
    int c, bits;

    if(*next_codepoint > start) {
        if(*next_codepoint > end) {
            return eReUtf8SegKeepNextRange;
        }
        start = *next_codepoint;
    }

    if(segment->bytes == 0) {
        _regexUtf8SegmentInit(segment, start);
    } else {
        if(!_regexUtf8SegmentInBounds(segment, start, end)) {
            return eReUtf8SegDoneRangeRemaining;
        }
    }

    seg_end = _regexUtf8CalculateSegmentBoundary(segment);
    for(c = start; c <= seg_end; c++) {
        bits = (int)((unsigned)c & UTF8_LOW_BYTE_BITMASK);
        segment->bitmap[bits / 32] |= (1u << (unsigned)(bits % 32));
    }
    *next_codepoint = seg_end + 1;

    if(seg_end < end) {
        return eReUtf8SegDoneRangeRemaining;
    } else if(seg_end == end) {
        return eReUtf8SegDoneNextRange;
    } else {
        return eReUtf8SegKeepNextRange;
    }
}

static int _regexUtf8RangeCharClass(int start, int end, unsigned int *bitmap) {
    int k;

    if(start <= 127) {
        for(k = start; ((k <= 127) && (k <= end)); k++) {
            bitmap[k / 32u] |= (1u << (k % 32u));
        }
        return 1;
    }
    return 0;
}

static int _regexUtf8SubroutineCreate(regex_build_t *build, regex_utf8_range_t *utf8range,
                                      const char *name, int name_len, int invert) {
    unsigned int bitmap[8];
    int use_char_class = 0;
    int next_codepoint = 128;
    regex_utf8_segment_t segment;
    regex_utf8_range_t *walk;

    memset(bitmap, 0, 32);

    for(walk = utf8range; walk != NULL; walk = walk->next) {
        if(_regexUtf8RangeCharClass(walk->start, walk->end, bitmap)) {
            use_char_class = 1;
            continue;
        }
        break;
    }
    if(use_char_class) {
        // low byte char class is needed
        // TODO
    }

    memset(&segment, 0, sizeof(regex_utf8_segment_t));

    for(walk = utf8range; walk != NULL; walk = walk->next) {
keep_processing:
        switch(_regexUtf8RangeSegment(&next_codepoint, walk->start, walk->end, &segment)) {
            case eReUtf8SegDoneRangeRemaining:
                if(!_regexUtf8SegmentGenerate(build, &segment)) {
                    build->status = eCompileOutOfMem;
                    return 0;
                }
                memset(&segment, 0, sizeof(regex_utf8_segment_t));
                goto keep_processing;

            case eReUtf8SegDoneNextRange:
                if(!_regexUtf8SegmentGenerate(build, &segment)) {
                    build->status = eCompileOutOfMem;
                    return 0;
                }
                memset(&segment, 0, sizeof(regex_utf8_segment_t));
                break;

            case eReUtf8SegKeepNextRange:
                break;
        }
    }
    if(segment.bytes) {
        if(!_regexUtf8SegmentGenerate(build, &segment)) {
            build->status = eCompileOutOfMem;
            return 0;
        }
    }

    // TODO - register the subroutine
    return 1;
}

static int _parseCharClassBitmapSet(regex_build_t *build, regex_utf8_range_t **range, unsigned int *bitmap, int c) {
    if((c > 127) && (range != NULL)) {
        return _parseCreateUtf8Range(build, range, c, c);
    }
    bitmap[c / 32u] |= (1u << (c % 32u));
    return 1;
}

static int _parseCharClassBitmapRangeSet(regex_build_t *build, regex_utf8_range_t **range, unsigned int *bitmap, int a, int b) {
    if((b > 127) && (range != NULL)) {
        return _parseCreateUtf8Range(build, range, a, b);
    }
    for(; a <= b; a++) {
        bitmap[a / 32u] |= (1u << (a % 32u));
    }
    return 1;
}

// Parse a class pattern, and generate a eTokenCharClass or eTokenUtf8Class token.
// By default, uses the pattern in tokenizer. If class_pattern is defined, it will
// use it as the pattern instead of the tokenizer pattern. If class_pattern and
// name are defined, then name will be associated with the class_pattern in
// diagnostic output. If class_pattern is not defined, but name is defined, then
// it is assumed that name is referencing a utf8 property class, and an attempt
// will be made to lookup the class pattern in the registered unicode classes.
static int _parseCharClassAndCreateToken(regex_build_t *build, const char *class_pattern,
                                         const char *name, int name_len, int invert) {
    unsigned int bitmap[8], *ptr;
    parseChar_t c;
    regex_utf8_range_t *utf8range = NULL;
    int range = 0;
    int last = 0;
    int k;
    regex_pattern_t pattern;
    int use_unicode = 0;
    int used_class_pattern = 0;
    regex_subroutine_t *subroutine;
    const char *sub_name;
    const char *sub_alias;

    if((name != NULL) && (class_pattern == NULL)) {
        if((subroutine = _regexSubroutineIndexEntryGet(&(build->build_ctx->subroutine_index), "class:", name, name_len, -1)) != NULL) {
            // Found the class in the subroutine index
            if(!_regexCreateTokenCall(build, subroutine->id)) {
                return 0;
            }
            return 1;
        }
        if((class_pattern = _regexRegUnicodeCharClassGet(name, name_len)) == NULL) {
            build->status = eCompileUnknownUnicodeClass;
            return 0;
        }
        if(!_regexRegUnicodeCharClassLookup(name, name_len, &sub_name, &sub_alias)) {
            build->status = eCompileInternalError;
            INTERNAL_ERROR("failed to retrieve utf8 char class");
            return 0;
        }
        switch(_regexSubroutineIndexRegisterPattern(&(build->build_ctx->subroutine_index), "class:",
                                                    sub_name, sub_alias, class_pattern)) {
            case RE_SUBROUTINE_INVALID_CALL:
                build->status = eCompileInternalError;
                INTERNAL_ERROR("failed to register subroutine for utf8 char class");
                return 0;
            case RE_SUBROUTINE_COLLISION:
                build->status = eCompileSubroutineNameCollision;
                return 0;
            case RE_SUBROUTINE_OUTOFMEM:
                build->status = eCompileOutOfMem;
                return 0;
            default:
                break;
        }
    }

    if(class_pattern != NULL) {
        used_class_pattern = 1;
        regexBuildPatternPush(build, regexPatternInit(&pattern, eReBuildSrcClass, class_pattern, RE_STR_NULL_TERM));
    }

    memset(bitmap, 0, 32);

    c = _parseGetNextPatternChar(&(build->pattern->pattern), NULL, NULL);
    if(_parsePatternIsValid(&c)) {
        _parsePatternCharAdvance(&(build->pattern->pattern), &c);
    }
    if(c.state == eRegexPatternEnd) {
        build->status = eCompileCharClassIncomplete;
        return 0;
    }

    if(c.state == eRegexPatternMetaChar && c.c == '^') {
        invert = 1;
        c = _parseGetNextPatternChar(&(build->pattern->pattern), NULL, NULL);
        if(_parsePatternIsValid(&c)) {
            _parsePatternCharAdvance(&(build->pattern->pattern), &c);
        }
    }

    for(;;) {
        switch(c.state) {
            case eRegexPatternInvalid:
                build->status = eCompileEscapeCharIncomplete;
                return 0;

            case eRegexPatternInvalidEscape:
                build->status = eCompileInvalidEscapeChar;
                return 0;

            case eRegexPatternUnicode:
                use_unicode = 1;
                // intentional fall through

            case eRegexPatternChar:
                if(c.c == ']') {
                    if(range == 2) {
                        build->status = eCompileCharClassRangeIncomplete;
                        return 0;
                    }
                    goto parseClassCompleted;
                }
                // intentional fall through

            case eRegexPatternMetaChar:
            case eRegexPatternEscapedChar:
                if(range == 0) {
                    last = c.c;
                    if(!_parseCharClassBitmapSet(build, (use_unicode ? &utf8range : NULL), bitmap, last)) {
                        build->status = eCompileOutOfMem;
                        return 0;
                    }
                    range = 1;
                } else if(range == 1) {
                    if(c.state != eRegexPatternEscapedChar && c.c == '-') {
                        range = 2;
                    } else {
                        last = c.c;
                        if(!_parseCharClassBitmapSet(build, (use_unicode ? &utf8range : NULL), bitmap, last)) {
                            build->status = eCompileOutOfMem;
                            return 0;
                        }
                    }
                } else {
                    if(!_parseCharClassBitmapRangeSet(build, (use_unicode ? &utf8range : NULL), bitmap, last, c.c)) {
                        build->status = eCompileOutOfMem;
                        return 0;
                    }
                    range = 0;
                }
                break;

            case eRegexPatternMetaClass:
            case eRegexPatternUnicodeMetaClass:
                build->status = eCompileInvalidEscapeChar;
                return 0;

            case eRegexPatternEnd:
                if(range == 2) {
                    build->status = eCompileCharClassRangeIncomplete;
                    return 0;
                }
                if(class_pattern != NULL) {
                    goto parseClassCompleted;
                }
                build->status = eCompileCharClassIncomplete;
                return 0;
        }
        c = _parseGetNextPatternChar(&(build->pattern->pattern), NULL, NULL);
        if(_parsePatternIsValid(&c)) {
            _parsePatternCharAdvance(&(build->pattern->pattern), &c);
        }
    }

parseClassCompleted:

    build->status = eCompileOk;

    if(utf8range != NULL) {
        // Merge any entries in the single byte bitmap into the utf8 tree
        for(k = 0; k < 256; k++) {
            if(bitmap[k / 32u] & (1u << k % 32u)) {
                if(!_parseCreateUtf8Range(build, &utf8range, k, k)) {
                    build->status = eCompileOutOfMem;
                    return 0;
                }
            }
        }

        if(!_regexUtf8SubroutineCreate(build, utf8range, name, name_len, invert)) {
            _regexUtf8RangeFreeAll(build, utf8range);
            return 0;
        }

        _regexUtf8RangeFreeAll(build, utf8range);
    } else {
        // Strictly single byte, use a regular char class only
        if(invert) {
            for(k = 0; k < 8; k++) {
                bitmap[k] ^= (unsigned int)0xFFFFFFFFu;
            }
        }

        if((ptr = _regexAlloc(32, _regexMemContext)) == NULL) {
            build->status = eCompileOutOfMem;
            return 0;
        }
        memcpy(ptr, bitmap, 32);

        if(!_regexCreateTokenCharClass(build, (unsigned int *)ptr)) {
            build->status = eCompileOutOfMem;
            return 0;
        }

        if(name != NULL) {
            // TODO - associate the name with this class, for diagnostic output
        }
    }

    if(used_class_pattern) {
        regexBuildPatternPop(build);
    }
    return 1;
}

/////////////////////////////////////////////////////////////////////////////
// Lexer - tokenizes the pattern into a token list (infix form)
/////////////////////////////////////////////////////////////////////////////

static eRegexCompileStatus_t _regexTokenizePattern(regex_build_t *build) {
    const char *class;
    parseChar_t c;
    parse_str_t str_count;
    int len;
    const char *str;
    unsigned int flags;
    regex_token_t *subroutine;
    eRegexCompileStatus_t status;
    int subexpr = 0;
    int named;
    int index;

    // Parse the regex pattern into a sequence of tokens (operators and operands)
    // The output of this stage is a sequence of lexical tokens in infix form

    // Loop through the pattern until we've handled it all
    for(; *(build->pattern->pattern) != '\0';) {
        // Get the next character in the pattern. The helper function assists
        // in disambiguating escaped characters.
        c = _parseGetNextPatternChar(&(build->pattern->pattern), &str, &len);
        if(_parsePatternIsValid(&c)) {
            _parsePatternCharAdvance(&(build->pattern->pattern), &c);
        }

        switch(c.state) {
            case eRegexPatternEnd:
                return eCompileOk;

            case eRegexPatternInvalid:
                return eCompileEscapeCharIncomplete;

            case eRegexPatternInvalidEscape:
                return eCompileInvalidEscapeChar;

            case eRegexPatternMetaChar:
                switch(c.c) {
                    case '.': // Meta, any char
                        if(!_regexCreateTokenCharAny(build, (int)((unsigned int)(build->build_ctx->flags) & RE_FLAG_DOT_ALL))) {
                            return eCompileOutOfMem;
                        }
                        continue;

                    case '[': // Meta, char class
                        if(!_parseCharClassAndCreateToken(build, NULL, NULL, 0, 0)) {
                            return build->status;
                        }
                        continue;

                    case '?': // Meta, zero or one quantifier
                        if(!_regexCreateTokenZeroOrOne(build)) {
                            return eCompileOutOfMem;
                        }
                        continue;

                    case '*': // Meta, kleene star, zero or many quantifier
                        if(!_regexCreateTokenZeroOrMany(build)) {
                            return eCompileOutOfMem;
                        }
                        continue;

                    case '+': // Meta, one or many quantifier
                        if(!_regexCreateTokenOneOrMany(build)) {
                            return eCompileOutOfMem;
                        }
                        continue;

                    case '|': // Meta, alternative operator
                        if(!_regexCreateTokenAlternative(build)) {
                            return eCompileOutOfMem;
                        }
                        continue;

                    case '(':
#if 0
                        // Grouped subexpression, complex operator, resolves to a compound operand
                        // Speculative subexpression number. If non-capturing, will NOT actually
                        // increase the group count.
                        subexpr = build->build_ctx->group_count + 1;
                        flags = 0;
                        named = 0;
                        index = 0;
                        // Check for group meta modifiers
                        while(*(build->pattern->pattern) == '?') {
                            build->pattern->pattern++;
                            if(_parseCheckNextPatternChar(&(build->pattern->pattern), 'P')) {
                                // Named sub expression
                                if(named || (flags & (RE_TOK_FLAG_SUBROUTINE | RE_TOK_FLAG_NO_CAPTURE))) {
                                    return eCompileConflictingAttrs;
                                }
                                if(_parseCheckIdentifier(build->pattern->pattern, '<', '>', &str, &len) != eRegexPatternIdOk) {
                                    return eCompileMalformedSubExprName;
                                }
                                build->pattern->pattern += len + 2;
                                if(!regexVMGroupTableEntryAdd(build, str, len, subexpr)) {
                                    return eCompileOutOfMem;
                                }
                                named = 1;
                            } else if(_parseCheckNextPatternChar(&(build->pattern->pattern), ':')) {
                                // Non-capturing subexpression
                                if(named || (flags & (RE_TOK_FLAG_COMPOUND))) {
                                    return eCompileConflictingAttrs;
                                }
                                flags |= RE_TOK_FLAG_NO_CAPTURE;
                            } else if(_parseCheckNextPatternChar(&(build->pattern->pattern), '*')) {
                                // Compound capturing subexpression
                                if(flags & (RE_TOK_FLAG_NO_CAPTURE | RE_TOK_FLAG_SUBROUTINE)) {
                                    return eCompileConflictingAttrs;
                                }
                                flags |= RE_TOK_FLAG_COMPOUND;
                            } else if(_parseCheckNextPatternChar(&(build->pattern->pattern), 'R')) {
                                // Named subroutine
                                if(named || (flags & RE_TOK_FLAG_COMPOUND)) {
                                    return eCompileConflictingAttrs;
                                }
                                if(_parseCheckIdentifier(build->pattern->pattern, '<', '>', &str, &len) != eRegexPatternIdOk) {
                                    return eCompileMalformedSubExprName;
                                }
                                build->pattern->pattern += len + 2;
                                if((index = _regexSubroutineIndexRegisterIntent(&(build->new_subroutine_index), NULL, str, len,
                                                                                NULL, 0)) < 0) {
                                    if(index == RE_SUBROUTINE_COLLISION) {
                                        return eCompileSubroutineNameCollision;
                                    }
                                    return eCompileOutOfMem;
                                }
                                flags |= RE_TOK_FLAG_SUBROUTINE | RE_TOK_FLAG_NO_CAPTURE;
                            } else {
                                return eCompileUnsupportedMeta;
                            }
                        }
                        if(!named) {
                            if(!regexVMGroupTableEntryAdd(build, NULL, 0, subexpr)) {
                                return eCompileOutOfMem;
                            }
                        }
                        if(!(flags & (RE_TOK_FLAG_NO_CAPTURE | RE_TOK_FLAG_SUBROUTINE))) {
                            build->build_ctx->group_count++;
                        }
                        if(!_regexCreateTokenSubExprStart(build, subexpr, index, (int)(flags & RE_TOK_FLAG_COMPOUND), (int)(flags & RE_TOK_FLAG_NO_CAPTURE))) {
                            return eCompileOutOfMem;
                        }
#endif
                        continue;

                    case ')': // Meta, end of subexpression (group)
                        if(!_regexCreateTokenSubExprEnd(build)) {
                            return eCompileOutOfMem;
                        }
                        continue;

                    case '^': // Meta, assertion - start of line
                        if(!_regexCreateTokenAssertion(build, RE_TOK_FLAG_ASSERT_START_OF_LINE)) {
                            return eCompileOutOfMem;
                        }
                        continue;

                    case '$': // Meta, assertion - end of line
                        if(!_regexCreateTokenAssertion(build, RE_TOK_FLAG_ASSERT_END_OF_LINE)) {
                            return eCompileOutOfMem;
                        }
                        continue;

                    case '<': // Meta, assertion - start of word
                        if(!_regexCreateTokenAssertion(build, RE_TOK_FLAG_ASSERT_START_OF_WORD)) {
                            return eCompileOutOfMem;
                        }
                        continue;

                    case '>': // Meta, assertion - end of word
                        if(!_regexCreateTokenAssertion(build, RE_TOK_FLAG_ASSERT_END_OF_WORD)) {
                            return eCompileOutOfMem;
                        }
                        continue;

                    default: // Unexpected meta character
                        INTERNAL_ERROR("unexpected meta char cleared parser");
                        return eCompileInternalError;
                }

            case eRegexPatternMetaClass:
#if 0
                switch(c.c) {
                    case 'R': // subroutine call
                        if((routine_entry = _regexSubroutineIndexEntryGet(&(build->new_subroutine_index), NULL, str, len, -1)) == NULL) {
                            return eCompileUnknownSubroutine;
                        }
                        if(!_regexCreateTokenCall(build, routine_entry->id)) {
                            return eCompileOutOfMem;
                        }
                        continue;
                    case 'X': // full unicode glyph (base + markers)
                        // Represented by a micro NFA
                        subroutine = NULL;
                        // TODO
//#if 0
                    if((status = regexSubroutineGenerateFromPattern(build, tokenizer, &(tokenizer->tokens), &subroutine, "\\p{M}\\p{M}*", NULL, 0, NULL)) != eCompileOk) {
                            return status;
                        }
//#endif
                        continue;
                    case 'B': // explicit byte
                        if(!_regexCreateTokenByte(build)) {
                            return eCompileOutOfMem;
                        }
                        continue;
                }
                switch(c.c) {
                    case 'd': // digit
                        class = META_CLASS(META_DIGITS_PATTERN);
                        break;

                    case 'D': // non digit
                        class = META_CLASS_INV(META_DIGITS_PATTERN);
                        break;

                    case 's': // whitespace
                        class = META_CLASS(META_WHITESPACE_PATTERN);
                        break;

                    case 'S': // non whitespace
                        class = META_CLASS_INV(META_WHITESPACE_PATTERN);
                        break;

                    case 'w': // word character
                        class = META_CLASS(META_WORD_PATTERN);
                        break;

                    case 'W': // non word character
                        class = META_CLASS_INV(META_WORD_PATTERN);
                        break;

                    default:
                        return eCompileUnsupportedMeta;
                }
                if(!_parseCharClassAndCreateToken(build, tokenizer, class, NULL, 0, 0)) {
                    return tokenizer->status;
                }
#endif
                continue;

            case eRegexPatternUnicodeMetaClass:
#if 0
                if(_regexRegUnicodeCharClassGet(str, len) == NULL) {
                    return eCompileUnknownUnicodeClass;
                }
                if(!_parseCharClassAndCreateToken(build, tokenizer, NULL, str, len, ((c.c == 'P') ? 1 : 0))) {
                    return tokenizer->status;
                }
#endif
                continue;

            case eRegexPatternChar:
            case eRegexPatternUnicode:
            case eRegexPatternEscapedChar:
                // Operand, either character literal or string literal
                _parsePatternCharBacktrack(&(build->pattern->pattern), &c);
                str_count = _parseGetPatternStrLen(&(build->pattern->pattern));
                if(str_count.characters == -1) {
                    INTERNAL_ERROR("backtrack failed!");
                    return eCompileEscapeCharIncomplete;
                }
                if(str_count.characters == 1) {
                    if(str_count.bytes > 1) {
                        if(!_regexCreateTokenUtf8Literal(build, c.c, 0)) {
                            return eCompileOutOfMem;
                        }
                    } else if(!_regexCreateTokenCharLiteral(build, (char) (c.c), 0)) {
                        return eCompileOutOfMem;
                    }
                } else {
                    if((str = _parseGetPatternStr(&(build->pattern->pattern), str_count.characters,
                                                  str_count.bytes)) == NULL) {
                        return eCompileOutOfMem;
                    }
                    if(!_regexCreateTokenStringLiteral(build, str, str_count.bytes)) {
                        return eCompileOutOfMem;
                    }
                }
                continue;
        }
    }
    return eCompileOk;
}

eRegexCompileStatus_t regexTokenizePattern(regex_build_t *build) {
    regex_pattern_t *pattern;

    build->status = _regexTokenizePattern(build);
    for(pattern = build->pattern; pattern != NULL; pattern = pattern->next) {
        pattern->pos = (int)(pattern->pattern - pattern->base_pattern);
    }
    return build->status;
}

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

    return 0;
}

#endif // MOJO_REGEX_TEST_MAIN

#endif // _MOJO_REGEX_HEADER_
