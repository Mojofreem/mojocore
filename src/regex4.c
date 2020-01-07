#ifndef _MOJO_REGEX_HEADER_
#define _MOJO_REGEX_HEADER_

/*///////////////////////////////////////////////////////////////////////////

Regex VM Bytecode (v9 - in development)

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
        eTokenRange             E       min/max                 program counter
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

    Note about eTokenRange:

        +-------------------------------+-------------------------------+
        |  3                   2        '          1                    |
        |1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6'5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0|
        +-------------------------------+-------------------------------+
        |                              1'                               |
        |F E D C B A 9 8 7 6 5 4 3 2 1 0'F E D C B A 9 8 7 6 5 4 3 2 1 0|
        +-------------+-------------+---+-----------------------+-------+
        |  31 ... 25  | 24 ... 18   |          17 ... 4         | 3...0 |
        +-------------+-------------+---------------------------+-------+
        |  Min value  |  Max value  |      program counter      |4bit op|
        +-------------+-------------+---------------------------+-------+

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
#include <inttypes.h>

#define MOJO_REGEX_DEBUG_INTERNAL_ERRORS

#ifdef MOJO_REGEX_DEBUG_INTERNAL_ERRORS
#   define INTERNAL_ERROR(msg)  fprintf(stderr, "ERROR [%s] in %s @ %d\n", msg, __FUNCTION__, __LINE__)
#else
#   define INTERNAL_ERROR(msg)
#endif

#ifndef __unused
#   define __unused
#endif // __unused

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

#define REGEX_VM_MACHINE_VERSION    9

#ifdef MOJO_REGEX_VM_DEBUG
typedef struct regex_vm_sub_names_s regex_vm_sub_names_t;
struct regex_vm_sub_names_s {
    int id;
    int pc;
    char *name;
    char *alias;
};
#endif // MOJO_REGEX_VM_DEBUG

typedef struct regex_vm_s regex_vm_t;
struct regex_vm_s {
    int vm_version;                 // VM machine version
    unsigned int *program;          // VM encoded regex pattern
    int size;                       // number of instructions in the program
    char **string_table;            // table of string literals used in the pattern
    int string_tbl_size;            // number of strings in the string table
    int *string_tbl_len;            // table of string lengths for string in the string table
    unsigned int **class_table;     // table of character class bitmaps (32 bytes each)
    int class_tbl_size;             // number of bitmaps in the class table
    unsigned int **utf8_class_table;// table of utf8 encoding class bitmaps (8 bytes each)
    int utf8_tbl_size;              // number of bitmaps in the utf8 table
    char **group_table;             // table of subexpression group names
    int group_tbl_size;             // number of groups in the group table
    int sub_name_tbl_size;          // always include, for VM compatability even when not compiled in
#ifdef MOJO_REGEX_VM_DEBUG
    regex_vm_sub_names_t *sub_name_table;
#endif // MOJO_REGEX_VM_DEBUG
};

// Global memory allocator prototypes. If you change these, be sure and do so
// BEFORE calling any API functions, or memory management may end up in an
// unstable state. Any caller provided functions are expected to clear any
// allocated memory to 0. Any caller provided deallocator should ignore NULL
// pointers.
typedef void *(*regexMemAllocator_t)(size_t size, void *ctx);
typedef void (*regexMemDeallocator_t)(void *ptr, void *ctx);
typedef void *(*regexMemReallocator_t)(void *ptr, size_t old_size, size_t new_size, void *ctx);

void regexSetMemoryAllocators(regexMemAllocator_t alloc,
                              regexMemDeallocator_t dealloc,
                              regexMemReallocator_t re_alloc,
                              void *context);

typedef enum {
    // The first tokens map directly to VM opcodes
    eTokenFirstToken = 0,
    eTokenCharLiteral = 0,      // 0
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
    eTokenRange,
#ifdef MOJO_REGEX_COMPILE_IMPLEMENTATION
    // The remaining tokens are abstractions, and reduce down to the above VM opcodes
    eTokenConcatenation,
    eTokenAlternative,
    eTokenZeroOrOne,
    eTokenZeroOrMany,
    eTokenOneOrMany,
    eTokenSubExprStart,     // 20
    eTokenSubExprEnd,
#endif // MOJO_REGEX_COMPILE_IMPLEMENTATION
    eTokenUnknown // <-- this should always be the last token enum
} eRegexToken_t;

#ifdef MOJO_REGEX_COMPILE_IMPLEMENTATION

#define DEF_VM_SIZE_INC 1024

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
    eCompileMissingSubexprEnd,
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

typedef struct regex_unicode_class_s regex_unicode_class_t;
struct regex_unicode_class_s {
    const char *alias;
    const char *name;
    const char *class_string;
    regex_unicode_class_t *next;
};

int regexRegUnicodeCharClassAdd(regex_unicode_class_t *utf8class);
int regexRegUnicodeCharClassAddTable(regex_unicode_class_t *utf8class);
void regexRegUnicodeCharClassRemove(const char *classId);

// Token flags //////////////////////////////////////////////////////////////

#define RE_TOK_FLAG_CASE_INS                0x08u   // eTokenSubExpr
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

#define RE_VM_FLAG_CHAR_INVERT          VM_OP_A_BIT(31u)
#define RE_VM_FLAG_CHARANY_DOTALL       0x1u
#define RE_VM_FLAG_MATCH_END_OF_INPUT   0x1u
#define RE_VM_FLAG_SAVE_COMPOUND        0x1u
#define RE_VM_FLAG_UTF8_CLASS_INVERT    VM_OP_B_BIT(17u)
#define RE_VM_FLAG_ASSERT_START_OF_LINE 0x0u
#define RE_VM_FLAG_ASSERT_END_OF_LINE   0x1u
#define RE_VM_FLAG_ASSERT_START_OF_WORD 0x2u
#define RE_VM_FLAG_ASSERT_END_OF_WORD   0x3u
#define RE_VM_FLAG_UTF8_LITERAL_INVERT  VM_OP_A_BIT(29u)

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
        int jump; // eTokenCharLiteral
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

typedef struct regex_expr_s regex_expr_t;
struct regex_expr_s {
    regex_token_t *tokens;
    regex_token_t *operators;
    regex_expr_t *parent;
};

typedef struct regex_build_ctx_s regex_build_ctx_t;
struct regex_build_ctx_s {
    regex_token_t *token_pool;
    regex_ptrlist_t *ptrlist_pool;
    regex_expr_t *expr_pool;

    regex_vm_t *vm;
};

typedef struct regex_pattern_s regex_pattern_t;
struct regex_pattern_s {
    const char *pattern;
    const char *base_pattern;
    int pos;
    int len;
    regex_pattern_t *parent;
};

typedef struct regex_build_s regex_build_t;
struct regex_build_s {
    regex_build_ctx_t *context;
    regex_expr_t *expr;
    regex_pattern_t *pattern;
    eRegexCompileStatus_t status;
};

regex_build_t *regexBuildCreate(regex_build_ctx_t *context);
void regexBuildDestroy(regex_build_t *build);
regex_expr_t *regexExprCreate(regex_build_t *build);
int regexExprFinalize(regex_expr_t *expr);
void regexExprDestroy(regex_build_ctx_t *context, regex_expr_t *expr);
int regexBuildFinalize(regex_build_t *build);

regex_pattern_t *regexPatternCreate(const char *pattern);
void regexPatternDestroy(regex_pattern_t *pattern);
int regexBuildPatternPush(regex_build_t *build, const char *pattern);
void regexBuildPatternPop(regex_build_t *build);

int regexBuildConcatToken(regex_build_t *build, regex_token_t *token);
int regexBuildConcatChar(regex_build_t *build, int c, int invert);
int regexBuildConcatCharClass(regex_build_t *build, unsigned int *bitmap);
int regexBuildConcatString(regex_build_t *build, char *str, int len);
int regexBuildConcatCharAny(regex_build_t *build, int dot_all);
int regexBuildConcatUtf8Class(regex_build_t *build, int lead_bytes, int mid_high, int mid_low, int invert, unsigned int *bitmap);
int regexBuildConcatCall(regex_build_t *build, int sub_index);
int regexBuildConcatByte(regex_build_t *build);
int regexBuildConcatAssertion(regex_build_t *build, int assertion);
int regexBuildConcatUtf8Literal(regex_build_t *build, int c, int invert);

int regexBuildGroupStart(regex_build_t *build, const char *name, int name_len,
                         int no_capture, int compound, const char *subroutine, int sub_len);
int regexBuildGroupEnd(regex_build_t *build);
int regexBuildWrapSubexpression(regex_build_t *build, const char *name, int name_len,
                                int no_capture, int compound, const char *subroutine, int sub_len);

typedef enum {
    eReQuantifyZeroOrOne,
    eReQuantifyZeroOrMany,
    eReQuantifyOneOrMany
} eReQuantifier_t;

int regexBuildQuantify(regex_build_t *build, eReQuantifier_t quantifier);
int regexBuildRange(regex_build_t *build, int min, int max);

regex_vm_t *regexVMCreate(regex_build_ctx_t *context);
void regexVMDestroy(regex_build_ctx_t *context);
int regexVMStringTableEntryAdd(regex_build_ctx_t *context, char *str, int len);
int regexVMCharClassEntryAdd(regex_build_ctx_t *context, unsigned int *bitmap);
int regexVMUtf8ClassEntryAdd(regex_build_ctx_t *context, unsigned int *bitmap);
int regexVMGroupEntryAdd(regex_build_ctx_t *context, const char *name, int len, int index);
int regexVMSubNameEntryAdd(regex_build_ctx_t *context, const char *name, const char *alias, int index);
int regexVMSubNameEntrySetPC(regex_vm_t *vm, int index, int pc);
#endif // MOJO_REGEX_COMPILE_IMPLEMENTATION
#ifdef MOJO_REGEX_COMMON_IMPLEMENTATION
const char *regexVMStringTableEntryGet(regex_vm_t *vm, int index, int *len);
const unsigned int *regexVMCharClassEntryGet(regex_vm_t *vm, int index);
const unsigned int *regexVMUtf8ClassEntryGet(regex_vm_t *vm, int index);
const char *regexVMGroupEntryGet(regex_vm_t *vm, int group);
const char *regexVMSubNameEntryGet(regex_vm_t *vm, int pc);
const char *regexVMSubAliasEntryGet(regex_vm_t *vm, int pc);
#endif // MOJO_REGEX_COMMON_IMPLEMENTATION

#ifdef MOJO_REGEX_COMPILE_IMPLEMENTATION

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

void _reMetaTokEmit_eTokenCharLiteral(FILE *fp, regex_build_ctx_t *build_ctx, regex_token_t *token);
void _reMetaTokEmit_eTokenCharClass(FILE *fp, regex_build_ctx_t *build_ctx, regex_token_t *token);
void _reMetaTokEmit_eTokenStringLiteral(FILE *fp, regex_build_ctx_t *build_ctx, regex_token_t *token);
void _reMetaTokEmit_eTokenCharAny(FILE *fp, regex_build_ctx_t *build_ctx, regex_token_t *token);
void _reMetaTokEmit_eTokenMatch(FILE *fp, regex_build_ctx_t *build_ctx, regex_token_t *token);
void _reMetaTokEmit_eTokenUtf8Class(FILE *fp, regex_build_ctx_t *build_ctx, regex_token_t *token);
void _reMetaTokEmit_eTokenCall(FILE *fp, regex_build_ctx_t *build_ctx, regex_token_t *token);
void _reMetaTokEmit_eTokenAssertion(FILE *fp, regex_build_ctx_t *build_ctx, regex_token_t *token);
void _reMetaTokEmit_eTokenUtf8Literal(FILE *fp, regex_build_ctx_t *build_ctx, regex_token_t *token);
void _reMetaTokEmit_eTokenRange(FILE *fp, regex_build_ctx_t *build_ctx, regex_token_t *token);
void _reMetaTokEmit_eTokenSubExprStart(FILE *fp, regex_build_ctx_t *build_ctx, regex_token_t *token);
void _reMetaTokEmit_eTokenSave(FILE *fp, regex_build_ctx_t *build_ctx, regex_token_t *token);

#endif // MOJO_REGEX_COMPILE_IMPLEMENTATION
#ifdef MOJO_REGEX_COMMON_IMPLEMENTATION

#define RE_META_VM_EMIT_N(token)    NULL
#define RE_META_VM_EMIT_Y(token)    _reMetaVMEmit_ ## token
#define RE_META_VM_EMIT(unique,token)  RE_META_VM_EMIT_ ## unique(token)

void _reMetaVMEmit_eTokenCharLiteral(FILE *fp, regex_vm_t *vm, int opcode, unsigned int operand_a, unsigned int operand_b);
void _reMetaVMEmit_eTokenCharClass(FILE *fp, regex_vm_t *vm, int opcode, unsigned int operand_a, unsigned int operand_b);
void _reMetaVMEmit_eTokenStringLiteral(FILE *fp, regex_vm_t *vm, int opcode, unsigned int operand_a, unsigned int operand_b);
void _reMetaVMEmit_eTokenCharAny(FILE *fp, regex_vm_t *vm, int opcode, unsigned int operand_a, unsigned int operand_b);
void _reMetaVMEmit_eTokenMatch(FILE *fp, regex_vm_t *vm, int opcode, unsigned int operand_a, unsigned int operand_b);
void _reMetaVMEmit_eTokenSplit(FILE *fp, regex_vm_t *vm, int opcode, unsigned int operand_a, unsigned int operand_b);
void _reMetaVMEmit_eTokenJmp(FILE *fp, regex_vm_t *vm, int opcode, unsigned int operand_a, unsigned int operand_b);
void _reMetaVMEmit_eTokenSave(FILE *fp, regex_vm_t *vm, int opcode, unsigned int operand_a, unsigned int operand_b);
void _reMetaVMEmit_eTokenUtf8Class(FILE *fp, regex_vm_t *vm, int opcode, unsigned int operand_a, unsigned int operand_b);
void _reMetaVMEmit_eTokenCall(FILE *fp, regex_vm_t *vm, int opcode, unsigned int operand_a, unsigned int operand_b);
void _reMetaVMEmit_eTokenAssertion(FILE *fp, regex_vm_t *vm, int opcode, unsigned int operand_a, unsigned int operand_b);
void _reMetaVMEmit_eTokenUtf8Literal(FILE *fp, regex_vm_t *vm, int opcode, unsigned int operand_a, unsigned int operand_b);
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
        RE_TOK_DETAIL(eTokenSave,           "save",       Y,    Y,    None,     eReTokNotTerminal,  0),
        RE_TOK_DETAIL(eTokenUtf8Class,      "utf8class",  Y,    Y,    None,     eReTokTerminal,     0),
        RE_TOK_DETAIL(eTokenCall,           "call",       Y,    Y,    None,     eReTokTerminal,     0),
        RE_TOK_DETAIL(eTokenReturn,         "return",     N,    N,    None,     eReTokTerminal,     0),
        RE_TOK_DETAIL(eTokenByte,           "byte",       N,    N,    None,     eReTokTerminal,     0),
        RE_TOK_DETAIL(eTokenAssertion,      "assertion",  Y,    Y,    None,     eReTokTerminal,     0),
        RE_TOK_DETAIL(eTokenUtf8Literal,    "utf8literal",Y,    Y,    None,     eReTokTerminal,     0),
#endif // MOJO_REGEX_COMMON_IMPLEMENTATION
#ifdef MOJO_REGEX_COMPILE_IMPLEMENTATION
        RE_TOK_DETAIL(eTokenConcatenation,  NULL,         N,    N,    Medium,   eReTokNotTerminal,  2),
        RE_TOK_DETAIL(eTokenAlternative,    NULL,         N,    N,    Medium,   eReTokNotTerminal,  2),
        RE_TOK_DETAIL(eTokenZeroOrOne,      NULL,         N,    N,    High,     eReTokPreceeding,   1),
        RE_TOK_DETAIL(eTokenZeroOrMany,     NULL,         N,    N,    High,     eReTokPreceeding,   1),
        RE_TOK_DETAIL(eTokenOneOrMany,      NULL,         N,    N,    High,     eReTokPreceeding,   1),
        RE_TOK_DETAIL(eTokenRange,          NULL,         N,    Y,    High,     eReTokPreceeding,   1),
        RE_TOK_DETAIL(eTokenSubExprStart,   NULL,         N,    Y,    Low,      eReTokNotPreceeding,0),
        RE_TOK_DETAIL(eTokenSubExprEnd,     NULL,         N,    N,    Low,      eReTokPreceeding,   0),
#endif // MOJO_REGEX_COMPILE_IMPLEMENTATION
#ifdef MOJO_REGEX_COMMON_IMPLEMENTATION
        RE_TOK_DETAIL_END
};
#endif // MOJO_REGEX_COMMON_IMPLEMENTATION
#ifdef MOJO_REGEX_COMPILE_IMPLEMENTATION

int regexVerifyTokenDetails(void);
// detail: 1 - address, 2 - concatenation, 3 - pc, 4 - DFA pointers
int regexTokenStrEmit(FILE *fp, regex_build_ctx_t *build_ctx, regex_token_t *token, int detail);
void regexTokenStrStackEmit(FILE *fp, regex_build_ctx_t *build_ctx, regex_token_t *token, int detail);
void regexTokenWalkTree(regex_token_t *token, int val);
void regexTokenStrDFAEmit(FILE *fp, regex_build_ctx_t *build_ctx, regex_token_t *token);
void regexVMInstrEmit(FILE *fp, regex_vm_t *vm, int pc);

// Determines whether a given token is a terminal operand. Used when comparing
// two adjacent tokens, the preceeding flag indicates whether the token being
// checked is leftmost.
int regexTokenIsTerminal(regex_token_t *token, int preceeding);

// Token priority when applying operators to operands in the shunting yard
eRegexTokenPriority_t regexGetTokenTypePriority(eRegexToken_t tokenType);

// Operator arity (number of tokens that the operator functions on) in the
// shunting yard
int regexGetOperatorArity(regex_token_t *token);

#endif // MOJO_REGEX_COMPILE_IMPLEMENTATION

/////////////////////////////////////////////////////////////////////////////
#ifdef MOJO_REGEX_IMPLEMENTATION
/////////////////////////////////////////////////////////////////////////////

/////////////////////////////////////////////////////////////////////////////
// Delegated memory allocation handlers
/////////////////////////////////////////////////////////////////////////////

static void *_regexDefMemAllocator(size_t size, void __unused *ctx) {
    void *ptr = malloc(size);
    memset(ptr, 0, size);
    return ptr;
}

static void _regexDefMemDeallocator(void *ptr, void __unused *ctx) {
    if(ptr != NULL) {
        free(ptr);
    }
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

regex_unicode_class_t _regex_char_class_default_import_table[] = {
        {"M", "Mark", _uax_db_Mark, NULL},
        {"N", "Number", _uax_db_Number, NULL},
        {"P", "Punctuation", _uax_db_Punctuation, NULL},
        {"Z", "Separator", _uax_db_Separator, NULL},
        {"Lu", "Uppercase_Letter", _uax_db_Uppercase_Letter, NULL},
        {"Ll", "Lowercase_Letter", _uax_db_Lowercase_Letter, NULL},
        {"L", "Letter", _uax_db_Letter, NULL},
        {NULL, NULL, NULL, NULL}
};

regex_unicode_class_t _subroutine_test = {
        NULL, "test",
        "A-Za-z\\uA000-\\uA0FF",
        //"A-Za-z\\u0123-\\u0138\\uA000-\\uA0FF",
        NULL
};

static int _regex_unicode_table_initialized = 0;
regex_unicode_class_t *_regex_unicode_charclass_table = NULL;

int regexRegUnicodeCharClassAdd(regex_unicode_class_t *utf8class) {
    regex_unicode_class_t *walk;

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

int regexRegUnicodeCharClassAddTable(regex_unicode_class_t *utf8class) {
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
    _regex_unicode_table_initialized = 1;
    return regexRegUnicodeCharClassAddTable(_regex_char_class_default_import_table);
}

static const char *_regexRegUnicodeCharClassGet(const char *classId, int len) {
    regex_unicode_class_t *walk;

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
    regex_unicode_class_t *walk;

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
    regex_unicode_class_t *entry, *last = NULL;

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
// VM image management functions
/////////////////////////////////////////////////////////////////////////////

regex_vm_t *regexVMCreate(regex_build_ctx_t *context) {
    if((context->vm = _regexAlloc(sizeof(regex_vm_t), _regexMemContext)) == NULL) {
        return NULL;
    }
    context->vm->vm_version = REGEX_VM_MACHINE_VERSION;
    return context->vm;
}

void regexVMDestroy(regex_build_ctx_t *context) {
    int k;

    if(context->vm != NULL) {
        for(k = 0; k < context->vm->string_tbl_size; k++) {
            _regexDealloc(context->vm->string_table[k], _regexMemContext);
        }
        _regexDealloc(context->vm->string_table, _regexMemContext);
        _regexDealloc(context->vm->string_tbl_len, _regexMemContext);
        for(k = 0; k < context->vm->class_tbl_size; k++) {
            _regexDealloc(context->vm->class_table[k], _regexMemContext);
        }
        _regexDealloc(context->vm->class_table, _regexMemContext);
        for(k = 0; k < context->vm->utf8_tbl_size; k++) {
            _regexDealloc(context->vm->utf8_class_table[k], _regexMemContext);
        }
        _regexDealloc(context->vm->utf8_class_table, _regexMemContext);
        for(k = 0; k < context->vm->group_tbl_size; k++) {
            _regexDealloc(context->vm->group_table[k], _regexMemContext);
        }
        _regexDealloc(context->vm->group_table, _regexMemContext);
#ifdef MOJO_REGEX_VM_DEBUG
        for(k = 0; k < context->vm->sub_name_tbl_size; k++) {
            _regexDealloc(context->vm->sub_name_table[k].name, _regexMemContext);
            _regexDealloc(context->vm->sub_name_table[k].alias, _regexMemContext);
        }
        _regexDealloc(context->vm->sub_name_table, _regexMemContext);
#endif // MOJO_REGEX_VM_DEBUG
        _regexDealloc(context->vm, _regexMemContext);
        context->vm = NULL;
    }
}

// Returns -1 if out of mem, otherwise returns the index of the entry in the
// string table. This function will take ownership of the string, freeing any
// duplicate table entry values.
int regexVMStringTableEntryAdd(regex_build_ctx_t *context, char *str, int len) {
    int k;

    for(k = 0; k < context->vm->string_tbl_size; k++) {
        if((context->vm->string_tbl_len[k] == len) &&
           (!memcmp(context->vm->string_table[k], str, len))) {
            _regexDealloc(str, _regexMemContext);
            return k;
        }
    }
    if(((context->vm->string_table = _regexRealloc(context->vm->string_table,
                                                   (context->vm->string_tbl_size) * sizeof(char *),
                                                   (context->vm->string_tbl_size + 1) * sizeof(char *),
                                                   _regexMemContext)) == NULL) ||
       ((context->vm->string_tbl_len = _regexRealloc(context->vm->string_tbl_len,
                                                     (context->vm->string_tbl_size) * sizeof(int),
                                                     (context->vm->string_tbl_size + 1) * sizeof(int),
                                                     _regexMemContext)) == NULL)) {
        return -1;
    }
    context->vm->string_table[context->vm->string_tbl_size] = str;
    context->vm->string_tbl_len[context->vm->string_tbl_size] = len;
    context->vm->string_tbl_size++;
    return context->vm->string_tbl_size - 1;
}

int regexVMCharClassEntryAdd(regex_build_ctx_t *context, unsigned int *bitmap) {
    int k;

    for(k = 0; k <context->vm->class_tbl_size; k++) {
        if(!memcmp(context->vm->class_table[k], bitmap, 32)) {
            _regexDealloc(bitmap, _regexMemContext);
            return k;
        }
    }
    if((context->vm->class_table = _regexRealloc(context->vm->class_table,
                                                 (context->vm->class_tbl_size) * sizeof(unsigned int *),
                                                 (context->vm->class_tbl_size + 1) * sizeof(unsigned int *),
                                                 _regexMemContext)) == NULL) {
        return -1;
    }
    context->vm->class_table[context->vm->class_tbl_size] = bitmap;
    context->vm->class_tbl_size++;
    return context->vm->class_tbl_size - 1;
}

int regexVMUtf8ClassEntryAdd(regex_build_ctx_t *context, unsigned int *bitmap) {
    int k;

    for(k = 0; k <context->vm->utf8_tbl_size; k++) {
        if(!memcmp(context->vm->utf8_class_table[k], bitmap, 8)) {
            _regexDealloc(bitmap, _regexMemContext);
            return k;
        }
    }
    if((context->vm->utf8_class_table = _regexRealloc(context->vm->utf8_class_table,
                                                      (context->vm->utf8_tbl_size) * sizeof(unsigned int *),
                                                      (context->vm->utf8_tbl_size + 1) * sizeof(unsigned int *),
                                                      _regexMemContext)) == NULL) {
        return -1;
    }
    context->vm->utf8_class_table[context->vm->utf8_tbl_size] = bitmap;
    context->vm->utf8_tbl_size++;
    return context->vm->utf8_tbl_size - 1;
}

int regexVMGroupEntryAdd(regex_build_ctx_t *context, const char *name, int len, int index) {
    if(index >= context->vm->group_tbl_size) {
        if((context->vm->group_table = _regexRealloc(context->vm->group_table,
                                                     (context->vm->group_tbl_size) * sizeof(char *),
                                                     (index + 1) * sizeof(char *),
                                                     _regexMemContext)) == NULL) {
            return 0;
        }
        context->vm->group_tbl_size = index + 1;
    }
    if(context->vm->group_table[index] == NULL) {
        if((context->vm->group_table[index] = _regexStrdup(name, len)) == NULL) {
            return 0;
        }
    }
    return 1;
}

int regexVMSubNameEntryAdd(regex_build_ctx_t *context, const char *name, const char *alias, int index) {
    int k;

    for(k = 0; k < context->vm->sub_name_tbl_size; k++) {
        if(context->vm->sub_name_table[k].id == index) {
            break;
        }
    }
    if(k >= context->vm->sub_name_tbl_size) {
        if((context->vm->sub_name_table = _regexRealloc(context->vm->sub_name_table,
                                                        (context->vm->sub_name_tbl_size) * sizeof(regex_vm_sub_names_t),
                                                        (context->vm->sub_name_tbl_size + 1) * sizeof(regex_vm_sub_names_t),
                                                        _regexMemContext)) == NULL) {
            return 0;
        }
        k = context->vm->sub_name_tbl_size;
        context->vm->sub_name_tbl_size++;
    }
    if((name != NULL) && (context->vm->sub_name_table[k].name == NULL)) {
        if((context->vm->sub_name_table[k].name = _regexStrdup(name, RE_STR_NULL_TERM)) == NULL) {
            return 0;
        }
    }
    if((alias != NULL) && (context->vm->sub_name_table[k].alias == NULL)) {
        if((context->vm->sub_name_table[k].alias = _regexStrdup(alias, RE_STR_NULL_TERM)) == NULL) {
            return 0;
        }
    }
    return 1;
}

int regexVMSubNameEntrySetPC(regex_vm_t *vm, int index, int pc) {
    int k;

    for(k = 0; k < vm->sub_name_tbl_size; k++) {
        if(vm->sub_name_table[k].id == index) {
            vm->sub_name_table[k].pc = pc;
            return 1;
        }
    }
    return 0;
}

#endif // MOJO_REGEX_COMPILE_IMPLEMENTATION
#ifdef MOJO_REGEX_COMMON_IMPLEMENTATION

const char *regexVMStringTableEntryGet(regex_vm_t *vm, int index, int *len) {
    if((index < 0) || (index >= vm->string_tbl_size)) {
        return NULL;
    }
    if(len != NULL) {
        *len = vm->string_tbl_len[index];
    }
    return vm->string_table[index];
}

const unsigned int *regexVMCharClassEntryGet(regex_vm_t *vm, int index) {
    if((index < 0) || (index >= vm->class_tbl_size)) {
        return NULL;
    }
    return vm->class_table[index];
}

const unsigned int *regexVMUtf8ClassEntryGet(regex_vm_t *vm, int index) {
    if((index < 0) || (index >= vm->utf8_tbl_size)) {
        return NULL;
    }
    return vm->utf8_class_table[index];
}

const char *regexVMGroupEntryGet(regex_vm_t *vm, int group) {
    if((group < 1) || ((group / 2) >= vm->group_tbl_size)) {
        return NULL;
    }
    return vm->group_table[group / 2];
}

const char *regexVMSubNameEntryGet(regex_vm_t *vm, int pc) {
    int k;

    for(k = 0; k <vm->sub_name_tbl_size; k++) {
        if(vm->sub_name_table[k].pc == pc) {
            return vm->sub_name_table[k].name;
        }
    }
    return NULL;
}

const char *regexVMSubAliasEntryGet(regex_vm_t *vm, int pc) {
    int k;

    for(k = 0; k <vm->sub_name_tbl_size; k++) {
        if(vm->sub_name_table[k].pc == pc) {
            return vm->sub_name_table[k].alias;
        }
    }
    return NULL;
}

#endif // MOJO_REGEX_COMMON_IMPLEMENTATION
#ifdef MOJO_REGEX_COMPILE_IMPLEMENTATION

/////////////////////////////////////////////////////////////////////////////
// Pattern tracing
/////////////////////////////////////////////////////////////////////////////

regex_pattern_t *regexPatternCreate(const char *pattern) {
    regex_pattern_t *pat;

    if((pat = _regexAlloc(sizeof(regex_pattern_t), _regexMemContext)) == NULL) {
        return NULL;
    }
    pat->pattern = pattern;
    pat->base_pattern = pattern;
    return pat;
}

void regexPatternDestroy(regex_pattern_t *pattern) {
    if(pattern != NULL) {
        _regexDealloc(pattern, _regexMemContext);
    }
}

int regexBuildPatternPush(regex_build_t *build, const char *pattern) {
    regex_pattern_t *pat;

    if((pat = regexPatternCreate(pattern)) == NULL) {
        return 0;
    }
    pat->parent = build->pattern;
    build->pattern = pat;
    return 1;
}

void regexBuildPatternPop(regex_build_t *build) {
    regex_pattern_t *pattern;

    if((pattern = build->pattern) != NULL) {
        build->pattern = pattern->parent;
        regexPatternDestroy(pattern);
    }
}

/////////////////////////////////////////////////////////////////////////////
// Build token stream assembly
/////////////////////////////////////////////////////////////////////////////

regex_expr_t *regexExprCreate(regex_build_t *build) {
    regex_expr_t *expr;

    if((build == NULL) || (build->context->expr_pool == NULL)) {
        expr = _regexAlloc(sizeof(regex_expr_t), _regexMemContext);
    } else {
        expr = build->context->expr_pool;
        build->context->expr_pool = expr->parent;
        memset(expr, 0, sizeof(regex_expr_t));
    }
    return expr;
}

int regexExprFinalize(regex_expr_t *expr) {
    // TODO
    return 0;
}

void _regexTokenDestroy(regex_build_ctx_t *context, regex_token_t *token, int full_stack);

void regexExprDestroy(regex_build_ctx_t *context, regex_expr_t *expr) {
    regex_expr_t *next;

    for(; expr != NULL; expr = next) {
        next = expr->parent;
        _regexTokenDestroy(context, expr->tokens, 1);
        _regexTokenDestroy(context, expr->operators, 1);
        memset(expr, 0, sizeof(regex_expr_t));
        expr->parent = context->expr_pool;
        context->expr_pool = expr;
    }
}

static regex_ptrlist_t *_regexPtrlistCreate(regex_build_ctx_t *context,
                                            regex_token_t *token,
                                            eRePtrListType_t type) {
    regex_ptrlist_t *entry;

    if((context != NULL) && (context->ptrlist_pool != NULL)) {
        entry = context->ptrlist_pool;
        context->ptrlist_pool = entry->next;
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
static int _regexPtrlistPatch(regex_build_ctx_t *context, regex_ptrlist_t **list, regex_token_t *token, int no_jumps) {
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
        if(context != NULL) {
            walk->next = context->ptrlist_pool;
            context->ptrlist_pool = walk;
        } else {
            _regexDealloc(walk, _regexMemContext);
        }
        patched++;
    }
    *list = jmps;
    return patched;
}

static void _regexPtrListFree(regex_build_ctx_t *context, regex_ptrlist_t *list) {
    regex_ptrlist_t *next = NULL;

    for(; list != NULL; list = next) {
        next = list->next;
        if(context != NULL) {
            list->next = context->ptrlist_pool;
            context->ptrlist_pool = list;
        } else {
            _regexDealloc(list, _regexMemContext);
        }
    }
}

static void _regexPtrListDump(regex_ptrlist_t *list) {
    for(; list != NULL; list = list->next) {
        printf("PtrList node 0x%4.4X - %s\n", (int)((uintptr_t)list->token & 0xFFFFu), (list->type == eRePtrOutA ? "out_a" : "out_b"));
    }
}

static regex_token_t *_regexAllocToken(regex_build_ctx_t *context, eRegexToken_t tokenType,
                                       const char *str, const unsigned int *bitmap,
                                       int len, int retain_ptr) {
    regex_token_t *token;

    if((context != NULL) && (context->token_pool != NULL)) {
        token = context->token_pool;
        context->token_pool = token->next;
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

void _regexTokenDestroy(regex_build_ctx_t *context, regex_token_t *token, int full_stack) {
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
                token->str = NULL;
                break;
            default:
                break;
        }
        _regexPtrListFree(NULL, token->ptrlist);
        if(context != NULL) {
            token->next = context->token_pool;
            context->token_pool = token;
        } else {
            _regexDealloc(token, _regexMemContext);
        }
        if(!full_stack) {
            break;
        }
    }
}

static void _regexPushOperand(regex_build_t *build, regex_token_t *token) {
    token->next = build->expr->tokens;
    build->expr->tokens = token;
}

static void _regexPushOperator(regex_build_t *build, regex_token_t *token) {
    token->next = build->expr->operators;
    build->expr->operators = token;
}

static regex_token_t *_regexTokenBaseCreate(regex_build_t *build,
                                            eRegexToken_t tokenType,
                                            const void *str, const void *ptr,
                                            int retain_ptr, int len) {
    regex_token_t *token, *concat;

    if((token = _regexAllocToken(build->context, tokenType, str, ptr, len, retain_ptr)) == NULL) {
        return NULL;
    }

    if((build->expr->tokens != NULL) && regexTokenIsTerminal(token, 0) && regexTokenIsTerminal(build->expr->tokens, 1)) {
        // Two adjacent terminals have an implicit concatenation
        if((concat = _regexAllocToken(build->context, eTokenConcatenation, NULL, NULL, 0, 0)) == NULL) {
            _regexTokenDestroy(build->context, token, 0);
            return NULL;
        }
        _regexPushOperator(build, concat);
    }
    _regexPushOperand(build, token);
    return token;
}

int regexBuildConcatChar(regex_build_t *build, int c, int invert) {
    regex_token_t *token;

    if((token = _regexTokenBaseCreate(build, eTokenCharLiteral, NULL, NULL, 0, 0)) == NULL) {
        return 0;
    }
    token->c = c;
    token->jump = 0x3FFF;
    token->flags = (invert ? RE_TOK_FLAG_INVERT : 0);
    return 1;
}

int regexBuildConcatUtf8Literal(regex_build_t *build, int c, int invert) {
    regex_token_t *token;

    if((token = _regexTokenBaseCreate(build, eTokenUtf8Literal, NULL, NULL, 0, 0)) == NULL) {
        return 0;
    }
    token->c = c;
    token->flags = (invert ? RE_TOK_FLAG_INVERT : 0);
    return 1;
}

int regexBuildConcatCharClass(regex_build_t *build, unsigned int *bitmap) {
    return _regexTokenBaseCreate(build, eTokenCharClass, NULL, bitmap, 1, 0) != NULL;
}

int regexBuildConcatString(regex_build_t *build, char *str, int len) {
    return _regexTokenBaseCreate(build, eTokenStringLiteral, str, NULL, 0, len) != NULL;
}

int regexBuildConcatCharAny(regex_build_t *build, int dot_all) {
    regex_token_t *token;

    if((token = _regexTokenBaseCreate(build, eTokenCharAny, NULL, NULL, 0, 0)) == NULL) {
        return 0;
    }
    token->flags = (dot_all ? RE_TOK_FLAG_DOT_ALL : 0);
    return 1;
}

int regexBuildConcatUtf8Class(regex_build_t *build, int lead_bytes, int mid_high,
                              int mid_low, int invert, unsigned int *bitmap) {
    regex_token_t *token;
    short lead_bits = (short)(((unsigned int)mid_high << 6u) | (unsigned int)mid_low);

    if((token = _regexTokenBaseCreate(build, eTokenUtf8Class, NULL, bitmap, 0, 64)) == NULL) {
        return 0;
    }
    token->flags = (invert ? RE_TOK_FLAG_INVERT : 0);
    token->lead_bits = (short)lead_bits;
    token->lead_count = (short)lead_bytes;
    return 1;
}

int regexBuildConcatCall(regex_build_t *build, int sub_index) {
    regex_token_t *token;

    if((token = _regexTokenBaseCreate(build, eTokenCall, NULL, NULL, 0, 0)) == NULL) {
        return 0;
    }
    token->sub_index = (short)sub_index;
    return 1;
}

int regexBuildConcatByte(regex_build_t *build) {
    return _regexTokenBaseCreate(build, eTokenByte, NULL, NULL, 0, 0) != NULL;
}

int regexBuildConcatAssertion(regex_build_t *build, int assertion) {
    regex_token_t *token;

    if((token = _regexTokenBaseCreate(build, eTokenAssertion, NULL, NULL, 0, 0)) == NULL) {
        return 0;
    }
    token->flags = (unsigned int)assertion;
    return 1;
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

int regexBuildGroupStart(regex_build_t *build, const char *name, int name_len,
                         int no_capture, int compound, const char *subroutine, int sub_len);
int regexBuildGroupEnd(regex_build_t *build);
int regexBuildWrapSubexpression(regex_build_t *build, const char *name, int name_len,
                                int no_capture, int compound, const char *subroutine, int sub_len);

int regexBuildQuantify(regex_build_t *build, eReQuantifier_t quantifier) {
    regex_token_t *token;
    eRegexToken_t type;

    switch(quantifier) {
        case eReQuantifyZeroOrOne: type = eTokenZeroOrOne; break;
        case eReQuantifyZeroOrMany: type = eTokenZeroOrMany; break;
        case eReQuantifyOneOrMany: type = eTokenOneOrMany; break;
        default: return 0;
    }
    if((token = _regexAllocToken(build->context, type, NULL, NULL, 0, 0)) == NULL) {
        return 0;
    }
    _regexPushOperator(build, token);
}

int regexBuildRange(regex_build_t *build, int min, int max) {
    regex_token_t *token;
    if((token = _regexAllocToken(build->context, eTokenRange, NULL, NULL, 0, 0)) == NULL) {
        return 0;
    }
    token->min = min;
    token->max = max;
    _regexPushOperator(build, token);
}

/////////////////////////////////////////////////////////////////////////////
// Builder handling
/////////////////////////////////////////////////////////////////////////////

regex_build_t *regexBuildCreate(regex_build_ctx_t *context) {
    regex_build_t *build;

    if((build = _regexAlloc(sizeof(regex_build_t), _regexMemContext)) == NULL) {
        return NULL;
    }
    build->context = context;
    build->status = eCompileOk;
    return build;
}

void regexBuildDestroy(regex_build_t *build) {
    if(build != NULL) {
        while(build->pattern != NULL) {
            regexBuildPatternPop(build);
        }
        _regexDealloc(build, _regexMemContext);
    }
}

int regexBuildFinalize(regex_build_t *build) {
    if(build == NULL) {
        return 0;
    }
    return regexExprFinalize(build->expr);
}

/////////////////////////////////////////////////////////////////////////////
// Build context management functions
/////////////////////////////////////////////////////////////////////////////

regex_build_ctx_t *regexBuildContextCreate(void) {
    regex_build_ctx_t *context;

    if((context = _regexAlloc(sizeof(regex_build_ctx_t), _regexMemContext)) == NULL) {
        return NULL;
    }
    _regexRegUnicodeInitializeTable();
    if(regexVMCreate(context) == NULL) {
        _regexDealloc(context, _regexMemContext);
        return NULL;
    }
    return context;
}

void regexBuildContextDestroy(regex_build_ctx_t *context) {
    regexVMDestroy(context);
    _regexTokenDestroy(context, context->token_pool, 1);
    _regexDealloc(context, _regexMemContext);
}

/////////////////////////////////////////////////////////////////////////////
// Token details and diagnostics
/////////////////////////////////////////////////////////////////////////////

void _reMetaTokEmit_eTokenCharLiteral(FILE *fp, regex_build_ctx_t *build_ctx, regex_token_t *token) {
    fprintf(fp, "'%c',%03d%s",
            (((token->c >= ' ') && (token->c <= 127)) ? token->c : '-'),
            token->c,
            ((token->flags & RE_TOK_FLAG_INVERT) ? ":invert" : ""));
}

void _reMetaTokEmit_eTokenCharClass(FILE *fp, regex_build_ctx_t *build_ctx, regex_token_t *token) {
    fputc('[', fp);
    // TODO
    //regexCharClassBitmapEmit(fp, token->bitmap);
    fputc(']', fp);
}

void _reMetaTokEmit_eTokenStringLiteral(FILE *fp, regex_build_ctx_t *build_ctx, regex_token_t *token) {
    fputc('\"', fp);
    // TODO
    //regexEscapedStrEmit(fp, token->str, token->len);
    fputc('\"', fp);
}

void _reMetaTokEmit_eTokenCharAny(FILE *fp, regex_build_ctx_t *build_ctx, regex_token_t *token) {
    if(token->flags & RE_TOK_FLAG_DOT_ALL) {
        fputs("dot_all", fp);
    }
}

void _reMetaTokEmit_eTokenMatch(FILE *fp, regex_build_ctx_t *build_ctx, regex_token_t *token) {
    if(token->flags & RE_TOK_FLAG_END_OF_INPUT) {
        fputs("end_of_input", fp);
    }
}

void _reMetaTokEmit_eTokenUtf8Class(FILE *fp, regex_build_ctx_t *build_ctx, regex_token_t *token) {
    // TODO
}

void _reMetaTokEmit_eTokenCall(FILE *fp, regex_build_ctx_t *build_ctx, regex_token_t *token) {
    // TODO
#if 0
    regex_subroutine_t *routine;

    fprintf(fp, "%d", token->sub_index);
    if((routine = _regexSubroutineIndexEntryGet(&(build_ctx->subroutine_index), NULL, NULL, 0, token->sub_index)) == NULL) {
        fputs("SUB_NOT_FOUND", fp);
    } else {
        if(routine->name != NULL) {
            fprintf(fp, ":%s", routine->name);
        }
        if(routine->alias != NULL) {
            fprintf(fp, ":%s", routine->alias);
        }
    }
#endif
}

void _reMetaTokEmit_eTokenAssertion(FILE *fp, regex_build_ctx_t *build_ctx, regex_token_t *token) {
    switch(token->flags) {
        case RE_TOK_FLAG_ASSERT_START_OF_LINE: fputs("^:start_of_line", fp); break;
        case RE_TOK_FLAG_ASSERT_END_OF_LINE: fputs("$:end_of_line", fp); break;
        case RE_TOK_FLAG_ASSERT_START_OF_WORD: fputs("\\<:start_of_word", fp); break;
        case RE_TOK_FLAG_ASSERT_END_OF_WORD: fputs("\\>:end_of_word", fp); break;
        default: fputs("unknown", fp); break;
    }
}

void _reMetaTokEmit_eTokenUtf8Literal(FILE *fp, regex_build_ctx_t *build_ctx, regex_token_t *token) {
    // TODO
}

void _reMetaTokEmit_eTokenRange(FILE *fp, regex_build_ctx_t *build_ctx, regex_token_t *token) {
    if(token->min >= 0) {
        fprintf(fp, "%d,", token->min);
    } else {
        fputs("-,", fp);
    }
    if(token->max >= 0) {
        fprintf(fp, "%d", token->max);
    } else {
        fputc('-', fp);
    }
}

void _reMetaTokEmit_eTokenSubExprStart(FILE *fp, regex_build_ctx_t *build_ctx, regex_token_t *token) {
    // TODO
#if 0
    const char *name;
    regex_subroutine_t *routine;
    int len;

    fprintf(fp, "%d", token->group);
    if(token->flags & RE_TOK_FLAG_COMPOUND) {
        fputs(":compound", fp);
    }
    if(token->flags & RE_TOK_FLAG_SUBROUTINE) {
        fprintf(fp, ":subroutine %d", token->sub_index);
        if((routine = _regexSubroutineIndexEntryGet(&(build_ctx->subroutine_index), NULL, NULL, 0, token->sub_index)) != NULL) {
            if((routine->name != NULL) || (routine->alias != NULL)) {
                fputc('(', fp);
                if(routine->name != NULL) {
                    fputs(routine->name, fp);
                    if(routine->alias != NULL) {
                        fputc(':', fp);
                    }
                }
                if(routine->alias != NULL) {
                    fputs(routine->alias, fp);
                }
                fputc(')', fp);
            }
        }
    }
    if(token->flags & RE_TOK_FLAG_NO_CAPTURE) {
        fputs(":no_capture", fp);
    }
    if((name = regexVMGroupTableEntryLookup(build_ctx->vm, token->group, &len)) != NULL) {
        fprintf(fp, ":named(%s)", name);
    }
#endif
}

void _reMetaTokEmit_eTokenSave(FILE *fp, regex_build_ctx_t *build_ctx, regex_token_t *token) {
    // TODO
#if 0
    const char *name;
    regex_subroutine_t *routine;
    int len;

    fprintf(fp, "%d", token->group);
    if(token->flags & RE_TOK_FLAG_COMPOUND) {
        fputs(":compound", fp);
    }
    if((name = regexVMGroupTableEntryLookup(build_ctx->vm, token->group, &len)) != NULL) {
        fprintf(fp, ":named(%s)", name);
    }
#endif
}

#endif // MOJO_REGEX_COMPILE_IMPLEMENTATION
#ifdef MOJO_REGEX_COMMON_IMPLEMENTATION

void _reMetaVMEmit_eTokenCharLiteral(FILE *fp, regex_vm_t *vm, int opcode, unsigned int operand_a, unsigned int operand_b) {
    char c = (char)((unsigned)operand_a & 0xFFu);
    fprintf(fp, "'%c' (%03d)%s",
            (((c >= ' ') && (c <= 127)) ? c : '-'), c,
            (((unsigned)operand_a & RE_VM_FLAG_CHAR_INVERT) ? "  inverse" : ""));
    if(operand_b != 0x3FFF) {
        fprintf(fp,"  jmp(%4.4d)", operand_b);
    }
}

void _reMetaVMEmit_eTokenCharClass(FILE *fp, regex_vm_t *vm, int opcode, unsigned int operand_a, unsigned int operand_b) {
    fputc('[', fp);
    // TODO
    //regexCharClassBitmapEmit(fp, vm->class_table[operand_a]);
    fputc(']', fp);
}

void _reMetaVMEmit_eTokenStringLiteral(FILE *fp, regex_vm_t *vm, int opcode, unsigned int operand_a, unsigned int operand_b) {
    const char *str;
    int len;

    str = regexVMStringTableEntryGet(vm, (int)operand_a, &len);
    fputc('"', fp);
    // TODO
    //regexEscapedStrEmit(fp, str, len);
    fputc('"', fp);
}

void _reMetaVMEmit_eTokenCharAny(FILE *fp, regex_vm_t *vm, int opcode, unsigned int operand_a, unsigned int operand_b) {
    if(operand_a & RE_VM_FLAG_CHARANY_DOTALL) {
        fputs("dot_all", fp);
    }
}

void _reMetaVMEmit_eTokenMatch(FILE *fp, regex_vm_t *vm, int opcode, unsigned int operand_a, unsigned int operand_b) {
    if(operand_a & RE_VM_FLAG_MATCH_END_OF_INPUT) {
        fputs("($)", fp);
    }
}

void _reMetaVMEmit_eTokenSplit(FILE *fp, regex_vm_t *vm, int opcode, unsigned int operand_a, unsigned int operand_b) {
    fprintf(fp, "%d, %d", operand_a, operand_b);
}

void _reMetaVMEmit_eTokenJmp(FILE *fp, regex_vm_t *vm, int opcode, unsigned int operand_a, unsigned int operand_b) {
    fprintf(fp, "%d", operand_a);
}

void _reMetaVMEmit_eTokenSave(FILE *fp, regex_vm_t *vm, int opcode, unsigned int operand_a, unsigned int operand_b) {
    // TODO
#if 0
    const char *str = regexVMGroupNameFromIndex(vm, (int)operand_a);

    fprintf(fp, "%d", operand_a);
    if(str != NULL) {
        fprintf(fp, " (%s)", str);
    }
#endif
}

void _reMetaVMEmit_eTokenUtf8Class(FILE *fp, regex_vm_t *vm, int opcode, unsigned int operand_a, unsigned int operand_b) {
    // TODO
}

void _reMetaVMEmit_eTokenCall(FILE *fp, regex_vm_t *vm, int opcode, unsigned int operand_a, unsigned int operand_b) {
#ifdef MOJO_REGEX_VM_DEBUG
    const char *name, *alias;
#endif // MOJO_REGEX_VM_DEBUG
    fprintf(fp, "%d", operand_a);
#ifdef MOJO_REGEX_VM_DEBUG
    name = regexVMSubNameEntryGet(vm, (int)operand_a);
    alias = regexVMSubAliasEntryGet(vm, (int)operand_a);
    if((name != NULL) || (alias != NULL)) {
        fputs(" (", fp);
        if(name != NULL) {
            fputs(name, fp);
            if(alias != NULL) {
                fputc(':', fp);
            }
        }
        if(alias != NULL) {
            fputs(alias, fp);
        }
        fputc(')', fp);
    }
#endif // MOJO_REGEX_VM_DEBUG
}

void _reMetaVMEmit_eTokenAssertion(FILE *fp, regex_vm_t *vm, int opcode, unsigned int operand_a, unsigned int operand_b) {
    switch(operand_a) {
        case RE_VM_FLAG_ASSERT_START_OF_LINE: fputs("(^)", fp); break;
        case RE_VM_FLAG_ASSERT_END_OF_LINE: fputs("($)", fp); break;
        case RE_VM_FLAG_ASSERT_START_OF_WORD: fputs("(\\<)", fp); break;
        case RE_VM_FLAG_ASSERT_END_OF_WORD: fputs("(\\>)", fp); break;
        default: fputs("(unknown)", fp); break;
    }
}

void _reMetaVMEmit_eTokenUtf8Literal(FILE *fp, regex_vm_t *vm, int opcode, unsigned int operand_a, unsigned int operand_b) {
    // TODO
}

int regexVerifyTokenDetails(void) {
    int k;
    for(k = 0; ((_regexTokenDetails[k].token != eTokenUnknown) &&
                (_regexTokenDetails[k].token == k)); k++);
    return ((_regexTokenDetails[k].token == eTokenUnknown) &&
            (_regexTokenDetails[k].token == k));
}

#endif // MOJO_REGEX_COMMON_IMPLEMENTATION
#ifdef MOJO_REGEX_COMPILE_IMPLEMENTATION

// detail 1 - address
// detail 2 - concatenation
// detail 3 - pc
// detail 4 - DFA pointers
int regexTokenStrEmit(FILE *fp, regex_build_ctx_t *build_ctx, regex_token_t *token, int detail) {
    if((detail < 2) && (token->tokenType == eTokenConcatenation)) {
        return 0;
    }
    if(detail) {
        fprintf(fp, "0x%4.4X:", (unsigned int)(((uintptr_t)token) & 0xFFFFu));
    }
    if(token == NULL) {
        fprintf(fp, "NULL(");
    } else if((token->tokenType < eTokenFirstToken) || (token->tokenType >= eTokenUnknown)) {
        fprintf(fp, "INVALID(%d", token->tokenType);
    } else {
        fprintf(fp, "%s(", _regexTokenDetails[token->tokenType].name);
        if(_regexTokenDetails[token->tokenType].emitToken != NULL) {
            _regexTokenDetails[token->tokenType].emitToken(fp, build_ctx, token);
        }
    }
    fputc(')', fp);
    if(detail > 2) {
        if(detail > 3) {
            if(token->out_a == NULL) {
                fputs("{------}", fp);
            } else {
                fprintf(fp, "{0x%4.4X}", (int)((uintptr_t)(token->out_a) & 0xFFFFu));
            }
            if(token->out_b == NULL) {
                fputs("{------}", fp);
            } else {
                fprintf(fp, "{0x%4.4X}", (int)((uintptr_t)(token->out_b) & 0xFFFFu));
            }
        } else {
            if(token == NULL) {
                fputs("[]", fp);
            } else {
                fprintf(fp, "[%d]", token->pc);
            }
        }
    }
    return 1;
}

void regexTokenStrStackEmit(FILE *fp, regex_build_ctx_t *build_ctx, regex_token_t *token, int detail) {
    for(; token != NULL; token = token->next) {
        if(regexTokenStrEmit(fp, build_ctx, token, detail)) {
            fputc('\n', fp);
        }
    }
}

void _regexTokenStrDFAEmit(FILE *fp, regex_build_ctx_t *build_ctx, regex_token_t *token, int indent, const char *prefix) {
    if(token != NULL) {
        if(token->pc == -1) {
            token->pc = -2;
            fprintf(fp, "%*.*s (%s)  ", indent, indent, "", prefix);
            regexTokenStrEmit(fp, build_ctx, token, 4);
            fputc('\n', fp);
            if(token->out_a != NULL) {
                _regexTokenStrDFAEmit(fp, build_ctx, token->out_a, indent + 4, "out_a");
            }
            if(token->out_b != NULL) {
                _regexTokenStrDFAEmit(fp, build_ctx, token->out_b, indent + 4, "out_b");
            }
        }
    }
}

void regexTokenWalkTree(regex_token_t *token, int val) {
    if(token != NULL) {
        if(token->pc != val) {
            token->pc = val;
            if(token->out_a != NULL) {
                regexTokenWalkTree(token->out_a, val);
            }
            if(token->out_b != NULL) {
                regexTokenWalkTree(token->out_b, val);
            }
        }
    }
}

void regexTokenStrDFAEmit(FILE *fp, regex_build_ctx_t *build_ctx, regex_token_t *token) {
    regexTokenWalkTree(token, -1);
    if(token != NULL) {
        _regexTokenStrDFAEmit(fp, build_ctx, token, 0, "root");
    }
    regexTokenWalkTree(token, -1);
}

#endif // MOJO_REGEX_COMPILE_IMPLEMENTATION
#ifdef MOJO_REGEX_COMMON_IMPLEMENTATION

void regexVMInstrEmit(FILE *fp, regex_vm_t *vm, int pc) {
    eRegexToken_t token;
    unsigned int oper_a, oper_b, instr;

    instr = vm->program[pc];
    token = (eRegexToken_t)(instr & 0xFu);
    oper_a = (instr >> 4u) & 0x3FFFu;
    oper_b = (instr >> 18u) & 0x3FFFu;

    fprintf(fp, "%4d  %11s", pc, _regexTokenDetails[token].instr);
    if(_regexTokenDetails[token].emitVMInstr != NULL) {
        fputs("  ", fp);
        _regexTokenDetails[token].emitVMInstr(fp, vm, token, oper_a, oper_b);
    }
    fputc('\n', fp);
}

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

/////////////////////////////////////////////////////////////////////////////
// Unicode property class definitions
/////////////////////////////////////////////////////////////////////////////

#ifdef MOJO_REGEX_UNICODE

// These property classes were generated from the unicode database by the
// extract_unicode_props.py script

// TODO - replace these stripped down placeholders with the actual unicode property classes

const char _uax_db_Mark[] = "\\u05BF\\u05C7\\u0670\\u0711\\u07FD\\u09BC\\u09D7";
const char _uax_db_Number[] = "\\u00B9\\u2070\\u2CFD\\u3007\\u{10341}";
const char _uax_db_Punctuation[] = "\\u005F\\u007B\\u007D\\u00A1\\u00A7\\u00AB";
const char _uax_db_Separator[] = "\\u0020\\u00A0\\u1680\\u202F\\u205F\\u3000";
const char _uax_db_Uppercase_Letter[] = "\\u0100\\u0102\\u0104\\u0106\\u0108";
const char _uax_db_Lowercase_Letter[] = "\\u00B5\\u0101\\u0103\\u0105\\u0107";
const char _uax_db_Letter[] = "\\u00AA\\u00B5\\u00BA\\u02EC\\u02EE\\u037F";

#endif // MOJO_REGEX_UNICODE

#endif // MOJO_REGEX_COMPILE_IMPLEMENTATION

#endif // MOJO_REGEX_IMPLEMENTATION

#endif // _MOJO_REGEX_HEADER_
