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



    TODO
        generate utf8 class tree

        pattern -> () -> token stream
        () -> token stream
        token stream -> dfa

        dfa* -> vm

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

#define REGEX_VM_MACHINE_VERSION    8

#ifdef MOJO_REGEX_VM_DEBUG
typedef struct regex_vm_sub_names_s regex_vm_sub_names_t;
struct regex_vm_sub_names_s {
    int id;
    int pc;
    const char *name;
    const char *alias;
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

typedef struct regex_compile_ctx_s regex_compile_ctx_t;
struct regex_compile_ctx_s {
    eRegexCompileStatus_t status;
    eRegexCompilePhase_t phase;
    const char *pattern;
    int position;
    regex_vm_t *vm;
};

#define RE_FLAG_DOT_ALL     0x1u

eRegexCompileStatus_t regexCompile(regex_compile_ctx_t *ctx, const char *pattern,
                                   unsigned int flags);

int regexEscapeStrLen(const char *str, int len);
void regexEscapedStrEmit(FILE *fp, const char *str, int len);
void regexCharLiteralEmit(FILE *fp, int c);
void regexCharClassBitmapEmit(FILE *fp, const unsigned int *bitmap);

#define DEF_VM_SIZE_INC 1024
#define REGEX_THREAD_CALLSTACK_MAX_DEPTH 5

#define META_CLASS_DIGITS_PATTERN           "0-9"
#define META_CLASS_INV_DIGITS_PATTERN       "^0-9"
#define META_CLASS_WHITESPACE_PATTERN       " \\t\\f\\v\\r"
#define META_CLASS_INV_WHITESPACE_PATTERN   "^ \\t\\f\\v\\r"
#define META_CLASS_WORD_PATTERN             "a-zA-Z0-9_"
#define META_CLASS_INV_WORD_PATTERN         "^a-zA-Z0-9_"

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

typedef struct regex_subroutine_s regex_subroutine_t;
struct regex_subroutine_s {
    int id;
    char *name;
    char *alias;
    char *pattern;
    size_t len;
    regex_token_t *tokens;
    int is_ref;     // the subroutine is referenced within an expression
    int is_token;   // the subroutine has a token sequence
    int is_graph;   // the subroutine has been processed by the shunting yard (postfix -> infix)
    int is_vm;      // the subroutine has had VM instructions generated
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

typedef struct regex_vm_pc_patch_s regex_vm_pc_patch_t;
struct regex_vm_pc_patch_s {
    regex_token_t *token;
    int pc;
    int operand;
    regex_vm_pc_patch_t *next;
};

typedef struct regex_vm_gen_path_s regex_vm_gen_path_t;
struct regex_vm_gen_path_s {
    regex_token_t *token;
    regex_vm_gen_path_t *next;
};

typedef struct regex_build_ctx_s regex_build_ctx_t;
struct regex_build_ctx_s {
    regex_token_t *token_pool;
    regex_ptrlist_t *ptrlist_pool;
    regex_utf8_range_t *utf8_range_pool;

    regex_vm_t *vm;

    regex_vm_pc_patch_t *patch_pool;
    regex_vm_gen_path_t *gen_path_pool;

    regex_subroutine_index_t subroutine_index;

    int pc;
    regex_vm_pc_patch_t *patch_list;
    regex_vm_gen_path_t *gen_path_list;
    regex_vm_gen_path_t *sub_path_list;
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

const char *regexVMGroupTableEntryLookup(regex_vm_t *vm, int group, int *len);
static regex_subroutine_t *_regexSubroutineIndexEntryGet(regex_subroutine_index_t *index,
                                                         const char *prefix,
                                                         const char *name, size_t len,
                                                         int id);

void _reMetaTokEmit_eTokenCharLiteral(FILE *fp, regex_build_ctx_t *build_ctx, regex_token_t *token) {
    fprintf(fp, "'%c',%03d%s",
            (((token->c >= ' ') && (token->c <= 127)) ? token->c : '-'),
            token->c,
            ((token->flags & RE_TOK_FLAG_INVERT) ? ":invert" : ""));
}

void _reMetaTokEmit_eTokenCharClass(FILE *fp, regex_build_ctx_t *build_ctx, regex_token_t *token) {
    fputc('[', fp);
    regexCharClassBitmapEmit(fp, token->bitmap);
    fputc(']', fp);
}

void _reMetaTokEmit_eTokenStringLiteral(FILE *fp, regex_build_ctx_t *build_ctx, regex_token_t *token) {
    fputc('\"', fp);
    regexEscapedStrEmit(fp, token->str, token->len);
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
}

void _reMetaTokEmit_eTokenSave(FILE *fp, regex_build_ctx_t *build_ctx, regex_token_t *token) {
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
}

#endif // MOJO_REGEX_COMPILE_IMPLEMENTATION
#ifdef MOJO_REGEX_COMMON_IMPLEMENTATION

const char *regexVMGroupNameFromIndex(regex_vm_t *vm, int index);
const char *regexVMStringTableEntryGet(regex_vm_t *vm, int string_table_id, int *len);
#ifdef MOJO_REGEX_VM_DEBUG
const char *regexVMSubNameGet(regex_vm_t *vm, int pc);
const char *regexVMSubAliasGet(regex_vm_t *vm, int pc);
#endif // MOJO_REGEX_VM_DEBUG

#define RE_META_VM_EMIT_N(token)    NULL
#define RE_META_VM_EMIT_Y(token)    _reMetaVMEmit_ ## token
#define RE_META_VM_EMIT(unique,token)  RE_META_VM_EMIT_ ## unique(token)

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
    regexCharClassBitmapEmit(fp, vm->class_table[operand_a]);
    fputc(']', fp);
}

void _reMetaVMEmit_eTokenStringLiteral(FILE *fp, regex_vm_t *vm, int opcode, unsigned int operand_a, unsigned int operand_b) {
    const char *str;
    int len;

    str = regexVMStringTableEntryGet(vm, operand_a, &len);
    fputc('"', fp);
    regexEscapedStrEmit(fp, str, len);
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
    const char *str = regexVMGroupNameFromIndex(vm, (int)operand_a);

    fprintf(fp, "%d", operand_a);
    if(str != NULL) {
        fprintf(fp, " (%s)", str);
    }
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
    name = regexVMSubNameGet(vm, operand_a);
    alias = regexVMSubAliasGet(vm, operand_a);
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

int regexVerifyTokenDetails(void) {
    int k;
    for(k = 0; ((_regexTokenDetails[k].token != eTokenUnknown) &&
                (_regexTokenDetails[k].token == k)); k++);
    return ((_regexTokenDetails[k].token == eTokenUnknown) &&
            (_regexTokenDetails[k].token == k));
}

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

static int _regexStrTableEntryAdd(regex_str_table_t *table, int entry_size,
                                  unsigned char *entry_data, unsigned int flags,
                                  int *deduped) {
    regex_str_entry_t *entry, *walk;
    int index;

    if(entry_size == RE_STR_NULL_TERM) {
        entry_size = (int)((entry_data != NULL) ? strlen((char *)entry_data) : 0);
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

typedef struct regex_unicode_class_s regex_unicode_class_t;
struct regex_unicode_class_s {
    const char *alias;
    const char *name;
    const char *class_string;
    regex_unicode_class_t *next;
};

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
// Subroutine management data structures and support functions
/////////////////////////////////////////////////////////////////////////////

static void regexTokenDestroy(regex_build_t *build, regex_token_t *token, int full_stack);

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
        regexTokenDestroy(NULL, entry->tokens, 1);
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
    entry->is_ref = 1;
    return entry->id;
}

eRegexCompileStatus_t regexTokenizePattern(regex_build_t *build);

eRegexCompileStatus_t  regexSubroutineResolveImports(regex_build_t *build) {
    regex_subroutine_t *entry;
    regex_token_t *expression = build->tokens;
    static regex_pattern_t sub_pattern;

    for(entry = build->build_ctx->subroutine_index.subroutines; entry != NULL; entry = entry->next) {
        if(entry->is_ref) {
            if(!entry->is_token) {
                if((entry->tokens == NULL) && (entry->pattern != NULL)) {
                    regexPatternInit(&sub_pattern, eReBuildSrcSub, entry->pattern, entry->len);
                    regexBuildPatternPush(build, &sub_pattern);
                    build->tokens = NULL;
                    if(regexTokenizePattern(build) != eCompileOk) {
                        return build->status;
                    }
                    entry->tokens = build->tokens;
                    regexBuildPatternPop(build);
                }
                entry->is_token = 1;
            }
        }
    }
    build->tokens = expression;
    return eCompileOk;
}

/////////////////////////////////////////////////////////////////////////////
// Build context management functions
/////////////////////////////////////////////////////////////////////////////

static void _regexPtrListFree(regex_build_t *build, regex_ptrlist_t *list);
void regexVMStringTableFree(regex_vm_t *vm);
void regexVMClassTableFree(regex_vm_t *vm);
void regexVMUtf8TableFree(regex_vm_t *vm);
void regexVMGroupTableFree(regex_vm_t *vm);

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

    if((build_ctx->vm = _regexAlloc(sizeof(regex_vm_t), _regexMemContext)) == NULL) {
        return 0;
    }
    build_ctx->vm->vm_version = REGEX_VM_MACHINE_VERSION;

#ifdef MOJO_REGEX_UNICODE
    if(_regexSubroutineIndexRegisterPattern(&(build_ctx->subroutine_index), "meta",
                                            "glyph", "\\X", "\\P{M}\\p{M}*") < 0) {
        INTERNAL_ERROR("failed tto register unicode glyph pattern");
        return NULL;
    }
#endif // MOJO_REGEX_UNICODE
    return build_ctx;
}

void regexBuildVMFree(regex_vm_t *vm) {
    regexVMStringTableFree(vm);
    regexVMClassTableFree(vm);
    regexVMUtf8TableFree(vm);
    regexVMGroupTableFree(vm);
    _regexDealloc(vm, _regexMemContext);
}

void regexBuildContextDeinit(regex_build_ctx_t *build_ctx) {
    if(build_ctx != NULL) {
        regexTokenDestroy(NULL, build_ctx->token_pool, 1);
        _regexPtrListFree(NULL, build_ctx->ptrlist_pool);
        if(build_ctx->vm != NULL) {
            regexBuildVMFree(build_ctx->vm);
            build_ctx->vm = NULL;
        }
        _regexSubroutineIndexFreeAll(&(build_ctx->subroutine_index));
        memset(build_ctx, 0, sizeof(regex_build_ctx_t));
    }
}

void regexBuildContextDestroy(regex_build_ctx_t *build_ctx) {
    if(build_ctx != NULL) {
        regexBuildContextDeinit(build_ctx);
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

void regexBuilderDeinit(regex_build_t *build) {
    if(build != NULL) {
        if(build->tokens != NULL) {
            regexTokenDestroy(build, build->tokens, 1);
        }
        if(build->pattern != NULL) {
            INTERNAL_ERROR("pattern left in build");
        }
        memset(build, 0, sizeof(regex_build_t));
    }
}

void regexBuilderDestroy(regex_build_t *build) {
    if(build != NULL) {
        if(build->tokens != NULL) {
            regexTokenDestroy(build, build->tokens, 1);
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

static void _regexPtrListDump(regex_ptrlist_t *list) {
    for(; list != NULL; list = list->next) {
        printf("PtrList node 0x%4.4X - %s\n", (int)((uintptr_t)list->token & 0xFFFFu), (list->type == eRePtrOutA ? "out_a" : "out_b"));
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

void regexTokenDestroy(regex_build_t *build, regex_token_t *token, int full_stack) {
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
                regexTokenDestroy(build, token, 0);
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
    token->jump = 0x3FFF;
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

// The subexpression group number is derived from it's pattern order. This
// group number can be used to derive the expression number, via the following
// utility functions.

int regexSubexprStartFromGroup(int group) {
    return group * 2;
}

int regexSubexprEndFromGroup(int group) {
    return (group * 2) + 1;
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

static void _parseCharEmit(FILE *fp, parseChar_t *pc) {
    fputs("parseChar(", fp);
    if((pc->c >= ' ') && (pc->c <= 127)) {
        fprintf(fp, "'%c'", pc->c);
    } else{
        fputs("'-'", fp);
    }
    fprintf(fp, ":%03d:\"%*.*s\":", pc->c, pc->len, pc->len, pc->ptr);
    switch(pc->state) {
        case eRegexPatternEnd: fputs("end", fp); break;
        case eRegexPatternChar: fputs("char", fp); break;
        case eRegexPatternUnicode: fputs("unicode", fp); break;
        case eRegexPatternInvalid: fputs("invalid", fp); break;
        case eRegexPatternInvalidEscape: fputs("inv_esc", fp); break;
        case eRegexPatternMetaChar: fputs("metachar", fp); break;
        case eRegexPatternMetaClass: fputs("metaclass", fp); break;
        case eRegexPatternUnicodeMetaClass: fputs("utf8meta", fp); break;
        case eRegexPatternEscapedChar: fputs("esc_char", fp); break;
    }
    fputc(')', fp);
    fflush(fp);
}

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

static parseChar_t _parseGetNextPatternChar(const char **pattern, int allow_brace, const char **id, int *len);

static int _parseCheckNextPatternChar(const char **pattern, char c) {
    parseChar_t pc;

    pc = _parseGetNextPatternChar(pattern, (c == '{' ? 1 : 0), NULL, NULL);
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

static parseChar_t _parseGetNextPatternChar(const char **pattern, int allow_brace, const char **id, int *len) {
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
                    printf("choke?\n");
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
            if(allow_brace) {
                result.c = '{';
                result.state = eRegexPatternChar;
                return result;
            }

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
        c = _parseGetNextPatternChar(&ptr, 0, NULL, NULL);
        switch(c.state) {
            case eRegexPatternEnd:
            case eRegexPatternMetaChar:
            case eRegexPatternUnicodeMetaClass:
            case eRegexPatternMetaClass:
                return count;

            case eRegexPatternChar:
            case eRegexPatternEscapedChar:
                count.characters++;
                count.bytes++;
                break;

            case eRegexPatternUnicode:
                count.characters++;
                count.bytes += _parseUtf8EncodingByteLen(c.c);
                break;

            case eRegexPatternInvalid:
            case eRegexPatternInvalidEscape:
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
        c = _parseGetNextPatternChar(pattern, 0, NULL, NULL);
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
    int codepoint;

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
    if(len == RE_STR_NULL_TERM) {
        len = (int)strlen(str);
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
    }
    return 1;
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
    return 0;
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

    c = _parseGetNextPatternChar(&(build->pattern->pattern), 0, NULL, NULL);
    if(_parsePatternIsValid(&c)) {
        _parsePatternCharAdvance(&(build->pattern->pattern), &c);
    }
    if(c.state == eRegexPatternEnd) {
        build->status = eCompileCharClassIncomplete;
        return 0;
    }

    if(c.state == eRegexPatternMetaChar && c.c == '^') {
        invert = 1;
        c = _parseGetNextPatternChar(&(build->pattern->pattern), 0, NULL, NULL);
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
        c = _parseGetNextPatternChar(&(build->pattern->pattern), 0, NULL, NULL);
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

static int _regexCharClassBitmapCheck(const unsigned int *bitmap, int pos) {
    return (int)(bitmap[pos / 32u] & (1u << pos % 32u));
}

void regexCharClassBitmapEmit(FILE *fp, const unsigned int *bitmap) {
    int k;
    int run;

    for(k = 0; k < 256; k++) {
        if(_regexCharClassBitmapCheck(bitmap, k)) {
            for(run = k + 1; run < 256 && _regexCharClassBitmapCheck(bitmap, run); run++);
            run--;
            fprintf(fp, "%c", ((k < 32) || (k > 127)) ? '.' : k);
            if(run - k > 3) {
                fprintf(fp, "-%c", ((run < 32) || (run > 127)) ? '.' : run);
                k = run;
            }
        }
    }
}

/////////////////////////////////////////////////////////////////////////////
// Lexer - tokenizes the pattern into a token list (infix form)
/////////////////////////////////////////////////////////////////////////////

int regexVMGroupTableEntryAdd(regex_build_t *build, const char *group, int len, int index);

static eRegexCompileStatus_t _regexTokenizePattern(regex_build_t *build) {
    const char *class;
    parseChar_t c;
    parse_str_t str_count;
    int len;
    const char *str;
    unsigned int flags;
    regex_subroutine_t *routine;
    int subexpr;
    int named;
    int index;

    // Parse the regex pattern into a sequence of tokens (operators and operands)
    // The output of this stage is a sequence of lexical tokens in infix form

    // Loop through the pattern until we've handled it all
    for(; *(build->pattern->pattern) != '\0';) {
        // Get the next character in the pattern. The helper function assists
        // in disambiguating escaped characters.
        c = _parseGetNextPatternChar(&(build->pattern->pattern), 0, &str, &len);
//        _parseCharEmit(stdout, &c);
//        fputc('\n', stdout);
        if(_parsePatternIsValid(&c)) {
            _parsePatternCharAdvance(&(build->pattern->pattern), &c);
        }

        switch(c.state) {
            case eRegexPatternEnd:
                return eCompileOk;

            case eRegexPatternInvalid:
                return eCompileEscapeCharIncomplete;

            case eRegexPatternInvalidEscape:
                printf("invalid escape\n");
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
                        // Grouped subexpression, complex operator, resolves to a compound operand
                        // Speculative subexpression number. If non-capturing, will NOT actually
                        // increase the group count.
                        subexpr = build->build_ctx->group_count;
                        flags = 0;
                        named = 0;
                        index = -1;
                        // Check for group meta modifiers
                        while(*(build->pattern->pattern) == '?') {
                            build->pattern->pattern++;
                            if(_parseCheckNextPatternChar(&(build->pattern->pattern), 'i')) {
                                // Case insensitive matching
                                flags |= RE_TOK_FLAG_CASE_INS;
                            } else if(_parseCheckNextPatternChar(&(build->pattern->pattern), 'P')) {
                                // Named sub expression
                                if(named || (flags & (RE_TOK_FLAG_SUBROUTINE | RE_TOK_FLAG_NO_CAPTURE))) {
                                    return eCompileConflictingAttrs;
                                }
                                if(_parseCheckIdentifier(build->pattern->pattern, '<', '>', &str, &len) != eRegexPatternIdOk) {
                                    return eCompileMalformedSubExprName;
                                }
                                build->pattern->pattern += len + 2;
                                if(regexVMGroupTableEntryAdd(build, str, len, subexpr) < 0) {
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
                                if((index = _regexSubroutineIndexRegisterIntent(&(build->build_ctx->subroutine_index), NULL, str, len,
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
                            if(regexVMGroupTableEntryAdd(build, NULL, 0, subexpr) < 0) {
                                return eCompileOutOfMem;
                            }
                        }
                        build->build_ctx->group_count++;
                        if(!_regexCreateTokenSubExprStart(build, subexpr, index, (int)(flags & RE_TOK_FLAG_COMPOUND), (int)(flags & RE_TOK_FLAG_NO_CAPTURE))) {
                            return eCompileOutOfMem;
                        }
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
                switch(c.c) {
                    case 'R': // subroutine call
                        if((routine = _regexSubroutineIndexEntryGet(&(build->build_ctx->subroutine_index), NULL, str, len, -1)) == NULL) {
                            return eCompileUnknownSubroutine;
                        }
                        if(!_regexCreateTokenCall(build, routine->id)) {
                            return eCompileOutOfMem;
                        }
                        routine->is_ref = 1;
                        continue;
#ifdef MOJO_REGEX_UNICODE
                    case 'X': // full unicode glyph (base + markers)
                        if((routine = _regexSubroutineIndexEntryGet(&(build->build_ctx->subroutine_index), "meta", "glyph", len, -1)) == NULL) {
                            INTERNAL_ERROR("glyph subroutine is missing");
                            return eCompileUnknownSubroutine;
                        }
                        if(!_regexCreateTokenCall(build, routine->id)) {
                            return eCompileOutOfMem;
                        }
                        continue;
#endif // MOJO_REGEX_UNICODE
                    case 'B': // explicit byte
                        if(!_regexCreateTokenByte(build)) {
                            return eCompileOutOfMem;
                        }
                        continue;
                }
                switch(c.c) {
                    case 'd': // digit
                        class = META_CLASS_DIGITS_PATTERN;
                        break;
                    case 'D': // non digit
                        class = META_CLASS_INV_DIGITS_PATTERN;
                        break;
                    case 's': // whitespace
                        class = META_CLASS_WHITESPACE_PATTERN;
                        break;
                    case 'S': // non whitespace
                        class = META_CLASS_INV_WHITESPACE_PATTERN;
                        break;
                    case 'w': // word character
                        class = META_CLASS_WORD_PATTERN;
                        break;
                    case 'W': // non word character
                        class = META_CLASS_INV_WORD_PATTERN;
                        break;
                    default:
                        return eCompileUnsupportedMeta;
                }
                if(!_parseCharClassAndCreateToken(build, class, NULL, 0, 0)) {
                    return build->status;
                }
                continue;

            case eRegexPatternUnicodeMetaClass:
                if(_regexRegUnicodeCharClassGet(str, len) == NULL) {
                    return eCompileUnknownUnicodeClass;
                }
                if(!_parseCharClassAndCreateToken(build, NULL, str, len, ((c.c == 'P') ? 1 : 0))) {
                    return build->status;
                }
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
                    _parsePatternCharAdvance(&(build->pattern->pattern), &c);
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

/////////////////////////////////////////////////////////////////////////////
// Shunting yard implementation - convert infix to postfix to an NFA
/////////////////////////////////////////////////////////////////////////////

typedef struct regex_yard_s regex_yard_t;
struct regex_yard_s {
    regex_token_t *operands;
    regex_token_t *operators;
};

void regexYardInit(regex_yard_t *yard) {
    memset(yard, 0, sizeof(regex_yard_t));
}

typedef enum {
    OP_GREATER_OR_EQUAL,
    OP_ALL
} eRegexOpApply;

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

eRegexToken_t regexTokenStackPeekType(regex_token_t *stack) {
    if(stack == NULL) {
        return -1;
    }
    return stack->tokenType;
}

int regexStackTypeGreaterOrEqualToToken(regex_token_t *stack, eRegexToken_t tokenType) {
    eRegexTokenPriority_t stackPriority;

    stackPriority = regexGetTokenTypePriority(regexTokenStackPeekType(stack));
    return (stackPriority >= regexGetTokenTypePriority(tokenType));
}

/////////////////////////////////////////////////////////////////////////////
// NFA form regex support
/////////////////////////////////////////////////////////////////////////////

int regexOperatorLiteralCreate(regex_build_t *build, regex_token_t **stack, regex_token_t *token) {
    if((token->ptrlist = _regexPtrlistCreate(build, token, eRePtrOutA)) == NULL) {
        return 0;
    }
    regexTokenStackPush(stack, token);
    return 1;
}

int regexOperatorMatchCreate(regex_build_t *build, regex_token_t **stack) {
    regex_token_t *token = NULL;

    if(!_regexCreateTokenMatch(build, &token, (int)(build->build_ctx->flags & RE_FLAG_DOT_ALL))) {
        return 0;
    }
    regexTokenStackPush(stack, token);
    return 1;
}

int regexOperatorReturnCreate(regex_build_t *build, regex_token_t **stack) {
    regex_token_t *token = NULL;

    if(!_regexCreateTokenReturn(build, &token)) {
        return 0;
    }
    regexTokenStackPush(stack, token);
    return 1;
}

int regexOperatorSubexprCreate(regex_build_t *build, regex_token_t **stack, regex_token_t *token) {
    token->tokenType = eTokenSave;
    if((token->ptrlist = _regexPtrlistCreate(build, token, eRePtrOutA)) == NULL) {
        return 0;
    }
    regexTokenStackPush(stack, token);
    return 1;
}

int regexOperatorConcatenationCreate(regex_build_t *build, regex_token_t **stack, regex_token_t *token) {
    regex_token_t *e1, *e2;

    if(token != NULL) {
        regexTokenDestroy(build, token, 1);
    }
    if(((e2 = regexTokenStackPop(stack)) == NULL) ||
       ((e1 = regexTokenStackPop(stack)) == NULL)) {
        return 0;
    }
    _regexPtrlistPatch(build, &(e1->ptrlist), e2, 0);
    e1->ptrlist = e2->ptrlist;
    e2->ptrlist = NULL;
    regexTokenStackPush(stack, e1);

    return 1;
}

int regexOperatorAlternationCreate(regex_build_t *build, regex_token_t **stack, regex_token_t *token) {
    regex_token_t *e1, *e2;
    regex_token_t *jmp = NULL;

    if(((e2 = regexTokenStackPop(stack)) == NULL) ||
       ((e1 = regexTokenStackPop(stack)) == NULL)) {
        return 0;
    }

    if(!_regexCreateTokenJmp(build, &jmp)) {
        return 0;
    }
#ifdef MOJO_REGEX_EXPERIMENTAL_CULL_JMP
    if(regexPtrlistPatch(build, &(e1->ptrlist), jmp, 1) == 0) {
#else // !MOJO_REGEX_EXPERIMENTAL_CULL_JMP
    if(_regexPtrlistPatch(build, &(e1->ptrlist), jmp, 0) == 0) {
#endif
        // The jmp token was unused
        regexTokenDestroy(build, jmp, 0);
        token->ptrlist = e1->ptrlist;
        e1->ptrlist = NULL;
    } else {
        if((token->ptrlist = _regexPtrlistCreate(build, jmp, eRePtrOutA)) == NULL) {
            return 0;
        }
    }
    token->tokenType = eTokenSplit;
    token->out_a = e1;
    token->out_b = e2;
    token->ptrlist = _regexPtrlistAppend(token->ptrlist, e2->ptrlist);
    e2->ptrlist = NULL;
    regexTokenStackPush(stack, token);

    return 1;
}

int regexOperatorZeroOrOneCreate(regex_build_t *build, regex_token_t **stack, regex_token_t *token) {
    regex_token_t *e;

    if((e = regexTokenStackPop(stack)) == NULL) {
        return 0;
    }
    token->tokenType = eTokenSplit;
    token->out_a = e;
    if((token->ptrlist = _regexPtrlistCreate(build, token, eRePtrOutB)) == NULL) {
        return 0;
    }
    token->ptrlist = _regexPtrlistAppend(e->ptrlist, token->ptrlist);
    e->ptrlist = NULL;
    regexTokenStackPush(stack, token);

    return 1;
}

int regexOperatorZeroOrMoreCreate(regex_build_t *build, regex_token_t **stack, regex_token_t *token) {
    regex_token_t *e;
    regex_token_t *jmp = NULL;

    if((e = regexTokenStackPop(stack)) == NULL) {
        return 0;
    }
    token->tokenType = eTokenSplit;
    token->out_a = e;
    if((token->ptrlist = _regexPtrlistCreate(build, token, eRePtrOutB)) == NULL) {
        return 0;
    }
    if(!_regexCreateTokenJmp(build, &jmp)) {
        return 0;
    }
    jmp->out_a = token;
    _regexPtrlistPatch(build, &(e->ptrlist), jmp, 0);
    regexTokenStackPush(stack, token);

    return 1;
}

int regexOperatorOneOrMoreCreate(regex_build_t *build, regex_token_t **stack, regex_token_t *token) {
    regex_token_t *e;

    if((e = regexTokenStackPop(stack)) == NULL) {
        return 0;
    }
    token->tokenType = eTokenSplit;
    token->out_a = e;
    _regexPtrlistPatch(build, &(e->ptrlist), token, 0);
    if((e->ptrlist = _regexPtrlistCreate(build, token, eRePtrOutB)) == NULL) {
        return 0;
    }
    regexTokenStackPush(stack, e);
    return 1;
}

int regexOperatorRangeCreate(regex_build_t *build, regex_token_t **stack, regex_token_t *token) {
    // TODO - implement the range operator in the shunting yard
    INTERNAL_ERROR("range operator not yet implemented in yard");
    return 0;
}

int regexOperandWrapSubexpression(regex_build_t *build, regex_token_t **stack, int group_num, unsigned int flags) {
    regex_token_t *e1, *e2, *e3;
    regex_build_t fragment = {.tokens = NULL, .build_ctx = build->build_ctx};

    if(!_regexCreateTokenSubExprStart(&fragment, (group_num + 1) * 2, -1, (int)(flags & RE_TOK_FLAG_COMPOUND), (int)(flags & RE_TOK_FLAG_NO_CAPTURE))) {
        return 0;
    }
    e1 = fragment.tokens;
    e1->tokenType = eTokenSave;
    if((e1->ptrlist = _regexPtrlistCreate(build, e1, eRePtrOutA)) == NULL) {
        return 0;
    }

    fragment.tokens = NULL;
    if(!_regexCreateTokenSubExprEnd(&fragment)) {
        return 0;
    }
    e3 = fragment.tokens;
    e3->group = ((group_num + 1) * 2) + 1;
    e3->tokenType = eTokenSave;
    if((e3->ptrlist = _regexPtrlistCreate(build, e3, eRePtrOutA)) == NULL) {
        return 0;
    }

    if((e2 = regexTokenStackPop(stack)) == NULL) {
        return 0;
    }

    _regexPtrlistPatch(build, &(e1->ptrlist), e2, 0);
    _regexPtrlistPatch(build, &(e2->ptrlist), e3, 0);
    e1->ptrlist = e3->ptrlist;
    e2->ptrlist = NULL;
    e3->ptrlist = NULL;
    regexTokenStackPush(stack, e1);
    return 1;
}

int regexHasSufficientOperands(regex_token_t *fragments, int arity) {
    for(; ((fragments != NULL) && (arity)); arity--, fragments = fragments->next);
    return arity == 0;
}

eRegexCompileStatus_t regexOperatorApply(regex_build_t *build, regex_token_t **operators, eRegexOpApply apply, eRegexToken_t tokenType, regex_token_t **operands) {
    regex_token_t *operator;

    while((*operators != NULL) &&
          ((apply == OP_ALL) ||
           (apply == OP_GREATER_OR_EQUAL && regexStackTypeGreaterOrEqualToToken(*operators, tokenType)))) {
        if((operator = regexTokenStackPop(operators)) == NULL) {
            INTERNAL_ERROR("missing operator in yard op apply");
            return eCompileInternalError;
        }

        if(!regexHasSufficientOperands(*operands, regexGetOperatorArity(operator))) {
            return eCompileMissingOperand;
        }
//        printf("Apply: ");
//        regexTokenStrEmit(stdout, build->build_ctx, operator, 2);
//        printf("\n");
        switch(operator->tokenType) {
            case eTokenZeroOrOne:
                if(!regexOperatorZeroOrOneCreate(build, operands, operator)) {
                    return eCompileOutOfMem;
                }
                break;

            case eTokenZeroOrMany:
                if(!regexOperatorZeroOrMoreCreate(build, operands, operator)) {
                    return eCompileOutOfMem;
                }
                break;

            case eTokenOneOrMany:
                if(!regexOperatorOneOrMoreCreate(build, operands, operator)) {
                    return eCompileOutOfMem;
                }
                break;

            case eTokenRange:
                if(!regexOperatorRangeCreate(build, operands, operator)) {
                    return eCompileOutOfMem;
                }
                break;

            case eTokenConcatenation:
                if(!regexOperatorConcatenationCreate(build, operands, operator)) {
                    return eCompileOutOfMem;
                }
                break;

            case eTokenAlternative:
                if(!regexOperatorAlternationCreate(build, operands, operator)) {
                    return eCompileOutOfMem;
                }
                break;

            default:
                INTERNAL_ERROR("unexpected operator in shunting yard");
                return eCompileInternalError;
        }
    }
    return eCompileOk;
}

typedef struct regex_subroutine_stack_s regex_subroutine_stack_t;
struct regex_subroutine_stack_s {
    regex_token_t *stack;
    int index;
    regex_subroutine_stack_t *next;
};

static int _regexSubroutineYardQueue(regex_subroutine_stack_t **substack, int index, regex_token_t *tokens) {
    regex_subroutine_stack_t *entry;

    for(entry = *substack; entry != NULL; entry = entry->next) {
        if(entry->index == index) {
            return 1;
        }
    }

    if((entry = _regexAlloc(sizeof(regex_subroutine_stack_t), _regexMemContext)) == NULL) {
        return 0;
    }
    entry->stack = tokens;
    entry->index = index;
    entry->next = *substack;
    *substack = entry;
    return 1;
}

static regex_subroutine_stack_t *_regexSubroutineYardPop(regex_subroutine_stack_t **stack) {
    regex_subroutine_stack_t *entry;

    if((entry = *stack) != NULL) {
        *stack = entry->next;
        entry->next = NULL;
    }
    return entry;
}

static int _regexSubroutineYardComplete(regex_build_ctx_t *build_ctx, regex_subroutine_stack_t *entry) {
    regex_subroutine_t *routine;

    if(entry != NULL) {
        if((routine = _regexSubroutineIndexEntryGet(&(build_ctx->subroutine_index), NULL, NULL, 0, entry->index)) == NULL) {
            return 0;
        }
        routine->tokens = entry->stack;
        return 1;
    }
    return 0;
}

static eRegexCompileStatus_t _regexShuntingYardFragment(regex_build_t *build, regex_token_t **stack,
                                                        regex_subroutine_stack_t **substack, int is_subexpr) {
    regex_token_t *token = NULL, *group, *operators = NULL;
    regex_token_t *operands = NULL;
    regex_subroutine_t *subroutine;
    eRegexCompileStatus_t status = eCompileOk;
    int found_subexpr_end = 0;

    printf(">> enter shunting yard%s\n", (is_subexpr ? "(sub expression)" : ""));

    while((!found_subexpr_end) && ((token = regexTokenStackPop(&(build->tokens))) != NULL)) {
        if(regexTokenStrEmit(stdout, build->build_ctx, token, 1)) {
            fputc('\n', stdout);
        }
        switch(token->tokenType) {
            default:
                INTERNAL_ERROR("unknown token found in shunting yard");
                return eCompileInternalError;
            case eTokenCall:
                // TODO: Check subroutine, and intentional fall through
            case eTokenCharLiteral:
            case eTokenCharClass:
            case eTokenStringLiteral:
            case eTokenUtf8Class:
            case eTokenCharAny:
            case eTokenByte:
            case eTokenAssertion:
            case eTokenReturn:
            case eTokenMatch:
                if(!regexOperatorLiteralCreate(build, &operands, token)) {
                    return eCompileOutOfMem;
                }
                break;

            case eTokenZeroOrOne:
            case eTokenZeroOrMany:
            case eTokenOneOrMany:
            case eTokenRange:
            case eTokenConcatenation:
            case eTokenAlternative:
                if((status = regexOperatorApply(build, &operators, OP_GREATER_OR_EQUAL,
                                                token->tokenType, &operands)) != eCompileOk) {
                    return status;
                }
                regexTokenStackPush(&operators, token);
                break;

            case eTokenSubExprStart:
                // TODO - handle case insensitive propogation
                group = NULL;
                printf("forming sub expression...\n");
                if((status = _regexShuntingYardFragment(build, &group, substack, 1)) != eCompileOk) {
                    return status;
                }
                printf("sub expression formed\n");
                if(!(token->flags & RE_TOK_FLAG_NO_CAPTURE)) {
                    // Wrap the expression in start/end
                    if(!regexOperandWrapSubexpression(build, &group, token->group, token->flags)) {
                        return eCompileOutOfMem;
                    }
                    printf("expression wrapped\n");
                }
                if(token->flags & RE_TOK_FLAG_SUBROUTINE) {
                    if((subroutine = _regexSubroutineIndexEntryGet(&(build->build_ctx->subroutine_index), NULL, NULL, 0,
                                                                   token->sub_index)) == NULL) {
                        INTERNAL_ERROR("failed to find subroutine for group in shunting yard");
                        return eCompileInternalError;
                    }
                    switch(regexOperatorReturnCreate(build, &group)) {
                        case -2: return eCompileOutOfMem;
                        case -1:
                            INTERNAL_ERROR("no operands to return from in shunting yard");
                            return eCompileInternalError;
                        default:
                            break;
                    }
                    if(!regexOperatorConcatenationCreate(build, &group, NULL)) {
                        INTERNAL_ERROR("failed to close expression by concatenating return op");
                        return eCompileOutOfMem;
                    }
                    subroutine->tokens = group;
                    subroutine->is_token = 1;
                    subroutine->is_graph = 1;

                    token->tokenType = eTokenCall;
                    group = token;
                    printf("calling expression\n");
                } else {
                    printf("inserting expression\n");
                }
                if(operands != NULL) {
                    if((!regexOperatorLiteralCreate(build, &operands, group)) ||
                       (!regexOperatorConcatenationCreate(build, &operands, NULL))) {
                        return eCompileOutOfMem;
                    }
                } else {
                    regexOperatorLiteralCreate(build, &operands, group);
                }
                //regexTokenStackPush(&operands, group);
                printf("####================\n");
                regexTokenStrDFAEmit(stdout, build->build_ctx, group);
                _regexPtrListDump(group->ptrlist);
                printf("####================\n");
                break;

            case eTokenSubExprEnd:
                if(!is_subexpr) {
                    return eCompileMissingSubexprStart;
                }
                found_subexpr_end = 1;
                regexTokenDestroy(build, token, 0);
                break;
        }
    }
    if((is_subexpr) && (!found_subexpr_end)) {
        return eCompileMissingSubexprEnd;
    }

    if((status = regexOperatorApply(build, &operators, OP_ALL, 0, &operands)) != eCompileOk) {
        printf("<< fail shunting yard%s\n", (is_subexpr ? "(sub expression)" : ""));
        return status;
    }
    if(operands->next != NULL) {
        INTERNAL_ERROR("failed to resolve all operands when closing group in shunting yard");
        return eCompileInternalError;
    }
    *stack = operands;
    printf("<< exit shunting yard%s\n", (is_subexpr ? "(sub expression)" : ""));

    printf("---------------------------------------\n");
    regexTokenStrDFAEmit(stdout, build->build_ctx, operands);
    printf("---------------------------------------\n");

    return eCompileOk;
}

int _regexYardWalkForSubroutines(regex_build_t *build, regex_token_t *token) {
    regex_subroutine_t *routine;

    if(token->pc != -2) {
        return 1;
    }
    token->pc = -1;
    if((token->tokenType == eTokenCall) && (token->out_b == NULL)) {
        if((routine = _regexSubroutineIndexEntryGet(&(build->build_ctx->subroutine_index), NULL, NULL, 0, token->sub_index)) == NULL) {
            INTERNAL_ERROR("failed to locate token call subroutine");
            return 0;
        }
        token->out_b = routine->tokens;
    }
    if(token->out_a != NULL) {
        if(!_regexYardWalkForSubroutines(build, token->out_a)) {
            return 0;
        }
    }
    if(token->out_b != NULL) {
        if(!_regexYardWalkForSubroutines(build, token->out_b)) {
            return 0;
        }
    }
    return 1;
}

#ifdef MOJO_REGEX_VM_DEBUG
int regexVMSubNameEntryAdd(regex_vm_t *vm, int index, const char *name, const char *alias);
#endif // MOJO_REGEX_VM_DEBUG

eRegexCompileStatus_t regexShuntingYard(regex_build_t *build) {
    regex_token_t *stack = NULL;
    regex_subroutine_t *routine;
    regex_subroutine_stack_t *substack = NULL, *entry;
    regex_build_t sub_build = {.build_ctx = build->build_ctx, .tokens = NULL};
    eRegexCompileStatus_t status;

    status = _regexShuntingYardFragment(build, &stack, &substack, 0);
    if(status != eCompileOk) {
        regexTokenDestroy(build, stack, 1);
        return status;
    }
    if(!regexOperatorMatchCreate(build, &stack)) {
        INTERNAL_ERROR("failed to close expression with match op");
        return eCompileOutOfMem;
    }
    if(!regexOperatorConcatenationCreate(build, &stack, NULL)) {
        INTERNAL_ERROR("failed to close expression by concatenating match op");
        return eCompileOutOfMem;
    }
    build->tokens = stack;
    while((entry = _regexSubroutineYardPop(&substack)) != NULL) {
        stack = NULL;
        sub_build.tokens = entry->stack;
        status = _regexShuntingYardFragment(&sub_build, &stack, &substack, 0);
        if(status != eCompileOk) {
            return status;
        }
//        printf("==================================\n");
//        regexTokenStrDFAEmit(stdout, build->build_ctx, stack);
//        _regexPtrListDump(stack->ptrlist);
//        printf("----------------------------------\n");
        switch(regexOperatorReturnCreate(build, &stack)) {
            case -2: return eCompileOutOfMem;
            case -1:
                INTERNAL_ERROR("no operands to return from in shunting yard");
                return eCompileInternalError;
            default:
                break;
        }
        if(!regexOperatorConcatenationCreate(build, &stack, NULL)) {
            INTERNAL_ERROR("failed to close expression by concatenating return op");
            return eCompileOutOfMem;
        }
//        regexTokenStrDFAEmit(stdout, build->build_ctx, stack);
//        printf("==================================\n");
//        entry->stack = stack;
        _regexSubroutineYardComplete(build->build_ctx, entry);
    }

    printf("sub patch...\n");
    regexTokenWalkTree(build->tokens, -2);
    // Match call tokens to their corresponding subroutines
    if(build->tokens == NULL) {
        INTERNAL_ERROR("no regex stream after yard");
    } else {
        if(!_regexYardWalkForSubroutines(build, build->tokens)) {
            return eCompileInternalError;
        }
        for(routine = build->build_ctx->subroutine_index.subroutines; routine != NULL; routine = routine->next) {
            if(!(routine->is_graph)) {
                continue;
            }
            if(routine->tokens != NULL) {
                if(!_regexYardWalkForSubroutines(build, routine->tokens)) {
                    return eCompileInternalError;
                }
#ifdef MOJO_REGEX_VM_DEBUG
                if(!regexVMSubNameEntryAdd(build->build_ctx->vm, routine->id, routine->name, routine->alias)) {
                    return 0;
                }
#endif // MOJO_REGEX_VM_DEBUG
            }
        }
    }

    printf("and onward...\n");
    return status;
}

/////////////////////////////////////////////////////////////////////////////
// Regex VM builder and evaluator
/////////////////////////////////////////////////////////////////////////////

// String table /////////////////////////////////////////////////////////////

int regexVMStringTableEntryAdd(regex_build_t *build, const char *str, int len) {
    regex_vm_t *vm = build->build_ctx->vm;
    int k;

    for(k = 0; k < vm->string_tbl_size; k++) {
        if((vm->string_tbl_len[k] == len) &&
           (!memcmp(vm->string_table[k], str, len))) {
            return k;
        }
    }
    if(((vm->string_table = _regexRealloc(vm->string_table,
                                          (vm->string_tbl_size) * sizeof(char *),
                                          (vm->string_tbl_size + 1) * sizeof(char *),
                                          _regexMemContext)) == NULL) ||
       ((vm->string_tbl_len = _regexRealloc(vm->string_tbl_len,
                                         (vm->string_tbl_size) * sizeof(int),
                                         (vm->string_tbl_size + 1) * sizeof(int),
                                         _regexMemContext)) == NULL)) {
        build->status = eCompileOutOfMem;
        return -1;
    }
    vm->string_table[vm->string_tbl_size] = (char *)str;
    vm->string_tbl_len[vm->string_tbl_size] = len;
    k = vm->string_tbl_size;
    vm->string_tbl_size++;

    return k;
}

const char *regexVMStringTableEntryGet(regex_vm_t *vm, int string_table_id, int *len) {
    if((string_table_id < 0) || (string_table_id >= vm->string_tbl_size)) {
        return NULL;
    }
    if(len != NULL) {
        *len = vm->string_tbl_len[string_table_id];
    }
    return vm->string_table[string_table_id];
}

void regexVMStringTableFree(regex_vm_t *vm) {
    int k;

    for(k = 0; k < vm->string_tbl_size; k++) {
        if(vm->string_table[k] != NULL) {
            _regexDealloc(vm->string_table[k], _regexMemContext);
        }
    }
    _regexDealloc(vm->string_table, _regexMemContext);
    _regexDealloc(vm->string_tbl_len, _regexMemContext);
    vm->string_table = NULL;
    vm->string_tbl_len = NULL;
    vm->string_tbl_size = 0;
}

// Class table //////////////////////////////////////////////////////////////

int regexVMCharClassTableEntryAdd(regex_build_t *build,
                                  const unsigned int *bitmap) {
    regex_vm_t *vm = build->build_ctx->vm;
    int k;

    for(k = 0; k < vm->class_tbl_size; k++) {
        if(!memcmp(vm->class_table[k], bitmap, 32)) {
            return k;
        }
    }
    if((vm->class_table = _regexRealloc(vm->class_table,
                                        (vm->class_tbl_size) * sizeof(unsigned int *),
                                        (vm->class_tbl_size + 1) * sizeof(unsigned int *),
                                        _regexMemContext)) == NULL) {
        build->status = eCompileOutOfMem;
        return -1;
    }
    vm->class_table[vm->class_tbl_size] = (unsigned int *)bitmap;
    k = vm->class_tbl_size;
    vm->class_tbl_size++;

    return k;
}

const unsigned int *regexVMClassTableEntryGet(regex_vm_t *vm, int class_table_id) {
    if((class_table_id < 0) || (class_table_id >= vm->class_tbl_size)) {
        return NULL;
    }
    return vm->class_table[class_table_id];
}

void regexVMClassTableFree(regex_vm_t *vm) {
    int k;

    for(k = 0; k < vm->class_tbl_size; k++) {
        if(vm->class_table[k] != NULL) {
            _regexDealloc(vm->class_table[k], _regexMemContext);
        }
    }
    _regexDealloc(vm->class_table, _regexMemContext);
    vm->class_table = NULL;
    vm->class_tbl_size = 0;
}

// utf8 table ///////////////////////////////////////////////////////////////

int regexVMUtf8ClassTableEntryAdd(regex_build_t *build,
                                  const unsigned int *bitmap) {
    regex_vm_t *vm = build->build_ctx->vm;
    int k;

    for(k = 0; k < vm->utf8_tbl_size; k++) {
        if(!memcmp(vm->utf8_class_table[k], bitmap, 8)) {
            return k;
        }
    }
    if((vm->utf8_class_table = _regexRealloc(vm->utf8_class_table,
                                        (vm->utf8_tbl_size) * sizeof(unsigned int *),
                                        (vm->utf8_tbl_size + 1) * sizeof(unsigned int *),
                                        _regexMemContext)) == NULL) {
        build->status = eCompileOutOfMem;
        return -1;
    }
    vm->utf8_class_table[vm->utf8_tbl_size] = (unsigned int *)bitmap;
    k = vm->utf8_tbl_size;
    vm->utf8_tbl_size++;

    return k;
}

const unsigned int *regexVMUtf8TableEntryGet(regex_vm_t *vm, int utf8_table_id) {
    if((utf8_table_id < 0) || (utf8_table_id >= vm->utf8_tbl_size)) {
        return NULL;
    }
    return vm->utf8_class_table[utf8_table_id];
}

void regexVMUtf8TableFree(regex_vm_t *vm) {
    int k;

    for(k = 0; k < vm->utf8_tbl_size; k++) {
        if(vm->utf8_class_table[k] != NULL) {
            _regexDealloc(vm->utf8_class_table[k], _regexMemContext);
        }
    }
    _regexDealloc(vm->utf8_class_table, _regexMemContext);
    vm->utf8_class_table = NULL;
    vm->utf8_tbl_size = 0;
}

// Group table //////////////////////////////////////////////////////////////

int _regexVMGroupTableEntryAdd(regex_build_t *build,
                               const char *group, int len) {
    regex_vm_t *vm = build->build_ctx->vm;

    if((vm->group_table = _regexRealloc(vm->group_table,
                                        (vm->group_tbl_size) * sizeof(char *),
                                        (vm->group_tbl_size + 1) * sizeof(char *),
                                        _regexMemContext)) == NULL) {
        build->status = eCompileOutOfMem;
        return -1;
    }
    if(group != NULL) {
        if((vm->group_table[vm->group_tbl_size] = _regexStrdup(group, len)) == NULL) {
            build->status = eCompileOutOfMem;
            return -1;
        }
    } else {
        vm->group_table[vm->group_tbl_size] = NULL;
    }
    vm->group_tbl_size++;

    return vm->group_tbl_size - 1;
}

int regexVMGroupTableEntryAdd(regex_build_t *build,
                              const char *group, int len, int index) {
    int id;

    while(index < build->build_ctx->vm->group_tbl_size) {
        if(_regexVMGroupTableEntryAdd(build, NULL, 0) == -1) {
            build->status = eCompileOutOfMem;
            return -1;
        }
    }
    if((id = _regexVMGroupTableEntryAdd(build, group, len)) == -1) {
        build->status = eCompileOutOfMem;
        return -1;
    }

    return id;
}

const char *regexVMGroupTableEntryLookup(regex_vm_t *vm, int group, int *len) {
    if((group < 0) || (group >= vm->group_tbl_size)) {
        return NULL;
    }

    if(len != NULL) {
        if(vm->group_table[group] == NULL) {
            *len = 0;
        } else {
            *len = strlen(vm->group_table[group]);
        }
    }
    return vm->group_table[group];
}

void regexVMGroupTableFree(regex_vm_t *vm) {
    int k;

    for(k = 0; k < vm->group_tbl_size; k++) {
        if(vm->group_table[k] != NULL) {
            _regexDealloc(vm->group_table[k], _regexMemContext);
        }
    }
    _regexDealloc(vm->group_table, _regexMemContext);
    vm->group_table = NULL;
    vm->group_tbl_size = 0;
}

const char *regexVMGroupNameFromIndex(regex_vm_t *vm, int index) {
    if((index < 2) || (index > (vm->group_tbl_size * 2))) {
        return NULL;
    }
    return vm->group_table[(index / 2) - 1];
}

int regexVMGroupNameLookup(regex_vm_t *vm, const char *name) {
    for(int k = 1; k < vm->group_tbl_size; k++) {
        if(!strcmp(vm->group_table[k - 1], name)) {
            return k;
        }
    }
    return -1;
}

#ifdef MOJO_REGEX_VM_DEBUG
int regexVMSubNameEntryAdd(regex_vm_t *vm, int index, const char *name, const char *alias) {
    int k;

    for(k = 0; k < vm->sub_name_tbl_size; k++) {
        if(vm->sub_name_table[k].id == index) {
            return 1;
        }
    }
    if((vm->sub_name_table = _regexRealloc(vm->sub_name_table,
                                           (vm->sub_name_tbl_size) * sizeof(regex_vm_sub_names_t),
                                           (vm->sub_name_tbl_size + 1) * sizeof(regex_vm_sub_names_t),
                                           _regexMemContext)) == NULL) {
        return 0;
    }
    k = vm->sub_name_tbl_size;
    vm->sub_name_tbl_size++;
    vm->sub_name_table[k].id = index;
    if(vm->sub_name_table[k].name == NULL) {
        if((name != NULL) && ((vm->sub_name_table[k].name = _regexStrdup(name, RE_STR_NULL_TERM)) == NULL)) {
            return 0;
        }
    }
    if(vm->sub_name_table[k].alias == NULL) {
        if((alias != NULL) && ((vm->sub_name_table[k].alias = _regexStrdup(alias, RE_STR_NULL_TERM)) == NULL)) {
            return 0;
        }
    }
    return 1;
}

regex_vm_sub_names_t *_regexVMSubNameEntryGet(regex_vm_t *vm, int index, int pc) {
    int k;
    for(k = 0; k < vm->sub_name_tbl_size; k++) {
        if((index >= 0) && (vm->sub_name_table[k].id == index)) {
            return vm->sub_name_table + k;
        }
        if((pc >= 0) && (vm->sub_name_table[k].pc == pc)) {
            return vm->sub_name_table + k;
        }
    }
    return NULL;
}

void regexVMSubNameEntrySetPC(regex_vm_t *vm, int index, int pc) {
    regex_vm_sub_names_t *entry;

    if((entry = _regexVMSubNameEntryGet(vm, index, -1)) != NULL) {
        entry->pc = pc;
    }
}

const char *regexVMSubNameGet(regex_vm_t *vm, int pc) {
    regex_vm_sub_names_t *entry = _regexVMSubNameEntryGet(vm, -1, pc);
    if(entry == NULL) {
        return NULL;
    }
    return entry->name;
}

const char *regexVMSubAliasGet(regex_vm_t *vm, int pc) {
    regex_vm_sub_names_t *entry = _regexVMSubNameEntryGet(vm, -1, pc);
    if(entry == NULL) {
        return NULL;
    }
    return entry->alias;
}
#endif // MOJO_REGEX_VM_DEBUG

// VM Patch list management functions ///////////////////////////////////////

#define REGEX_VM_INSTR_DECODE(var,vm,pc)   (var)[0] = (vm)->program[(pc)] & 0xFU; \
                                            (var)[1] = ((vm)->program[(pc)] & 0x3FFF0U) >> 4U; \
                                            (var)[2] = ((vm)->program[(pc)] & 0xFFFC0000U) >> 18U;

#define REGEX_VM_INSTR_ENCODE(vm,pc,instr,opa,opb) (vm)->program[(pc)] = (instr & 0xFU) | ((opa & 0x3FFFU) << 4U) | ((opb & 0x3FFFU) << 18U)

int regexAddPCPatchEntry(regex_build_t *build, regex_token_t *token, int operand) {
    regex_vm_pc_patch_t *entry;

    if(build->build_ctx->patch_pool != NULL) {
        entry = build->build_ctx->patch_pool;
        build->build_ctx->patch_pool = entry->next;
        memset(entry, 0, sizeof(regex_vm_pc_patch_t));
    } else if((entry = _regexAlloc(sizeof(regex_vm_pc_patch_t), _regexMemContext)) == NULL) {
        return 0;
    }
    entry->pc = build->build_ctx->pc;
    entry->operand = operand;
    entry->token = token;
    entry->next = build->build_ctx->patch_list;
    build->build_ctx->patch_list = entry;
    return 1;
}

void regexVMPatchListFree(regex_build_t *build, regex_vm_pc_patch_t *patch_list) {
    regex_vm_pc_patch_t *next;

    for(; patch_list != NULL; patch_list = next) {
        next = patch_list->next;
        if(build != NULL) {
            patch_list->next = build->build_ctx->patch_pool;
            build->build_ctx->patch_pool = patch_list;
        } else {
            _regexDealloc(patch_list, _regexMemContext);
        }
    }
}

void regexVMPatchJumps(regex_build_t *build, regex_vm_pc_patch_t **patch_list) {
    regex_vm_t *vm = build->build_ctx->vm;
    regex_vm_pc_patch_t *patch, *next;
    unsigned int instr[3];

    for(patch = *patch_list; patch != NULL; patch = next) {
        next = patch->next;
        REGEX_VM_INSTR_DECODE(instr, vm, patch->pc);
        if(patch->operand == 1) {
            instr[1] = patch->token->pc;
        } else {
            instr[2] = patch->token->pc;
        }
        REGEX_VM_INSTR_ENCODE(vm, patch->pc, instr[0], instr[1], instr[2]);
        if(build != NULL) {
            patch->next = build->build_ctx->patch_pool;
            build->build_ctx->patch_pool = patch;
        } else {
            _regexDealloc(patch, _regexMemContext);
        }
    }
    *patch_list = NULL;
}

int regexVMGenPathSubroutineExists(regex_build_t *build, regex_token_t *token) {
    regex_vm_gen_path_t *entry;

    for(entry = build->build_ctx->sub_path_list; entry != NULL; entry = entry->next) {
        if(entry->token == token) {
            return 1;
        }
    }
    return 0;
}

int regexVMGenPathCreate(regex_build_t *build, regex_token_t *token, int subroutine) {
    regex_vm_gen_path_t *entry;

    if(build->build_ctx->gen_path_pool != NULL) {
        entry = build->build_ctx->gen_path_pool;
        build->build_ctx->gen_path_pool = entry->next;
    } else {
        if((entry = _regexAlloc(sizeof(regex_vm_gen_path_t), _regexMemContext)) == NULL) {
            return 0;
        }
    }
    entry->token = token;
    if(subroutine) {
        entry->next = build->build_ctx->sub_path_list;
        build->build_ctx->sub_path_list = entry;
    } else {
        entry->next = build->build_ctx->gen_path_list;
        build->build_ctx->gen_path_list = entry;
    }
    return 1;
}

regex_token_t *regexVMGenPathGetNext(regex_build_t *build, int subroutine) {
    regex_vm_gen_path_t *entry;
    regex_vm_gen_path_t **base = (subroutine ? &(build->build_ctx->sub_path_list) : &(build->build_ctx->gen_path_list));

    if((entry = *base) == NULL) {
        return NULL;
    }
    *base = entry->next;
    entry->next = build->build_ctx->gen_path_pool;
    build->build_ctx->gen_path_pool = entry;
    return entry->token;
}

void regexVMGenPathFree(regex_build_t *build, int pool) {
    regex_vm_gen_path_t *walk, *next;

    for(walk = build->build_ctx->gen_path_list; walk != NULL; walk = next) {
        next = walk->next;
        if(pool) {
            walk->next = build->build_ctx->gen_path_pool;
            build->build_ctx->gen_path_pool = walk;
        } else {
            _regexDealloc(walk, _regexMemContext);
        }
    }
    build->build_ctx->gen_path_list = NULL;

    for(walk = build->build_ctx->sub_path_list; walk != NULL; walk = next) {
        next = walk->next;
        if(pool) {
            walk->next = build->build_ctx->gen_path_pool;
            build->build_ctx->gen_path_pool = walk;
        } else {
            _regexDealloc(walk, _regexMemContext);
        }
    }
    build->build_ctx->sub_path_list = NULL;

    if(!pool) {
        for(walk = build->build_ctx->gen_path_pool; walk != NULL; walk = next) {
            next = walk->next;
            free(walk);
        }
        build->build_ctx->gen_path_pool = NULL;
    }
}

// VM bytecode generator ////////////////////////////////////////////////////

int regexVMProgramAdd(regex_build_t *build, eRegexToken_t opcode,
                      unsigned int arg1, unsigned int arg2) {
    regex_vm_t *vm = build->build_ctx->vm;

    if((vm->size - build->build_ctx->pc) <= 0) {
        if((vm->program = _regexRealloc(vm->program,
                                        (vm->size * sizeof(int)),
                                        (vm->size + DEF_VM_SIZE_INC) * sizeof(int),
                                        _regexMemContext)) == NULL) {
            return 0;
        }
        vm->size += DEF_VM_SIZE_INC;
    }

    REGEX_VM_INSTR_ENCODE(vm, build->build_ctx->pc, opcode, arg1, arg2);
    build->build_ctx->pc++;

    return 1;
}

int regexVMProgramGenerateInstr(regex_build_t *build, regex_token_t *token, regex_token_t **next) {
    int idx_a = 0, idx_b = 0;
    regex_subroutine_t *routine;

    if(token == NULL) {
        *next = NULL;
        return 1;
    }
    if(token->pc != -1) {
        *next = NULL;
        return 1;
    }
    token->pc = build->build_ctx->pc;

    switch(token->tokenType) {
        case eTokenCharLiteral:
            idx_a = (int)(((unsigned)token->c) | ((token->flags & RE_TOK_FLAG_INVERT) ? RE_VM_FLAG_CHAR_INVERT : 0));
            idx_b = token->jump;
            break;
        case eTokenStringLiteral:
            if((idx_a = regexVMStringTableEntryAdd(build, token->str, token->len)) == -1) {
                return 0;
            }
            token->str = NULL;
            break;
        case eTokenCharClass:
            if((idx_a = regexVMCharClassTableEntryAdd(build, token->bitmap)) == -1) {
                return 0;
            }
            token->bitmap = NULL;
            break;
        case eTokenUtf8Class:
            if((idx_a = regexVMUtf8ClassTableEntryAdd(build, token->bitmap)) == -1) {
                return 0;
            }
            token->bitmap = NULL;
            break;
        case eTokenCall:
            if((idx_a = token->out_b->pc) == -1) {
                if(!regexAddPCPatchEntry(build, token->out_b, 1)) {
                    return 0;
                }
                if(!regexVMGenPathSubroutineExists(build, token->out_b)) {
                    if(!regexVMGenPathCreate(build, token->out_b, 1)) {
                        return 0;
                    }
                }
            }
            break;
        case eTokenSave:
            idx_a = token->group;
            idx_b = (int)((token->flags & RE_TOK_FLAG_COMPOUND) ? RE_VM_FLAG_SAVE_COMPOUND : 0);
            break;
        case eTokenSplit:
            if((idx_a = token->out_a->pc) == -1) {
                if(!regexAddPCPatchEntry(build, token->out_a, 1)) {
                    return 0;
                }
            }
            if((idx_b = token->out_b->pc) == -1) {
                if(!regexAddPCPatchEntry(build, token->out_b, 2)) {
                    return 0;
                }
            }
            break;
        case eTokenJmp:
            if((idx_a = token->out_a->pc) == -1) {
                if(!regexAddPCPatchEntry(build, token->out_a, 1)) {
                    return 0;
                }
            }
            break;
        case eTokenAssertion:
            idx_a = token->flags;
            break;
        default:
            break;
    }

    if(!regexVMProgramAdd(build, token->tokenType, idx_a, idx_b)) {
        return 0;
    }

//    printf("VM instr: 0x%4.4X - %d\n", (int)((uintptr_t)token & 0xFFFFu), token->pc);
    if(token->tokenType == eTokenSplit) {
        if(!regexVMGenPathCreate(build, token->out_b, 0)) {
            return 0;
        }
    }
//    printf("Next up 0x%4.4X\n", (int)((uintptr_t)token->out_a & 0xFFFFu));
    *next = token->out_a;
    return 1;
}

int regexVMProgramUnanchoredPrefix(regex_build_t *build) {
    if((!regexVMProgramAdd(build, eTokenSplit, 1, 3)) ||
       (!regexVMProgramAdd(build, eTokenByte, 0, 0)) ||
       (!regexVMProgramAdd(build, eTokenJmp, 0, 0))) {
        return 0;
    }
    return 1;
}

int regexVMProgramGenerate(regex_build_t *build) {
    regex_token_t *token = build->tokens;
    regex_subroutine_t *routine;

    if(!regexVMProgramUnanchoredPrefix(build)) {
        return 0;
    }
    printf("building primary...\n");
    do {
        for(;token != NULL;) {
            if(!regexVMProgramGenerateInstr(build, token, &token)) {
                return 0;
            }
        }
    } while((token = regexVMGenPathGetNext(build, 0)) != NULL);

    printf("building subroutines...\n");
    token = regexVMGenPathGetNext(build, 1);
    do {
        do {
            for(;token != NULL;) {
                if(!regexVMProgramGenerateInstr(build, token, &token)) {
                    return 0;
                }
            }
        } while((token = regexVMGenPathGetNext(build, 0)) != NULL);
    } while((token = regexVMGenPathGetNext(build, 1)) != NULL);

#ifdef MOJO_REGEX_VM_DEBUG
    for(routine = build->build_ctx->subroutine_index.subroutines; routine != NULL; routine = routine->next) {
        if(!(routine->is_graph)) {
            continue;
        }
        if(routine->tokens != NULL) {
            regexVMSubNameEntrySetPC(build->build_ctx->vm, routine->id, routine->tokens->pc);
        }
    }
#endif // MOJO_REGEX_VM_DEBUG

    printf("patching...\n");
    regexVMPatchJumps(build, &(build->build_ctx->patch_list));
    regexVMPatchListFree(build, build->build_ctx->patch_list);
    build->build_ctx->vm->size = build->build_ctx->pc;

    return 1;
}

void regexVMBuildDestroy(regex_build_t *build) {
    regexVMGenPathFree(build, 0);
    regexVMPatchListFree(NULL, build->build_ctx->patch_list);
    build->build_ctx->patch_list = NULL;
    regexTokenDestroy(NULL, build->build_ctx->token_pool, 1);
    build->build_ctx->token_pool = NULL;
}

void regexVMPrintProgram(FILE *fp, regex_vm_t *vm) {
    int pc;

    for(pc = 0; pc < vm->size; pc++) {
        regexVMInstrEmit(fp, vm, pc);
    }
}

/////////////////////////////////////////////////////////////////////////////
// Pattern compilation
/////////////////////////////////////////////////////////////////////////////

const char *regexCompilePhaseStrGet(eRegexCompilePhase_t phase) {
    switch(phase) {
        case ePhaseTokenize: return "tokenizer";
        case ePhaseNFAGraph: return "NFA graph generation";
        case ePhaseVMGen: return "VM generation";
        case ePhaseComplete: return "compilation completed";
        default: return "unknown phase";
    }
}

const char *regexCompileStatusStrGet(eRegexCompileStatus_t status) {
    switch(status) {
        case eCompileOk: return "compiled successfully";
        case eCompileCharClassRangeIncomplete: return "char class range is incomplete";
        case eCompileCharClassIncomplete: return "char class definition is incomplete";
        case eCompileEscapeCharIncomplete: return "escape character is incomplete";
        case eCompileInvalidEscapeChar: return "invalid escaped metacharacter";
        case eCompileMalformedSubExprName: return "subexpression name is malformed";
        case eCompileUnsupportedMeta: return "expression uses an unsupported meta character";
        case eCompileOutOfMem: return "out of memory";
        case eCompileMissingOperand: return "missing operand during postfix transform";
        case eCompileMissingSubexprStart: return "missing subexpr start \"(\"";
        case eCompileInternalError: return "unknown internal error token";
        case eCompileUnknownUnicodeClass: return "unknown unicode property class set";
        case eCompileConflictingAttrs: return "conflicting subexpression attributes specified";
        case eCompileUnknownSubroutine: return "unknown subroutine call identifier";
        default: return "unknown failure";
    }
}

int regexGetTensDigit(int val) {
    for(; val >= 100; val /= 100);
    return val / 10;
}

int regexGetHundredsDigit(int val) {
    for(; val >= 1000; val /= 1000);
    return val / 100;
}

int regexGetPatternDetailOffset(const char *pattern, int linelen, int pos) {
    int k, offset = 0, plen = 0, out, stride;

    for(k = 0; (k < pos) && (pattern[k] != '\0');) {
        out = regexEscapeCharLen(pattern + k, &stride);
        k += stride;
        plen += out;
        while(plen > linelen) {
            out = regexEscapeCharLen(pattern + offset, &stride);
            plen -= out;
            offset += stride;
        }
    }
    return offset;
}

void regexEmitPatternDetail(FILE *fp, const char *label, const char *pattern, int pos, int linelen) {
    int lead, offset = 0, k, end;

    if(label == NULL) {
        label = "Pattern";
    }
    lead = (int)strlen(label) + 2;
    linelen -= lead + 1;
    if(linelen <= 0) {
        return;
    }

    offset = regexGetPatternDetailOffset(pattern, linelen, pos);
    end = (int)strlen(pattern) - offset;
    if(end > (linelen + 1)) {
        end = linelen + 1;
    }

    if(pos >= 100) {
        fprintf(fp, "%*.*s", lead, lead, "");
        for(k = offset; k < end; k++) {
            fputc((!(k % 100) ? regexGetHundredsDigit(k) + '0' : ' '), fp);
        }
        fputc('\n', fp);
    }
    fprintf(fp, "%*.*s", lead, lead, "");
    for(k = offset; k < end; k++) {
        fputc((!(k % 10) ? regexGetTensDigit(k) + '0' : ' '), fp);
    }
    fprintf(fp, "\n%*.*s", lead, lead, "");
    for(k = offset; k < end; k++) {
        fputc((k % 10) + '0', fp);
    }
    fputc('\n', fp);

    fprintf(fp, "%s: ", label);
    regexEscapedStrEmit(fp, pattern + offset, end);
    fputc('\n', fp);

    fprintf(fp, "(@%-*d)  %*.*s^\n", lead - 5, pos, pos - offset, pos - offset, "");
}

void regexCompileResultEmit(FILE *fp, regex_compile_ctx_t *ctx) {
    fprintf(fp, "Result: %s\n", regexCompileStatusStrGet(ctx->status));
    if(ctx->status != eCompileOk) {
        fprintf(fp, "Phase: %s\n", regexCompilePhaseStrGet(ctx->phase));
        if(ctx->phase == ePhaseTokenize) {
            regexEmitPatternDetail(fp, "Pattern", ctx->pattern, ctx->position, 78);
        } else if(ctx->phase == ePhaseNFAGraph) {
            // ...?
        } else if(ctx->phase == ePhaseVMGen) {
            // ...?
        }
    }
}

// Return a compiled regex, or an error and position within the pattern
eRegexCompileStatus_t regexCompile(regex_compile_ctx_t *ctx, const char *pattern,
                                   unsigned int flags) {
    regex_build_ctx_t build_ctx;
    regex_build_t build;
    regex_pattern_t build_pattern;

    if((!regexVerifyTokenDetails()) || (!_regexRegUnicodeInitializeTable())) {
        ctx->status = eCompileInternalError;
        return ctx->status;
    }

    if(!regexRegUnicodeCharClassAdd(&_subroutine_test)) {
        ctx->status = eCompileInternalError;
        return ctx->status;
    }

    if((regexBuildContextInit(&build_ctx) == NULL) ||
       (regexBuilderInit(&build_ctx, &build) == NULL)) {
        ctx->status = eCompileInternalError;
        return ctx->status;
    }
    build.build_ctx->flags = flags;

    if(!regexBuildPatternPush(&build, regexPatternInit(&build_pattern, eReBuildSrcAPI, pattern, RE_STR_NULL_TERM))) {
        ctx->status = eCompileInternalError;
        return ctx->status;
    }

    memset(ctx, 0, sizeof(regex_compile_ctx_t));
    ctx->status = eCompileOk;

    // Parse the regex pattern into a sequence of tokens (operators and operands)
    // The output of this stage is a sequence of lexical tokens in infix form

   ctx->phase = ePhaseTokenize;

    if((ctx->status = regexTokenizePattern(&build)) != eCompileOk) {
        ctx->position = build_pattern.pos;
        ctx->pattern = build_pattern.base_pattern;
        regexBuildPatternPop(&build);
        regexBuilderDeinit(&build);
        regexBuildContextDeinit(&build_ctx);
        return ctx->status;
    }
    regexBuildPatternPop(&build);

    // TODO - remove debug emit
    regexTokenStrStackEmit(stdout, &build_ctx, build.tokens, 1);
    fputc('\n', stdout);

    if(regexSubroutineResolveImports(&build) != eCompileOk) {
        return ctx->status;
    }

    // Next, convert the infix form of the regular expression to postfix form,
    // and derive an NFA representation. We accomplish this using the shunting
    // yard algorithm. This is then converted into a VM bytecode sequence, for
    // runtime evaluation.
    printf("NFA graph...\n");
    ctx->phase = ePhaseNFAGraph;
    if((ctx->status = regexShuntingYard(&build)) != eCompileOk) {
        regexBuildPatternPop(&build);
        regexBuilderDeinit(&build);
        regexBuildContextDeinit(&build_ctx);
        return ctx->status;
    }

    printf("Generate VM...\n");
    ctx->phase = ePhaseVMGen;
    if(!regexVMProgramGenerate(&build)) {
        regexBuildPatternPop(&build);
        regexBuilderDeinit(&build);
        regexBuildContextDeinit(&build_ctx);
        return ctx->status;
    }
    ctx->phase = ePhaseComplete;
    ctx->vm = build_ctx.vm;
    build_ctx.vm = NULL;

    return eCompileOk;
}

#endif // MOJO_REGEX_COMPILE_IMPLEMENTATION

#ifdef MOJO_REGEX_VM_SOURCE_GENERATION
#endif // MOJO_REGEX_VM_SOURCE_GENERATION

#ifdef MOJO_REGEX_EVALUATE_IMPLEMENTATION
#endif // MOJO_REGEX_EVALUATE_IMPLEMENTATION

#ifdef MOJO_REGEX_COMMON_IMPLEMENTATION

/////////////////////////////////////////////////////////////////////////////
// Main test stub
/////////////////////////////////////////////////////////////////////////////

#ifdef MOJO_REGEX_TEST_MAIN

int main(int argc, char *argv[]) {
    regex_compile_ctx_t result;
#if 0
    const char *pattern = "a(?P<foolio>\\Bcd(?i\\w*[hijk](?*f.o)+)?)\\d+cat";
    const char *test = "abcdefgefgjfoofwofyo8167287catfoo";
#elif 1
    const char *pattern = "a(?P<foolio>?i\\Bcd(?i(?R<chunk>efg[hijk])(?*f.o)*)?)(?R<digits>\\d+)c[at]+\\R{digits}";
    const char *test = "abcdefgjfoofoofoo033195cat72364ghj";
#endif

    printf("Regex2 validation test stub\n");
    printf("===========================\n");

    printf("\nPattern: ");
    regexEscapedStrEmit(stdout, pattern, RE_STR_NULL_TERM);
    fputs("\n\n", stdout);

    // To silence the "unused function" editor warnings. Not neccessary for
    // normal usage, as the default have already been set.
    regexSetMemoryAllocators(_regexDefMemAllocator, _regexDefMemDeallocator,
                             _regexDefMemReallocator, NULL);



    if(regexCompile(&result, pattern, 0) != eCompileOk) {
        //printf("Compile failed: %s", regexCompileStatusStrGet(result.status));
        regexCompileResultEmit(stdout, &result);
        return 1;
    }
    regexCompileResultEmit(stdout, &result);

    regexVMPrintProgram(stdout, result.vm);

    return 0;
}

#endif // MOJO_REGEX_TEST_MAIN

#endif // MOJO_REGEX_COMMON_IMPLEMENTATION

#ifdef MOJO_REGEX_COMPILE_IMPLEMENTATION

/////////////////////////////////////////////////////////////////////////////
// Unicode property class definitions
/////////////////////////////////////////////////////////////////////////////

#ifdef MOJO_REGEX_UNICODE

// These property classes were generated from the unicode database by the
// extract_unicode_props.py script

const char _uax_db_Mark[] = "\\u05BF\\u05C7\\u0670\\u0711\\u07FD\\u09BC\\u09D7"
                            "\\u09FE\\u0A3C\\u0A51\\u0A75\\u0ABC\\u0B3C\\u0B82\\u0BD7\\u0CBC"
                            "\\u0D57\\u0DCA\\u0DD6\\u0E31\\u0EB1\\u0F35\\u0F37\\u0F39\\u0FC6"
                            "\\u108F\\u17DD\\u18A9\\u1A7F\\u1CED\\u1CF4\\u2D7F\\uA802\\uA806"
                            "\\uA80B\\uA8FF\\uA9E5\\uAA43\\uAAB0\\uAAC1\\uFB1E\\u{101FD}\\u{102E0}"
                            "\\u{10A3F}\\u{11173}\\u{1123E}\\u{11357}\\u{1145E}\\u{119E4}"
                            "\\u{11A47}\\u{11D3A}\\u{11D47}\\u{16F4F}\\u{1DA75}\\u{1DA84}\\u0300-"
                            "\\u036F\\u0483-\\u0489\\u0591-\\u05BD\\u05C1-\\u05C2\\u05C4-\\u05C5"
                            "\\u0610-\\u061A\\u064B-\\u065F\\u06D6-\\u06DC\\u06DF-\\u06E4\\u06E7-"
                            "\\u06E8\\u06EA-\\u06ED\\u0730-\\u074A\\u07A6-\\u07B0\\u07EB-\\u07F3"
                            "\\u0816-\\u0819\\u081B-\\u0823\\u0825-\\u0827\\u0829-\\u082D\\u0859-"
                            "\\u085B\\u08D3-\\u08E1\\u08E3-\\u0903\\u093A-\\u093C\\u093E-\\u094F"
                            "\\u0951-\\u0957\\u0962-\\u0963\\u0981-\\u0983\\u09BE-\\u09C4\\u09C7-"
                            "\\u09C8\\u09CB-\\u09CD\\u09E2-\\u09E3\\u0A01-\\u0A03\\u0A3E-\\u0A42"
                            "\\u0A47-\\u0A48\\u0A4B-\\u0A4D\\u0A70-\\u0A71\\u0A81-\\u0A83\\u0ABE-"
                            "\\u0AC5\\u0AC7-\\u0AC9\\u0ACB-\\u0ACD\\u0AE2-\\u0AE3\\u0AFA-\\u0AFF"
                            "\\u0B01-\\u0B03\\u0B3E-\\u0B44\\u0B47-\\u0B48\\u0B4B-\\u0B4D\\u0B56-"
                            "\\u0B57\\u0B62-\\u0B63\\u0BBE-\\u0BC2\\u0BC6-\\u0BC8\\u0BCA-\\u0BCD"
                            "\\u0C00-\\u0C04\\u0C3E-\\u0C44\\u0C46-\\u0C48\\u0C4A-\\u0C4D\\u0C55-"
                            "\\u0C56\\u0C62-\\u0C63\\u0C81-\\u0C83\\u0CBE-\\u0CC4\\u0CC6-\\u0CC8"
                            "\\u0CCA-\\u0CCD\\u0CD5-\\u0CD6\\u0CE2-\\u0CE3\\u0D00-\\u0D03\\u0D3B-"
                            "\\u0D3C\\u0D3E-\\u0D44\\u0D46-\\u0D48\\u0D4A-\\u0D4D\\u0D62-\\u0D63"
                            "\\u0D82-\\u0D83\\u0DCF-\\u0DD4\\u0DD8-\\u0DDF\\u0DF2-\\u0DF3\\u0E34-"
                            "\\u0E3A\\u0E47-\\u0E4E\\u0EB4-\\u0EBC\\u0EC8-\\u0ECD\\u0F18-\\u0F19"
                            "\\u0F3E-\\u0F3F\\u0F71-\\u0F84\\u0F86-\\u0F87\\u0F8D-\\u0F97\\u0F99-"
                            "\\u0FBC\\u102B-\\u103E\\u1056-\\u1059\\u105E-\\u1060\\u1062-\\u1064"
                            "\\u1067-\\u106D\\u1071-\\u1074\\u1082-\\u108D\\u109A-\\u109D\\u135D-"
                            "\\u135F\\u1712-\\u1714\\u1732-\\u1734\\u1752-\\u1753\\u1772-\\u1773"
                            "\\u17B4-\\u17D3\\u180B-\\u180D\\u1885-\\u1886\\u1920-\\u192B\\u1930-"
                            "\\u193B\\u1A17-\\u1A1B\\u1A55-\\u1A5E\\u1A60-\\u1A7C\\u1AB0-\\u1ABE"
                            "\\u1B00-\\u1B04\\u1B34-\\u1B44\\u1B6B-\\u1B73\\u1B80-\\u1B82\\u1BA1-"
                            "\\u1BAD\\u1BE6-\\u1BF3\\u1C24-\\u1C37\\u1CD0-\\u1CD2\\u1CD4-\\u1CE8"
                            "\\u1CF7-\\u1CF9\\u1DC0-\\u1DF9\\u1DFB-\\u1DFF\\u20D0-\\u20F0\\u2CEF-"
                            "\\u2CF1\\u2DE0-\\u2DFF\\u302A-\\u302F\\u3099-\\u309A\\uA66F-\\uA672"
                            "\\uA674-\\uA67D\\uA69E-\\uA69F\\uA6F0-\\uA6F1\\uA823-\\uA827\\uA880-"
                            "\\uA881\\uA8B4-\\uA8C5\\uA8E0-\\uA8F1\\uA926-\\uA92D\\uA947-\\uA953"
                            "\\uA980-\\uA983\\uA9B3-\\uA9C0\\uAA29-\\uAA36\\uAA4C-\\uAA4D\\uAA7B-"
                            "\\uAA7D\\uAAB2-\\uAAB4\\uAAB7-\\uAAB8\\uAABE-\\uAABF\\uAAEB-\\uAAEF"
                            "\\uAAF5-\\uAAF6\\uABE3-\\uABEA\\uABEC-\\uABED\\uFE00-\\uFE0F\\uFE20-"
                            "\\uFE2F\\u{10376}-\\u{1037A}\\u{10A01}-\\u{10A03}\\u{10A05}-"
                            "\\u{10A06}\\u{10A0C}-\\u{10A0F}\\u{10A38}-\\u{10A3A}\\u{10AE5}-"
                            "\\u{10AE6}\\u{10D24}-\\u{10D27}\\u{10F46}-\\u{10F50}\\u{11000}-"
                            "\\u{11002}\\u{11038}-\\u{11046}\\u{1107F}-\\u{11082}\\u{110B0}-"
                            "\\u{110BA}\\u{11100}-\\u{11102}\\u{11127}-\\u{11134}\\u{11145}-"
                            "\\u{11146}\\u{11180}-\\u{11182}\\u{111B3}-\\u{111C0}\\u{111C9}-"
                            "\\u{111CC}\\u{1122C}-\\u{11237}\\u{112DF}-\\u{112EA}\\u{11300}-"
                            "\\u{11303}\\u{1133B}-\\u{1133C}\\u{1133E}-\\u{11344}\\u{11347}-"
                            "\\u{11348}\\u{1134B}-\\u{1134D}\\u{11362}-\\u{11363}\\u{11366}-"
                            "\\u{1136C}\\u{11370}-\\u{11374}\\u{11435}-\\u{11446}\\u{114B0}-"
                            "\\u{114C3}\\u{115AF}-\\u{115B5}\\u{115B8}-\\u{115C0}\\u{115DC}-"
                            "\\u{115DD}\\u{11630}-\\u{11640}\\u{116AB}-\\u{116B7}\\u{1171D}-"
                            "\\u{1172B}\\u{1182C}-\\u{1183A}\\u{119D1}-\\u{119D7}\\u{119DA}-"
                            "\\u{119E0}\\u{11A01}-\\u{11A0A}\\u{11A33}-\\u{11A39}\\u{11A3B}-"
                            "\\u{11A3E}\\u{11A51}-\\u{11A5B}\\u{11A8A}-\\u{11A99}\\u{11C2F}-"
                            "\\u{11C36}\\u{11C38}-\\u{11C3F}\\u{11C92}-\\u{11CA7}\\u{11CA9}-"
                            "\\u{11CB6}\\u{11D31}-\\u{11D36}\\u{11D3C}-\\u{11D3D}\\u{11D3F}-"
                            "\\u{11D45}\\u{11D8A}-\\u{11D8E}\\u{11D90}-\\u{11D91}\\u{11D93}-"
                            "\\u{11D97}\\u{11EF3}-\\u{11EF6}\\u{16AF0}-\\u{16AF4}\\u{16B30}-"
                            "\\u{16B36}\\u{16F51}-\\u{16F87}\\u{16F8F}-\\u{16F92}\\u{1BC9D}-"
                            "\\u{1BC9E}\\u{1D165}-\\u{1D169}\\u{1D16D}-\\u{1D172}\\u{1D17B}-"
                            "\\u{1D182}\\u{1D185}-\\u{1D18B}\\u{1D1AA}-\\u{1D1AD}\\u{1D242}-"
                            "\\u{1D244}\\u{1DA00}-\\u{1DA36}\\u{1DA3B}-\\u{1DA6C}\\u{1DA9B}-"
                            "\\u{1DA9F}\\u{1DAA1}-\\u{1DAAF}\\u{1E000}-\\u{1E006}\\u{1E008}-"
                            "\\u{1E018}\\u{1E01B}-\\u{1E021}\\u{1E023}-\\u{1E024}\\u{1E026}-"
                            "\\u{1E02A}\\u{1E130}-\\u{1E136}\\u{1E2EC}-\\u{1E2EF}\\u{1E8D0}-"
                            "\\u{1E8D6}\\u{1E944}-\\u{1E94A}\\u{E0100}-\\u{E01EF}";

const char _uax_db_Number[] = "\\u00B9\\u2070\\u2CFD\\u3007\\u{10341}"
                              "\\u{1034A}\\u0030-\\u0039\\u00B2-\\u00B3\\u00BC-\\u00BE\\u0660-"
                              "\\u0669\\u06F0-\\u06F9\\u07C0-\\u07C9\\u0966-\\u096F\\u09E6-\\u09EF"
                              "\\u09F4-\\u09F9\\u0A66-\\u0A6F\\u0AE6-\\u0AEF\\u0B66-\\u0B6F\\u0B72-"
                              "\\u0B77\\u0BE6-\\u0BF2\\u0C66-\\u0C6F\\u0C78-\\u0C7E\\u0CE6-\\u0CEF"
                              "\\u0D58-\\u0D5E\\u0D66-\\u0D78\\u0DE6-\\u0DEF\\u0E50-\\u0E59\\u0ED0-"
                              "\\u0ED9\\u0F20-\\u0F33\\u1040-\\u1049\\u1090-\\u1099\\u1369-\\u137C"
                              "\\u16EE-\\u16F0\\u17E0-\\u17E9\\u17F0-\\u17F9\\u1810-\\u1819\\u1946-"
                              "\\u194F\\u19D0-\\u19DA\\u1A80-\\u1A89\\u1A90-\\u1A99\\u1B50-\\u1B59"
                              "\\u1BB0-\\u1BB9\\u1C40-\\u1C49\\u1C50-\\u1C59\\u2074-\\u2079\\u2080-"
                              "\\u2089\\u2150-\\u2182\\u2185-\\u2189\\u2460-\\u249B\\u24EA-\\u24FF"
                              "\\u2776-\\u2793\\u3021-\\u3029\\u3038-\\u303A\\u3192-\\u3195\\u3220-"
                              "\\u3229\\u3248-\\u324F\\u3251-\\u325F\\u3280-\\u3289\\u32B1-\\u32BF"
                              "\\uA620-\\uA629\\uA6E6-\\uA6EF\\uA830-\\uA835\\uA8D0-\\uA8D9\\uA900-"
                              "\\uA909\\uA9D0-\\uA9D9\\uA9F0-\\uA9F9\\uAA50-\\uAA59\\uABF0-\\uABF9"
                              "\\uFF10-\\uFF19\\u{10107}-\\u{10133}\\u{10140}-\\u{10178}\\u{1018A}-"
                              "\\u{1018B}\\u{102E1}-\\u{102FB}\\u{10320}-\\u{10323}\\u{103D1}-"
                              "\\u{103D5}\\u{104A0}-\\u{104A9}\\u{10858}-\\u{1085F}\\u{10879}-"
                              "\\u{1087F}\\u{108A7}-\\u{108AF}\\u{108FB}-\\u{108FF}\\u{10916}-"
                              "\\u{1091B}\\u{109BC}-\\u{109BD}\\u{109C0}-\\u{109CF}\\u{109D2}-"
                              "\\u{109FF}\\u{10A40}-\\u{10A48}\\u{10A7D}-\\u{10A7E}\\u{10A9D}-"
                              "\\u{10A9F}\\u{10AEB}-\\u{10AEF}\\u{10B58}-\\u{10B5F}\\u{10B78}-"
                              "\\u{10B7F}\\u{10BA9}-\\u{10BAF}\\u{10CFA}-\\u{10CFF}\\u{10D30}-"
                              "\\u{10D39}\\u{10E60}-\\u{10E7E}\\u{10F1D}-\\u{10F26}\\u{10F51}-"
                              "\\u{10F54}\\u{11052}-\\u{1106F}\\u{110F0}-\\u{110F9}\\u{11136}-"
                              "\\u{1113F}\\u{111D0}-\\u{111D9}\\u{111E1}-\\u{111F4}\\u{112F0}-"
                              "\\u{112F9}\\u{11450}-\\u{11459}\\u{114D0}-\\u{114D9}\\u{11650}-"
                              "\\u{11659}\\u{116C0}-\\u{116C9}\\u{11730}-\\u{1173B}\\u{118E0}-"
                              "\\u{118F2}\\u{11C50}-\\u{11C6C}\\u{11D50}-\\u{11D59}\\u{11DA0}-"
                              "\\u{11DA9}\\u{11FC0}-\\u{11FD4}\\u{12400}-\\u{1246E}\\u{16A60}-"
                              "\\u{16A69}\\u{16B50}-\\u{16B59}\\u{16B5B}-\\u{16B61}\\u{16E80}-"
                              "\\u{16E96}\\u{1D2E0}-\\u{1D2F3}\\u{1D360}-\\u{1D378}\\u{1D7CE}-"
                              "\\u{1D7FF}\\u{1E140}-\\u{1E149}\\u{1E2F0}-\\u{1E2F9}\\u{1E8C7}-"
                              "\\u{1E8CF}\\u{1E950}-\\u{1E959}\\u{1EC71}-\\u{1ECAB}\\u{1ECAD}-"
                              "\\u{1ECAF}\\u{1ECB1}-\\u{1ECB4}\\u{1ED01}-\\u{1ED2D}\\u{1ED2F}-"
                              "\\u{1ED3D}\\u{1F100}-\\u{1F10C}";

const char _uax_db_Punctuation[] = "\\u005F\\u007B\\u007D\\u00A1\\u00A7\\u00AB"
                                   "\\u00BB\\u00BF\\u037E\\u0387\\u05BE\\u05C0\\u05C3\\u05C6\\u061B"
                                   "\\u06D4\\u085E\\u0970\\u09FD\\u0A76\\u0AF0\\u0C77\\u0C84\\u0DF4"
                                   "\\u0E4F\\u0F14\\u0F85\\u10FB\\u1400\\u166E\\u1CD3\\u2D70\\u3030"
                                   "\\u303D\\u30A0\\u30FB\\uA673\\uA67E\\uA8FC\\uA95F\\uABEB\\uFE63"
                                   "\\uFE68\\uFF3F\\uFF5B\\uFF5D\\u{1039F}\\u{103D0}\\u{1056F}\\u{10857}"
                                   "\\u{1091F}\\u{1093F}\\u{10A7F}\\u{111CD}\\u{111DB}\\u{112A9}"
                                   "\\u{1145B}\\u{1145D}\\u{114C6}\\u{1183B}\\u{119E2}\\u{11FFF}"
                                   "\\u{16AF5}\\u{16B44}\\u{16FE2}\\u{1BC9F}\\u0021-\\u0023\\u0025-"
                                   "\\u002A\\u002C-\\u002F\\u003A-\\u003B\\u003F-\\u0040\\u005B-\\u005D"
                                   "\\u00B6-\\u00B7\\u055A-\\u055F\\u0589-\\u058A\\u05F3-\\u05F4\\u0609-"
                                   "\\u060A\\u060C-\\u060D\\u061E-\\u061F\\u066A-\\u066D\\u0700-\\u070D"
                                   "\\u07F7-\\u07F9\\u0830-\\u083E\\u0964-\\u0965\\u0E5A-\\u0E5B\\u0F04-"
                                   "\\u0F12\\u0F3A-\\u0F3D\\u0FD0-\\u0FD4\\u0FD9-\\u0FDA\\u104A-\\u104F"
                                   "\\u1360-\\u1368\\u169B-\\u169C\\u16EB-\\u16ED\\u1735-\\u1736\\u17D4-"
                                   "\\u17D6\\u17D8-\\u17DA\\u1800-\\u180A\\u1944-\\u1945\\u1A1E-\\u1A1F"
                                   "\\u1AA0-\\u1AA6\\u1AA8-\\u1AAD\\u1B5A-\\u1B60\\u1BFC-\\u1BFF\\u1C3B-"
                                   "\\u1C3F\\u1C7E-\\u1C7F\\u1CC0-\\u1CC7\\u2010-\\u2027\\u2030-\\u2043"
                                   "\\u2045-\\u2051\\u2053-\\u205E\\u207D-\\u207E\\u208D-\\u208E\\u2308-"
                                   "\\u230B\\u2329-\\u232A\\u2768-\\u2775\\u27C5-\\u27C6\\u27E6-\\u27EF"
                                   "\\u2983-\\u2998\\u29D8-\\u29DB\\u29FC-\\u29FD\\u2CF9-\\u2CFC\\u2CFE-"
                                   "\\u2CFF\\u2E00-\\u2E2E\\u2E30-\\u2E4F\\u3001-\\u3003\\u3008-\\u3011"
                                   "\\u3014-\\u301F\\uA4FE-\\uA4FF\\uA60D-\\uA60F\\uA6F2-\\uA6F7\\uA874-"
                                   "\\uA877\\uA8CE-\\uA8CF\\uA8F8-\\uA8FA\\uA92E-\\uA92F\\uA9C1-\\uA9CD"
                                   "\\uA9DE-\\uA9DF\\uAA5C-\\uAA5F\\uAADE-\\uAADF\\uAAF0-\\uAAF1\\uFD3E-"
                                   "\\uFD3F\\uFE10-\\uFE19\\uFE30-\\uFE52\\uFE54-\\uFE61\\uFE6A-\\uFE6B"
                                   "\\uFF01-\\uFF03\\uFF05-\\uFF0A\\uFF0C-\\uFF0F\\uFF1A-\\uFF1B\\uFF1F-"
                                   "\\uFF20\\uFF3B-\\uFF3D\\uFF5F-\\uFF65\\u{10100}-\\u{10102}\\u{10A50}-"
                                   "\\u{10A58}\\u{10AF0}-\\u{10AF6}\\u{10B39}-\\u{10B3F}\\u{10B99}-"
                                   "\\u{10B9C}\\u{10F55}-\\u{10F59}\\u{11047}-\\u{1104D}\\u{110BB}-"
                                   "\\u{110BC}\\u{110BE}-\\u{110C1}\\u{11140}-\\u{11143}\\u{11174}-"
                                   "\\u{11175}\\u{111C5}-\\u{111C8}\\u{111DD}-\\u{111DF}\\u{11238}-"
                                   "\\u{1123D}\\u{1144B}-\\u{1144F}\\u{115C1}-\\u{115D7}\\u{11641}-"
                                   "\\u{11643}\\u{11660}-\\u{1166C}\\u{1173C}-\\u{1173E}\\u{11A3F}-"
                                   "\\u{11A46}\\u{11A9A}-\\u{11A9C}\\u{11A9E}-\\u{11AA2}\\u{11C41}-"
                                   "\\u{11C45}\\u{11C70}-\\u{11C71}\\u{11EF7}-\\u{11EF8}\\u{12470}-"
                                   "\\u{12474}\\u{16A6E}-\\u{16A6F}\\u{16B37}-\\u{16B3B}\\u{16E97}-"
                                   "\\u{16E9A}\\u{1DA87}-\\u{1DA8B}\\u{1E95E}-\\u{1E95F}";

const char _uax_db_Separator[] = "\\u0020\\u00A0\\u1680\\u202F\\u205F\\u3000"
                                 "\\u2000-\\u200A\\u2028-\\u2029";

const char _uax_db_Uppercase_Letter[] = "\\u0100\\u0102\\u0104\\u0106\\u0108"
                                        "\\u010A\\u010C\\u010E\\u0110\\u0112\\u0114\\u0116\\u0118\\u011A"
                                        "\\u011C\\u011E\\u0120\\u0122\\u0124\\u0126\\u0128\\u012A\\u012C"
                                        "\\u012E\\u0130\\u0132\\u0134\\u0136\\u0139\\u013B\\u013D\\u013F"
                                        "\\u0141\\u0143\\u0145\\u0147\\u014A\\u014C\\u014E\\u0150\\u0152"
                                        "\\u0154\\u0156\\u0158\\u015A\\u015C\\u015E\\u0160\\u0162\\u0164"
                                        "\\u0166\\u0168\\u016A\\u016C\\u016E\\u0170\\u0172\\u0174\\u0176"
                                        "\\u017B\\u017D\\u0184\\u01A2\\u01A4\\u01A9\\u01AC\\u01B5\\u01BC"
                                        "\\u01C4\\u01C7\\u01CA\\u01CD\\u01CF\\u01D1\\u01D3\\u01D5\\u01D7"
                                        "\\u01D9\\u01DB\\u01DE\\u01E0\\u01E2\\u01E4\\u01E6\\u01E8\\u01EA"
                                        "\\u01EC\\u01EE\\u01F1\\u01F4\\u01FA\\u01FC\\u01FE\\u0200\\u0202"
                                        "\\u0204\\u0206\\u0208\\u020A\\u020C\\u020E\\u0210\\u0212\\u0214"
                                        "\\u0216\\u0218\\u021A\\u021C\\u021E\\u0220\\u0222\\u0224\\u0226"
                                        "\\u0228\\u022A\\u022C\\u022E\\u0230\\u0232\\u0241\\u0248\\u024A"
                                        "\\u024C\\u024E\\u0370\\u0372\\u0376\\u037F\\u0386\\u038C\\u03CF"
                                        "\\u03D8\\u03DA\\u03DC\\u03DE\\u03E0\\u03E2\\u03E4\\u03E6\\u03E8"
                                        "\\u03EA\\u03EC\\u03EE\\u03F4\\u03F7\\u0460\\u0462\\u0464\\u0466"
                                        "\\u0468\\u046A\\u046C\\u046E\\u0470\\u0472\\u0474\\u0476\\u0478"
                                        "\\u047A\\u047C\\u047E\\u0480\\u048A\\u048C\\u048E\\u0490\\u0492"
                                        "\\u0494\\u0496\\u0498\\u049A\\u049C\\u049E\\u04A0\\u04A2\\u04A4"
                                        "\\u04A6\\u04A8\\u04AA\\u04AC\\u04AE\\u04B0\\u04B2\\u04B4\\u04B6"
                                        "\\u04B8\\u04BA\\u04BC\\u04BE\\u04C3\\u04C5\\u04C7\\u04C9\\u04CB"
                                        "\\u04CD\\u04D0\\u04D2\\u04D4\\u04D6\\u04D8\\u04DA\\u04DC\\u04DE"
                                        "\\u04E0\\u04E2\\u04E4\\u04E6\\u04E8\\u04EA\\u04EC\\u04EE\\u04F0"
                                        "\\u04F2\\u04F4\\u04F6\\u04F8\\u04FA\\u04FC\\u04FE\\u0500\\u0502"
                                        "\\u0504\\u0506\\u0508\\u050A\\u050C\\u050E\\u0510\\u0512\\u0514"
                                        "\\u0516\\u0518\\u051A\\u051C\\u051E\\u0520\\u0522\\u0524\\u0526"
                                        "\\u0528\\u052A\\u052C\\u052E\\u10C7\\u10CD\\u1E00\\u1E02\\u1E04"
                                        "\\u1E06\\u1E08\\u1E0A\\u1E0C\\u1E0E\\u1E10\\u1E12\\u1E14\\u1E16"
                                        "\\u1E18\\u1E1A\\u1E1C\\u1E1E\\u1E20\\u1E22\\u1E24\\u1E26\\u1E28"
                                        "\\u1E2A\\u1E2C\\u1E2E\\u1E30\\u1E32\\u1E34\\u1E36\\u1E38\\u1E3A"
                                        "\\u1E3C\\u1E3E\\u1E40\\u1E42\\u1E44\\u1E46\\u1E48\\u1E4A\\u1E4C"
                                        "\\u1E4E\\u1E50\\u1E52\\u1E54\\u1E56\\u1E58\\u1E5A\\u1E5C\\u1E5E"
                                        "\\u1E60\\u1E62\\u1E64\\u1E66\\u1E68\\u1E6A\\u1E6C\\u1E6E\\u1E70"
                                        "\\u1E72\\u1E74\\u1E76\\u1E78\\u1E7A\\u1E7C\\u1E7E\\u1E80\\u1E82"
                                        "\\u1E84\\u1E86\\u1E88\\u1E8A\\u1E8C\\u1E8E\\u1E90\\u1E92\\u1E94"
                                        "\\u1E9E\\u1EA0\\u1EA2\\u1EA4\\u1EA6\\u1EA8\\u1EAA\\u1EAC\\u1EAE"
                                        "\\u1EB0\\u1EB2\\u1EB4\\u1EB6\\u1EB8\\u1EBA\\u1EBC\\u1EBE\\u1EC0"
                                        "\\u1EC2\\u1EC4\\u1EC6\\u1EC8\\u1ECA\\u1ECC\\u1ECE\\u1ED0\\u1ED2"
                                        "\\u1ED4\\u1ED6\\u1ED8\\u1EDA\\u1EDC\\u1EDE\\u1EE0\\u1EE2\\u1EE4"
                                        "\\u1EE6\\u1EE8\\u1EEA\\u1EEC\\u1EEE\\u1EF0\\u1EF2\\u1EF4\\u1EF6"
                                        "\\u1EF8\\u1EFA\\u1EFC\\u1EFE\\u1F59\\u1F5B\\u1F5D\\u1F5F\\u2102"
                                        "\\u2107\\u2115\\u2124\\u2126\\u2128\\u2145\\u2183\\u2C60\\u2C67"
                                        "\\u2C69\\u2C6B\\u2C72\\u2C75\\u2C82\\u2C84\\u2C86\\u2C88\\u2C8A"
                                        "\\u2C8C\\u2C8E\\u2C90\\u2C92\\u2C94\\u2C96\\u2C98\\u2C9A\\u2C9C"
                                        "\\u2C9E\\u2CA0\\u2CA2\\u2CA4\\u2CA6\\u2CA8\\u2CAA\\u2CAC\\u2CAE"
                                        "\\u2CB0\\u2CB2\\u2CB4\\u2CB6\\u2CB8\\u2CBA\\u2CBC\\u2CBE\\u2CC0"
                                        "\\u2CC2\\u2CC4\\u2CC6\\u2CC8\\u2CCA\\u2CCC\\u2CCE\\u2CD0\\u2CD2"
                                        "\\u2CD4\\u2CD6\\u2CD8\\u2CDA\\u2CDC\\u2CDE\\u2CE0\\u2CE2\\u2CEB"
                                        "\\u2CED\\u2CF2\\uA640\\uA642\\uA644\\uA646\\uA648\\uA64A\\uA64C"
                                        "\\uA64E\\uA650\\uA652\\uA654\\uA656\\uA658\\uA65A\\uA65C\\uA65E"
                                        "\\uA660\\uA662\\uA664\\uA666\\uA668\\uA66A\\uA66C\\uA680\\uA682"
                                        "\\uA684\\uA686\\uA688\\uA68A\\uA68C\\uA68E\\uA690\\uA692\\uA694"
                                        "\\uA696\\uA698\\uA69A\\uA722\\uA724\\uA726\\uA728\\uA72A\\uA72C"
                                        "\\uA72E\\uA732\\uA734\\uA736\\uA738\\uA73A\\uA73C\\uA73E\\uA740"
                                        "\\uA742\\uA744\\uA746\\uA748\\uA74A\\uA74C\\uA74E\\uA750\\uA752"
                                        "\\uA754\\uA756\\uA758\\uA75A\\uA75C\\uA75E\\uA760\\uA762\\uA764"
                                        "\\uA766\\uA768\\uA76A\\uA76C\\uA76E\\uA779\\uA77B\\uA780\\uA782"
                                        "\\uA784\\uA786\\uA78B\\uA78D\\uA790\\uA792\\uA796\\uA798\\uA79A"
                                        "\\uA79C\\uA79E\\uA7A0\\uA7A2\\uA7A4\\uA7A6\\uA7A8\\uA7B6\\uA7B8"
                                        "\\uA7BA\\uA7BC\\uA7BE\\uA7C2\\u{1D49C}\\u{1D4A2}\\u{1D546}\\u{1D7CA}"
                                        "\\u0041-\\u005A\\u00C0-\\u00D6\\u00D8-\\u00DE\\u0178-\\u0179\\u0181-"
                                        "\\u0182\\u0186-\\u0187\\u0189-\\u018B\\u018E-\\u0191\\u0193-\\u0194"
                                        "\\u0196-\\u0198\\u019C-\\u019D\\u019F-\\u01A0\\u01A6-\\u01A7\\u01AE-"
                                        "\\u01AF\\u01B1-\\u01B3\\u01B7-\\u01B8\\u01F6-\\u01F8\\u023A-\\u023B"
                                        "\\u023D-\\u023E\\u0243-\\u0246\\u0388-\\u038A\\u038E-\\u038F\\u0391-"
                                        "\\u03A1\\u03A3-\\u03AB\\u03D2-\\u03D4\\u03F9-\\u03FA\\u03FD-\\u042F"
                                        "\\u04C0-\\u04C1\\u0531-\\u0556\\u10A0-\\u10C5\\u13A0-\\u13F5\\u1C90-"
                                        "\\u1CBA\\u1CBD-\\u1CBF\\u1F08-\\u1F0F\\u1F18-\\u1F1D\\u1F28-\\u1F2F"
                                        "\\u1F38-\\u1F3F\\u1F48-\\u1F4D\\u1F68-\\u1F6F\\u1FB8-\\u1FBB\\u1FC8-"
                                        "\\u1FCB\\u1FD8-\\u1FDB\\u1FE8-\\u1FEC\\u1FF8-\\u1FFB\\u210B-\\u210D"
                                        "\\u2110-\\u2112\\u2119-\\u211D\\u212A-\\u212D\\u2130-\\u2133\\u213E-"
                                        "\\u213F\\u2C00-\\u2C2E\\u2C62-\\u2C64\\u2C6D-\\u2C70\\u2C7E-\\u2C80"
                                        "\\uA77D-\\uA77E\\uA7AA-\\uA7AE\\uA7B0-\\uA7B4\\uA7C4-\\uA7C6\\uFF21-"
                                        "\\uFF3A\\u{10400}-\\u{10427}\\u{104B0}-\\u{104D3}\\u{10C80}-"
                                        "\\u{10CB2}\\u{118A0}-\\u{118BF}\\u{16E40}-\\u{16E5F}\\u{1D400}-"
                                        "\\u{1D419}\\u{1D434}-\\u{1D44D}\\u{1D468}-\\u{1D481}\\u{1D49E}-"
                                        "\\u{1D49F}\\u{1D4A5}-\\u{1D4A6}\\u{1D4A9}-\\u{1D4AC}\\u{1D4AE}-"
                                        "\\u{1D4B5}\\u{1D4D0}-\\u{1D4E9}\\u{1D504}-\\u{1D505}\\u{1D507}-"
                                        "\\u{1D50A}\\u{1D50D}-\\u{1D514}\\u{1D516}-\\u{1D51C}\\u{1D538}-"
                                        "\\u{1D539}\\u{1D53B}-\\u{1D53E}\\u{1D540}-\\u{1D544}\\u{1D54A}-"
                                        "\\u{1D550}\\u{1D56C}-\\u{1D585}\\u{1D5A0}-\\u{1D5B9}\\u{1D5D4}-"
                                        "\\u{1D5ED}\\u{1D608}-\\u{1D621}\\u{1D63C}-\\u{1D655}\\u{1D670}-"
                                        "\\u{1D689}\\u{1D6A8}-\\u{1D6C0}\\u{1D6E2}-\\u{1D6FA}\\u{1D71C}-"
                                        "\\u{1D734}\\u{1D756}-\\u{1D76E}\\u{1D790}-\\u{1D7A8}\\u{1E900}-"
                                        "\\u{1E921}";

const char _uax_db_Lowercase_Letter[] = "\\u00B5\\u0101\\u0103\\u0105\\u0107"
                                        "\\u0109\\u010B\\u010D\\u010F\\u0111\\u0113\\u0115\\u0117\\u0119"
                                        "\\u011B\\u011D\\u011F\\u0121\\u0123\\u0125\\u0127\\u0129\\u012B"
                                        "\\u012D\\u012F\\u0131\\u0133\\u0135\\u013A\\u013C\\u013E\\u0140"
                                        "\\u0142\\u0144\\u0146\\u014B\\u014D\\u014F\\u0151\\u0153\\u0155"
                                        "\\u0157\\u0159\\u015B\\u015D\\u015F\\u0161\\u0163\\u0165\\u0167"
                                        "\\u0169\\u016B\\u016D\\u016F\\u0171\\u0173\\u0175\\u0177\\u017A"
                                        "\\u017C\\u0183\\u0185\\u0188\\u0192\\u0195\\u019E\\u01A1\\u01A3"
                                        "\\u01A5\\u01A8\\u01AD\\u01B0\\u01B4\\u01B6\\u01C6\\u01C9\\u01CC"
                                        "\\u01CE\\u01D0\\u01D2\\u01D4\\u01D6\\u01D8\\u01DA\\u01DF\\u01E1"
                                        "\\u01E3\\u01E5\\u01E7\\u01E9\\u01EB\\u01ED\\u01F3\\u01F5\\u01F9"
                                        "\\u01FB\\u01FD\\u01FF\\u0201\\u0203\\u0205\\u0207\\u0209\\u020B"
                                        "\\u020D\\u020F\\u0211\\u0213\\u0215\\u0217\\u0219\\u021B\\u021D"
                                        "\\u021F\\u0221\\u0223\\u0225\\u0227\\u0229\\u022B\\u022D\\u022F"
                                        "\\u0231\\u023C\\u0242\\u0247\\u0249\\u024B\\u024D\\u0371\\u0373"
                                        "\\u0377\\u0390\\u03D9\\u03DB\\u03DD\\u03DF\\u03E1\\u03E3\\u03E5"
                                        "\\u03E7\\u03E9\\u03EB\\u03ED\\u03F5\\u03F8\\u0461\\u0463\\u0465"
                                        "\\u0467\\u0469\\u046B\\u046D\\u046F\\u0471\\u0473\\u0475\\u0477"
                                        "\\u0479\\u047B\\u047D\\u047F\\u0481\\u048B\\u048D\\u048F\\u0491"
                                        "\\u0493\\u0495\\u0497\\u0499\\u049B\\u049D\\u049F\\u04A1\\u04A3"
                                        "\\u04A5\\u04A7\\u04A9\\u04AB\\u04AD\\u04AF\\u04B1\\u04B3\\u04B5"
                                        "\\u04B7\\u04B9\\u04BB\\u04BD\\u04BF\\u04C2\\u04C4\\u04C6\\u04C8"
                                        "\\u04CA\\u04CC\\u04D1\\u04D3\\u04D5\\u04D7\\u04D9\\u04DB\\u04DD"
                                        "\\u04DF\\u04E1\\u04E3\\u04E5\\u04E7\\u04E9\\u04EB\\u04ED\\u04EF"
                                        "\\u04F1\\u04F3\\u04F5\\u04F7\\u04F9\\u04FB\\u04FD\\u04FF\\u0501"
                                        "\\u0503\\u0505\\u0507\\u0509\\u050B\\u050D\\u050F\\u0511\\u0513"
                                        "\\u0515\\u0517\\u0519\\u051B\\u051D\\u051F\\u0521\\u0523\\u0525"
                                        "\\u0527\\u0529\\u052B\\u052D\\u052F\\u1E01\\u1E03\\u1E05\\u1E07"
                                        "\\u1E09\\u1E0B\\u1E0D\\u1E0F\\u1E11\\u1E13\\u1E15\\u1E17\\u1E19"
                                        "\\u1E1B\\u1E1D\\u1E1F\\u1E21\\u1E23\\u1E25\\u1E27\\u1E29\\u1E2B"
                                        "\\u1E2D\\u1E2F\\u1E31\\u1E33\\u1E35\\u1E37\\u1E39\\u1E3B\\u1E3D"
                                        "\\u1E3F\\u1E41\\u1E43\\u1E45\\u1E47\\u1E49\\u1E4B\\u1E4D\\u1E4F"
                                        "\\u1E51\\u1E53\\u1E55\\u1E57\\u1E59\\u1E5B\\u1E5D\\u1E5F\\u1E61"
                                        "\\u1E63\\u1E65\\u1E67\\u1E69\\u1E6B\\u1E6D\\u1E6F\\u1E71\\u1E73"
                                        "\\u1E75\\u1E77\\u1E79\\u1E7B\\u1E7D\\u1E7F\\u1E81\\u1E83\\u1E85"
                                        "\\u1E87\\u1E89\\u1E8B\\u1E8D\\u1E8F\\u1E91\\u1E93\\u1E9F\\u1EA1"
                                        "\\u1EA3\\u1EA5\\u1EA7\\u1EA9\\u1EAB\\u1EAD\\u1EAF\\u1EB1\\u1EB3"
                                        "\\u1EB5\\u1EB7\\u1EB9\\u1EBB\\u1EBD\\u1EBF\\u1EC1\\u1EC3\\u1EC5"
                                        "\\u1EC7\\u1EC9\\u1ECB\\u1ECD\\u1ECF\\u1ED1\\u1ED3\\u1ED5\\u1ED7"
                                        "\\u1ED9\\u1EDB\\u1EDD\\u1EDF\\u1EE1\\u1EE3\\u1EE5\\u1EE7\\u1EE9"
                                        "\\u1EEB\\u1EED\\u1EEF\\u1EF1\\u1EF3\\u1EF5\\u1EF7\\u1EF9\\u1EFB"
                                        "\\u1EFD\\u1FBE\\u210A\\u2113\\u212F\\u2134\\u2139\\u214E\\u2184"
                                        "\\u2C61\\u2C68\\u2C6A\\u2C6C\\u2C71\\u2C81\\u2C83\\u2C85\\u2C87"
                                        "\\u2C89\\u2C8B\\u2C8D\\u2C8F\\u2C91\\u2C93\\u2C95\\u2C97\\u2C99"
                                        "\\u2C9B\\u2C9D\\u2C9F\\u2CA1\\u2CA3\\u2CA5\\u2CA7\\u2CA9\\u2CAB"
                                        "\\u2CAD\\u2CAF\\u2CB1\\u2CB3\\u2CB5\\u2CB7\\u2CB9\\u2CBB\\u2CBD"
                                        "\\u2CBF\\u2CC1\\u2CC3\\u2CC5\\u2CC7\\u2CC9\\u2CCB\\u2CCD\\u2CCF"
                                        "\\u2CD1\\u2CD3\\u2CD5\\u2CD7\\u2CD9\\u2CDB\\u2CDD\\u2CDF\\u2CE1"
                                        "\\u2CEC\\u2CEE\\u2CF3\\u2D27\\u2D2D\\uA641\\uA643\\uA645\\uA647"
                                        "\\uA649\\uA64B\\uA64D\\uA64F\\uA651\\uA653\\uA655\\uA657\\uA659"
                                        "\\uA65B\\uA65D\\uA65F\\uA661\\uA663\\uA665\\uA667\\uA669\\uA66B"
                                        "\\uA66D\\uA681\\uA683\\uA685\\uA687\\uA689\\uA68B\\uA68D\\uA68F"
                                        "\\uA691\\uA693\\uA695\\uA697\\uA699\\uA69B\\uA723\\uA725\\uA727"
                                        "\\uA729\\uA72B\\uA72D\\uA733\\uA735\\uA737\\uA739\\uA73B\\uA73D"
                                        "\\uA73F\\uA741\\uA743\\uA745\\uA747\\uA749\\uA74B\\uA74D\\uA74F"
                                        "\\uA751\\uA753\\uA755\\uA757\\uA759\\uA75B\\uA75D\\uA75F\\uA761"
                                        "\\uA763\\uA765\\uA767\\uA769\\uA76B\\uA76D\\uA76F\\uA77A\\uA77C"
                                        "\\uA77F\\uA781\\uA783\\uA785\\uA787\\uA78C\\uA78E\\uA791\\uA797"
                                        "\\uA799\\uA79B\\uA79D\\uA79F\\uA7A1\\uA7A3\\uA7A5\\uA7A7\\uA7A9"
                                        "\\uA7AF\\uA7B5\\uA7B7\\uA7B9\\uA7BB\\uA7BD\\uA7BF\\uA7C3\\uA7FA"
                                        "\\u{1D4BB}\\u{1D7CB}\\u0061-\\u007A\\u00DF-\\u00F6\\u00F8-\\u00FF"
                                        "\\u0137-\\u0138\\u0148-\\u0149\\u017E-\\u0180\\u018C-\\u018D\\u0199-"
                                        "\\u019B\\u01AA-\\u01AB\\u01B9-\\u01BA\\u01BD-\\u01BF\\u01DC-\\u01DD"
                                        "\\u01EF-\\u01F0\\u0233-\\u0239\\u023F-\\u0240\\u024F-\\u0293\\u0295-"
                                        "\\u02AF\\u037B-\\u037D\\u03AC-\\u03CE\\u03D0-\\u03D1\\u03D5-\\u03D7"
                                        "\\u03EF-\\u03F3\\u03FB-\\u03FC\\u0430-\\u045F\\u04CE-\\u04CF\\u0560-"
                                        "\\u0588\\u10D0-\\u10FA\\u10FD-\\u10FF\\u13F8-\\u13FD\\u1C80-\\u1C88"
                                        "\\u1D00-\\u1D2B\\u1D6B-\\u1D77\\u1D79-\\u1D9A\\u1E95-\\u1E9D\\u1EFF-"
                                        "\\u1F07\\u1F10-\\u1F15\\u1F20-\\u1F27\\u1F30-\\u1F37\\u1F40-\\u1F45"
                                        "\\u1F50-\\u1F57\\u1F60-\\u1F67\\u1F70-\\u1F7D\\u1F80-\\u1F87\\u1F90-"
                                        "\\u1F97\\u1FA0-\\u1FA7\\u1FB0-\\u1FB4\\u1FB6-\\u1FB7\\u1FC2-\\u1FC4"
                                        "\\u1FC6-\\u1FC7\\u1FD0-\\u1FD3\\u1FD6-\\u1FD7\\u1FE0-\\u1FE7\\u1FF2-"
                                        "\\u1FF4\\u1FF6-\\u1FF7\\u210E-\\u210F\\u213C-\\u213D\\u2146-\\u2149"
                                        "\\u2C30-\\u2C5E\\u2C65-\\u2C66\\u2C73-\\u2C74\\u2C76-\\u2C7B\\u2CE3-"
                                        "\\u2CE4\\u2D00-\\u2D25\\uA72F-\\uA731\\uA771-\\uA778\\uA793-\\uA795"
                                        "\\uAB30-\\uAB5A\\uAB60-\\uAB67\\uAB70-\\uABBF\\uFB00-\\uFB06\\uFB13-"
                                        "\\uFB17\\uFF41-\\uFF5A\\u{10428}-\\u{1044F}\\u{104D8}-\\u{104FB}"
                                        "\\u{10CC0}-\\u{10CF2}\\u{118C0}-\\u{118DF}\\u{16E60}-\\u{16E7F}"
                                        "\\u{1D41A}-\\u{1D433}\\u{1D44E}-\\u{1D454}\\u{1D456}-\\u{1D467}"
                                        "\\u{1D482}-\\u{1D49B}\\u{1D4B6}-\\u{1D4B9}\\u{1D4BD}-\\u{1D4C3}"
                                        "\\u{1D4C5}-\\u{1D4CF}\\u{1D4EA}-\\u{1D503}\\u{1D51E}-\\u{1D537}"
                                        "\\u{1D552}-\\u{1D56B}\\u{1D586}-\\u{1D59F}\\u{1D5BA}-\\u{1D5D3}"
                                        "\\u{1D5EE}-\\u{1D607}\\u{1D622}-\\u{1D63B}\\u{1D656}-\\u{1D66F}"
                                        "\\u{1D68A}-\\u{1D6A5}\\u{1D6C2}-\\u{1D6DA}\\u{1D6DC}-\\u{1D6E1}"
                                        "\\u{1D6FC}-\\u{1D714}\\u{1D716}-\\u{1D71B}\\u{1D736}-\\u{1D74E}"
                                        "\\u{1D750}-\\u{1D755}\\u{1D770}-\\u{1D788}\\u{1D78A}-\\u{1D78F}"
                                        "\\u{1D7AA}-\\u{1D7C2}\\u{1D7C4}-\\u{1D7C9}\\u{1E922}-\\u{1E943}";

const char _uax_db_Letter[] = "\\u00AA\\u00B5\\u00BA\\u02EC\\u02EE\\u037F"
                              "\\u0386\\u038C\\u0559\\u06D5\\u06FF\\u0710\\u07B1\\u07FA\\u081A"
                              "\\u0824\\u0828\\u093D\\u0950\\u09B2\\u09BD\\u09CE\\u09FC\\u0A5E"
                              "\\u0ABD\\u0AD0\\u0AF9\\u0B3D\\u0B71\\u0B83\\u0B9C\\u0BD0\\u0C3D"
                              "\\u0C80\\u0CBD\\u0CDE\\u0D3D\\u0D4E\\u0DBD\\u0E84\\u0EA5\\u0EBD"
                              "\\u0EC6\\u0F00\\u103F\\u1061\\u108E\\u10C7\\u10CD\\u1258\\u12C0"
                              "\\u17D7\\u17DC\\u18AA\\u1AA7\\u1CFA\\u1F59\\u1F5B\\u1F5D\\u1FBE"
                              "\\u2071\\u207F\\u2102\\u2107\\u2115\\u2124\\u2126\\u2128\\u214E"
                              "\\u2D27\\u2D2D\\u2D6F\\u2E2F\\uA8FB\\uA9CF\\uAA7A\\uAAB1\\uAAC0"
                              "\\uAAC2\\uFB1D\\uFB3E\\u{10808}\\u{1083C}\\u{10A00}\\u{10F27}"
                              "\\u{11144}\\u{11176}\\u{111DA}\\u{111DC}\\u{11288}\\u{1133D}"
                              "\\u{11350}\\u{1145F}\\u{114C7}\\u{11644}\\u{116B8}\\u{118FF}"
                              "\\u{119E1}\\u{119E3}\\u{11A00}\\u{11A3A}\\u{11A50}\\u{11A9D}"
                              "\\u{11C40}\\u{11D46}\\u{11D98}\\u{16F50}\\u{16FE3}\\u{1D4A2}"
                              "\\u{1D4BB}\\u{1D546}\\u{1E14E}\\u{1E94B}\\u{1EE24}\\u{1EE27}"
                              "\\u{1EE39}\\u{1EE3B}\\u{1EE42}\\u{1EE47}\\u{1EE49}\\u{1EE4B}"
                              "\\u{1EE54}\\u{1EE57}\\u{1EE59}\\u{1EE5B}\\u{1EE5D}\\u{1EE5F}"
                              "\\u{1EE64}\\u{1EE7E}\\u0041-\\u005A\\u0061-\\u007A\\u00C0-\\u00D6"
                              "\\u00D8-\\u00F6\\u00F8-\\u02C1\\u02C6-\\u02D1\\u02E0-\\u02E4\\u0370-"
                              "\\u0374\\u0376-\\u0377\\u037A-\\u037D\\u0388-\\u038A\\u038E-\\u03A1"
                              "\\u03A3-\\u03F5\\u03F7-\\u0481\\u048A-\\u052F\\u0531-\\u0556\\u0560-"
                              "\\u0588\\u05D0-\\u05EA\\u05EF-\\u05F2\\u0620-\\u064A\\u066E-\\u066F"
                              "\\u0671-\\u06D3\\u06E5-\\u06E6\\u06EE-\\u06EF\\u06FA-\\u06FC\\u0712-"
                              "\\u072F\\u074D-\\u07A5\\u07CA-\\u07EA\\u07F4-\\u07F5\\u0800-\\u0815"
                              "\\u0840-\\u0858\\u0860-\\u086A\\u08A0-\\u08B4\\u08B6-\\u08BD\\u0904-"
                              "\\u0939\\u0958-\\u0961\\u0971-\\u0980\\u0985-\\u098C\\u098F-\\u0990"
                              "\\u0993-\\u09A8\\u09AA-\\u09B0\\u09B6-\\u09B9\\u09DC-\\u09DD\\u09DF-"
                              "\\u09E1\\u09F0-\\u09F1\\u0A05-\\u0A0A\\u0A0F-\\u0A10\\u0A13-\\u0A28"
                              "\\u0A2A-\\u0A30\\u0A32-\\u0A33\\u0A35-\\u0A36\\u0A38-\\u0A39\\u0A59-"
                              "\\u0A5C\\u0A72-\\u0A74\\u0A85-\\u0A8D\\u0A8F-\\u0A91\\u0A93-\\u0AA8"
                              "\\u0AAA-\\u0AB0\\u0AB2-\\u0AB3\\u0AB5-\\u0AB9\\u0AE0-\\u0AE1\\u0B05-"
                              "\\u0B0C\\u0B0F-\\u0B10\\u0B13-\\u0B28\\u0B2A-\\u0B30\\u0B32-\\u0B33"
                              "\\u0B35-\\u0B39\\u0B5C-\\u0B5D\\u0B5F-\\u0B61\\u0B85-\\u0B8A\\u0B8E-"
                              "\\u0B90\\u0B92-\\u0B95\\u0B99-\\u0B9A\\u0B9E-\\u0B9F\\u0BA3-\\u0BA4"
                              "\\u0BA8-\\u0BAA\\u0BAE-\\u0BB9\\u0C05-\\u0C0C\\u0C0E-\\u0C10\\u0C12-"
                              "\\u0C28\\u0C2A-\\u0C39\\u0C58-\\u0C5A\\u0C60-\\u0C61\\u0C85-\\u0C8C"
                              "\\u0C8E-\\u0C90\\u0C92-\\u0CA8\\u0CAA-\\u0CB3\\u0CB5-\\u0CB9\\u0CE0-"
                              "\\u0CE1\\u0CF1-\\u0CF2\\u0D05-\\u0D0C\\u0D0E-\\u0D10\\u0D12-\\u0D3A"
                              "\\u0D54-\\u0D56\\u0D5F-\\u0D61\\u0D7A-\\u0D7F\\u0D85-\\u0D96\\u0D9A-"
                              "\\u0DB1\\u0DB3-\\u0DBB\\u0DC0-\\u0DC6\\u0E01-\\u0E30\\u0E32-\\u0E33"
                              "\\u0E40-\\u0E46\\u0E81-\\u0E82\\u0E86-\\u0E8A\\u0E8C-\\u0EA3\\u0EA7-"
                              "\\u0EB0\\u0EB2-\\u0EB3\\u0EC0-\\u0EC4\\u0EDC-\\u0EDF\\u0F40-\\u0F47"
                              "\\u0F49-\\u0F6C\\u0F88-\\u0F8C\\u1000-\\u102A\\u1050-\\u1055\\u105A-"
                              "\\u105D\\u1065-\\u1066\\u106E-\\u1070\\u1075-\\u1081\\u10A0-\\u10C5"
                              "\\u10D0-\\u10FA\\u10FC-\\u1248\\u124A-\\u124D\\u1250-\\u1256\\u125A-"
                              "\\u125D\\u1260-\\u1288\\u128A-\\u128D\\u1290-\\u12B0\\u12B2-\\u12B5"
                              "\\u12B8-\\u12BE\\u12C2-\\u12C5\\u12C8-\\u12D6\\u12D8-\\u1310\\u1312-"
                              "\\u1315\\u1318-\\u135A\\u1380-\\u138F\\u13A0-\\u13F5\\u13F8-\\u13FD"
                              "\\u1401-\\u166C\\u166F-\\u167F\\u1681-\\u169A\\u16A0-\\u16EA\\u16F1-"
                              "\\u16F8\\u1700-\\u170C\\u170E-\\u1711\\u1720-\\u1731\\u1740-\\u1751"
                              "\\u1760-\\u176C\\u176E-\\u1770\\u1780-\\u17B3\\u1820-\\u1878\\u1880-"
                              "\\u1884\\u1887-\\u18A8\\u18B0-\\u18F5\\u1900-\\u191E\\u1950-\\u196D"
                              "\\u1970-\\u1974\\u1980-\\u19AB\\u19B0-\\u19C9\\u1A00-\\u1A16\\u1A20-"
                              "\\u1A54\\u1B05-\\u1B33\\u1B45-\\u1B4B\\u1B83-\\u1BA0\\u1BAE-\\u1BAF"
                              "\\u1BBA-\\u1BE5\\u1C00-\\u1C23\\u1C4D-\\u1C4F\\u1C5A-\\u1C7D\\u1C80-"
                              "\\u1C88\\u1C90-\\u1CBA\\u1CBD-\\u1CBF\\u1CE9-\\u1CEC\\u1CEE-\\u1CF3"
                              "\\u1CF5-\\u1CF6\\u1D00-\\u1DBF\\u1E00-\\u1F15\\u1F18-\\u1F1D\\u1F20-"
                              "\\u1F45\\u1F48-\\u1F4D\\u1F50-\\u1F57\\u1F5F-\\u1F7D\\u1F80-\\u1FB4"
                              "\\u1FB6-\\u1FBC\\u1FC2-\\u1FC4\\u1FC6-\\u1FCC\\u1FD0-\\u1FD3\\u1FD6-"
                              "\\u1FDB\\u1FE0-\\u1FEC\\u1FF2-\\u1FF4\\u1FF6-\\u1FFC\\u2090-\\u209C"
                              "\\u210A-\\u2113\\u2119-\\u211D\\u212A-\\u212D\\u212F-\\u2139\\u213C-"
                              "\\u213F\\u2145-\\u2149\\u2183-\\u2184\\u2C00-\\u2C2E\\u2C30-\\u2C5E"
                              "\\u2C60-\\u2CE4\\u2CEB-\\u2CEE\\u2CF2-\\u2CF3\\u2D00-\\u2D25\\u2D30-"
                              "\\u2D67\\u2D80-\\u2D96\\u2DA0-\\u2DA6\\u2DA8-\\u2DAE\\u2DB0-\\u2DB6"
                              "\\u2DB8-\\u2DBE\\u2DC0-\\u2DC6\\u2DC8-\\u2DCE\\u2DD0-\\u2DD6\\u2DD8-"
                              "\\u2DDE\\u3005-\\u3006\\u3031-\\u3035\\u303B-\\u303C\\u3041-\\u3096"
                              "\\u309D-\\u309F\\u30A1-\\u30FA\\u30FC-\\u30FF\\u3105-\\u312F\\u3131-"
                              "\\u318E\\u31A0-\\u31BA\\u31F0-\\u31FF\\u3400-\\u4DB5\\u4E00-\\u9FEF"
                              "\\uA000-\\uA48C\\uA4D0-\\uA4FD\\uA500-\\uA60C\\uA610-\\uA61F\\uA62A-"
                              "\\uA62B\\uA640-\\uA66E\\uA67F-\\uA69D\\uA6A0-\\uA6E5\\uA717-\\uA71F"
                              "\\uA722-\\uA788\\uA78B-\\uA7BF\\uA7C2-\\uA7C6\\uA7F7-\\uA801\\uA803-"
                              "\\uA805\\uA807-\\uA80A\\uA80C-\\uA822\\uA840-\\uA873\\uA882-\\uA8B3"
                              "\\uA8F2-\\uA8F7\\uA8FD-\\uA8FE\\uA90A-\\uA925\\uA930-\\uA946\\uA960-"
                              "\\uA97C\\uA984-\\uA9B2\\uA9E0-\\uA9E4\\uA9E6-\\uA9EF\\uA9FA-\\uA9FE"
                              "\\uAA00-\\uAA28\\uAA40-\\uAA42\\uAA44-\\uAA4B\\uAA60-\\uAA76\\uAA7E-"
                              "\\uAAAF\\uAAB5-\\uAAB6\\uAAB9-\\uAABD\\uAADB-\\uAADD\\uAAE0-\\uAAEA"
                              "\\uAAF2-\\uAAF4\\uAB01-\\uAB06\\uAB09-\\uAB0E\\uAB11-\\uAB16\\uAB20-"
                              "\\uAB26\\uAB28-\\uAB2E\\uAB30-\\uAB5A\\uAB5C-\\uAB67\\uAB70-\\uABE2"
                              "\\uAC00-\\uD7A3\\uD7B0-\\uD7C6\\uD7CB-\\uD7FB\\uF900-\\uFA6D\\uFA70-"
                              "\\uFAD9\\uFB00-\\uFB06\\uFB13-\\uFB17\\uFB1F-\\uFB28\\uFB2A-\\uFB36"
                              "\\uFB38-\\uFB3C\\uFB40-\\uFB41\\uFB43-\\uFB44\\uFB46-\\uFBB1\\uFBD3-"
                              "\\uFD3D\\uFD50-\\uFD8F\\uFD92-\\uFDC7\\uFDF0-\\uFDFB\\uFE70-\\uFE74"
                              "\\uFE76-\\uFEFC\\uFF21-\\uFF3A\\uFF41-\\uFF5A\\uFF66-\\uFFBE\\uFFC2-"
                              "\\uFFC7\\uFFCA-\\uFFCF\\uFFD2-\\uFFD7\\uFFDA-\\uFFDC\\u{10000}-"
                              "\\u{1000B}\\u{1000D}-\\u{10026}\\u{10028}-\\u{1003A}\\u{1003C}-"
                              "\\u{1003D}\\u{1003F}-\\u{1004D}\\u{10050}-\\u{1005D}\\u{10080}-"
                              "\\u{100FA}\\u{10280}-\\u{1029C}\\u{102A0}-\\u{102D0}\\u{10300}-"
                              "\\u{1031F}\\u{1032D}-\\u{10340}\\u{10342}-\\u{10349}\\u{10350}-"
                              "\\u{10375}\\u{10380}-\\u{1039D}\\u{103A0}-\\u{103C3}\\u{103C8}-"
                              "\\u{103CF}\\u{10400}-\\u{1049D}\\u{104B0}-\\u{104D3}\\u{104D8}-"
                              "\\u{104FB}\\u{10500}-\\u{10527}\\u{10530}-\\u{10563}\\u{10600}-"
                              "\\u{10736}\\u{10740}-\\u{10755}\\u{10760}-\\u{10767}\\u{10800}-"
                              "\\u{10805}\\u{1080A}-\\u{10835}\\u{10837}-\\u{10838}\\u{1083F}-"
                              "\\u{10855}\\u{10860}-\\u{10876}\\u{10880}-\\u{1089E}\\u{108E0}-"
                              "\\u{108F2}\\u{108F4}-\\u{108F5}\\u{10900}-\\u{10915}\\u{10920}-"
                              "\\u{10939}\\u{10980}-\\u{109B7}\\u{109BE}-\\u{109BF}\\u{10A10}-"
                              "\\u{10A13}\\u{10A15}-\\u{10A17}\\u{10A19}-\\u{10A35}\\u{10A60}-"
                              "\\u{10A7C}\\u{10A80}-\\u{10A9C}\\u{10AC0}-\\u{10AC7}\\u{10AC9}-"
                              "\\u{10AE4}\\u{10B00}-\\u{10B35}\\u{10B40}-\\u{10B55}\\u{10B60}-"
                              "\\u{10B72}\\u{10B80}-\\u{10B91}\\u{10C00}-\\u{10C48}\\u{10C80}-"
                              "\\u{10CB2}\\u{10CC0}-\\u{10CF2}\\u{10D00}-\\u{10D23}\\u{10F00}-"
                              "\\u{10F1C}\\u{10F30}-\\u{10F45}\\u{10FE0}-\\u{10FF6}\\u{11003}-"
                              "\\u{11037}\\u{11083}-\\u{110AF}\\u{110D0}-\\u{110E8}\\u{11103}-"
                              "\\u{11126}\\u{11150}-\\u{11172}\\u{11183}-\\u{111B2}\\u{111C1}-"
                              "\\u{111C4}\\u{11200}-\\u{11211}\\u{11213}-\\u{1122B}\\u{11280}-"
                              "\\u{11286}\\u{1128A}-\\u{1128D}\\u{1128F}-\\u{1129D}\\u{1129F}-"
                              "\\u{112A8}\\u{112B0}-\\u{112DE}\\u{11305}-\\u{1130C}\\u{1130F}-"
                              "\\u{11310}\\u{11313}-\\u{11328}\\u{1132A}-\\u{11330}\\u{11332}-"
                              "\\u{11333}\\u{11335}-\\u{11339}\\u{1135D}-\\u{11361}\\u{11400}-"
                              "\\u{11434}\\u{11447}-\\u{1144A}\\u{11480}-\\u{114AF}\\u{114C4}-"
                              "\\u{114C5}\\u{11580}-\\u{115AE}\\u{115D8}-\\u{115DB}\\u{11600}-"
                              "\\u{1162F}\\u{11680}-\\u{116AA}\\u{11700}-\\u{1171A}\\u{11800}-"
                              "\\u{1182B}\\u{118A0}-\\u{118DF}\\u{119A0}-\\u{119A7}\\u{119AA}-"
                              "\\u{119D0}\\u{11A0B}-\\u{11A32}\\u{11A5C}-\\u{11A89}\\u{11AC0}-"
                              "\\u{11AF8}\\u{11C00}-\\u{11C08}\\u{11C0A}-\\u{11C2E}\\u{11C72}-"
                              "\\u{11C8F}\\u{11D00}-\\u{11D06}\\u{11D08}-\\u{11D09}\\u{11D0B}-"
                              "\\u{11D30}\\u{11D60}-\\u{11D65}\\u{11D67}-\\u{11D68}\\u{11D6A}-"
                              "\\u{11D89}\\u{11EE0}-\\u{11EF2}\\u{12000}-\\u{12399}\\u{12480}-"
                              "\\u{12543}\\u{13000}-\\u{1342E}\\u{14400}-\\u{14646}\\u{16800}-"
                              "\\u{16A38}\\u{16A40}-\\u{16A5E}\\u{16AD0}-\\u{16AED}\\u{16B00}-"
                              "\\u{16B2F}\\u{16B40}-\\u{16B43}\\u{16B63}-\\u{16B77}\\u{16B7D}-"
                              "\\u{16B8F}\\u{16E40}-\\u{16E7F}\\u{16F00}-\\u{16F4A}\\u{16F93}-"
                              "\\u{16F9F}\\u{16FE0}-\\u{16FE1}\\u{17000}-\\u{187F7}\\u{18800}-"
                              "\\u{18AF2}\\u{1B000}-\\u{1B11E}\\u{1B150}-\\u{1B152}\\u{1B164}-"
                              "\\u{1B167}\\u{1B170}-\\u{1B2FB}\\u{1BC00}-\\u{1BC6A}\\u{1BC70}-"
                              "\\u{1BC7C}\\u{1BC80}-\\u{1BC88}\\u{1BC90}-\\u{1BC99}\\u{1D400}-"
                              "\\u{1D454}\\u{1D456}-\\u{1D49C}\\u{1D49E}-\\u{1D49F}\\u{1D4A5}-"
                              "\\u{1D4A6}\\u{1D4A9}-\\u{1D4AC}\\u{1D4AE}-\\u{1D4B9}\\u{1D4BD}-"
                              "\\u{1D4C3}\\u{1D4C5}-\\u{1D505}\\u{1D507}-\\u{1D50A}\\u{1D50D}-"
                              "\\u{1D514}\\u{1D516}-\\u{1D51C}\\u{1D51E}-\\u{1D539}\\u{1D53B}-"
                              "\\u{1D53E}\\u{1D540}-\\u{1D544}\\u{1D54A}-\\u{1D550}\\u{1D552}-"
                              "\\u{1D6A5}\\u{1D6A8}-\\u{1D6C0}\\u{1D6C2}-\\u{1D6DA}\\u{1D6DC}-"
                              "\\u{1D6FA}\\u{1D6FC}-\\u{1D714}\\u{1D716}-\\u{1D734}\\u{1D736}-"
                              "\\u{1D74E}\\u{1D750}-\\u{1D76E}\\u{1D770}-\\u{1D788}\\u{1D78A}-"
                              "\\u{1D7A8}\\u{1D7AA}-\\u{1D7C2}\\u{1D7C4}-\\u{1D7CB}\\u{1E100}-"
                              "\\u{1E12C}\\u{1E137}-\\u{1E13D}\\u{1E2C0}-\\u{1E2EB}\\u{1E800}-"
                              "\\u{1E8C4}\\u{1E900}-\\u{1E943}\\u{1EE00}-\\u{1EE03}\\u{1EE05}-"
                              "\\u{1EE1F}\\u{1EE21}-\\u{1EE22}\\u{1EE29}-\\u{1EE32}\\u{1EE34}-"
                              "\\u{1EE37}\\u{1EE4D}-\\u{1EE4F}\\u{1EE51}-\\u{1EE52}\\u{1EE61}-"
                              "\\u{1EE62}\\u{1EE67}-\\u{1EE6A}\\u{1EE6C}-\\u{1EE72}\\u{1EE74}-"
                              "\\u{1EE77}\\u{1EE79}-\\u{1EE7C}\\u{1EE80}-\\u{1EE89}\\u{1EE8B}-"
                              "\\u{1EE9B}\\u{1EEA1}-\\u{1EEA3}\\u{1EEA5}-\\u{1EEA9}\\u{1EEAB}-"
                              "\\u{1EEBB}\\u{20000}-\\u{2A6D6}\\u{2A700}-\\u{2B734}\\u{2B740}-"
                              "\\u{2B81D}\\u{2B820}-\\u{2CEA1}\\u{2CEB0}-\\u{2EBE0}\\u{2F800}-"
                              "\\u{2FA1D}";

#endif // MOJO_REGEX_UNICODE

#endif // MOJO_REGEX_COMPILE_IMPLEMENTATION

#endif // MOJO_REGEX_IMPLEMENTATION

#endif // _MOJO_REGEX_HEADER_
