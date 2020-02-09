#ifndef _MOJO_REGEX_HEADER_
#define _MOJO_REGEX_HEADER_

/*///////////////////////////////////////////////////////////////////////////

Regex VM Bytecode (v9 - in development)

    Each operation is encoded into a single 32 bit int value:

       +-------------------------------+-------------------------------+
       |                    1          '        2                   3  |
       |0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5'6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1|
       +-------------------------------+-------------------------------+
       |                               '1                              |
       |0 1 2 3 4 5 6 7 8 9 A B C D E F'0 1 2 3 4 5 6 7 8 9 A B C D E F|
       +-------+-----------------------+---+---------------------------+
       | 0...3 |          4 ... 17         |         18 ... 31         |
       +-------+---------------------------+---------------------------+
       |4bit op|    14 bits (operand A)    |    14 bits (operand B)    |
       +-------+---------------------------+---------------------------+

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
       |                    1          '        2                   3  |
       |0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5'6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1|
       +-------------------------------+-------------------------------+
       |                               '1                              |
       |0 1 2 3 4 5 6 7 8 9 A B C D E F'0 1 2 3 4 5 6 7 8 9 A B C D E F|
       +-------+---------------+-------+-+-+---------------------------+
       | 0...3 |    4 ... 11   |         | |         18 ... 31         |
       +-------+---------------+---------+-+---------------------------+
       |4bit op|     char      | unused  |I|   (opt) program counter   |
       +-------+---------------+---------+-+---------------------------+

        Matches the character specified. If the inverse flag (I) is set, the
        logic is inverted, and the instruction matches any character EXCEPT the
        character specified. Operand B is optional, and, if specified, is the
        program counter to jmp to in the case of no match (effectively, this
        integrates a reduced split either/or case). If the value is 0x3FFF,
        then the instruction reverts to default no match behaviour. Note that
        this instruction matches a single BYTE. For utf8 literals, use the
        eTokenUtf8Literal.

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
       |                    1          '        2                   3  |
       |0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5'6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1|
       +-------------------------------+-------------------------------+
       |                               '1                              |
       |0 1 2 3 4 5 6 7 8 9 A B C D E F'0 1 2 3 4 5 6 7 8 9 A B C D E F|
       +-------+-----------------------+-+-+-----------+-----------+---+
       | 0...3 |          4 ... 16       | | 18 ... 23 | 24 ... 29 |   |
       +-------+-------------------------+-+-----------+-----------+---+
       |4bit op|     utf8 class index    |I|  midlow   |  midhigh  |Enc|
       +-------+-------------------------+-+-----------+-----------+---+

        This instruction represents the low bytes of a utf8 encoding (for 2, 3,
        and 4 byte encodings). For single byte encodings, a standard char class
        is used. num is the number of low bytes to match, with the lowest
        being the class bitmap range specified in operand A, and the midlow,
        and midhigh bytes stored in operand B. The low byte encoding is 6 bits
        per byte, with the 2 bit 0x80 prefix removed. If the inverse bit (I) is
        set, then the match logic is inverted for all bytes present, including
        the class bitmap (it is NOT pre-inverted).

        00 - 2 byte encoding (midlow, low)
        01 - 3 byte encoding (midhigh, midlow, low)
        10 - 4 byte encoding, seperate leading high byte (midhigh, midlow, low)
        11 - unused

        Rationale: prior to this instruction, a UTF8 class was constructed
        using multiple eTokenSplits and eTokenCharLiterals. This compound
        instruction can help reduce instruction count. Trivialized example:

        AB,AC,DE

        split -> D -> [E] -------+-+--> match
          |                      ^ ^
          +-> A -> split -> [B] -+ |               9 instructions
                     |             |    (splits incur a jmp penalty to converge)
                     +-> [C] ------+

        split -> D[E] -------+-+--> match
          |                  ^ ^
          +-> split -> A[B] -+ |                  7 instructions
                |              |
                +-> A[C] ------+

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
        from eTokenCharAny in that it both always matches newline, and always
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
       |                    1          '        2                   3  |
       |0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5'6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1|
       +-------------------------------+-------------------------------+
       |                               '1                              |
       |0 1 2 3 4 5 6 7 8 9 A B C D E F'0 1 2 3 4 5 6 7 8 9 A B C D E F|
       +-------+-----------------------+-----------------+-----------+-+
       | 0...3 |                 4 ... 24                |           | |
       +-------+-----------------------------------------+-----------+-+
       |4bit op|                 codepoint               |  unused   |I|
       +-------+-----------------------------------------+-----------+-+

        This instruction explicitly matches the encoded utf8 codepoint. The
        inverse bit (I) will match any character other than the one specified.

    Note about eTokenRange:

        +-------------------------------+-------------------------------+
        |                    1          '        2                   3  |
        |0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5'6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1|
        +-------------------------------+-------------------------------+
        |                               '1                              |
        |0 1 2 3 4 5 6 7 8 9 A B C D E F'0 1 2 3 4 5 6 7 8 9 A B C D E F|
        +-------+-----------------------+---+-------------+-------------+
        | 0...3 |          4 ... 17         |  18 ... 24  |  25 ... 31  |
        +-------+---------------------------+-------------+-------------+
        |4bit op|      program counter      |  Min value  |  Max value  |
        +-------+---------------------------+---------------------------+

///////////////////////////////////////////////////////////////////////////*/

// Build the main test driver stub
#define MOJO_REGEX_TEST_MAIN

// Build the regex compilation support features of the API
#define MOJO_REGEX_COMPILE_IMPLEMENTATION

// Build the VM source generation features of the API
#define MOJO_REGEX_VM_SOURCE_GENERATION

// Include debug diagnostic support in the generated VM image
#define MOJO_REGEX_VM_DEBUG

// Build the regex evaluation features of the API
#define MOJO_REGEX_EVALUATE_IMPLEMENTATION

// Include unicode (explicit UTF8) support
#define MOJO_REGEX_UNICODE

// Cull redundant jmp instructions when building DFAs
//#define MOJO_REGEX_EXPERIMENTAL_CULL_JMP

// Build the implementation (if not defined, simply declares symbols)
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

#if defined(MOJO_REGEX_COMPILE_IMPLEMENTATION) || defined(MOJO_REGEX_VM_DEBUG) || defined(MOJO_REGEX_EVALUATE_IMPLEMENTATION)
// Build features relevant to compilation and evaluation
#   define MOJO_REGEX_COMMON_IMPLEMENTATION
#endif

#if defined(MOJO_REGEX_COMPILE_IMPLEMENTATION) || defined(MOJO_REGEX_VM_DEBUG)
// Build features relevant to compilation, and debug diagnostics
#   define MOJO_REGEX_DEBUG_IMPLEMENTATION
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
    int max_call_depth;             // maximum number of nested subroutine calls
                                    // represents the evaluation thread call stack size
    int max_nested_ranges;          // maximum number of simultaneously nested range quantifiers
                                    // represents the evaluation thread range stack size
    int sub_name_tbl_size;          // always include, for VM compatibility even when not compiled in
#ifdef MOJO_REGEX_VM_DEBUG
    regex_vm_sub_names_t *sub_name_table;
#endif // MOJO_REGEX_VM_DEBUG
};

typedef struct regex_match_s regex_match_t;
struct regex_match_s {
    const char *text;
    int len;
    const char *pos;
    regex_vm_t *vm;
    const char **subexprs;
    const char ***compound;
    const char _buffer[0];
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
    eCompileMissingPattern,
    eCompileInternalError
} eRegexCompileStatus_t;

typedef enum {
    ePhaseTokenize,
    ePhaseNFAGraph,
    ePhaseVMGen,
    ePhaseComplete
} eRegexCompilePhase_t;

#ifndef DEF_REGEX_UNICODE_CLASS
#define DEF_REGEX_UNICODE_CLASS

typedef struct regex_unicode_class_s regex_unicode_class_t;
struct regex_unicode_class_s {
    const char *alias;
    const char *name;
    const char *class_string;
    regex_unicode_class_t *next;
};

#endif // DEF_REGEX_UNICODE_CLASS

#define RE_FLAG_DOT_ALL     0x1u

#define META_CLASS_DIGITS_PATTERN           "0-9"
#define META_CLASS_INV_DIGITS_PATTERN       "^0-9"
#define META_CLASS_WHITESPACE_PATTERN       " \\t\\f\\v\\r"
#define META_CLASS_INV_WHITESPACE_PATTERN   "^ \\t\\f\\v\\r"
#define META_CLASS_WORD_PATTERN             "a-zA-Z0-9_"
#define META_CLASS_INV_WORD_PATTERN         "^a-zA-Z0-9_"

#define DEF_UNICODE_GLYPH_SUB_PREFIX    "meta:"
#define DEF_UNICODE_GLYPH_SUB_NAME      "glyph"
#define DEF_UTF8_CLASS_SUB_PREFIX       "class:"

// Adds a unicode property class to the global property class table. Returns 1
// on success, or 0 if the "alias" or "name" have already been registered.
int regexRegUnicodeCharClassAdd(regex_unicode_class_t *utf8class);

// Adds an array of unicode property classes to the gloval property class
// table. Note that the "next" pointer will be overwritten for each entry, the
// global table will reference the entry memory (do NOT deallocate until you've
// removed the class from the global table), and the function will read entries
// until it reaches one with BOTH "alias" AND "name" equal to NULL. This
// function wraps regexRegUnicodeChasClassAdd(), and short circuits on the first
// failure.
int regexRegUnicodeCharClassAddTable(regex_unicode_class_t *utf8class);

// Removes the requested unicode property class from the global property class
// table. classId is checked against both "alias" and "name", and removes the
// first match found.
void regexRegUnicodeCharClassRemove(const char *classId);

// UTF8 handling and support functions //////////////////////////////////////

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

// Returns the number of bytes required to utf8 encode the codepoint that
// begins the utf8 encoded sequence starting with byte 'c'
int _parseUtf8EncodedHighByte(char c);

// Parses a utf8 encoded string sequence, and returns the codepoint value.
// Returns -1 if the str is not a multibyte utf8 encoded sequence.
int _parseUtf8DecodeSequence(const char *str);

// Token flags //////////////////////////////////////////////////////////////

#define RE_TOK_FLAG_CASE_INS                0x10u   // eTokenSubExpr
#define RE_TOK_FLAG_INVERT                  0x01u   // eTokenCharLiteral, eTokenUtf8Class, eTokenUtf8Literal
#define RE_TOK_FLAG_DOT_ALL                 0x01u   // eTokenCharAny
#define RE_TOK_FLAG_END_OF_INPUT            0x01u   // eTokenMatch
#define RE_TOK_FLAG_COMPOUND                0x01u   // eTokenSubExprStart, eTokenSave
#define RE_TOK_FLAG_NO_CAPTURE              0x02u   // eTokenSubExprStart
#define RE_TOK_FLAG_NAMED                   0x04u   // eTokenSubExprStart
#define RE_TOK_FLAG_SUBROUTINE              0x08u   // eTokenSubExprStart

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
#define RE_VM_INSTR_BIT(bit)    (1u << (bit))

#define RE_VM_FLAG_ASSERT_START_OF_LINE 0x0u
#define RE_VM_FLAG_ASSERT_END_OF_LINE   0x1u
#define RE_VM_FLAG_ASSERT_START_OF_WORD 0x2u
#define RE_VM_FLAG_ASSERT_END_OF_WORD   0x3u

#define RE_VM_FLAG_CHAR_INVERT          RE_VM_INSTR_BIT(17u)
#define RE_VM_FLAG_CHARANY_DOTALL       RE_VM_INSTR_BIT(4u)
#define RE_VM_FLAG_MATCH_END_OF_INPUT   RE_VM_INSTR_BIT(4u)
#define RE_VM_FLAG_SAVE_COMPOUND        RE_VM_INSTR_BIT(18u)
#define RE_VM_FLAG_UTF8_CLASS_INVERT    RE_VM_INSTR_BIT(17u)
#define RE_VM_FLAG_UTF8_LITERAL_INVERT  RE_VM_INSTR_BIT(31u)

#define RE_VM_OPCODE_MASK   0xFu
#define RE_VM_OP_A_MASK     0xFFFC0000u
#define RE_VM_OP_A_OFFSET   18u
#define RE_VM_OP_B_MASK     0x3FFF0u
#define RE_VM_OP_B_OFFSET   4u
#define RE_VM_OP_A_FROM_INSTR(instr)    ((((unsigned)instr) & RE_VM_OP_A_MASK) >> RE_VM_OP_A_OFFSET)
#define RE_VM_OP_B_FROM_INSTR(instr)    ((((unsigned)instr) & RE_VM_OP_B_MASK) >> RE_VM_OP_B_OFFSET)

// VM instruction operand/flag/value setter/getter macros ///////////////////

// eTokenCharLiteral
#define RE_VM_ENC_CHAR_TO_INSTR(c)          ((((unsigned)c) & 0xFFu) << 4u)
#define RE_VM_DEC_CHAR_FROM_INSTR(instr)    ((instr & 0xFF0u) >> 4u)
#define RE_VM_FLAG_CHAR_INVERT_TO_INSTR()   RE_VM_FLAG_CHAR_INVERT
#define RE_VM_FLAG_CHAR_INVERT_FROM_INSTR(instr) (instr & RE_VM_FLAG_CHAR_INVERT)

// eTokenCharAny
#define RE_VM_FLAG_DOTALL_TO_INSTR()            RE_VM_FLAG_CHARANY_DOTALL
#define RE_VM_FLAG_DOTALL_FROM_INSTR(instr)     (instr & RE_VM_FLAG_CHARANY_DOTALL)

// eTokenMatch
#define RE_VM_FLAG_EOF_TO_INSTR()               RE_VM_FLAG_MATCH_END_OF_INPUT
#define RE_VM_FLAG_EOF_FROM_INSTR(instr)        (instr & RE_VM_FLAG_MATCH_END_OF_INPUT)

// eTokenSave
#define RE_VM_FLAG_COMPOUND_TO_INSTR()          RE_VM_FLAG_SAVE_COMPOUND
#define RE_VM_FLAG_COMPOUND_FROM_INSTR(instr)   (instr & RE_VM_FLAG_SAVE_COMPOUND)

// eTokenUtf8Class
#define RE_VM_ENC_UTF8_INDEX_TO_INSTR(idx)              ((((unsigned)idx) & 0x1FFFu) << 4u)
#define RE_VM_DEC_UTF8_INDEX_FROM_INSTR(instr)          ((instr & 0x1FFF0u) >> 4u)
#define RE_VM_FLAG_UTF8_CLASS_INVERT_TO_INSTR()         RE_VM_FLAG_UTF8_CLASS_INVERT
#define RE_VM_FLAG_UTF8_CLASS_INVERT_FROM_INSTR(instr)  (instr & RE_VM_FLAG_UTF8_CLASS_INVERT)
#define RE_VM_FLAG_UTF8_CLASS_ENC_TO_INSTR(enc)         ((((unsigned)enc) & 0x3u) << 30u)
#define RE_VM_FLAG_UTF8_CLASS_ENC_FROM_INSTR(enc)       ((instr & 0xC0000000u) >> 30u)
#define RE_VM_ENC_UTF8_MIDLOW_TO_INSTR(midlow)          ((((unsigned)midlow) & 0x3Fu) << 18u)
#define RE_VM_DEC_UTF8_MIDLOW_FROM_INSTR(instr)         ((instr & 0xFC0000u) >> 18u)
#define RE_VM_ENC_UTF8_MIDHIGH_TO_INSTR(midhigh)        ((((unsigned)midhigh) & 0x3Fu) << 24u)
#define RE_VM_DEC_UTF8_MIDHIGH_FROM_INSTR(instr)        ((instr & 0x3F000000u) >> 24u)

// eTokenUtf8Literal
#define RE_VM_ENC_UTF8_LITERAL_TO_INSTR(codepoint)          ((((unsigned)codepoint) & 0x10FFFFu) << 4u)
#define RE_VM_DEC_UTF8_LITERAL_FROM_INSTR(instr)            ((instr & 0x10FFFF0u) >> 4u)
#define RE_VM_FLAG_UTF8_LITERAL_INVERT_TO_INSTR()           RE_VM_FLAG_UTF8_LITERAL_INVERT
#define RE_VM_FLAG_UTF8_LITERAL_INVERT_FROM_INSTR(instr)    (instr & RE_VM_FLAG_UTF8_LITERAL_INVERT)

// eTokenRange
#define RE_VM_ENC_RANGE_MIN_TO_INSTR(m)         ((((unsigned)m) & 0x3Fu) << 20u)
#define RE_VM_DEC_RANGE_MIN_FROM_INSTR(instr)   (((instr & 0x3F00000u) >> 20u) & 0x3Fu)
#define RE_VM_ENC_RANGE_MAX_TO_INSTR(m)         ((((unsigned)m) & 0x3Fu) << 26u)
#define RE_VM_DEC_RANGE_MAX_FROM_INSTR(instr)   (((instr & 0xFC000000u) >> 26u) & 0x3Fu)


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
    union {
        int c; // eTokenCharLiteral, eTokenUtf8Literal
        struct {
            short mid_high; // eTokenUtf8Class
            short mid_low;  // eTokenUtf8Class
        };
        int group; // eTokenSubExprStart, eTokenSave
    };
    union {
        char *str; // eTokenStrLiteral
        unsigned int *bitmap; // eTokenCharClass, eTokenUtf8Class
        char *name; // eTokenSubExprStart
        int min; // eTokenRange
        int jump; // eTokenCharLiteral
    };
    union {
        int len; // eTokenStrLiteral - str length, to allow embedded \0 chars
        int max; // eTokenRange
        struct {
            unsigned short flags; // eTokenChar, eTokenCharAny, eTokenUtf8Class,
                                  // eTokenUtf8Literal, eTokenMatch,
                                  // eTokenSubExprStart, eTokenSave,
                                  // eTokenAssertion
            union {
                short sub_index; // eTokenCall, eTokenSubExprStart
                short encoding; // eTokenUtf8Class, eTokenUtf8Literal
            };
        };
    };
    int pc; // program counter
    regex_ptrlist_t *ptrlist;
    regex_token_t *out_a;
    regex_token_t *out_b;
    regex_token_t *next;
};

typedef enum {
    eReExprSuccess,
    eReExprOutOfMemory,
    eReExprEmpty,
    eReExprUnclosed,
    eReExprUnusedOperators,
    eReExprUnusedOperands,
    eReExprMissingOperand,
    eReExprMissingOpen,
    eReExprInvalidClose,
    eReExprInternalError
} eReExprStatus_t;

typedef struct regex_build_ctx_s regex_build_ctx_t;

typedef struct regex_expr_s regex_expr_t;
struct regex_expr_s {
    regex_build_ctx_t *context;
    regex_token_t *tokens;
    regex_token_t *operators;
    union {
        regex_expr_t *parent;
        regex_expr_t *next;
    };
};

typedef struct regex_pattern_s regex_pattern_t;
struct regex_pattern_s {
    const char *pattern;
    const char *base;
    int pos;
    int len;
    union {
        regex_pattern_t *parent;
        regex_pattern_t *next;
    };
};

typedef struct regex_sub_s regex_sub_t;
struct regex_sub_s {
    char *name;
    char *alias;
    int index;
    regex_token_t *expr;
    regex_sub_t *next;
};

typedef struct regex_utf8_range_s regex_utf8_range_t;
struct regex_utf8_range_s {
    int start;
    int end;
    regex_utf8_range_t *next;
};

struct regex_build_ctx_s {
    unsigned int flags;

    regex_token_t *token_pool;
    regex_ptrlist_t *ptrlist_pool;
    regex_pattern_t *pattern_pool;
    regex_expr_t  *expr_pool;
    regex_utf8_range_t *utf8_range_pool;

    int next_sub;
    regex_sub_t *subroutines;

    int group_count;

    regex_vm_t *vm;
};

typedef struct regex_build_s regex_build_t;
struct regex_build_s {
    regex_build_ctx_t *context;
    regex_expr_t *expr;
    regex_pattern_t *pattern;
    eRegexCompileStatus_t status;
};

#endif // MOJO_REGEX_COMPILE_IMPLEMENTATION
#ifdef MOJO_REGEX_EVALUATE_IMPLEMENTATION

typedef struct regex_compound_s regex_compound_t;
struct regex_compound_s {
    int subexpr;
    const char *start;
    const char *end;
    regex_compound_t *next;
};

#define RE_VM_THREAD_INVERT_UTF8_FLAG   0x100u
#define RE_VM_THREAD_INVERT_MASK_POS(pos)   (((unsigned)pos) & 0xFFu)
#define RE_VM_THREAD_INVERT_MASK_FLAG(pos)   (((unsigned)pos) & RE_VM_THREAD_INVERT_UTF8_FLAG)

#define RE_VM_THREAD_RANGE_PC_FROM_STACK(thread)            (thread->range_stack[thread->range_idx - 1] & 0xFFFFu)
#define RE_VM_THREAD_RANGE_PC_TO_STACK(thread,pc)           (thread->range_stack[thread->range_idx - 1] = (((unsigned)pc) & 0xFFFFu))
#define RE_VM_THREAD_RANGE_COUNT_FROM_STACK(thread)         ((thread->range_stack[thread->range_idx - 1] & 0xFFFF0000u) >> 16u)
#define RE_VM_THREAD_RANGE_COUNT_CLEAR_STACK(thread)        (thread->range_stack[thread->range_idx - 1] &= 0xFFFFu)
#define RE_VM_THREAD_RANGE_COUNT_SET_STACK(thread,count)    (thread->range_stack[thread->range_idx - 1] |= ((((unsigned)count) & 0xFFFFu) << 16u))
#define RE_VM_THREAD_RANGE_COUNT_TO_STACK(thread,count)     RE_VM_THREAD_RANGE_COUNT_CLEAR_STACK(thread); RE_VM_THREAD_RANGE_COUNT_SET_STACK(thread,count);

typedef struct regex_thread_s regex_thread_t;
struct regex_thread_s {
    unsigned int pc;
    int pos; // Tracks state in multi-char tokens (initialized to -1)
        // eTokenStringLiteral
        //     -1: this is the start of this token handling
        //     #: the char within the string that is being checked
        // eTokenUtf8Literal
        //     -1: this is the start of this token handling
        //     #*: encoded byte being checked
        //     *: if the match is inverted, 256 is added to flag a invert match
        //        (ie., a byte did NOT match, indicating the invert condition was met)
        // eTokenCharAny
        //     -1: this is an initial evalation. If unicode support is compiled
        //         in, and the character is the start of a utf8 encoded sequence,
        //         pos will be set to the number of encoded bytes.
        //     >0: number of expected bytes remaining in a valid utf8 sequence
    unsigned int *call_stack;
    int call_idx;
    unsigned *range_stack;
    int range_idx;
    int last_was_word;
    regex_thread_t *next;
    regex_compound_t *compound;
    const char **subexprs;
    char _buffer[0];
};

typedef struct regex_eval_s regex_eval_t;
struct regex_eval_s {
    const char *sp;
    int start_of_line;
    int end_of_line;
    int len;
    int pos;
    int anchored;
    int use_boundaries;
    regex_thread_t *thread;
    regex_thread_t *queue;
    regex_vm_t *vm;
    const char **subexprs;

    regex_compound_t *compound_pool;
    regex_thread_t *thread_pool;
#ifdef MOJO_REGEX_VM_DEBUG
    FILE *debug;
#endif // MOJO_REGEX_VM_DEBUG
};

typedef enum {
    eEvalStackOverrun = -3,
    eEvalInternalError = -2,
    eEvalOutOfMem = -1,
    eEvalNoMatch = 0,
    eEvalMatch = 1,
    eEvalContinue = 2
} eReEvalResult_t;

#endif // MOJO_REGEX_EVALUATE_IMPLEMENTATION
#ifdef MOJO_REGEX_COMPILE_IMPLEMENTATION

/////////////////////////////////////////////////////////////////////////////
// Regex token meta details
/////////////////////////////////////////////////////////////////////////////

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

// Emit token details (compilation only) ////////////////////////////////////

typedef void (*regexMetaTokenEmit_t)(FILE *fp, regex_build_ctx_t *build_ctx, regex_token_t *token);

#define RE_META_TOK_EMIT_N(token)   NULL
#define RE_META_TOK_EMIT_Y(token)   _reMetaTokEmit_ ## token
#define RE_META_TOK_EMIT(unique,token)  RE_META_TOK_EMIT_ ## unique(token)

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

// Apply operators to tokens in shunting yard implementation (compilation only)

typedef int (*regexMetaApplyOperator_t)(regex_expr_t *expr, regex_token_t *operand_a, regex_token_t *operand_b, regex_token_t *operator);

#define RE_META_IS_OPERATOR_N(token)    NULL
#define RE_META_IS_OPERATOR_Y(token)    _reMetaApplyOp_ ## token
#define RE_META_IS_OPERATOR(unique,token)   RE_META_IS_OPERATOR_ ## unique(token)

int _reMetaApplyOp_eTokenRange(regex_expr_t *expr, regex_token_t *operand_a, regex_token_t *operand_b, regex_token_t *operator);
int _reMetaApplyOp_eTokenConcatenation(regex_expr_t *expr, regex_token_t *operand_a, regex_token_t *operand_b, regex_token_t *operator);
int _reMetaApplyOp_eTokenAlternative(regex_expr_t *expr, regex_token_t *operand_a, regex_token_t *operand_b, regex_token_t *operator);
int _reMetaApplyOp_eTokenZeroOrOne(regex_expr_t *expr, regex_token_t *operand_a, regex_token_t *operand_b, regex_token_t *operator);
int _reMetaApplyOp_eTokenZeroOrMany(regex_expr_t *expr, regex_token_t *operand_a, regex_token_t *operand_b, regex_token_t *operator);
int _reMetaApplyOp_eTokenOneOrMany(regex_expr_t *expr, regex_token_t *operand_a, regex_token_t *operand_b, regex_token_t *operator);

#endif // MOJO_REGEX_COMPILE_IMPLEMENTATION
#ifdef MOJO_REGEX_COMMON_IMPLEMENTATION

// Emit VM instruction details (compilation and evaluation, debug only) /////

typedef void (*regexMetaVMInstrEmit_t)(FILE *fp, regex_vm_t *vm, int opcode, unsigned int instr);

#define RE_META_VM_EMIT_N(token)    NULL
#define RE_META_VM_EMIT_Y(token)    _reMetaVMEmit_ ## token
#define RE_META_VM_EMIT(unique,token)  RE_META_VM_EMIT_ ## unique(token)

void _reMetaVMEmit_eTokenCharLiteral(FILE *fp, regex_vm_t *vm, int opcode, unsigned int instr);
void _reMetaVMEmit_eTokenCharClass(FILE *fp, regex_vm_t *vm, int opcode, unsigned int instr);
void _reMetaVMEmit_eTokenStringLiteral(FILE *fp, regex_vm_t *vm, int opcode, unsigned int instr);
void _reMetaVMEmit_eTokenCharAny(FILE *fp, regex_vm_t *vm, int opcode, unsigned int instr);
void _reMetaVMEmit_eTokenMatch(FILE *fp, regex_vm_t *vm, int opcode, unsigned int instr);
void _reMetaVMEmit_eTokenSplit(FILE *fp, regex_vm_t *vm, int opcode, unsigned int instr);
void _reMetaVMEmit_eTokenJmp(FILE *fp, regex_vm_t *vm, int opcode, unsigned int instr);
void _reMetaVMEmit_eTokenSave(FILE *fp, regex_vm_t *vm, int opcode, unsigned int instr);
void _reMetaVMEmit_eTokenUtf8Class(FILE *fp, regex_vm_t *vm, int opcode, unsigned int instr);
void _reMetaVMEmit_eTokenCall(FILE *fp, regex_vm_t *vm, int opcode, unsigned int instr);
void _reMetaVMEmit_eTokenAssertion(FILE *fp, regex_vm_t *vm, int opcode, unsigned int instr);
void _reMetaVMEmit_eTokenUtf8Literal(FILE *fp, regex_vm_t *vm, int opcode, unsigned int instr);
void _reMetaVMEmit_eTokenRange(FILE *fp, regex_vm_t *vm, int opcode, unsigned int instr);
// eTokenReturn and eTokenByte have no additional VM metadata to display

#endif // MOJO_REGEX_COMMON_IMPLEMENTATION
#ifdef MOJO_REGEX_EVALUATE_IMPLEMENTATION

// Process the VM instruction (evaluation handler) //////////////////////////

typedef eReEvalResult_t (*regexVMInstr_t)(regex_eval_t *eval, regex_thread_t *thread, unsigned int instr, unsigned char c);

#define RE_META_VM_INSTR_N(token)   NULL
#define RE_META_VM_INSTR_Y(token)   _reVMInstr_ ## token
#define RE_META_VM_INSTR(unique,token)     RE_META_VM_INSTR_ ## unique(token)

eReEvalResult_t _reVMInstr_eTokenCharLiteral(regex_eval_t *eval, regex_thread_t *thread, unsigned int instr, unsigned char c);
eReEvalResult_t _reVMInstr_eTokenCharClass(regex_eval_t *eval, regex_thread_t *thread, unsigned int instr, unsigned char c);
eReEvalResult_t _reVMInstr_eTokenStringLiteral(regex_eval_t *eval, regex_thread_t *thread, unsigned int instr, unsigned char c);
eReEvalResult_t _reVMInstr_eTokenCharAny(regex_eval_t *eval, regex_thread_t *thread, unsigned int instr, unsigned char c);
eReEvalResult_t _reVMInstr_eTokenMatch(regex_eval_t *eval, regex_thread_t *thread, unsigned int instr, unsigned char c);
eReEvalResult_t _reVMInstr_eTokenSplit(regex_eval_t *eval, regex_thread_t *thread, unsigned int instr, unsigned char c);
eReEvalResult_t _reVMInstr_eTokenJmp(regex_eval_t *eval, regex_thread_t *thread, unsigned int instr, unsigned char c);
eReEvalResult_t _reVMInstr_eTokenSave(regex_eval_t *eval, regex_thread_t *thread, unsigned int instr, unsigned char c);
eReEvalResult_t _reVMInstr_eTokenUtf8Class(regex_eval_t *eval, regex_thread_t *thread, unsigned int instr, unsigned char c);
eReEvalResult_t _reVMInstr_eTokenCall(regex_eval_t *eval, regex_thread_t *thread, unsigned int instr, unsigned char c);
eReEvalResult_t _reVMInstr_eTokenReturn(regex_eval_t *eval, regex_thread_t *thread, unsigned int instr, unsigned char c);
eReEvalResult_t _reVMInstr_eTokenByte(regex_eval_t *eval, regex_thread_t *thread, unsigned int instr, unsigned char c);
eReEvalResult_t _reVMInstr_eTokenAssertion(regex_eval_t *eval, regex_thread_t *thread, unsigned int instr, unsigned char c);
eReEvalResult_t _reVMInstr_eTokenUtf8Literal(regex_eval_t *eval, regex_thread_t *thread, unsigned int instr, unsigned char c);
eReEvalResult_t _reVMInstr_eTokenRange(regex_eval_t *eval, regex_thread_t *thread, unsigned int instr, unsigned char c);

#else // !MOJO_REGEX_EVALUATE_IMPLEMENTATION

#define RE_META_VM_INSTR(unique,token)     NULL

#endif // MOJO_REGEX_EVALUATE_IMPLEMENTATION
#ifdef MOJO_REGEX_COMMON_IMPLEMENTATION

// Token meta details table /////////////////////////////////////////////////

typedef struct regex_token_detail_s regex_token_detail_t;
struct regex_token_detail_s {
    eRegexToken_t token;
    int textAdvance;
    regexVMInstr_t handleInstr;
#endif // MOJO_REGEX_COMMON_IMPLEMENTATION
#ifdef MOJO_REGEX_DEBUG_IMPLEMENTATION
    const char *name;
    const char *instr;
    regexMetaVMInstrEmit_t emitVMInstr;
#endif // MOJO_REGEX_DEBUG_IMPLEMENTATION
#ifdef MOJO_REGEX_COMPILE_IMPLEMENTATION
    regexMetaTokenEmit_t emitToken;
    eRegexTokenPriority_t priority;
    eRegexTokenTerminality_t terminal;
    regexMetaApplyOperator_t operator;
    int arity;
#endif // MOJO_REGEX_COMPILE_IMPLEMENTATION
#ifdef MOJO_REGEX_COMMON_IMPLEMENTATION
};
#endif // MOJO_REGEX_COMMON_IMPLEMENTATION

#ifndef MOJO_REGEX_COMPILE_IMPLEMENTATION
#   define RE_TOK_DETAIL(token,advance,handleV,instr,emitV,emitT,priority,terminal,operator,arity) \
        {token, advance, RE_RE_META_VM_INSTR(handleV,token), #token, instr, RE_META_VM_EMIT(emitV,token)}
#else // MOJO_REGEX_COMPILE_IMPLEMENTATION
#   define RE_TOK_DETAIL(token,advance,handleV,instr,emitV,emitT,priority,terminal,operator,arity) \
        {token, advance, RE_META_VM_INSTR(handleV,token), #token, instr, \
        RE_META_VM_EMIT(emitV,token), RE_META_TOK_EMIT(emitT,token), \
        ePriority ## priority, terminal, RE_META_IS_OPERATOR(operator,token), arity}
#endif // MOJO_REGEX_COMPILE_IMPLEMENTATION

#define RE_TOK_DETAIL_END RE_TOK_DETAIL(eTokenUnknown, 0, N, "<unknown>", N, N, None, eReTokNotTerminal, N, 0)

//  Detail                              VM text  VM                   VM    Tok                                 Is
//  Table         Token                 advance  hndlr  VM instr      Emit  Emit  Priority  Terminal            Operator  Arity
// -------------  --------------------  -------  -----  ------------  ----  ----  --------  --------            --------  -----
regex_token_detail_t _regexTokenDetails[] = {
    RE_TOK_DETAIL(eTokenCharLiteral,    1,       Y,     "char",       Y,    Y,    None,     eReTokTerminal,     N,        0),
    RE_TOK_DETAIL(eTokenCharClass,      1,       Y,     "class",      Y,    Y,    None,     eReTokTerminal,     N,        0),
    RE_TOK_DETAIL(eTokenStringLiteral,  1,       Y,     "string",     Y,    Y,    None,     eReTokTerminal,     N,        0),
    RE_TOK_DETAIL(eTokenCharAny,        1,       Y,     "anychar",    Y,    Y,    None,     eReTokTerminal,     N,        0),
    RE_TOK_DETAIL(eTokenMatch,          0,       Y,     "match",      Y,    Y,    None,     eReTokNotTerminal,  N,        0),
    RE_TOK_DETAIL(eTokenSplit,          0,       Y,     "split",      Y,    N,    None,     eReTokNotTerminal,  N,        0),
    RE_TOK_DETAIL(eTokenJmp,            0,       Y,     "jmp",        Y,    N,    None,     eReTokNotTerminal,  N,        0),
    RE_TOK_DETAIL(eTokenSave,           0,       Y,     "save",       Y,    Y,    None,     eReTokNotTerminal,  N,        0),
    RE_TOK_DETAIL(eTokenUtf8Class,      1,       Y,     "utf8class",  Y,    Y,    None,     eReTokTerminal,     N,        0),
    RE_TOK_DETAIL(eTokenCall,           0,       Y,     "call",       Y,    Y,    None,     eReTokTerminal,     N,        0),
    RE_TOK_DETAIL(eTokenReturn,         0,       Y,     "return",     N,    N,    None,     eReTokTerminal,     N,        0),
    RE_TOK_DETAIL(eTokenByte,           1,       Y,     "byte",       N,    N,    None,     eReTokTerminal,     N,        0),
    RE_TOK_DETAIL(eTokenAssertion,      0,       Y,     "assertion",  Y,    Y,    None,     eReTokTerminal,     N,        0),
    RE_TOK_DETAIL(eTokenUtf8Literal,    1,       Y,     "utf8literal",Y,    Y,    None,     eReTokTerminal,     N,        0),
    RE_TOK_DETAIL(eTokenRange,          0,       Y,     "range",      Y,    Y,    High,     eReTokPreceeding,   Y,        1),
#endif // MOJO_REGEX_COMMON_IMPLEMENTATION
#ifdef MOJO_REGEX_COMPILE_IMPLEMENTATION
    RE_TOK_DETAIL(eTokenConcatenation,  0,       N,     NULL,         N,    N,    Medium,   eReTokNotTerminal,  Y,        2),
    RE_TOK_DETAIL(eTokenAlternative,    0,       N,     NULL,         N,    N,    Medium,   eReTokNotTerminal,  Y,        2),
    RE_TOK_DETAIL(eTokenZeroOrOne,      0,       N,     NULL,         N,    N,    High,     eReTokPreceeding,   Y,        1),
    RE_TOK_DETAIL(eTokenZeroOrMany,     0,       N,     NULL,         N,    N,    High,     eReTokPreceeding,   Y,        1),
    RE_TOK_DETAIL(eTokenOneOrMany,      0,       N,     NULL,         N,    N,    High,     eReTokPreceeding,   Y,        1),
    RE_TOK_DETAIL(eTokenSubExprStart,   0,       N,     NULL,         N,    Y,    Low,      eReTokNotPreceeding,N,        0),
    RE_TOK_DETAIL(eTokenSubExprEnd,     0,       N,     NULL,         N,    N,    Low,      eReTokPreceeding,   N,        0),
#endif // MOJO_REGEX_COMPILE_IMPLEMENTATION
#ifdef MOJO_REGEX_COMMON_IMPLEMENTATION
    RE_TOK_DETAIL_END
};

// This should be adjusted to the length of the longest VM instr name
// (Currently 11, the length of "utf8literal")
#define MAX_TOKEN_INSTR_NAME_LEN    11

#endif // MOJO_REGEX_COMMON_IMPLEMENTATION

#ifdef MOJO_REGEX_COMPILE_IMPLEMENTATION

// Sanity check to ensure that the token table values are properly aligned.
// If any tokens are not stored at the table index of the token enumeration
// value, then the table is mis-aligned (developer error), and should be
// corrected before continuing (hard exit).
int regexVerifyTokenDetails(void);

// Diagnostic output functions, to enumerate some aspect of the expression state.

typedef enum {
    eReEmitToken = 0,
    eReEmitAttrs = 1,
    eReEmitAddr = 2,
    eReEmitDFA = 4,
    eReEmitPC = 8,
    eReEmitConcat = 16,
    eReEmitBrief = 1, // eReEmitAttrs,
    eReEmitFull = 3, // eReEmitAttrs | eReEmitAddr,
    eReEmitVerboseFull = 19, // eReEmitFull | eReEmitConcat,
    eReEmitVerboseDFA = 23, // eReEmitVerboseFull | eReEmitDFA,
    eReEmitVerboseVM = 27 // eReEmitVerboseFull | eReEmitPC
} eReEmitFlag;

// Emits the stringified details of a single token:
//    1        2                   3               4            5
// 0x####:eTokenName(token specific attributes){0x####}{0x####}[##]
//
//     1 : eReEmitAddr  : the token struct 2 byte address suffix
//     2 : eReEmitToken : the token enum name (always emitted)
//     3 : eReEmitAttrs : token specific attributes (paren delimited)
//     4 : eReEmitDFA   : addresses of linked DFA (a & b) nodes
//     5 : eReEmitPC    : program counter for the token
//
// Note that concatenation nodes are excluded without the eReEmitConcat flag
int regexTokenStrEmit(FILE *fp, regex_build_ctx_t *build_ctx,
                      regex_token_t *token, unsigned int emitFlags);

// Emits the stringified details of a sequence of tokens (using regexTokenStrEmit)
// Note: this emits the un-sequenced node chain (NOT the DFA chain)
// Note: entries are prefixed by their index in the stack (# - ), zero based
void regexTokenStrStackEmit(FILE *fp, regex_build_ctx_t *build_ctx,
                            regex_token_t *token, unsigned int emitFlags);

// Walks a token stack/tree, and sets all PC entries equal to val
void regexTokenWalkTree(regex_token_t *token, int val);

// Emits the stringified details of a DFA fragment.
// Note: this emits the sequenced node chains (NOT the un-sequenced token stack)
// Note: this will NOT function after VM generation, as it uses the PC value as
//       a sequence marker to avoid looping.
void regexTokenStrDFAEmit(FILE *fp, regex_build_ctx_t *build_ctx, regex_token_t *token);

#endif // MOJO_REGEX_COMMON_IMPLEMENTATION
#ifdef MOJO_REGEX_COMMON_IMPLEMENTATION

// Emits a single VM instruction in stringified form
void regexVMInstrEmit(FILE *fp, regex_vm_t *vm, int pc);

// Emits a VM program listing in stringified form, beginning at the program
// counter index pc. If primary is false, emits until the end of the program.
// If primary is true, emits until the end of the main program (no subroutines)
// if pc is in the main program body, or until the end of the subroutine if pc
// is within a subroutine.
void regexVMProgramEmit(FILE *fp, regex_vm_t *vm, int pc, int primary);

int regexVMFindSubroutinePCByName(regex_vm_t *vm, const char *name);

#endif // MOJO_REGEX_COMMON_IMPLEMENTATION
#ifdef MOJO_REGEX_COMPILE_IMPLEMENTATION

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

// Token handling functions /////////////////////////////////////////////////

regex_ptrlist_t *_regexPtrlistCreate(regex_build_ctx_t *context, regex_token_t *token, eRePtrListType_t type);
regex_ptrlist_t *_regexPtrlistAppend(regex_ptrlist_t *lista, regex_ptrlist_t *listb);
int _regexPtrlistPatch(regex_build_ctx_t *context, regex_ptrlist_t **list, regex_token_t *token, int no_jumps);
void _regexPtrlistFree(regex_build_ctx_t *context, regex_ptrlist_t *list);

regex_token_t *_regexTokenAlloc(regex_build_ctx_t *context, eRegexToken_t tokenType,
                                const char *str, const unsigned int *bitmap,
                                int len);
void _regexTokenLoopBreaker(regex_token_t *token);
void _regexTokenDestroy(regex_build_ctx_t *context, regex_token_t *token, int full_stack);

regex_token_t *_regexTokenBaseCreate(regex_build_t *build, eRegexToken_t tokenType,
                                     const void *str, const void *ptr,
                                     int len);

int regexBuildConcatChar(regex_build_t *build, int c, int invert);
int regexBuildConcatCharClass(regex_build_t *build, unsigned int *bitmap);
int regexBuildConcatString(regex_build_t *build, const char *str, int len);
int regexBuildConcatCharAny(regex_build_t *build, int dot_all);
int regexBuildConcatUtf8Class(regex_build_t *build, int encoding, int mid_high, int mid_low, int invert, unsigned int *bitmap);
int regexBuildConcatCall(regex_build_t *build, int sub_index);
int regexBuildConcatByte(regex_build_t *build);
int regexBuildConcatAssertion(regex_build_t *build, int assertion);
int regexBuildConcatUtf8Literal(regex_build_t *build, int c, int invert);

int regexIsIndexGroupStart(int index);
int regexLogicalGroupToIndex(int group, int start);
int regexIndexToLogicalGroup(int index);

int regexBuildGroupStart(regex_build_t *build, int expr_inline, const char *name, int name_len, int no_capture,
                         int compound, int subroutine_index);
eReExprStatus_t regexBuildGroupEnd(regex_build_t *build);

eReExprStatus_t regexBuildWrapSubexpression(regex_build_t *build, const char *name, int name_len,
                                            int no_capture, int compound, int subroutine, int index);

typedef enum {
    eReQuantifyZeroOrOne,
    eReQuantifyZeroOrMany,
    eReQuantifyOneOrMany
} eReQuantifier_t;

int regexBuildQuantify(regex_build_t *build, eReQuantifier_t quantifier);
int regexBuildRange(regex_build_t *build, int min, int max);
int regexBuildAlternative(regex_build_t *build);

int regexCreateTokenJmp(regex_build_ctx_t *context, regex_token_t **token);

// TODO: create match, create return

// Pattern parsing handling functions ///////////////////////////////////////

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

typedef enum {
    eRegexPatternIdOk,
    eRegexPatternIdMalformed,
    eRegexPatternIdMissing
} eRegexPatternId_t;

typedef struct parse_str_s parse_str_t;
struct parse_str_s {
    int characters;
    int bytes;
};

static int _parseIsHexDigit(char c);
static int _parseHexValueGet(char c);
static int _parseIsIdChar(char c);
static int _parseHexValueStr(const char *str, int len, unsigned int *value);

// Validates that pattern points to a valid range quantifier (ie., "{3,6}").
// Returns 0 if false, or the length of the quantifier (excluding delimiters)
// if true.
static int _parseRangeQuantifier(const char *pattern, int *min, int *max);
static void _parseCharEmit(FILE *fp, parseChar_t *pc);
static int _parsePatternIsValid(parseChar_t *pc);
static void _parsePatternCharAdvance(const char **pattern, parseChar_t *pc);
static void _parsePatternCharBacktrack(const char **pattern, parseChar_t *pc);
static int _parseCheckNextPatternChar(const char **pattern, char c);

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
                                               int start, int end, const char **id, int *len);
static parseChar_t _parseGetNextPatternChar(const char **pattern, int allow_brace,
                                            const char **id, int *len);

static parse_str_t _parseGetPatternStrLen(const char **pattern);
static char *_parseGetPatternStr(const char **pattern, int len, int size);
int regexEscapeCharLen(const char *str, int *stride);

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

regex_utf8_range_t *_regexUtf8RangeCreate(regex_build_ctx_t *context, int start, int end);
void _regexUtf8RangeDestroy(regex_build_ctx_t *context, regex_utf8_range_t *range, int free_all);
int _parseCreateUtf8Range(regex_build_t *build, regex_utf8_range_t **sequence, int start, int end);
static int _parseCharClassBitmapSet(regex_build_t *build, regex_utf8_range_t **range, unsigned int *bitmap, int c);
static int _parseCharClassBitmapRangeSet(regex_build_t *build, regex_utf8_range_t **range, unsigned int *bitmap, int a, int b);
static int _parseCharClassAndCreateToken(regex_build_t *build, const char *class_pattern,
                                         const char *name, int name_len, int invert);

static eRegexCompileStatus_t _regexTokenizePattern(regex_build_t *build);

// Token stream handling functions //////////////////////////////////////////

regex_build_ctx_t *regexBuildContextCreate(unsigned int flags);
void regexBuildContextDestroy(regex_build_ctx_t *context);

regex_build_t *regexBuildCreate(regex_build_ctx_t *context);
void regexBuildDestroy(regex_build_t *build);
int regexBuildFinalize(regex_build_t *build);

int regexExprCreate(regex_build_t *build);
void _regexExprOperandPush(regex_expr_t *expr, regex_token_t *token);
regex_token_t *_regexExprOperandPop(regex_expr_t *expr);
void _regexExprOperatorPush(regex_expr_t *expr, regex_token_t *token);
regex_token_t *_regexExprOperatorPop(regex_expr_t *expr);
eReExprStatus_t _regexExprOperatorApply(regex_expr_t *expr);
eReExprStatus_t _regexExprFinalize(regex_expr_t *expr);
eReExprStatus_t _regexExprTokenPush(regex_expr_t *expr, regex_token_t *token);
int regexExprDestroy(regex_build_t *build);

int regexPatternCreate(regex_build_t *build, const char *pattern, int len);
int regexPatternDestroy(regex_build_t *build);

typedef enum {
    eReSubOk,
    eReSubCollision,
    eReSubMissingName,
    eReSubMissingExpr,
    eReSubOutOfMemory
} eReSuboutineResult_t;

int regexSubroutineCreate(regex_build_ctx_t *context);
static regex_sub_t *_regexSubroutineGet(regex_build_ctx_t *context, int id);
eReSuboutineResult_t regexSubroutineName(regex_build_ctx_t *context, int id, const char *prefix, const char *name, int len);
eReSuboutineResult_t regexSubroutineAlias(regex_build_ctx_t *context, int id, const char *prefix, const char *alias, int len);
int regexSubroutineLookup(regex_build_ctx_t *context, const char *prefix, const char *name, int len);
eReSuboutineResult_t regexSubroutineExprSet(regex_build_ctx_t *context, int id, regex_token_t *tokens);
eReSuboutineResult_t regexSubroutineRegister(regex_build_ctx_t *context, const char *prefix, const char *name, const char *alias,
                                             regex_token_t *tokens);
void regexSubroutineFreeAll(regex_build_ctx_t *context);

// Compiles a regex pattern into a VM image, and provides a status value. If the
// compilation failed, then sufficient context for a detailed diagnostic error
// response is included in lieu of the VM.
regex_build_t *regexCompile(const char *pattern, unsigned int flags);

int regexBuildPatternProcess(regex_build_t *build);
#if 0
int regexBuildSubexprNextIndexGet(regex_build_t *build);
int regexBuildSubexprName(regex_build_t *build, int index, const char *name);
int regexBuildSubexprSet(regex_build_t *build, int index, regex_token_t *tokens);

int regexBuildExprSet(regex_build_t *build);
int regexBuildExprSubroutine(regex_build_t *build, int index, const char *name);
#endif // 0

regex_vm_t *regexVMCreate(regex_build_t *build);
int regexVMGenerate(regex_build_t *build);
void regexVMDestroy(regex_vm_t *vm);
int regexVMStringTableEntryAdd(regex_build_t *build, char *str, int len);
int regexVMCharClassEntryAdd(regex_build_t *build, unsigned int *bitmap);
int regexVMUtf8ClassEntryAdd(regex_build_t *build, unsigned int *bitmap);
int regexVMGroupEntryAdd(regex_build_t *build, const char *name, int len, int index);
int regexVMSubNameEntryAdd(regex_build_t *build, const char *name, const char *alias, int index);
int regexVMSubNameEntrySetPC(regex_vm_t *vm, int index, int pc);
#ifdef MOJO_REGEX_COMMON_IMPLEMENTATION
const char *regexVMStringTableEntryGet(regex_vm_t *vm, int index, int *len);
const unsigned int *regexVMCharClassEntryGet(regex_vm_t *vm, int index);
const unsigned int *regexVMUtf8ClassEntryGet(regex_vm_t *vm, int index);
const char *regexVMGroupEntryGet(regex_vm_t *vm, int index);
const char *regexVMSubNameEntryGet(regex_vm_t *vm, int pc);
const char *regexVMSubAliasEntryGet(regex_vm_t *vm, int pc);
#endif // MOJO_REGEX_COMMON_IMPLEMENTATION

// Regex evaluation handlers ////////////////////////////////////////////////

int _regexIsWordBoundaryChar(unsigned char c);
regex_compound_t *regexThreadCompoundCreate(regex_eval_t *eval, regex_thread_t *thread);
void regexThreadCompoundDestroy(regex_eval_t *eval, regex_compound_t *compound);
int regexThreadCompoundCopy(regex_eval_t *eval, regex_thread_t *dest, regex_thread_t *src);
int regexThreadCompoundCount(regex_compound_t *compound);
void regexThreadCompoundReverse(regex_compound_t **compound);
size_t regexThreadCompoundCalcMatchBufferSize(regex_vm_t *vm, regex_thread_t *thread);
void regexThreadCompoundStoreInMatch(regex_match_t *match, const char *baseMem, regex_thread_t *thread);
int regexThreadCompoundStart(regex_eval_t *eval, regex_thread_t *thread, int subexpr, const char *ptr);
int regexThreadCompoundEnd(regex_thread_t *thread, const char *ptr);
void regexThreadCopySubexprs(int count, regex_thread_t *dest, regex_thread_t *src);
regex_eval_t *regexEvalCreate(regex_vm_t *vm, const char *pattern, int len);
int regexThreadCreate(regex_eval_t *eval, regex_thread_t *parent, unsigned int pc, int from_range);
void regexThreadFree(regex_eval_t *eval, regex_thread_t *thread);
void regexEvalFree(regex_eval_t *eval);
eReEvalResult_t regexThreadProcess(regex_eval_t *eval, regex_thread_t *thread, int complete, unsigned char c);
void regexMatchFree(regex_match_t *match);
#ifdef MOJO_REGEX_VM_DEBUG
regex_match_t *regexMatch(regex_vm_t *vm, const char *text, int len, int anchored, FILE *debug);
#else // !MOJO_REGEX_VM_DEBUG
regex_match_t *regexMatch(regex_vm_t *vm, const char *text, int len, int anchored);
#endif // MOJO_REGEX_VM_DEBUG


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

// TODO - inline the base utf8 props classes to ensure this is a true single file library
#include "utf8_props.h"

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
// UTF8 handling support functions
/////////////////////////////////////////////////////////////////////////////

// Returns the number of bytes required to utf8 encode the codepoint that
// begins the utf8 encoded sequence starting with byte 'c'
int _parseUtf8EncodedHighByte(char c) {
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

// Parses a utf8 encoded string sequence, and returns the codepoint value.
// Returns -1 if the str is not a multibyte utf8 encoded sequence.
int _parseUtf8DecodeSequence(const char *str) {
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

int _parseUtf8CompareEncodingByte(int codepoint, unsigned char c, int pos) {
    if((pos > 4) || (pos <= 0)) {
        return 0;
    }
    switch(pos) {
        default:
            return 0;
        case 1:
            if(codepoint > 127) {
                return (((((unsigned)codepoint) & UTF8_LOW_BYTE_BITMASK) | UTF8_LOW_BYTE_PREFIX) == c);
            } else {
                return (((int)c) == codepoint);
            }
        case 2:
            if(codepoint <= 127) {
                return 0;
            }
            if(codepoint > 2047) {
                return ((((((unsigned)codepoint) >> 6u) & UTF8_LOW_BYTE_BITMASK) | UTF8_LOW_BYTE_PREFIX) == c);
            }
            return ((((((unsigned)codepoint) >> 6u) & UTF8_TWO_BYTE_BITMASK) | UTF8_TWO_BYTE_PREFIX) == c);
        case 3:
            if(codepoint <= 2047) {
                return 0;
            }
            if(codepoint > 65535) {
                return ((((((unsigned)codepoint) >> 12u) & UTF8_LOW_BYTE_BITMASK) | UTF8_LOW_BYTE_PREFIX) == c);
            }
            return ((((((unsigned)codepoint) >> 12u) & UTF8_THREE_BYTE_BITMASK) | UTF8_THREE_BYTE_PREFIX) == c);
        case 4:
            if(codepoint <= 65535) {
                return 0;
            }
            return ((((((unsigned)codepoint) >> 18u) & UTF8_FOUR_BYTE_BITMASK) | UTF8_FOUR_BYTE_PREFIX) == c);
    }
}

/////////////////////////////////////////////////////////////////////////////
// Regex token details emit handlers
/////////////////////////////////////////////////////////////////////////////

static void _regexCharLiteralEmit(FILE *fp, int c) {
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

static void _regexCharClassBitmapEmit(FILE *fp, const unsigned int *bitmap) {
    unsigned int k;
    unsigned int run;

    for(k = 0; k < 256; k++) {
        if(bitmap[k / 32] & (1u << (k % 32))) {
            for(run = k + 1; run < 256 && (bitmap[run / 32] & (1u << (run % 32))); run++);
            run--;
            fprintf(fp, "%c", ((k < 32) || (k > 127)) ? '.' : k);
            if(run - k > 3) {
                fprintf(fp, "-%c", ((run < 32) || (run > 127)) ? '.' : run);
                k = run;
            }
        }
    }
}

static void _regexUtf8ClassBitmapEmit(FILE *fp, const unsigned int *bitmap) {
    unsigned int k;
    unsigned int run;

    for(k = 0; k < 64; k++) {
        if(bitmap[k / 32] & (1u << (k % 32))) {
            for(run = k + 1; run < 64 && (bitmap[run / 32] & (1u << (run % 32))); run++);
            run--;
            fprintf(fp, "0x%2.2x", k);
            if(run - k > 3) {
                fprintf(fp, "-0x%2.2x", run);
                k = run;
            }
        }
    }
}

static void _regexEscapedStrEmit(FILE *fp, const char *str, int len) {
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
        if((codepoint = _parseUtf8DecodeSequence(str + k)) != -1) {
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

void _reMetaTokEmit_eTokenCharLiteral(FILE *fp, regex_build_ctx_t *build_ctx, regex_token_t *token) {
    fputc('\'', fp);
    _regexCharLiteralEmit(fp, token->c);
    fprintf(fp, "':%03d%s", token->c,
            ((token->flags & RE_TOK_FLAG_INVERT) ? ":invert" : ""));
}

void _reMetaTokEmit_eTokenCharClass(FILE *fp, regex_build_ctx_t *build_ctx, regex_token_t *token) {
    fputc('[', fp);
    _regexCharClassBitmapEmit(fp, token->bitmap);
    fputc(']', fp);
}

void _reMetaTokEmit_eTokenStringLiteral(FILE *fp, regex_build_ctx_t *build_ctx, regex_token_t *token) {
    fputc('\"', fp);
    _regexEscapedStrEmit(fp, token->str, token->len);
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
    fprintf(fp, "enc(%d):%s", token->encoding, ((token->flags & RE_TOK_FLAG_INVERT) ? "invert:" : ""));
    if(token->encoding >= 3) {
        fprintf(fp, "mid_high(0x%2.2x):", token->mid_high);
    }
    fprintf(fp, "mid_low(0x%2.2x):[", token->mid_low);
    _regexUtf8ClassBitmapEmit(fp, token->bitmap);
    fputc(']', fp);
}

void _reMetaTokEmit_eTokenCall(FILE *fp, regex_build_ctx_t *build_ctx, regex_token_t *token) {
    regex_sub_t *routine;

    fprintf(fp, "%d", token->sub_index);
    if((routine = _regexSubroutineGet(build_ctx, token->sub_index)) == NULL) {
        fputs("SUB_NOT_FOUND", fp);
    } else {
        if(routine->name != NULL) {
            fprintf(fp, ":name(%s)", routine->name);
        }
        if(routine->alias != NULL) {
            fprintf(fp, ":alias(%s)", routine->alias);
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
    fprintf(fp, "enc(%d):%s:\\u", token->encoding, ((token->flags & RE_TOK_FLAG_INVERT) ? "invert:" : ""));
    if(token->c > 0xFFFF) {
        fprintf(fp, "{%x}", token->c);
    } else {
        fprintf(fp, "%x", token->c);
    }
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
    regex_sub_t *routine;
    int len;

    fprintf(fp, "%d:idx(%d):%s:",
            regexIndexToLogicalGroup(token->group),
            token->group,
            (regexIsIndexGroupStart(token->group) ? "open" : "close"));
    if(token->flags & RE_TOK_FLAG_COMPOUND) {
        fputs(":compound", fp);
    }
    if(token->flags & RE_TOK_FLAG_NAMED) {
        fprintf(fp, ":named(%s)", token->name);
    }
    if(token->flags & RE_TOK_FLAG_SUBROUTINE) {
        fprintf(fp, ":subroutine(%d)", token->sub_index);
        if((routine = _regexSubroutineGet(build_ctx, token->sub_index)) != NULL) {
            if((routine->name != NULL) || (routine->alias != NULL)) {
                fputc('(', fp);
                if(routine->name != NULL) {
                    fprintf(fp, "name(%s)", routine->name);
                }
                if(routine->alias != NULL) {
                    fprintf(fp, "%salias(%s)", ((routine->name != NULL) ? ":" : ""),  routine->alias);
                }
                fputc(')', fp);
            }
        }
    }
    if(token->flags & RE_TOK_FLAG_NO_CAPTURE) {
        fputs(":no_capture", fp);
    }
}

void _reMetaTokEmit_eTokenSave(FILE *fp, regex_build_ctx_t *build_ctx, regex_token_t *token) {
    fprintf(fp, "%d:idx(%d):%s:",
            regexIndexToLogicalGroup(token->group),
            token->group,
            (regexIsIndexGroupStart(token->group) ? "open" : "close"));
    if(token->flags & RE_TOK_FLAG_COMPOUND) {
        fputs(":compound", fp);
    }
    if(token->flags & RE_TOK_FLAG_NAMED) {
        fprintf(fp, ":named(%s)", token->name);
    }
}

/////////////////////////////////////////////////////////////////////////////
// Apply operators to tokens in shunting yard implementation (compilation only)
/////////////////////////////////////////////////////////////////////////////

int _reMetaApplyOp_eTokenRange(regex_expr_t *expr, regex_token_t *operand_a, regex_token_t *operand_b, regex_token_t *operator) {
    regex_token_t *jmp = NULL;

    operator->out_a = operand_a;
    if((operator->ptrlist = _regexPtrlistCreate(expr->context, operator, eRePtrOutB)) == NULL) {
        return 0;
    }
    if(!regexCreateTokenJmp(expr->context, &jmp)) {
        return 0;
    }
    jmp->out_a = operator;
    _regexPtrlistPatch(expr->context, &(operand_a->ptrlist), jmp, 0);
    _regexExprOperandPush(expr, operator);

    return 1;
}

int _reMetaApplyOp_eTokenConcatenation(regex_expr_t *expr, regex_token_t *operand_a, regex_token_t *operand_b, regex_token_t *operator) {
    _regexTokenDestroy(expr->context, operator, 0);
    _regexPtrlistPatch(expr->context, &(operand_a->ptrlist), operand_b, 0);
    operand_a->ptrlist = operand_b->ptrlist;
    operand_b->ptrlist = NULL;
    _regexExprOperandPush(expr, operand_a);

    return 1;
}

int _reMetaApplyOp_eTokenAlternative(regex_expr_t *expr, regex_token_t *operand_a, regex_token_t *operand_b, regex_token_t *operator) {
    regex_token_t *jmp = NULL;

    if(!regexCreateTokenJmp(expr->context, &jmp)) {
        return 0;
    }

#ifdef MOJO_REGEX_EXPERIMENTAL_CULL_JMP
    if(regexPtrlistPatch(expr->context, &(operand_a->ptrlist), jmp, 1) == 0) {
#else // !MOJO_REGEX_EXPERIMENTAL_CULL_JMP
    if(_regexPtrlistPatch(expr->context, &(operand_a->ptrlist), jmp, 0) == 0) {
#endif
        // The jmp token was unused
        _regexTokenDestroy(expr->context, jmp, 0);
        operator->ptrlist = operand_a->ptrlist;
        operand_a->ptrlist = NULL;
    } else {
        if((operator->ptrlist = _regexPtrlistCreate(expr->context, jmp, eRePtrOutA)) == NULL) {
            return 0;
        }
    }
    operator->tokenType = eTokenSplit;
    operator->out_a = operand_a;
    operator->out_b = operand_b;
    operator->ptrlist = _regexPtrlistAppend(operator->ptrlist, operand_b->ptrlist);
    operand_b->ptrlist = NULL;
    _regexExprOperandPush(expr, operator);

    return 1;
}

int _reMetaApplyOp_eTokenZeroOrOne(regex_expr_t *expr, regex_token_t *operand_a, regex_token_t *operand_b, regex_token_t *operator) {
    operator->tokenType = eTokenSplit;
    operator->out_a = operand_a;
    if((operator->ptrlist = _regexPtrlistCreate(expr->context, operator, eRePtrOutB)) == NULL) {
        return 0;
    }
    operator->ptrlist = _regexPtrlistAppend(operand_a->ptrlist, operator->ptrlist);
    operand_a->ptrlist = NULL;
    _regexExprOperandPush(expr, operator);

    return 1;
}

int _reMetaApplyOp_eTokenZeroOrMany(regex_expr_t *expr, regex_token_t *operand_a, regex_token_t *operand_b, regex_token_t *operator) {
    regex_token_t *jmp = NULL;

    operator->tokenType = eTokenSplit;
    operator->out_a = operand_a;
    if((operator->ptrlist = _regexPtrlistCreate(expr->context, operator, eRePtrOutB)) == NULL) {
        return 0;
    }
    if(!regexCreateTokenJmp(expr->context, &jmp)) {
        return 0;
    }
    jmp->out_a = operator;
    _regexPtrlistPatch(expr->context, &(operand_a->ptrlist), jmp, 0);
    _regexExprOperandPush(expr, operator);

    return 1;
}

int _reMetaApplyOp_eTokenOneOrMany(regex_expr_t *expr, regex_token_t *operand_a, regex_token_t *operand_b, regex_token_t *operator) {
    operator->tokenType = eTokenSplit;
    operator->out_a = operand_a;
    _regexPtrlistPatch(expr->context, &(operand_a->ptrlist), operator, 0);
    if((operand_a->ptrlist = _regexPtrlistCreate(expr->context, operator, eRePtrOutB)) == NULL) {
        return 0;
    }
    _regexExprOperandPush(expr, operand_a);

    return 1;
}

/////////////////////////////////////////////////////////////////////////////
// Regex token details VM instruction emit handlers
/////////////////////////////////////////////////////////////////////////////

void _reMetaVMEmit_eTokenCharLiteral(FILE *fp, regex_vm_t *vm, int opcode, unsigned int instr) {
    char c = RE_VM_DEC_CHAR_FROM_INSTR(instr);
    fputc('\'', fp);
    _regexCharLiteralEmit(fp, c);
    fprintf(fp, "' (%03d)%s", c,
            (RE_VM_FLAG_CHAR_INVERT_FROM_INSTR(instr) ? "  inverse" : ""));
    if(RE_VM_OP_B_FROM_INSTR(instr) != 0x3FFF) {
        fprintf(fp,"  jmp(%4.4d)", RE_VM_OP_B_FROM_INSTR(instr));
    }
}

void _reMetaVMEmit_eTokenCharClass(FILE *fp, regex_vm_t *vm, int opcode, unsigned int instr) {
    fputc('[', fp);
    _regexCharClassBitmapEmit(fp, vm->class_table[RE_VM_OP_A_FROM_INSTR(instr)]);
    fputc(']', fp);
}

void _reMetaVMEmit_eTokenStringLiteral(FILE *fp, regex_vm_t *vm, int opcode, unsigned int instr) {
    const char *str;
    int len;

    str = regexVMStringTableEntryGet(vm, (int)RE_VM_OP_A_FROM_INSTR(instr), &len);
    fputc('"', fp);
    _regexEscapedStrEmit(fp, str, len);
    fputc('"', fp);
}

void _reMetaVMEmit_eTokenCharAny(FILE *fp, regex_vm_t *vm, int opcode, unsigned int instr) {
    if(RE_VM_FLAG_DOTALL_FROM_INSTR(instr)) {
        fputs("dot_all", fp);
    }
}

void _reMetaVMEmit_eTokenMatch(FILE *fp, regex_vm_t *vm, int opcode, unsigned int instr) {
    if(RE_VM_FLAG_EOF_FROM_INSTR(instr)) {
        fputs("($)", fp);
    }
}

void _reMetaVMEmit_eTokenSplit(FILE *fp, regex_vm_t *vm, int opcode, unsigned int instr) {
    fprintf(fp, "%d, %d", RE_VM_OP_A_FROM_INSTR(instr), RE_VM_OP_B_FROM_INSTR(instr));
}

void _reMetaVMEmit_eTokenJmp(FILE *fp, regex_vm_t *vm, int opcode, unsigned int instr) {
    fprintf(fp, "%d", RE_VM_OP_A_FROM_INSTR(instr));
}

void _reMetaVMEmit_eTokenSave(FILE *fp, regex_vm_t *vm, int opcode, unsigned int instr) {
    int operand_a = RE_VM_OP_A_FROM_INSTR(instr);
    const char *str = regexVMGroupEntryGet(vm, operand_a);

    fprintf(fp, "%d  group(%d %s)", operand_a,
            regexIndexToLogicalGroup((int)operand_a),
            (regexIsIndexGroupStart((int)operand_a) ? "open" : "close"));
    if(str != NULL) {
        fprintf(fp, "  (%s)", str);
    }
    if(RE_VM_FLAG_COMPOUND_FROM_INSTR(instr)) {
        fprintf(fp, "  compound");
    }
}

void _reMetaVMEmit_eTokenUtf8Class(FILE *fp, regex_vm_t *vm, int opcode, unsigned int instr) {
    int encoding = RE_VM_FLAG_UTF8_CLASS_ENC_FROM_INSTR(instr);
    fprintf(fp, "%d  enc(%d)",
            RE_VM_DEC_UTF8_INDEX_FROM_INSTR(instr),
            encoding);

    if(RE_VM_FLAG_UTF8_CLASS_INVERT_FROM_INSTR(instr)) {
        fputs(":invert", fp);
    }
    if(encoding >= 3) {
        fprintf(fp, ":high(0x%2.2x):", RE_VM_DEC_UTF8_MIDHIGH_FROM_INSTR(instr));
    }
    fprintf(fp, ":low(0x%2.2x):[", RE_VM_DEC_UTF8_MIDLOW_FROM_INSTR(instr));
    _regexUtf8ClassBitmapEmit(fp, regexVMUtf8ClassEntryGet(vm, RE_VM_DEC_UTF8_INDEX_FROM_INSTR(instr)));
    fputc(']', fp);
}

void _reMetaVMEmit_eTokenCall(FILE *fp, regex_vm_t *vm, int opcode, unsigned int instr) {
#ifdef MOJO_REGEX_VM_DEBUG
    const char *name, *alias;
#endif // MOJO_REGEX_VM_DEBUG
    fprintf(fp, "%d", RE_VM_OP_A_FROM_INSTR(instr));
#ifdef MOJO_REGEX_VM_DEBUG
    name = regexVMSubNameEntryGet(vm, RE_VM_OP_A_FROM_INSTR(instr));
    alias = regexVMSubAliasEntryGet(vm, RE_VM_OP_A_FROM_INSTR(instr));
    if((name != NULL) || (alias != NULL)) {
        fputs("  (", fp);
        if(name != NULL) {
            fprintf(fp, "name(%s)", name);
        }
        if(alias != NULL) {
            fprintf(fp, "%s:alias(%s)", ((name != NULL) ? ":" : ""), alias);
        }
        fputc(')', fp);
    }
#endif // MOJO_REGEX_VM_DEBUG
}

void _reMetaVMEmit_eTokenAssertion(FILE *fp, regex_vm_t *vm, int opcode, unsigned int instr) {
    switch(RE_VM_OP_A_FROM_INSTR(instr)) {
        case RE_VM_FLAG_ASSERT_START_OF_LINE: fputs("(^)", fp); break;
        case RE_VM_FLAG_ASSERT_END_OF_LINE: fputs("($)", fp); break;
        case RE_VM_FLAG_ASSERT_START_OF_WORD: fputs("(\\<)", fp); break;
        case RE_VM_FLAG_ASSERT_END_OF_WORD: fputs("(\\>)", fp); break;
        default: fputs("(unknown)", fp); break;
    }
}

void _reMetaVMEmit_eTokenUtf8Literal(FILE *fp, regex_vm_t *vm, int opcode, unsigned int instr) {
    int c = RE_VM_DEC_UTF8_LITERAL_FROM_INSTR(instr);

    fprintf(fp, "'\\u%s%.4x%s%s",
            ((c > 0xFFFF) ? "{" : ""), c, ((c > 0xFFFF) ? "}" : ""),
            (RE_VM_FLAG_UTF8_LITERAL_INVERT_FROM_INSTR(instr) ? "  invert" : ""));
}

void _reMetaVMEmit_eTokenRange(FILE *fp, regex_vm_t *vm, int opcode, unsigned int instr) {
    fprintf(fp, "{%d,%d}  pc(%d)",
            RE_VM_DEC_RANGE_MIN_FROM_INSTR(instr),
            RE_VM_DEC_RANGE_MAX_FROM_INSTR(instr),
            RE_VM_OP_A_FROM_INSTR(instr));
}

#endif // MOJO_REGEX_COMPILE_IMPLEMENTATION
#ifdef MOJO_REGEX_COMMON_IMPLEMENTATION

/////////////////////////////////////////////////////////////////////////////
// Regex token details VM instruction evaluation handlers
/////////////////////////////////////////////////////////////////////////////

eReEvalResult_t _reVMInstr_eTokenCharLiteral(regex_eval_t *eval, regex_thread_t *thread, unsigned int instr, unsigned char c) {
    int check = RE_VM_DEC_CHAR_FROM_INSTR(instr);

    if((RE_VM_FLAG_CHAR_INVERT_FROM_INSTR(instr) && (check == c)) || (check != c)) {
        return eEvalNoMatch;
    }
    thread->pc++;
    return eEvalMatch;
}

eReEvalResult_t _reVMInstr_eTokenCharClass(regex_eval_t *eval, regex_thread_t *thread, unsigned int instr, unsigned char c) {
    const unsigned int *bitmap;

    if((bitmap = regexVMCharClassEntryGet(eval->vm, RE_VM_OP_A_FROM_INSTR(instr))) == NULL) {
        return eEvalInternalError;
    }
    if(!(bitmap[c / 32] & (1u << (c % 32u)))) {
        return eEvalNoMatch;
    }
    thread->pc++;
    return eEvalMatch;
}

eReEvalResult_t _reVMInstr_eTokenStringLiteral(regex_eval_t *eval, regex_thread_t *thread, unsigned int instr, unsigned char c) {
    const char *str;
    int len;

    if((str = regexVMStringTableEntryGet(eval->vm, RE_VM_OP_A_FROM_INSTR(instr), &len)) == NULL) {
        return eEvalInternalError;
    }
    if(thread->pos == -1) {
        thread->pos = 0;
    }
    if(str[thread->pos] == c) {
        thread->pos++;
        if(thread->pos == len) {
            thread->pc++;
            thread->pos = -1;
            return eEvalMatch;
        }
        return eEvalContinue;
    }
    return eEvalNoMatch;
}

eReEvalResult_t _reVMInstr_eTokenCharAny(regex_eval_t *eval, regex_thread_t *thread, unsigned int instr, unsigned char c) {
    if(RE_VM_FLAG_DOTALL_FROM_INSTR(instr) && (c == '\n')) {
        return eEvalNoMatch;
    }
    if(1) { // TODO - compilation toggle unicode mode
        if(thread->pos == -1) {
            if((thread->pos = _parseUtf8EncodedHighByte((char)c)) == 1) {
                thread->pos = -1;
                thread->pc++;
                return eEvalMatch;
            }
            return eEvalContinue;
        } else if((c & UTF8_LOW_BYTE_MASK) != UTF8_LOW_BYTE_PREFIX) {
            return eEvalNoMatch;
        } else {
            thread->pos--;
            if(thread->pos == 0) {
                thread->pc++;
                thread->pos = -1;
                return eEvalMatch;
            }
            return eEvalContinue;
        }
    } else {
        thread->pc++;
        return eEvalMatch;
    }
}

eReEvalResult_t _reVMInstr_eTokenMatch(regex_eval_t *eval, regex_thread_t *thread, unsigned int instr, unsigned char c) {
    if((eval->anchored) || (RE_VM_FLAG_EOF_FROM_INSTR(instr))) {
        if(eval->pos == eval->len) {
            return eEvalMatch;
        }
    } else if((!eval->anchored) && (!RE_VM_FLAG_EOF_FROM_INSTR(instr))) {
        return eEvalMatch;
    }
    return eEvalNoMatch;
}

eReEvalResult_t _reVMInstr_eTokenSplit(regex_eval_t *eval, regex_thread_t *thread, unsigned int instr, unsigned char c) {
    if(!regexThreadCreate(eval, thread, RE_VM_OP_A_FROM_INSTR(instr), 0)) {
        return eEvalOutOfMem;
    }
    thread->pc = RE_VM_OP_B_FROM_INSTR(instr);
    return eEvalMatch;
}

eReEvalResult_t _reVMInstr_eTokenJmp(regex_eval_t *eval, regex_thread_t *thread, unsigned int instr, unsigned char c) {
    thread->pc = RE_VM_OP_A_FROM_INSTR(instr);
    return eEvalContinue;
}

eReEvalResult_t _reVMInstr_eTokenSave(regex_eval_t *eval, regex_thread_t *thread, unsigned int instr, unsigned char c) {
    int index;

    index = RE_VM_OP_A_FROM_INSTR(instr);
    if(!regexIsIndexGroupStart(index)) {
        // Group end
        thread->subexprs[index] = eval->sp;
        if(RE_VM_FLAG_COMPOUND_FROM_INSTR(instr)) {
            if(!regexThreadCompoundEnd(thread, eval->sp)) {
                return eEvalOutOfMem;
            }
        }
    } else {
        // Group start
        if(thread->subexprs[index] == NULL) {
            thread->subexprs[index] = eval->sp;
        }
        if(RE_VM_FLAG_COMPOUND_FROM_INSTR(instr)) {
            if(!regexThreadCompoundStart(eval, thread, index, eval->sp)) {
                return eEvalOutOfMem;
            }
        }
    }
    thread->pc++;
    return eEvalContinue;
}

eReEvalResult_t _reVMInstr_eTokenUtf8Class(regex_eval_t *eval, regex_thread_t *thread, unsigned int instr, unsigned char c) {
    const unsigned int *bitmap;

    // TODO

    if((bitmap = regexVMUtf8TableEntryGet(eval->vm, instr[1])) == NULL) {
        return eEvalInternalError;
    }
    if(!utf8ClassBitmapCheck(bitmap, c)) {
        return eEvalNoMatch;
    }
    thread->pc++;
    return eEvalMatch;
}

eReEvalResult_t _reVMInstr_eTokenCall(regex_eval_t *eval, regex_thread_t *thread, unsigned int instr, unsigned char c) {
    thread->call_stack[thread->call_idx] = thread->pc + 1;
    thread->call_idx++;
    thread->pc = RE_VM_OP_A_FROM_INSTR(instr);
    return eEvalContinue;
}

eReEvalResult_t _reVMInstr_eTokenReturn(regex_eval_t *eval, regex_thread_t *thread, unsigned int instr, unsigned char c) {
    if(thread->call_idx == 0) {
        return eEvalInternalError;
    }
    thread->pc = thread->call_stack[thread->call_idx];
    thread->call_idx--;
    return eEvalContinue;
}

eReEvalResult_t _reVMInstr_eTokenByte(regex_eval_t *eval, regex_thread_t *thread, unsigned int instr, unsigned char c) {
    thread->pc++;
    return eEvalMatch;
}

eReEvalResult_t _reVMInstr_eTokenAssertion(regex_eval_t *eval, regex_thread_t *thread, unsigned int instr, unsigned char c) {
    switch(RE_VM_OP_A_FROM_INSTR(instr)) {
        default:
            return eEvalInternalError;
        case RE_VM_FLAG_ASSERT_START_OF_LINE:
            if(!eval->start_of_line) {
                return eEvalNoMatch;
            }
            thread->pc++;
            return eEvalContinue;
        case RE_VM_FLAG_ASSERT_END_OF_LINE:
            if(!eval->end_of_line) {
                return eEvalNoMatch;
            }
            thread->pc++;
            return eEvalContinue;
        case RE_VM_FLAG_ASSERT_END_OF_WORD:
            if((!_regexIsWordBoundaryChar(c)) && (thread->last_was_word)) {
                thread->pc++;
                return eEvalMatch;
            }
            return eEvalNoMatch;
        case RE_VM_FLAG_ASSERT_START_OF_WORD:
            if((_regexIsWordBoundaryChar(c)) && (!thread->last_was_word)) {
                thread->pc++;
                return eEvalMatch;
            }
            return eEvalNoMatch;
    }
}

eReEvalResult_t _reVMInstr_eTokenUtf8Literal(regex_eval_t *eval, regex_thread_t *thread, unsigned int instr, unsigned char c) {
    int codepoint = RE_VM_DEC_UTF8_LITERAL_FROM_INSTR(instr);
    int bytes = _parseUtf8EncodingByteLen(codepoint);

    if(thread->pos == -1) {
        thread->pos = 1;
    } else {
        thread->pos++;
    }
    if(!_parseUtf8CompareEncodingByte(codepoint, c, RE_VM_THREAD_INVERT_MASK_POS(thread->pos))) {
        if(!RE_VM_FLAG_UTF8_LITERAL_INVERT_FROM_INSTR(instr)) {
            thread->pos = -1;
            return eEvalNoMatch;
        }
        thread->pos = (int)(((unsigned)thread->pos) | RE_VM_THREAD_INVERT_UTF8_FLAG);
    }
    if(thread->pos == bytes) {
        if(RE_VM_FLAG_UTF8_LITERAL_INVERT_FROM_INSTR(instr)) {
            if(!RE_VM_THREAD_INVERT_MASK_FLAG(thread->pos)) {
                thread->pos = -1;
                return eEvalNoMatch;
            }
        }
        thread->pos = -1;
        thread->pc++;
        return eEvalMatch;
    }
    return eEvalContinue;
}

eReEvalResult_t _reVMInstr_eTokenRange(regex_eval_t *eval, regex_thread_t *thread, unsigned int instr, unsigned char c) {
    int count, min, max;

    if((thread->range_idx > 0) && (thread->pc == RE_VM_THREAD_RANGE_PC_FROM_STACK(thread))) {
        // This is a continuing range value
        min = RE_VM_DEC_RANGE_MIN_FROM_INSTR(instr);
        max = RE_VM_DEC_RANGE_MAX_FROM_INSTR(instr);
        count = RE_VM_THREAD_RANGE_COUNT_FROM_STACK(thread);
        count++;
        if(count == max) {
            thread->range_idx--;
            thread->pc = RE_VM_OP_A_FROM_INSTR(instr);
            return eEvalMatch;
        } else if(count >= min) {
            if(!regexThreadCreate(eval, thread, RE_VM_OP_A_FROM_INSTR(instr), 1)) {
                return eEvalOutOfMem;
            }
        }
        thread->pc++;
        return eEvalMatch;
    } else {
        // This is the start of a range
        thread->range_idx++;
        RE_VM_THREAD_RANGE_PC_TO_STACK(thread, thread->pc);
        RE_VM_THREAD_RANGE_COUNT_CLEAR_STACK(thread);
        thread->pc++;
        return eEvalMatch;
    }
}

#endif // MOJO_REGEX_COMMON_IMPLEMENTATION
#ifdef MOJO_REGEX_COMPILE_IMPLEMENTATION

/////////////////////////////////////////////////////////////////////////////
// Regex token details meta handler functions
/////////////////////////////////////////////////////////////////////////////

int regexVerifyTokenDetails(void) {
    int k;

    for(k = 0; ((_regexTokenDetails[k].token != eTokenUnknown) &&
                (_regexTokenDetails[k].token == k)); k++);

    return ((_regexTokenDetails[k].token == eTokenUnknown) &&
            (_regexTokenDetails[k].token == k));
}

int regexTokenStrEmit(FILE *fp, regex_build_ctx_t *build_ctx,
                      regex_token_t *token, unsigned int emitFlags) {
    if((!(emitFlags & (unsigned)eReEmitConcat)) &&
       (token != NULL) &&
       (token->tokenType == eTokenConcatenation)){
        return 0;
    }
    if(emitFlags & (unsigned)eReEmitAddr) {
        fprintf(fp, "0x%4.4X:", (unsigned int)(((uintptr_t)token) & 0xFFFFu));
    }
    if(token == NULL) {
        fprintf(fp, "NULL(");
    } else if((token->tokenType < eTokenFirstToken) || (token->tokenType >= eTokenUnknown)) {
        fprintf(fp, "INVALID(%d", token->tokenType);
    } else {
        fprintf(fp, "%s(", _regexTokenDetails[token->tokenType].name);
        if(emitFlags & (unsigned)eReEmitAttrs) {
            if(_regexTokenDetails[token->tokenType].emitToken != NULL) {
                _regexTokenDetails[token->tokenType].emitToken(fp, build_ctx, token);
            }
        }
    }
    fputc(')', fp);
    if(emitFlags & (unsigned)eReEmitDFA) {
        if((token == NULL) || (token->out_a == NULL)) {
            fputs("{------}", fp);
        } else {
            fprintf(fp, "{0x%4.4X}", (int)((uintptr_t)(token->out_a) & 0xFFFFu));
        }
        if((token == NULL) || (token->out_b == NULL)) {
            fputs("{------}", fp);
        } else {
            fprintf(fp, "{0x%4.4X}", (int)((uintptr_t)(token->out_b) & 0xFFFFu));
        }
    }
    if(emitFlags & (unsigned)eReEmitPC) {
        if(token == NULL) {
            fputs("[]", fp);
        } else {
            fprintf(fp, "[%d]", token->pc);
        }
    }
    return 1;
}

void regexTokenStrStackEmit(FILE *fp, regex_build_ctx_t *build_ctx,
                            regex_token_t *token, unsigned int emitFlags) {
    int idx;

    for(idx = 0; token != NULL; token = token->next, idx++) {
        if((!(emitFlags & (unsigned)eReEmitConcat)) &&
           (token->tokenType == eTokenConcatenation)){
            continue;
        }
        fprintf(fp, "%d - ", idx);
        if(regexTokenStrEmit(fp, build_ctx, token, emitFlags)) {
            fputc('\n', fp);
        }
    }
}

void regexTokenWalkTree(regex_token_t *token, int val) {
    if((token == NULL) || (token->pc == val)) {
        return;
    }
    token->pc = val;
    if(token->next != NULL) {
        regexTokenWalkTree(token->next, val);
    }
    if(token->out_a != NULL) {
        regexTokenWalkTree(token->out_a, val);
    }
    if(token->out_b != NULL) {
        regexTokenWalkTree(token->out_b, val);
    }
}

void _regexTokenStrDFAEmit(FILE *fp, regex_build_ctx_t *build_ctx,
                           regex_token_t *token, int indent, const char *prefix) {
    int pad = indent * 4;
    if(pad) {
        pad++;
    }
    fprintf(fp, "%*.*s(%s) ", pad, pad, "", prefix);
    regexTokenStrEmit(fp, build_ctx, token, eReEmitVerboseDFA);
    fputc('\n', fp);
    if(token->out_a != NULL) {
        _regexTokenStrDFAEmit(fp, build_ctx, token->out_a, indent + 1, "out_a");
    }
    if(token->out_b != NULL) {
        _regexTokenStrDFAEmit(fp, build_ctx, token->out_b, indent + 1, "out_b");
    }
}

void regexTokenStrDFAEmit(FILE *fp, regex_build_ctx_t *build_ctx, regex_token_t *token) {
    if(token != NULL) {
        _regexTokenStrDFAEmit(fp, build_ctx, token, 0, "root");
    }
}

#endif // MOJO_REGEX_COMMON_IMPLEMENTATION
#ifdef MOJO_REGEX_COMMON_IMPLEMENTATION

void regexVMInstrEmit(FILE *fp, regex_vm_t *vm, int pc) {
    int opcode;

    if((vm == NULL) || (pc < 0) || (pc >= vm->size)) {
        return;
    }
    opcode = (int)(((unsigned)(vm->program[pc])) & RE_VM_OPCODE_MASK);
    fprintf(fp, "%4d  %*s", pc, MAX_TOKEN_INSTR_NAME_LEN, _regexTokenDetails[opcode].instr);
    if(_regexTokenDetails[opcode].emitVMInstr != NULL) {
        fputs("  ", fp);
        _regexTokenDetails[opcode].emitVMInstr(fp, vm, opcode, vm->program[pc]);
    }
    fputc('\n', fp);
}

void regexVMProgramEmit(FILE *fp, regex_vm_t *vm, int pc, int primary) {
    int end = vm->size;
#ifdef MOJO_REGEX_VM_DEBUG
    int sub = 0, jmp, pos;
    if(primary) {
        for(pos = 0; pos < vm->size; pos++) {
            if((vm->program[pos] & RE_VM_OPCODE_MASK) == eTokenCall) {
                jmp = RE_VM_OP_A_FROM_INSTR(vm->program[pos]);
                if(jmp <= pc) {
                    // pc is within a subroutine, track the most likely candidate
                    // found so far
                    sub = jmp;
                } else if((sub != 0) && (jmp > pc)) {
                    // pc was determined to be within a subroutine, and we've now
                    // identified the start of the NEXT subroutine after pc,
                    // which marks our endpoint.
                    end = jmp;
                    break;
                }
            }
        }
    }
#endif // MOJO_REGEX_VM_DEBUG
    for(; pc < end; pc++) {
        regexVMInstrEmit(fp, vm, pc);
    }
}

int regexVMFindSubroutinePCByName(regex_vm_t *vm, const char *name) {
#ifdef MOJO_REGEX_VM_DEBUG
    int idx;

    for(idx = 0; idx < vm->sub_name_tbl_size; idx++) {
        if((!strcasecmp(vm->sub_name_table[idx].name, name)) ||
           (!strcasecmp(vm->sub_name_table[idx].alias, name))) {
            return vm->sub_name_table[idx].pc;
        }
    }
#endif // MOJO_REGEX_VM_DEBUG
    return 0;
}

#endif // MOJO_REGEX_COMMON_IMPLEMENTATION
#ifdef MOJO_REGEX_COMPILE_IMPLEMENTATION

int regexTokenIsTerminal(regex_token_t *token, int preceeding) {
    if((token == NULL) || (token->tokenType < 0) || (token->tokenType > eTokenUnknown)) {
        if(token == NULL) {
            INTERNAL_ERROR("regexTokenIsTerminal called with NULL token");
        } else {
            INTERNAL_ERROR("regexTokenIsTerminal called with invalid token type");
        }
        return 0;
    }
    return ((_regexTokenDetails[token->tokenType].terminal == eReTokTerminal) ||
            (preceeding && (_regexTokenDetails[token->tokenType].terminal == eReTokPreceeding)) ||
            (!preceeding && (_regexTokenDetails[token->tokenType].terminal == eReTokNotPreceeding)));
}

eRegexTokenPriority_t regexGetTokenTypePriority(eRegexToken_t tokenType) {
    if((tokenType < 0) || (tokenType > eTokenUnknown)) {
        INTERNAL_ERROR("regexGetTokenTypePriority called with invalid token type");
        return 0;
    }
    return _regexTokenDetails[tokenType].priority;
}

int regexGetOperatorArity(regex_token_t *token) {
    if((token == NULL) || (token->tokenType < 0) || (token->tokenType > eTokenUnknown)) {
        if(token == NULL) {
            INTERNAL_ERROR("regexGetOperatorArity called with NULL token");
        } else {
            INTERNAL_ERROR("regexGetOperatorArity called with invalid token type");
        }
        return 0;
    }
    return _regexTokenDetails[token->tokenType].arity;
}

/////////////////////////////////////////////////////////////////////////////
// Regex compilation handling functions
/////////////////////////////////////////////////////////////////////////////

// Ptrlist handlers /////////////////////////////////////////////////////////

regex_ptrlist_t *_regexPtrlistCreate(regex_build_ctx_t *context,
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

regex_ptrlist_t *_regexPtrlistAppend(regex_ptrlist_t *lista, regex_ptrlist_t *listb) {
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
int _regexPtrlistPatch(regex_build_ctx_t *context, regex_ptrlist_t **list, regex_token_t *token, int no_jumps) {
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

void _regexPtrListFree(regex_build_ctx_t *context, regex_ptrlist_t *list) {
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

// Token assembly handlers //////////////////////////////////////////////////

regex_token_t *_regexTokenAlloc(regex_build_ctx_t *context, eRegexToken_t tokenType,
                                const char *str, const unsigned int *bitmap,
                                int len) {
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
        token->str = (char *)str;
    } else if(bitmap != NULL) {
        token->bitmap = (unsigned int *)bitmap;
        token->len = len;
    }
    return token;
}

#define DEF_TOKEN_LOOP_SENTINAL     -2

void _regexTokenLoopBreaker(regex_token_t *token) {
    if((token == NULL) || (token->pc == DEF_TOKEN_LOOP_SENTINAL)) {
        return;
    }
    token->pc = DEF_TOKEN_LOOP_SENTINAL;
    if(token->next != NULL) {
        if(token->next->pc == DEF_TOKEN_LOOP_SENTINAL) {
            token->next = NULL;
        } else {
            _regexTokenLoopBreaker(token->next);
        }
    }
    if(token->out_a != NULL) {
        if(token->out_a->pc == DEF_TOKEN_LOOP_SENTINAL) {
            token->out_a = NULL;
        } else {
            _regexTokenLoopBreaker(token->out_a);
        }
    }
    if(token->out_b != NULL) {
        if(token->out_b->pc == DEF_TOKEN_LOOP_SENTINAL) {
            token->out_b = NULL;
        } else {
            _regexTokenLoopBreaker(token->out_b);
        }
    }
}

void _regexTokenDestroy(regex_build_ctx_t *context, regex_token_t *token, int full_stack) {
    regex_token_t *next;

    if(token == NULL) {
        return;
    }

    if(full_stack) {
        _regexTokenLoopBreaker(token);
    }

    for(; token != NULL; token = next) {
        next = token->next;
        switch(token->tokenType) {
            case eTokenCharClass:
            case eTokenStringLiteral:
            case eTokenUtf8Class:
            case eTokenSubExprStart:
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

regex_token_t *_regexTokenBaseCreate(regex_build_t *build,
                                     eRegexToken_t tokenType,
                                     const void *str, const void *ptr,
                                     int len) {
    regex_token_t *token, *concat;

    if((token = _regexTokenAlloc(build->context, tokenType, str, ptr, len)) == NULL) {
        return NULL;
    }

    if((build->expr->tokens != NULL) && regexTokenIsTerminal(token, 0) && regexTokenIsTerminal(build->expr->tokens, 1)) {
        // Two adjacent terminals have an implicit concatenation
        if((concat = _regexTokenAlloc(build->context, eTokenConcatenation, NULL, NULL, 0)) == NULL) {
            _regexTokenDestroy(build->context, token, 0);
            return NULL;
        }
        if(!_regexExprTokenPush(build->expr, concat)) {
            return NULL;
        }
    }
    if(!_regexExprTokenPush(build->expr, token)) {
        return NULL;
    }
    return token;
}

int regexBuildConcatChar(regex_build_t *build, int c, int invert) {
    regex_token_t *token;

    if((token = _regexTokenBaseCreate(build, eTokenCharLiteral, NULL, NULL, 0)) == NULL) {
        return 0;
    }
    token->c = c;
    token->jump = 0x3FFF;
    token->flags = (invert ? RE_TOK_FLAG_INVERT : 0);
    return 1;
}

int regexBuildConcatCharClass(regex_build_t *build, unsigned int *bitmap) {
    return _regexTokenBaseCreate(build, eTokenCharClass, NULL, bitmap, 32) != NULL;
}

int regexBuildConcatString(regex_build_t *build, const char *str, int len) {
    char *ptr;

    if((ptr = _regexStrdup(str, len)) == NULL) {
        return 0;
    }
    return _regexTokenBaseCreate(build, eTokenStringLiteral, ptr, NULL, len) != NULL;
}

int regexBuildConcatCharAny(regex_build_t *build, int dot_all) {
    regex_token_t *token;

    if((token = _regexTokenBaseCreate(build, eTokenCharAny, NULL, NULL, 0)) == NULL) {
        return 0;
    }
    token->flags = (dot_all ? RE_TOK_FLAG_DOT_ALL : 0);
    return 1;
}

int regexBuildConcatUtf8Class(regex_build_t *build, int encoding, int mid_high,
                              int mid_low, int invert, unsigned int *bitmap) {
    regex_token_t *token;

    if((token = _regexTokenBaseCreate(build, eTokenUtf8Class, NULL, bitmap, 8)) == NULL) {
        return 0;
    }
    token->flags = (invert ? RE_TOK_FLAG_INVERT : 0);
    token->mid_low = (short)mid_low;
    token->mid_high = (short)mid_high;
    token->encoding = (short)encoding;
    return 1;
}

int regexBuildConcatCall(regex_build_t *build, int sub_index) {
    regex_token_t *token;

    if((token = _regexTokenBaseCreate(build, eTokenCall, NULL, NULL, 0)) == NULL) {
        return 0;
    }
    token->sub_index = (short)sub_index;
    return 1;
}

int regexBuildConcatByte(regex_build_t *build) {
    return _regexTokenBaseCreate(build, eTokenByte, NULL, NULL, 0) != NULL;
}

int regexBuildConcatAssertion(regex_build_t *build, int assertion) {
    regex_token_t *token;

    if((token = _regexTokenBaseCreate(build, eTokenAssertion, NULL, NULL, 0)) == NULL) {
        return 0;
    }
    token->flags = (unsigned int)assertion;
    return 1;
}

int regexBuildConcatUtf8Literal(regex_build_t *build, int c, int invert) {
    regex_token_t *token;

    if((token = _regexTokenBaseCreate(build, eTokenUtf8Literal, NULL, NULL, 0)) == NULL) {
        return 0;
    }
    token->c = c;
    token->flags = (invert ? RE_TOK_FLAG_INVERT : 0);
    return 1;
}

int regexIsIndexGroupStart(int index) {
    return !(((unsigned)index) & 1u);
}

int regexLogicalGroupToIndex(int group, int start) {
    group *= 2;
    if(!start) {
        group++;
    }
    return group;
}

int regexIndexToLogicalGroup(int index) {
    if(((unsigned)index) & 1u) {
        // Odd numbers are a subexpression close index
        index--;
    }
    return index / 2;
}

int regexBuildGroupStart(regex_build_t *build, int expr_inline, const char *name, int name_len, int no_capture,
                         int compound, int subroutine_index) {
    regex_token_t *token;

    if((token = _regexTokenBaseCreate(build, eTokenSubExprStart, name, NULL, name_len)) == NULL) {
        return 0;
    }

    if(subroutine_index > 0) {
        token->flags |= RE_TOK_FLAG_SUBROUTINE;
        token->sub_index = (short)subroutine_index;
    }
    if(compound) {
        token->flags |= RE_TOK_FLAG_COMPOUND;
    }
    if(no_capture) {
        token->flags |= RE_TOK_FLAG_NO_CAPTURE;
    }
    if(expr_inline) {
        build->context->group_count++;
        token->group = regexLogicalGroupToIndex(build->context->group_count, 1);
    }

    if(!regexExprCreate(build)) {
        return 0;
    }

    return 1;
}

eReExprStatus_t regexBuildGroupEnd(regex_build_t *build) {
    eReExprStatus_t status;
    regex_token_t *group, *start, *end;

    if((status = _regexExprFinalize(build->expr)) != eReExprSuccess) {
        return status;
    }
    if(build->expr->parent == NULL) {
        return eReExprInvalidClose;
    }
    group = build->expr->tokens;
    regexExprDestroy(build);

    if(((start = _regexExprOperandPop(build->expr)) == NULL) ||
       (start->tokenType != eTokenSubExprStart)) {
        return eReExprMissingOpen;
    }

    if(!(start->flags & RE_TOK_FLAG_NO_CAPTURE)) {
        start->tokenType = eTokenSave;

        if((end = _regexTokenAlloc(build->context, eTokenSave, NULL, NULL, 0)) == NULL) {
            return eReExprOutOfMemory;
        }
        if((end->ptrlist = _regexPtrlistCreate(build->context, end, eRePtrOutA)) == NULL) {
            return eReExprOutOfMemory;
        }
        end->flags = start->flags;
        end->out_b = start;
        end->group = regexLogicalGroupToIndex(regexIndexToLogicalGroup(start->group), 0);

        _regexPtrlistPatch(build->context, &(start->ptrlist), group, 0);
        _regexPtrlistPatch(build->context, &(group->ptrlist), end, 0);
        start->ptrlist = end->ptrlist;
        group->ptrlist = NULL;
        end->ptrlist = NULL;
        _regexExprOperandPush(build->expr, start);
    } else if(start->flags & RE_TOK_FLAG_SUBROUTINE) {
        // No capture subroutine
        start->tokenType = eTokenCall;
        regexSubroutineExprSet(build->context, start->sub_index, group);
        _regexExprOperandPush(build->expr, start);
    } else {
        // No capture, not a subroutine
        _regexExprOperandPush(build->expr, group);
    }
    return eReExprSuccess;
}

eReExprStatus_t regexBuildWrapSubexpression(regex_build_t *build, const char *name, int name_len,
                                            int no_capture, int compound, int subroutine, int index) {
    eReExprStatus_t status;
    regex_token_t *token;

    if((status = _regexExprFinalize(build->expr)) != eReExprSuccess) {
        return status;
    }
    token = _regexExprOperandPop(build->expr);
    if(!regexBuildGroupStart(build, 0, name, RE_STR_NULL_TERM, no_capture, compound, index)) {
        return eReExprOutOfMemory;
    }
    _regexExprOperandPush(build->expr, token);
    if((status = regexBuildGroupEnd(build)) != eReExprSuccess) {
        return status;
    }
    return eReExprSuccess;
}

int regexBuildQuantify(regex_build_t *build, eReQuantifier_t quantifier) {
    regex_token_t *token;
    eRegexToken_t type;

    switch(quantifier) {
        case eReQuantifyZeroOrOne: type = eTokenZeroOrOne; break;
        case eReQuantifyZeroOrMany: type = eTokenZeroOrMany; break;
        case eReQuantifyOneOrMany: type = eTokenOneOrMany; break;
        default: return 0;
    }
    if((token = _regexTokenAlloc(build->context, type, NULL, NULL, 0)) == NULL) {
        return 0;
    }
    if(!_regexExprTokenPush(build->expr, token)) {
        return 0;
    }
    return 1;
}

int regexBuildRange(regex_build_t *build, int min, int max) {
    regex_token_t *token;
    if((token = _regexTokenAlloc(build->context, eTokenRange, NULL, NULL, 0)) == NULL) {
        return 0;
    }
    token->min = min;
    token->max = max;
    if(!_regexExprTokenPush(build->expr, token)) {
        return 0;
    }
    return 1;
}

int regexBuildAlternative(regex_build_t *build) {
    regex_token_t *token;

    if((token = _regexTokenAlloc(build->context, eTokenAlternative, NULL, NULL, 0)) != NULL) {
        return 0;
    }
    if(!_regexExprTokenPush(build->expr, token)) {
        return 0;
    }
    return 1;
}

int regexCreateTokenJmp(regex_build_ctx_t *context, regex_token_t **token) {
    if((*token = _regexTokenAlloc(context, eTokenJmp, NULL, NULL, 0)) == NULL) {
        return 0;
    }
    return 1;
}

// TODO: match, return


/////////////////////////////////////////////////////////////////////////////
// Pattern parsing handler functions
/////////////////////////////////////////////////////////////////////////////

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
    INTERNAL_ERROR("invalid call to _parseHexValueGet() - non hex digit");
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

static parseChar_t _parseGetNextPatternChar(const char **pattern, int allow_brace,
                                            const char **id, int *len) {
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
// String parsing and escape functions
/////////////////////////////////////////////////////////////////////////////

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

    if((codepoint = _parseUtf8DecodeSequence(str)) != -1) {
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

/////////////////////////////////////////////////////////////////////////////
// Character class parsing handlers
/////////////////////////////////////////////////////////////////////////////

// Character class handling is performed with a bitmap mask of the character
// ranges. For ASCII data, a single eTokenCharClass token represents a class,
// with the character bitmap stored independently in the VM, allowing for
// efficient reuse of the class pattern. For UTF8 data, a mini-DFA is generated
// for the codepoint ranges, using multiple splits to differentiate mid and high
// bytes, and eTokenCharLiterals for the initial byte in 4 byte encodings. When
// the VM is generated, UTF8 classes are factored out into subroutines.

// For utf8 encoded characters, trailing bytes have 6 bits of character data,
// as the leading bits identify the byte as part of a multibyte codepoint.
// This simplifies the unicode char class table entries to only need 64 bits
// (8 bytes) to represent each byte.

// |  high byte  | midhigh byte| midlow byte | low byte |
// | 4 byte lead | 3 byte lead | 2 byte lead | low byte |
//                             |<---- 2 byte group ---->|
//               |<------------ 3 byte group ---------->|
// |<------------------ 4 byte group ------------------>|

static int _regexUtf8SubroutineCreate(regex_build_t *build, regex_utf8_range_t *utf8range,
                                      const char *name, int name_len, int invert) {
    unsigned int bitmap[32];
    int use_char_class = 0;
    int next_codepoint = 128;
    regex_utf8_segment_t segment;
    regex_utf8_range_t *walk;
    int k;

    memset(bitmap, 0, 32);

    for(walk = utf8range; walk != NULL; walk = walk->next) {
        if((walk->start <= 127) && (walk->end <= 127)) {
            for(k = walk->start; ((k <= 127) && (k <= walk->end)); k++) {
                bitmap[k / 32u] |= (1u << (k % 32u));
            }
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

regex_utf8_range_t *_regexUtf8RangeCreate(regex_build_ctx_t *context, int start, int end) {
    regex_utf8_range_t *range;

    if((range = context->utf8_range_pool) == NULL) {
        if((range = _regexAlloc(sizeof(regex_utf8_range_t), _regexMemContext)) == NULL) {
            return NULL;
        }
    } else {
        context->utf8_range_pool = range->next;
    }
    range->next = NULL;
    range->start = start;
    range->end = end;
}

void _regexUtf8RangeDestroy(regex_build_ctx_t *context, regex_utf8_range_t *range, int free_all) {
    regex_utf8_range_t *next;

    do {
        next = range->next;
        if(context != NULL) {
            range->next = context->utf8_range_pool;
            context->utf8_range_pool = range;
        } else {
            _regexDealloc(range, _regexMemContext);
        }
    } while((free_all) && (next != NULL));
}

int _parseCreateUtf8Range(regex_build_t *build, regex_utf8_range_t **sequence, int start, int end) {
    regex_utf8_range_t *range, *walk, *last, *next;
    int swap;

    if(start > end) {
        swap = start;
        start = end;
        end = swap;
    }
    if((range = _regexUtf8RangeCreate(build->context, start, end)) == NULL) {
        return 0;
    }

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

                    _regexUtf8RangeDestroy(build->context, range, 0);

                    // Resolve any resultant range bridges
                    range = walk;
                    for(; walk != NULL; walk = next) {
                        next = walk->next;
                        if(range->end >= walk->start) {
                            if(walk->end > range->end) {
                                range->end = walk->end;
                            }
                            range->next = walk->next;
                            _regexUtf8RangeDestroy(build->context, walk, 0);
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
        // Logically, last will NEVER be NULL here, regardless of what the IDE claims
        last->next = range;
    }
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

// Parse a class pattern, and generate a eTokenCharClass or eTokenUtf8Class
// sequence. By default, uses the pattern in tokenizer. If class_pattern is
// defined, it will use it as the pattern instead of the tokenizer pattern. If
// class_pattern and name are defined, then name will be associated with the
// class_pattern in diagnostic output. If class_pattern is not defined, but
// name is defined, then it is assumed that name is referencing a utf8 property
// class, and an attempt will be made to lookup the class pattern in the
// registered unicode classes.
static int _parseCharClassAndCreateToken(regex_build_t *build, const char *class_pattern,
                                         const char *name, int name_len, int invert) {
    unsigned int bitmap[8], *ptr;
    parseChar_t c;
    regex_utf8_range_t *utf8range = NULL;
    int range = 0;
    int last = 0;
    int k;
    regex_pattern_t pattern;
    int index;
    regex_token_t *token;
    const char *sub_name;
    const char *sub_alias;

    // TODO - re-evaluate character class interactions with subroutines
    if((name != NULL) && (class_pattern == NULL)) {
        if((index = regexSubroutineLookup(build->context, DEF_UTF8_CLASS_SUB_PREFIX, name, name_len)) > 0) {
            // Found the class in the subroutine index
            if(!regexBuildConcatCall(build, index)) {
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
        // TODO - build the class pattern
        token = NULL;
        switch(regexSubroutineRegister(build->context, DEF_UTF8_CLASS_SUB_PREFIX,
                                       sub_name, sub_alias, token)) {
            case eReSubMissingName:
            case eReSubMissingExpr:
                build->status = eCompileInternalError;
                INTERNAL_ERROR("failed to register subroutine for utf8 char class");
                return 0;
            case eReSubCollision:
                build->status = eCompileSubroutineNameCollision;
                return 0;
            case eReSubOutOfMemory:
                build->status = eCompileOutOfMem;
                return 0;
            default:
                break;
        }
    }

    if(class_pattern != NULL) {
        if(!regexPatternCreate(build, class_pattern, RE_STR_NULL_TERM)) {
            build->status = eCompileOutOfMem;
            return 0;
        }
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
                    if(!_parseCharClassBitmapSet(build, &utf8range, bitmap, last)) {
                        build->status = eCompileOutOfMem;
                        return 0;
                    }
                    range = 1;
                } else if(range == 1) {
                    if(c.state != eRegexPatternEscapedChar && c.c == '-') {
                        range = 2;
                    } else {
                        last = c.c;
                        if(!_parseCharClassBitmapSet(build, &utf8range, bitmap, last)) {
                            build->status = eCompileOutOfMem;
                            return 0;
                        }
                    }
                } else {
                    if(!_parseCharClassBitmapRangeSet(build, &utf8range, bitmap, last, c.c)) {
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
            _regexUtf8RangeDestroy(build->context, utf8range, 1);
            return 0;
        }

        _regexUtf8RangeDestroy(build->context, utf8range, 1);
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

        if(!regexBuildConcatCharClass(build, (unsigned int *)ptr)) {
            build->status = eCompileOutOfMem;
            return 0;
        }

        if(name != NULL) {
            // TODO - associate the name with this class, for diagnostic output
        }
    }

    if(class_pattern != NULL) {
        regexPatternDestroy(build);
    }

    return 1;
}

/////////////////////////////////////////////////////////////////////////////
// Lexer - tokenizes the pattern
/////////////////////////////////////////////////////////////////////////////

static eRegexCompileStatus_t _regexTokenizePattern(regex_build_t *build) {
    const char *class;
    parseChar_t c;
    parse_str_t str_count;
    int len;
    const char *str;
    const char *name;
    int name_len;
    unsigned int flags;
    int index;

    if(build->pattern == NULL) {
        return eCompileMissingPattern;
    }

    // Parse the regex pattern into a sequence of tokens (operators and operands)
    // The output of this stage is a sequence of lexical tokens in infix form

    // Loop through the pattern until we've handled it all
    for(; *(build->pattern->pattern) != '\0';) {
        // Get the next character in the pattern. The helper function assists
        // in disambiguating escaped characters.
        c = _parseGetNextPatternChar(&(build->pattern->pattern), 0, &str, &len);

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
                        if(!regexBuildConcatCharAny(build, (int)((unsigned)(build->context->flags) & RE_FLAG_DOT_ALL))) {
                            return eCompileOutOfMem;
                        }
                        continue;

                    case '[': // Meta, char class
                        if(!_parseCharClassAndCreateToken(build, NULL, NULL, 0, 0)) {
                            return build->status;
                        }
                        continue;

                    case '?': // Meta, zero or one quantifier
                        if(!regexBuildQuantify(build, eReQuantifyZeroOrOne)) {
                            return eCompileOutOfMem;
                        }
                        continue;

                    case '*': // Meta, kleene star, zero or many quantifier
                        if(!regexBuildQuantify(build, eReQuantifyZeroOrMany)) {
                            return eCompileOutOfMem;
                        }
                        continue;

                    case '+': // Meta, one or many quantifier
                        if(!regexBuildQuantify(build, eReQuantifyOneOrMany)) {
                            return eCompileOutOfMem;
                        }
                        continue;

                    case '|': // Meta, alternative operator
                        if(!regexBuildAlternative(build)) {
                            return eCompileOutOfMem;
                        }
                        continue;

                    case '(':
                        // Grouped subexpression, complex operator, resolves to a compound operand
                        // Speculative subexpression number. If non-capturing, will NOT actually
                        // increase the group count.
                        flags = 0;
                        index = 0;
                        name_len = 0;
                        name = NULL;
                        // Check for group meta modifiers
                        while(*(build->pattern->pattern) == '?') {
                            build->pattern->pattern++;
                            if(_parseCheckNextPatternChar(&(build->pattern->pattern), 'i')) {
                                // Case insensitive matching
                                flags |= RE_TOK_FLAG_CASE_INS;
                            } else if(_parseCheckNextPatternChar(&(build->pattern->pattern), 'P')) {
                                // Named sub expression
                                if(flags & (RE_TOK_FLAG_NAMED | RE_TOK_FLAG_NO_CAPTURE)) {
                                    return eCompileConflictingAttrs;
                                }
                                if(_parseCheckIdentifier(build->pattern->pattern, '<', '>', &name, &name_len) != eRegexPatternIdOk) {
                                    return eCompileMalformedSubExprName;
                                }
                                build->pattern->pattern += name_len + 2;
                                flags |= RE_TOK_FLAG_NAMED;
                            } else if(_parseCheckNextPatternChar(&(build->pattern->pattern), ':')) {
                                // Non-capturing subexpression
                                if(flags & (RE_TOK_FLAG_COMPOUND | RE_TOK_FLAG_NAMED)) {
                                    return eCompileConflictingAttrs;
                                }
                                flags |= RE_TOK_FLAG_NO_CAPTURE;
                            } else if(_parseCheckNextPatternChar(&(build->pattern->pattern), '*')) {
                                // Compound capturing subexpression
                                if(flags & (RE_TOK_FLAG_NO_CAPTURE)) {
                                    return eCompileConflictingAttrs;
                                }
                                flags |= RE_TOK_FLAG_COMPOUND;
                            } else if(_parseCheckNextPatternChar(&(build->pattern->pattern), 'R')) {
                                // Named subroutine
                                if(flags & (RE_TOK_FLAG_SUBROUTINE)) {
                                    return eCompileConflictingAttrs;
                                }
                                if(_parseCheckIdentifier(build->pattern->pattern, '<', '>', &str, &len) != eRegexPatternIdOk) {
                                    return eCompileMalformedSubExprName;
                                }
                                build->pattern->pattern += len + 2;
                                if(!(index = regexSubroutineCreate(build->context))) {
                                    return eCompileOutOfMem;
                                }
                                switch(regexSubroutineName(build->context, index, NULL, str, len)) {
                                    case eReSubCollision:
                                        return eCompileSubroutineNameCollision;
                                    case eReSubOutOfMemory:
                                        return eCompileOutOfMem;
                                    case eReSubOk:
                                        break;
                                    default:
                                        INTERNAL_ERROR("unanticipated response from regexSubroutineName() in _regexTokenizePattern()");
                                        return eCompileInternalError;
                                }
                                flags |= RE_TOK_FLAG_SUBROUTINE;
                            } else {
                                return eCompileUnsupportedMeta;
                            }
                        }
                        if(!(flags & RE_TOK_FLAG_NO_CAPTURE)) {
                            build->context->group_count++;
                        }
                        if(!regexBuildGroupStart(build,
                                                 ((flags & RE_TOK_FLAG_NO_CAPTURE) ? 0 : 1),
                                                 name, name_len,
                                                 (int)(flags & RE_TOK_FLAG_NO_CAPTURE),
                                                 (int)(flags & RE_TOK_FLAG_COMPOUND),
                                                 index)) {
                            return eCompileOutOfMem;
                        }
                        continue;

                    case ')': // Meta, end of subexpression (group)
                        // This should only be encountered in a recursive call
                        // (initiated by an opening group meta character)
                        switch(_regexExprFinalize(build->expr)) {
                            case eReExprSuccess:
                                regexBuildGroupEnd(build);
                                return build->status;
                            case eReExprOutOfMemory:
                                return eCompileOutOfMem;
                            case eReExprEmpty:
                                return eCompileMissingPattern;
                            case eReExprUnclosed:
                                return eCompileMissingSubexprEnd;
                            case eReExprUnusedOperators:
                                return eCompileMissingOperand;
                            case eReExprUnusedOperands:
                                INTERNAL_ERROR("unused operands in group subexpression");
                                return eCompileInternalError;
                            case eReExprMissingOperand:
                                return eCompileMissingOperand;
                            default:
                            case eReExprInternalError:
                                return eCompileInternalError;
                        }

                    case '^': // Meta, assertion - start of line
                        if(!regexBuildConcatAssertion(build, RE_TOK_FLAG_ASSERT_START_OF_LINE)) {
                            return eCompileOutOfMem;
                        }
                        continue;

                    case '$': // Meta, assertion - end of line
                        if(!regexBuildConcatAssertion(build, RE_TOK_FLAG_ASSERT_END_OF_LINE)) {
                            return eCompileOutOfMem;
                        }
                        continue;

                    case '<': // Meta, assertion - start of word
                        if(!regexBuildConcatAssertion(build, RE_TOK_FLAG_ASSERT_START_OF_WORD)) {
                            return eCompileOutOfMem;
                        }
                        continue;

                    case '>': // Meta, assertion - end of word
                        if(!regexBuildConcatAssertion(build, RE_TOK_FLAG_ASSERT_END_OF_WORD)) {
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
                        if(!(index = regexSubroutineLookup(build->context, NULL, str, len))) {
                            return eCompileUnknownSubroutine;
                        }
                        if(!regexBuildConcatCall(build, index)) {
                            return eCompileOutOfMem;
                        }
                        continue;
#ifdef MOJO_REGEX_UNICODE
                    case 'X': // full unicode glyph (base + markers)
                        if(!(index = regexSubroutineLookup(build->context, DEF_UNICODE_GLYPH_SUB_PREFIX, DEF_UNICODE_GLYPH_SUB_NAME, RE_STR_NULL_TERM))) {
                            INTERNAL_ERROR("glyph subroutine is missing");
                            return eCompileUnknownSubroutine;
                        }
                        if(!regexBuildConcatCall(build, index)) {
                            return eCompileOutOfMem;
                        }
                        continue;
#endif // MOJO_REGEX_UNICODE
                    case 'B': // explicit byte
                        if(!regexBuildConcatByte(build)) {
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
                        if(!regexBuildConcatUtf8Literal(build, c.c, 0)) {
                            return eCompileOutOfMem;
                        }
                    } else if(!regexBuildConcatChar(build, (char) (c.c), 0)) {
                        return eCompileOutOfMem;
                    }
                } else {
                    if((str = _parseGetPatternStr(&(build->pattern->pattern), str_count.characters,
                                                  str_count.bytes)) == NULL) {
                        return eCompileOutOfMem;
                    }
                    if(!regexBuildConcatString(build, str, str_count.bytes)) {
                        return eCompileOutOfMem;
                    }
                }
                continue;
        }
    }
    return eCompileOk;
}

/////////////////////////////////////////////////////////////////////////////
// Builder handling
/////////////////////////////////////////////////////////////////////////////

// Build context management functions

regex_build_ctx_t *regexBuildContextCreate(unsigned int flags) {
    regex_build_ctx_t *context;

    if((context = _regexAlloc(sizeof(regex_build_ctx_t), _regexMemContext)) == NULL) {
        return NULL;
    }
    if(!_regexRegUnicodeInitializeTable()) {
        _regexDealloc(context, _regexMemContext);
        return NULL;
    }
    context->next_sub = 1;
    return context;
}

void regexBuildContextDestroy(regex_build_ctx_t *context) {
    regex_sub_t *sub, *sub_next;
    regex_pattern_t *pattern, *pattern_next;
    regex_expr_t *expr, *expr_next;

    regexVMDestroy(context->vm);
    regexSubroutineFreeAll(context);
    for(expr = context->expr_pool; expr != NULL; expr = expr_next) {
        expr_next = expr->next;
        expr->context = NULL;
        expr->next = NULL;
        _regexDealloc(expr, _regexMemContext);
    }
    _regexTokenDestroy(NULL, context->token_pool, 1);
    _regexPtrlistFree(NULL, context->ptrlist_pool);
    for(sub = context->subroutines; sub != NULL; sub = sub_next) {
        sub_next = sub->next;
        if(sub->name != NULL) {
            _regexDealloc(sub->name, _regexMemContext);
        }
        _regexTokenDestroy(NULL, sub->expr, 1);
        _regexDealloc(sub, _regexMemContext);
    }
    for(pattern = context->pattern_pool; pattern != NULL; pattern = pattern_next) {
        pattern_next = pattern->next;
        _regexDealloc(pattern, _regexMemContext);
    }
    _regexUtf8RangeDestroy(NULL, context->utf8_range_pool, 1);
    _regexDealloc(context, _regexMemContext);
}

// Build management functions ///////////////////////////////////////////////

regex_build_t *regexBuildCreate(regex_build_ctx_t *context) {
    regex_build_t *build;

    if((build = _regexAlloc(sizeof(regex_build_t), _regexMemContext)) == NULL) {
        return NULL;
    }
    build->context = context;
    if(!regexExprCreate(build)) {
        build->status = eCompileOutOfMem;
    }
    build->status = eCompileOk;
    return build;
}

void regexBuildDestroy(regex_build_t *build) {
    if(build != NULL) {
        while(regexPatternDestroy(build));
        while(regexExprDestroy(build));
        _regexDealloc(build, _regexMemContext);
    }
}

int regexBuildFinalize(regex_build_t *build) {
    // TODO
    if(build == NULL) {
        return 0;
    }
    //return regexExprOperatorApply(build->expr, 0);
    return 0;
}

// Expression management functions //////////////////////////////////////////

int regexExprCreate(regex_build_t *build) {
    regex_expr_t *expr;

    if((build->context != NULL) && (build->context->expr_pool != NULL)) {
        expr = build->context->expr_pool;
        build->context->expr_pool = expr->next;
        memset(expr, 0, sizeof(regex_expr_t));
    } else {
        if((expr = _regexAlloc(sizeof(regex_expr_t), _regexMemContext)) == NULL) {
            return 0;
        }
    }
    expr->context = build->context;
    expr->parent = build->expr;
    build->expr = expr;
    return 1;
}

void _regexExprOperandPush(regex_expr_t *expr, regex_token_t *token) {
    token->next = expr->tokens;
    expr->tokens = token;
}

regex_token_t *_regexExprOperandPop(regex_expr_t *expr) {
    regex_token_t *token;

    if((token = expr->tokens) != NULL) {
        expr->tokens = token->next;
        token->next = NULL;
    }
    return token;
}

void _regexExprOperatorPush(regex_expr_t *expr, regex_token_t *token) {
    token->next = expr->operators;
    expr->operators = token;
}

regex_token_t *_regexExprOperatorPop(regex_expr_t *expr) {
    regex_token_t *token;

    if((token = expr->operators) != NULL) {
        expr->operators = token->next;
        token->next = NULL;
    }
    return token;
}

eReExprStatus_t _regexExprOperatorApply(regex_expr_t *expr) {
    regex_token_t *operand_a, *operand_b = NULL, *operator;

    operator = _regexExprOperatorPop(expr);
    if((operand_a = _regexExprOperandPop(expr)) == NULL) {
        return eReExprMissingOperand;
    }
    if(_regexTokenDetails[operator->tokenType].arity > 1) {
        operand_b = operand_a;
        if((operand_a = _regexExprOperandPop(expr)) == NULL) {
            return eReExprMissingOperand;
        }
    }
    if(!_regexTokenDetails[operator->tokenType].operator(expr, operand_a, operand_b, operator)) {
        return eReExprOutOfMemory;
    }
    return eReExprSuccess;
}

eReExprStatus_t _regexExprFinalize(regex_expr_t *expr) {
    eReExprStatus_t status;

    if(expr->tokens == NULL) {
        return eReExprEmpty;
    }
    while(expr->operators != NULL) {
        if((status = _regexExprOperatorApply(expr)) != eReExprSuccess) {
            return status;
        }
    }
    if(expr->tokens->next != NULL) {
        return eReExprUnusedOperands;
    }
    return eReExprSuccess;
}

eReExprStatus_t _regexExprTokenPush(regex_expr_t *expr, regex_token_t *token) {
    eReExprStatus_t status;

    if((expr == NULL) || (token == NULL)) {
        INTERNAL_ERROR("NULL expr or token passed to _regexExprTokenPush");
        return eReExprInternalError;
    }
    if((token->tokenType < 0) || (token->tokenType >= eTokenUnknown)) {
        INTERNAL_ERROR("Invalid token (out of range) type passed to _regexExprTokenPush");
        return eReExprInternalError;
    }
    if(_regexTokenDetails[token->tokenType].operator != NULL) {
        while((expr->operators != NULL) &&
              (_regexTokenDetails[expr->operators->tokenType].priority >=
               _regexTokenDetails[token->tokenType].priority)) {
            if((status = _regexExprOperatorApply(expr)) != eReExprSuccess) {
                return status;
            }
        }
        _regexExprOperatorPush(expr, token);
    } else {
        _regexExprOperandPush(expr, token);
        if((token->ptrlist = _regexPtrlistCreate(expr->context, token, eRePtrOutA)) == NULL) {
            return eReExprOutOfMemory;
        }
    }
    return eReExprSuccess;
}

int regexExprDestroy(regex_build_t *build) {
    regex_expr_t *parent = NULL;

    if((build == NULL) || (build->expr == NULL)) {
        return 0;
    }
    parent = build->expr->parent;
    _regexTokenDestroy(build->context, build->expr->tokens, 1);
    _regexTokenDestroy(build->context, build->expr->operators, 1);
    build->expr->next = build->context->expr_pool;
    build->context->expr_pool = build->expr;
    build->expr = parent;
    return 1;
}

// Pattern management functions /////////////////////////////////////////////

int regexPatternCreate(regex_build_t *build, const char *pattern, int len) {
    regex_pattern_t *pat;

    if((build->context != NULL) && (build->context->pattern_pool != NULL)) {
        pat = build->context->pattern_pool;
        build->context->pattern_pool = pat->next;
        memset(pat, 0, sizeof(regex_pattern_t));
    } else {
        if((pat = _regexAlloc(sizeof(regex_pattern_t), _regexMemContext)) == NULL) {
            return 0;
        }
    }
    pat->pattern = pattern;
    pat->base = pattern;
    pat->len = (int)((len == RE_STR_NULL_TERM) ? strlen(pattern) : len);
    pat->parent = build->pattern;
    build->pattern = pat;
    return 1;
}

int regexPatternDestroy(regex_build_t *build) {
    regex_pattern_t *parent;

    if((build == NULL) || (build->pattern == NULL)) {
        return 0;
    }
    parent = build->pattern->parent;
    build->pattern->next = build->context->pattern_pool;
    build->context->pattern_pool = build->pattern;
    build->pattern = parent;
    return 1;
}

// Subroutine management functions //////////////////////////////////////////

int regexSubroutineCreate(regex_build_ctx_t *context) {
    regex_sub_t *sub;

    if(context == NULL) {
        return 0;
    }

    if((sub = _regexAlloc(sizeof(regex_sub_t), _regexMemContext)) == NULL) {
        return 0;
    }
    sub->index = context->next_sub;
    context->next_sub++;
    sub->next = context->subroutines;
    context->subroutines = sub;
    return sub->index;
}

static regex_sub_t *_regexSubroutineGet(regex_build_ctx_t *context, int id) {
    regex_sub_t *walk;

    for(walk = context->subroutines; walk != NULL; walk = walk->next) {
        if(id == walk->index) {
            return walk;
        }
    }
    return NULL;
}

eReSuboutineResult_t regexSubroutineName(regex_build_ctx_t *context, int id, const char *prefix, const char *name, int len) {
    regex_sub_t *sub;

    if((sub = _regexSubroutineGet(context, id)) == NULL) {
        return 0;
    }
    if(sub->name != NULL) {
        _regexDealloc(sub->name, _regexMemContext);
    }
    if((sub->name = _regexPrefixedStrdup(prefix, name, len)) == NULL) {
        return 0;
    }
    return 1;
}

eReSuboutineResult_t regexSubroutineAlias(regex_build_ctx_t *context, int id, const char *prefix, const char *alias, int len) {
    regex_sub_t *sub;

    if((sub = _regexSubroutineGet(context, id)) == NULL) {
        return 0;
    }
    if(sub->alias != NULL) {
        _regexDealloc(sub->alias, _regexMemContext);
    }
    if((sub->name = _regexPrefixedStrdup(prefix, alias, len)) == NULL) {
        return 0;
    }
    return 1;
}

int regexSubroutineLookup(regex_build_ctx_t *context, const char *prefix, const char *name, int len) {
    regex_sub_t *walk;

    for(walk = context->subroutines; walk != NULL; walk = walk->next) {
        if(!_regexPrefixedStrncmp((const unsigned char *)prefix,
                                  (const unsigned char *)walk->name, RE_STR_NULL_TERM,
                                  (const unsigned char *)name, len)) {
            return walk->index;
        }
        if(!_regexPrefixedStrncmp((const unsigned char *)prefix,
                                  (const unsigned char *)walk->alias, RE_STR_NULL_TERM,
                                  (const unsigned char *)name, len)) {
            return walk->index;
        }
    }
    return 0;
}

eReSuboutineResult_t regexSubroutineExprSet(regex_build_ctx_t *context, int id, regex_token_t *tokens) {
    regex_sub_t *sub;

    // TODO - fix return values
    if((sub = _regexSubroutineGet(context, id)) == NULL) {
        return 0;
    }
    if(sub->expr != NULL) {
        return 0;
    }
    sub->expr = tokens;
    // TODO - append eTokenReturn
    return 1;
}

eReSuboutineResult_t regexSubroutineRegister(regex_build_ctx_t *context, const char *prefix, const char *name, const char *alias,
                                             regex_token_t *tokens) {
    int id;

    if(((name == NULL) && (alias == NULL)) || (tokens == NULL)) {
        return 0;
    }
    if(!(id = regexSubroutineCreate(context))) {
        return 0;
    }
    if(name != NULL) {
        if(!regexSubroutineName(context, id, NULL, name, RE_STR_NULL_TERM)) {
            return 0;
        }
    }
    if(alias != NULL) {
        if(!regexSubroutineAlias(context, id, NULL, alias, RE_STR_NULL_TERM)) {
            return 0;
        }
    }
    if(!regexSubroutineExprSet(context, id, tokens)) {
        return 0;
    }
    return id;
}

void regexSubroutineFreeAll(regex_build_ctx_t *context) {
    regex_sub_t *walk, *next;

    for(walk = context->subroutines, next = NULL; walk != NULL; walk = next) {
        next = walk->next;
        if(walk->expr != NULL) {
            _regexTokenDestroy(NULL, walk->expr, 1);
        }
        if(walk->name != NULL) {
            _regexDealloc(walk->name, _regexMemContext);
        }
        if(walk->alias != NULL) {
            _regexDealloc(walk->alias, _regexMemContext);
        }
        _regexDealloc(walk, _regexMemContext);
    }
    context->subroutines = NULL;
    context->next_sub = 1;
}

// Pattern compilation direct to VM convenience function ////////////////////

regex_build_t *regexCompile(const char *pattern, unsigned int flags) {
    regex_build_t *build;
    regex_build_ctx_t *context;

    if((context = regexBuildContextCreate(flags)) == NULL) {
        return NULL;
    }
    if((build = regexBuildCreate(context)) == NULL) {
        _regexDealloc(context, _regexMemContext);
        return NULL;
    }
    if(build->status != eCompileOk) {
        return build;
    }
    if(!regexPatternCreate(build, pattern, RE_STR_NULL_TERM)) {
        build->status = eCompileOutOfMem;
        return build;
    }

    if(!regexBuildPatternProcess(build)) {
        return build;
    }

    if(regexVMCreate(build) == NULL) {
        return build;
    }

    regexVMGenerate(build);

    return build;
}

int regexBuildPatternProcess(regex_build_t *build) {
    if((build->status = _regexTokenizePattern(build)) != eCompileOk) {
        return 0;
    }
    return 1;
}

/////////////////////////////////////////////////////////////////////////////
// VM image management functions
/////////////////////////////////////////////////////////////////////////////

regex_vm_t *regexVMCreate(regex_build_t *build) {
    if((build == NULL) || (build->context == NULL)) {
        return NULL;
    }
    if((build->context->vm = _regexAlloc(sizeof(regex_vm_t), _regexMemContext)) == NULL) {
        build->status = eCompileOutOfMem;
        return NULL;
    }
    build->context->vm->vm_version = REGEX_VM_MACHINE_VERSION;
    return build->context->vm;
}

int regexVMGenerate(regex_build_t *build) {
    // TODO
    return 0;
}

void regexVMDestroy(regex_vm_t *vm) {
    int k;

    if(vm != NULL) {
        for(k = 0; k < vm->string_tbl_size; k++) {
            _regexDealloc(vm->string_table[k], _regexMemContext);
        }
        _regexDealloc(vm->string_table, _regexMemContext);
        _regexDealloc(vm->string_tbl_len, _regexMemContext);
        for(k = 0; k < vm->class_tbl_size; k++) {
            _regexDealloc(vm->class_table[k], _regexMemContext);
        }
        _regexDealloc(vm->class_table, _regexMemContext);
        for(k = 0; k < vm->utf8_tbl_size; k++) {
            _regexDealloc(vm->utf8_class_table[k], _regexMemContext);
        }
        _regexDealloc(vm->utf8_class_table, _regexMemContext);
        for(k = 0; k < vm->group_tbl_size; k++) {
            _regexDealloc(vm->group_table[k], _regexMemContext);
        }
        _regexDealloc(vm->group_table, _regexMemContext);
#ifdef MOJO_REGEX_VM_DEBUG
        for(k = 0; k < vm->sub_name_tbl_size; k++) {
            _regexDealloc(vm->sub_name_table[k].name, _regexMemContext);
            _regexDealloc(vm->sub_name_table[k].alias, _regexMemContext);
        }
        _regexDealloc(vm->sub_name_table, _regexMemContext);
#endif // MOJO_REGEX_VM_DEBUG
        _regexDealloc(vm, _regexMemContext);
    }
}

// Returns -1 if out of mem, otherwise returns the index of the entry in the
// string table.
int regexVMStringTableEntryAdd(regex_build_t *build, char *str, int len) {
    int k;

    // Dedupe the string against the existing VM string table
    for(k = 0; k < build->context->vm->string_tbl_size; k++) {
        if((build->context->vm->string_tbl_len[k] == len) &&
           (!memcmp(build->context->vm->string_table[k], str, len))) {
            _regexDealloc(str, _regexMemContext);
            return k;
        }
    }
    if(((build->context->vm->string_table = _regexRealloc(build->context->vm->string_table,
                                                          (build->context->vm->string_tbl_size) * sizeof(char *),
                                                          (build->context->vm->string_tbl_size + 1) * sizeof(char *),
                                                          _regexMemContext)) == NULL) ||
       ((build->context->vm->string_tbl_len = _regexRealloc(build->context->vm->string_tbl_len,
                                                            (build->context->vm->string_tbl_size) * sizeof(int),
                                                            (build->context->vm->string_tbl_size + 1) * sizeof(int),
                                                            _regexMemContext)) == NULL)) {
        return -1;
    }
    if((build->context->vm->string_table[build->context->vm->string_tbl_size] = _regexStrdup(str, len)) == NULL) {
        return -1;
    }
    build->context->vm->string_tbl_len[build->context->vm->string_tbl_size] = len;
    build->context->vm->string_tbl_size++;
    return build->context->vm->string_tbl_size - 1;
}

int regexVMCharClassEntryAdd(regex_build_t *build, unsigned int *bitmap) {
    int k;

    // Dedupe the class bitmap against the existing VM class bitmap table
    for(k = 0; k <build->context->vm->class_tbl_size; k++) {
        if(!memcmp(build->context->vm->class_table[k], bitmap, 32)) {
            _regexDealloc(bitmap, _regexMemContext);
            return k;
        }
    }
    if((build->context->vm->class_table = _regexRealloc(build->context->vm->class_table,
                                                        (build->context->vm->class_tbl_size) * sizeof(unsigned int *),
                                                        (build->context->vm->class_tbl_size + 1) * sizeof(unsigned int *),
                                                        _regexMemContext)) == NULL) {
        return -1;
    }
    if((build->context->vm->class_table[build->context->vm->class_tbl_size] = _regexAlloc(32, _regexMemContext)) == NULL) {
        return -1;
    }
    memcpy(build->context->vm->class_table[build->context->vm->class_tbl_size], bitmap, 32);
    build->context->vm->class_tbl_size++;
    return build->context->vm->class_tbl_size - 1;
}

int regexVMUtf8ClassEntryAdd(regex_build_t *build, unsigned int *bitmap) {
    int k;

    // Dedupe the UTF8 class bitmap against the existing VM UTF8 class bitmap table
    for(k = 0; k <build->context->vm->utf8_tbl_size; k++) {
        if(!memcmp(build->context->vm->utf8_class_table[k], bitmap, 8)) {
            _regexDealloc(bitmap, _regexMemContext);
            return k;
        }
    }
    if((build->context->vm->utf8_class_table = _regexRealloc(build->context->vm->utf8_class_table,
                                                             (build->context->vm->utf8_tbl_size) * sizeof(unsigned int *),
                                                             (build->context->vm->utf8_tbl_size + 1) * sizeof(unsigned int *),
                                                             _regexMemContext)) == NULL) {
        return -1;
    }
    if((build->context->vm->utf8_class_table[build->context->vm->utf8_tbl_size] = _regexAlloc(8, _regexMemContext)) == NULL) {
        return -1;
    }
    memcpy(build->context->vm->utf8_class_table[build->context->vm->utf8_tbl_size], bitmap, 8);
    build->context->vm->utf8_tbl_size++;
    return build->context->vm->utf8_tbl_size - 1;
}

int regexVMGroupEntryAdd(regex_build_t *build, const char *name, int len, int index) {
    if(index >= build->context->vm->group_tbl_size) {
        if((build->context->vm->group_table = _regexRealloc(build->context->vm->group_table,
                                                            (build->context->vm->group_tbl_size) * sizeof(char *),
                                                            (index + 1) * sizeof(char *),
                                                            _regexMemContext)) == NULL) {
            return 0;
        }
        build->context->vm->group_tbl_size = index + 1;
    }
    if(build->context->vm->group_table[index] == NULL) {
        if((build->context->vm->group_table[index] = _regexStrdup(name, len)) == NULL) {
            return 0;
        }
    }
    return 1;
}

int regexVMSubNameEntryAdd(regex_build_t *build, const char *name, const char *alias, int index) {
    int k;

    for(k = 0; k < build->context->vm->sub_name_tbl_size; k++) {
        if(build->context->vm->sub_name_table[k].id == index) {
            break;
        }
    }
    if(k >= build->context->vm->sub_name_tbl_size) {
        if((build->context->vm->sub_name_table = _regexRealloc(build->context->vm->sub_name_table,
                                                               (build->context->vm->sub_name_tbl_size) * sizeof(regex_vm_sub_names_t),
                                                               (build->context->vm->sub_name_tbl_size + 1) * sizeof(regex_vm_sub_names_t),
                                                               _regexMemContext)) == NULL) {
            return 0;
        }
        k = build->context->vm->sub_name_tbl_size;
        build->context->vm->sub_name_tbl_size++;
    }
    if((name != NULL) && (build->context->vm->sub_name_table[k].name == NULL)) {
        if((build->context->vm->sub_name_table[k].name = _regexStrdup(name, RE_STR_NULL_TERM)) == NULL) {
            return 0;
        }
    }
    if((alias != NULL) && (build->context->vm->sub_name_table[k].alias == NULL)) {
        if((build->context->vm->sub_name_table[k].alias = _regexStrdup(alias, RE_STR_NULL_TERM)) == NULL) {
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

const char *regexVMGroupEntryGet(regex_vm_t *vm, int index) {
    if((index < 1) || ((index / 2) >= vm->group_tbl_size)) {
        return NULL;
    }
    return vm->group_table[index / 2];
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

/////////////////////////////////////////////////////////////////////////////
// Regex evaluation handlers
/////////////////////////////////////////////////////////////////////////////

int _regexIsWordBoundaryChar(unsigned char c) {
    // TODO
    return 0;
}

regex_compound_t *regexThreadCompoundCreate(regex_eval_t *eval, regex_thread_t *thread) {
    // TODO
    return NULL;
}

void regexThreadCompoundDestroy(regex_eval_t *eval, regex_compound_t *compound) {
    // TODO
}

int regexThreadCompoundCopy(regex_eval_t *eval, regex_thread_t *dest, regex_thread_t *src) {
    // TODO
    return 0;
}

int regexThreadCompoundCount(regex_compound_t *compound) {
    // TODO
    return 0;
}

void regexThreadCompoundReverse(regex_compound_t **compound) {
    // TODO
}

size_t regexThreadCompoundCalcMatchBufferSize(regex_vm_t *vm, regex_thread_t *thread) {
    // TODO
    return 0;
}

void regexThreadCompoundStoreInMatch(regex_match_t *match, const char *baseMem, regex_thread_t *thread) {
    // TODO
}

int regexThreadCompoundStart(regex_eval_t *eval, regex_thread_t *thread, int subexpr, const char *ptr) {
    // TODO
    return 0;
}

int regexThreadCompoundEnd(regex_thread_t *thread, const char *ptr) {
    // TODO
    return 0;
}

void regexThreadCopySubexprs(int count, regex_thread_t *dest, regex_thread_t *src) {
    // TODO
}

regex_eval_t *regexEvalCreate(regex_vm_t *vm, const char *pattern, int len) {
    // TODO
    return NULL;
}

int regexThreadCreate(regex_eval_t *eval, regex_thread_t *parent, unsigned int pc, int from_range) {
    // TODO
    return 0;
}

void regexThreadFree(regex_eval_t *eval, regex_thread_t *thread) {
    // TODO
}

void regexEvalFree(regex_eval_t *eval) {
    // TODO
}

eReEvalResult_t regexThreadProcess(regex_eval_t *eval, regex_thread_t *thread, int complete, unsigned char c) {
    int opcode;
    eReEvalResult_t status;
#ifdef MOJO_REGEX_VM_DEBUG
    int first = 1;
#endif // MOJO_REGEX_VM_DEBUG

    while(!_regexTokenDetails[(opcode = (int)(RE_VM_OPCODE_MASK & eval->vm->program[thread->pc]))].textAdvance) {
#ifdef MOJO_REGEX_VM_DEBUG
        if(eval->debug != NULL) {
            if(!first) {
                fprintf(eval->debug, "             ");
            }
            regexVMInstrEmit(eval->debug, eval->vm, (int)(thread->pc));
            first = 0;
        }
#endif // MOJO_REGEX_VM_DEBUG
        if((status = _regexTokenDetails[opcode].handleInstr(eval, thread, eval->vm->program[thread->pc], c)) != eEvalContinue) {
            return status;
        }
    }
#ifdef MOJO_REGEX_VM_DEBUG
    if(eval->debug) {
        if(!first) {
            fprintf(eval->debug, "             ");
        }
        regexVMInstrEmit(eval->debug, eval->vm, (int)thread->pc);
    }
#endif // MOJO_REGEX_VM_DEBUG
    if(eval->pos == eval->len) {
        return eEvalNoMatch;
    }
    return _regexTokenDetails[(int)(RE_VM_OPCODE_MASK & eval->vm->program[thread->pc])].handleInstr(eval, thread, eval->vm->program[thread->pc], c);
}

void regexMatchFree(regex_match_t *match) {
    // TODO
}

#ifdef MOJO_REGEX_VM_DEBUG
regex_match_t *regexMatch(regex_vm_t *vm, const char *text, int len, int anchored, FILE *debug) {
#else // !MOJO_REGEX_VM_DEBUG
regex_match_t *regexMatch(regex_vm_t *vm, const char *text, int len, int anchored) {
#endif // MOJO_REGEX_VM_DEBUG
    // TODO
    return NULL;
}

#endif // MOJO_REGEX_COMMON_IMPLEMENTATION

#ifdef MOJO_REGEX_COMPILE_IMPLEMENTATION

/////////////////////////////////////////////////////////////////////////////
// Unicode property class definitions
/////////////////////////////////////////////////////////////////////////////

#ifdef MOJO_REGEX_UNICODE

// These property classes were generated from the unicode database by the
// extract_unicode_props.py script

// TODO - inline the base utf8 property classes, so that this is a true single file library

#include "utf8_props.c"

#endif // MOJO_REGEX_UNICODE

#endif // MOJO_REGEX_COMPILE_IMPLEMENTATION

#endif // MOJO_REGEX_IMPLEMENTATION

#endif // _MOJO_REGEX_HEADER_