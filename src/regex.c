#ifndef _MOJO_REGEX_HEADER_
#define _MOJO_REGEX_HEADER_

#define MOJO_REGEX_TEST_MAIN
#define MOJO_REGEX_COMPILE_IMPLEMENTATION
#define MOJO_REGEX_VM_SOURCE_GENERATION
//#define MOJO_REGEX_VM_DEBUG
#define MOJO_REGEX_EVALUATE_IMPLEMENTATION
#define MOJO_REGEX_UNICODE
#define MOJO_REGEX_IMPLEMENTATION

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/*
    Flags (TODO)
    --------------------------
    unicode
        Handle UTF8 encoded glyphs explicitly. Properly treat multibyte chars
        as a single "character" in single chars, string, and character classes.
    caseinsensitive
        Treat the entire pattern as case insensitive.
    nocapture
        Subexpressions are not captured. Simplifies compilation, parsing, and
        lowers runtime memory overhead.
    dotall
        . matches ANY character (default is any EXCEPT newline)


    Character escape sequences
    --------------------------
    (implemented)
        \a alarm (bell)
        \b backspace
        \e escape
        \f formfeed
        \n newline
        \r carraige return
        \t tab
        \v vertical tab
        \0 null char
        \x## hex byte
        \u#### unicode codepoint
        \U##### unicode codepoint > 0xFFFF
        \d [0-9] - ASCII
        \D [^0-9] - ASCII
        \s [ \t\f\v\r\n] - ASCII
        \S [^ \t\f\v\r\n] - ASCII
        \w [a-zA-Z0-9_] - ASCII
        \W [^a-zA-Z0-9_] - ASCII
        \p{M} - combining mark (M_)
        \p{N} - numeric digit (N_)
        \p{P} - punctuation (P_)
        \p{Z} - any unicode whitespace (Z_)
        \p{L} - any unicode letter (L_)
        \p{Lu} - uppercase unicode letter (Lu)
        \p{Ll} - lowercase unicode letter (Ll)
        \p{__} - any unicode property set, when registered with regexRegUnicodeCharClass()
        \B - match a byte (differs from . in that it always matches a single byte, even '\n')
        \R{name} - calls a subroutine (previously defined with (?R<name>...))

    Note on utf8 support:
        clang follows sane expectations and converts \u#### into utf8 encoding
        within a char string. gcc illogically converts \u#### into utf16
        encoding, even within a char string, and does work properly with the
        evaluator. For gcc, you will need to manually encode utf8 characters.

    TODO
        \X full unicode letter glyph (may be multiple chars, includes marker glyphs)
            "\P{M}\p{M}*"
        ^  start of string (assertion, non consuming)
        $  end of string (assertion, non consuming) [see eTokenMatch]
        \< match start of word (assertion, non consuming, ASCII only)
        \> match end of word (assertion, non consuming, ASCII only)
        \P inverts unicode category class
        surface subroutine errors during compilation
        DFA generation rework:
            cull redundant jmps, and jmps to match/return:
                ie:   0 jmp 1
                      1 jmp 2   -->   0 jmp 2
                      2 jmp 4         1 ...
                      3 ...
            reduce '?', '+', and '*' quantifiers on char literal:
                '?'   0 split 1, 2
                      1 char 'a'    -->  0 char 'a', 1
                      2 ...              1 ...
                '+'   0 char 'a'
                      1 split 0, 2  -->  1 char 'a', 1
                      2 ...              2 ...
                '*'   0 split 1, 3
                      1 char 'a'    -->  1 char 'a' invert, 1
                      2 jump             2 ...
                      3 ...
            reduce alternation '|' with preceeding char literal:
                '|'   0 split 1, 3
                      1 char 'a'    --> 0 char 'a' invert, 3
                      2 jmp 4           1 ...
                      3 ...

    (?P<name>...)  named subexpressions
    (?:...) non capturing subexpressions
    (?*...) compound subexpressions
    (?R<name>...) named subroutine
    (?i) case insensitive match - TODO

    multiple subexpression meta prefixes may be defined, but must be at the
    head of the subexpression. Additional meta's must include their own unique
    '?' prefix, ie.: (?:?P<name>...). Not all permutations are valid:

        (?P<name>...) and (?:...) are mutually exclusive (why name something
        unused?).

        (?:...) and (?*...) are mutually exclusive (compound non-captures?).

        (?P<name>...) and (?R<name>...) are mutually exclusive. Subexpressions
        are not generated "in-line", but are called out of order using the call/
        return instructions. A subroutine may be wrapped in a named subexpression
        however.

        (?R<name>...) and (?*...) are mutually exclusive, as a subroutine is not
        a capturing expression.

    TODO
        unicode compilation toggle - do NOT generate unicode char classes in non
            unicode mode

    TODO: Future work (refining, not MVP critical)
        tag meta char class -> char class -> vm path, for better runtime debug
        update unicode parser script to parse arbitrary property sets/groups
        refactor unicode char class registration to a single struct type
            current has independent class id and class str values
        Shunting yard creates a valid, yet inefficient, DFA. There are various
            redundancies, particularly with regards to chained and unnecessary
            jumps.


Regex VM Bytecode (v7)

    Each operation is encoded into a single 32 bit int value:

         32    -     18 17     -     4 3- 0
        |--------------|--------------|----|
        |14 bits (op a)|14 bits (op b)|4bit|
        | Operand A    | Operand B    | Op |

    Operators:               opcode     Operand A               Operand B
        eTokenCharLiteral       1       char to match           inverse flag
        eTokenCharClass         2       class idx to match
        eTokenStringLiteral     3       str idx to match
        eTokenCharAny           4
        eTokenMatch             5       end of input flag
        eTokenSplit             6       program counter         program counter
        eTokenJmp               7       program counter
        eTokenSave              8       subexpression number    compound flag
        eTokenUtf8Class         9       utf8 idx to match
        eTokenCharAnyDotAll     A
        eTokenCall              B       program counter
        eTokenReturn            C
        eTokenByte              D
        <reserved>              E
        <reserved>              F

    Note about eTokenCharLiteral:
        Matches the character specified. If the inverse flag is set, the logic
        is inverted, and the instruction matches any character EXCEPT the
        character specified.

    Note about eTokenCharAny:
        If the DOTALL flag is enabled, the eTokenCharAnyDotAll is generated
        instead. In ASCII mode, this instruction matches any character _except_
        newline. In Unicode mode, this instruction matches a single unicode
        codepoint, which MAY match multiple bytes, but again, does NOT match
        newline.

    Note about eTokenMatch:
        If the end of input flag is not set, the pattern matches at this
        instruction. If the end of input flag IS set, this ONLY matches if the
        evaluated text is at the end of input ('$' assertion operator)

    Note about eTokenSave:
        By default, this instruction captures the widest matching range for the
        internal expression, ie., "(foo)+" evaluating "foofoofoo" would capture
        "foofoofoo" as a single value for the subexpression. If the compound
        flag is set, then in addition to this widest match behaviour, each
        individual occurence of a match would be captured as well. For example,
        the pattern "(?*foo)+" evaluating "foofoofoo" would capture "foofoofoo",
        as well as "foo", "foo", and "foo" (1 primary, 3 sub captures)

    Note about eTokenUtf8Class:
        This instruction represents the low byte of a utf8 encoding (for 2, 3,
        and 4 byte encodings). For single byte encodings, a standard char class
        is used. Multibyte codepoint are represented by compound VM
        instructions.

    Note about eTokenCharAnyDotAll:
        This instruction matches any character, including newline. In unicode
        mode, this matches a single codepoint, which MAY match multiple bytes.

    Note about eTokenCall:
        This instruction allows common repeating sequences to be factored out
        into a subroutine. The evaluation implementation is limited to an
        effective call depth of 1 (to limit thread overhead), so nested calls
        are NOT allowed. The initial design consideration for this feature was
        to reduce overhead when using unicode character classes, as they
        consist of a generated sub-pattern in and of themselves, due to the
        large and distributed range of branching byte sequence dependencies.

    Note about eTokenReturn:
        This instruction returns to the next consecutive instruction following
        a previously issued eTokenCall instruction. If no previous eTokenCall
        was issued, the instruction is considered a non match.

    Note about eTokenByte:
        This instruction explicitly matches the any single byte. This differs
        from both eTokenCharAny and eTokenCharAnyDotAll in that it always
        matches newline, and always matches a single byte.

All VM programs are prefixed with: TODO

        0 split 1 3
        1 anychar
        2 jmp 1

    At runtime, if a partial match is requested, execution begins at 0. If a
    full match is requested, execution begins at 3.

Test 0: a(?P<foolio>\Bcd(?i\w*[hijk](?*f.o)+)?)\d+cat
        abcdefgefgjfoofwofyo8167287catfoo

Test 1: a(?P<foolio>bcd(?iefg*[hijk]foo)?)[0-9]+cat
        abcdefgefgjfoo8167287catfoo

Test 2: a(?P<foolio>?i\Bcd(?i(?R<chunk>efg[hijk])(?*f.o)*)?)(?R<digits>\d+)c\p{L}+\R{digits}
        abcdefgjfoofoofoo033195cat72364ghj


 */

/////////////////////////////////////////////////////////////////////////////

#define REGEX_VM_MACHINE_VERSION    7

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
};

typedef void *(*regexMemAllocator_t)(size_t size, void *ctx);
typedef void (*regexMemDeallocator_t)(void *ptr, void *ctx);
typedef void *(*regexMemReallocator_t)(void *ptr, size_t old_size, size_t new_size, void *ctx);

void regexSetMemoryAllocator(regexMemAllocator_t alloc,
                             regexMemDeallocator_t dealloc,
                             regexMemReallocator_t re_alloc,
                             void *context);

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
    eCompileInternalError
} eRegexCompileStatus_t;

#define REGEX_UNICODE           0x01
#define REGEX_CASE_INSENSITIVE  0x02
#define REGEX_NO_CAPTURE        0x04
#define REGEX_DOTALL            0x08

#define REGEX_STR_NULL_TERMINATED   -1

typedef struct regex_compile_ctx_s regex_compile_ctx_t;
struct regex_compile_ctx_s {
    eRegexCompileStatus_t status;
    const char *pattern;
    int position;
    regex_vm_t *vm;
};

const char *regexGetCompileStatusStr(eRegexCompileStatus_t status);
eRegexCompileStatus_t regexCompile(regex_compile_ctx_t *ctx, const char *pattern,
                                   unsigned int flags);

void regexVMPrintProgram(FILE *fp, regex_vm_t *vm);

#ifdef MOJO_REGEX_VM_SOURCE_GENERATION

void regexVMGenerateDeclaration(regex_vm_t *vm, const char *symbol, FILE *fp);
void regexVMGenerateDefinition(regex_vm_t *vm, const char *symbol, FILE *fp);

#endif // MOJO_REGEX_VM_SOURCE_GENERATION

#endif // MOJO_REGEX_COMPILE_IMPLEMENTATION

#ifdef MOJO_REGEX_EVALUATE_IMPLEMENTATION

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

#ifdef MOJO_REGEX_VM_DEBUG
regex_match_t *regexMatch(regex_vm_t *vm, const char *text, int len, int anchored, FILE *debug);
#else // !MOJO_REGEX_VM_DEBUG
regex_match_t *regexMatch(regex_vm_t *vm, const char *text, int len, int anchored);
#endif // MOJO_REGEX_VM_DEBUG
void regexMatchFree(regex_match_t *match);
const char *regexGroupValueGet(regex_match_t *match, int group, int *len);
const char *regexGroupValueGetByName(regex_match_t *match, const char *name, int *len);
int regexGroupCountGet(regex_vm_t *vm);
const char *regexGroupNameLookup(regex_vm_t *vm, int group);
int regexGroupIndexLookup(regex_vm_t *vm, const char *name);
int regexGroupCompoundCountGet(regex_match_t *match, int group);
const char *regexGroupCompoundValueGet(regex_match_t *match, int group, int num, int *len);

#endif // MOJO_REGEX_EVALUATE_IMPLEMENTATION

void regexVMFree(regex_vm_t *vm);

/////////////////////////////////////////////////////////////////////////////
#ifdef MOJO_REGEX_IMPLEMENTATION
/////////////////////////////////////////////////////////////////////////////

/////////////////////////////////////////////////////////////////////////////
// Delegated memory allocation handlers
/////////////////////////////////////////////////////////////////////////////

static void *regexDefMemAllocator(size_t size, void *ctx) {
    void *ptr = malloc(size);
    memset(ptr, 0, size);
    return ptr;
}

static void regexDefMemDeallocator(void *ptr, void *ctx) {
    free(ptr);
}

static void *regexDefMemReallocator(void *ptr, size_t old_size, size_t new_size, void *ctx) {
    ptr = realloc(ptr, new_size);
    if(new_size > old_size) {
        memset(((unsigned char *)ptr) + old_size, 0, new_size - old_size);
    }
    return ptr;
}

regexMemAllocator_t _regexAlloc = regexDefMemAllocator;
regexMemDeallocator_t _regexDealloc = regexDefMemDeallocator;
regexMemReallocator_t _regexRealloc = regexDefMemReallocator;
void *_regexMemContext = NULL;

void regexSetMemoryAllocator(regexMemAllocator_t alloc,
                             regexMemDeallocator_t dealloc,
                             regexMemReallocator_t re_alloc,
                             void *context) {
    _regexAlloc = alloc;
    _regexDealloc = dealloc;
    _regexRealloc = re_alloc;
    _regexMemContext = context;
}

#ifdef MOJO_REGEX_COMPILE_IMPLEMENTATION

// If len == -1, copies the string up to the null terminator '\0'
char *_regexStrdup(const char *str, size_t len) {
    char *ptr;

    if(len == -1) {
        len = strlen(str) + 1;
    }

    if((ptr = _regexAlloc(len, _regexMemContext)) == NULL) {
        return NULL;
    }
    memcpy(ptr, str, len);
    return ptr;
}

/////////////////////////////////////////////////////////////////////////////
// String table management and data reproduction
/////////////////////////////////////////////////////////////////////////////

// Ensure that only a single entry for the data exists. When adding, if the
// data already exists in the table, then do not add a new entry.
#define REGEX_TABLE_DEDUPE      0x01u

// Retain the data provided to the table. The table retains ownership of the
// pointer, and will free it upon removal.
#define REGEX_TABLE_KEEP_PTR    0x02u

// Add one character to the str length, and null terminate.
#define REGEX_TABLE_NULL_TERMINATE  0x04u

// If entry_size == -1, the entry is expected to be a null terminated ('\0')
// string, and will be derived automatically.
int regexStrTableEntryAdd(char ***table, int *table_size, size_t entry_size,
                          const char *entry_data, int **len_table,
                          unsigned int flags, int *deduped) {
    int index, k;

    if(entry_size == -1) {
        entry_size = ((entry_data != NULL) ? (strlen(entry_data) + 1) : 0);
    }
    if(flags & REGEX_TABLE_NULL_TERMINATE) {
        entry_size++;
    }
    if(flags & REGEX_TABLE_DEDUPE) {
        for(k = 0; k < *table_size; k++) {
            if((entry_data == NULL) && ((*table)[k] != NULL)) {
                break;
            } else if((entry_data != NULL) && (table[k] != NULL)) {
                if(len_table != NULL) {
                    if(((*len_table)[k] == entry_size) &&
                       (!memcmp((*table)[k], entry_data, entry_size))) {
                        break;
                    }
                } else if(!memcmp((*table)[k], entry_data, entry_size)) {
                    break;
                }
            }
        }
        if(k < *table_size) {
            if(deduped != NULL) {
                *deduped = 1;
            }
            return k;
        }
    }
    if(deduped != NULL) {
        *deduped = 0;
    }
    if((*table = _regexRealloc(*table, *table_size * sizeof(char **),
                               (*table_size + 1) * sizeof(char **),
                               _regexMemContext)) == NULL) {
        return -1;
    }
    if(len_table != NULL) {
        if((*len_table = _regexRealloc(*len_table, *table_size * sizeof(int),
                                       (*table_size + 1) * sizeof(int),
                                       _regexMemContext)) == NULL) {
            return -1;
        }
        (*len_table)[*table_size] = entry_size;
    }
    index = *table_size;
    (*table_size)++;
    if((flags & REGEX_TABLE_KEEP_PTR) || (entry_data == NULL)) {
        (*table)[index] = (char *)entry_data;
    } else {
        if(((*table)[index] = _regexStrdup(entry_data,
                                           entry_size - ((flags & REGEX_TABLE_NULL_TERMINATE) ? 1 : 0))) == NULL) {
            return -1;
        }
        if(flags & REGEX_TABLE_NULL_TERMINATE) {
            (*table)[index][entry_size - 1] = '\0';
        }
    }
    return index;
}

const char *regexStrTableEntryGet(char **table, int table_size,
                                  const int *len_table, int index, int *len) {
    if((index < 0) || (index >= table_size)) {
        return NULL;
    }
    if(len != NULL) {
        *len = (len_table != NULL ? len_table[index] : 0);
    }
    return table[index];
}

void regexStrTableFreeAll(char ***table, int *table_size, int **len_table) {
    int k;

    for(k = 0; k < *table_size; k++) {
        if((*table)[k] != NULL) {
            _regexDealloc((*table)[k], _regexMemContext);
        }
    }
    _regexDealloc(*table, _regexMemContext);
    if((len_table != NULL) && (*len_table != NULL)) {
        _regexDealloc(*len_table, _regexMemContext);
        *len_table = NULL;
    }
    *table_size = 0;
    *table = NULL;
}

#endif // MOJO_REGEX_COMPILE_IMPLEMENTATION

/////////////////////////////////////////////////////////////////////////////
// Parsing, token, and VM data structures and function declarations
/////////////////////////////////////////////////////////////////////////////

#define DEF_VM_SIZE_INC 1024
#define REGEX_THREAD_CALLSTACK_MAX_DEPTH 5

#define VM_PC_UNVISITED     ((int)-1)

#define META_DIGITS_PATTERN     "0-9"
#define META_WHITESPACE_PATTERN " \\t\\f\\v\\r"
#define META_WORD_PATTERN       "a-zA-Z0-9_"

#define META_CLASS(pattern)     pattern
#define META_CLASS_INV(pattern) "^" pattern

typedef enum {
    // Maps directly to VM opcodes
    eTokenNone,             // 0
    eTokenCharLiteral,
    eTokenCharClass,
    eTokenStringLiteral,
    eTokenCharAny,
    eTokenMatch,
    eTokenSplit,
    eTokenJmp,
    eTokenSave,
    eTokenUtf8Class,
    eTokenCharAnyDotAll,    // 10
    eTokenCall,
    eTokenReturn,
    eTokenByte,
    // Abstractions, reduced down to the above VM opcodes
    eTokenConcatenation,
    eTokenAlternative,
    eTokenZeroOrOne,
    eTokenZeroOrMany,
    eTokenOneOrMany,        // 20
    eTokenSubExprStart,
    eTokenSubExprEnd,
    eTokenUnknown // <-- this should always be the last token enum
} eRegexToken_t;

#ifdef MOJO_REGEX_COMPILE_IMPLEMENTATION

#ifndef uint32_t
typedef unsigned int uint32_t;
#endif // uint32_t

#ifndef uint8_t
typedef unsigned char uint8_t;
#endif // uint8_t

// Token attribute flags ///////////////////////////
// Subexpression flags
#define REGEX_TOKEN_FLAG_CASEINS    0x1u
#define REGEX_TOKEN_FLAG_NOCAPTURE  0x2u
#define REGEX_TOKEN_FLAG_COMPOUND   0x4u

// Shunting yard subroutine flag
#define REGEX_TOKEN_FLAG_SUBROUTINE 0x8u

// char literal, char class, utf8 class
#define REGEX_TOKEN_FLAG_INVERT     0x1u

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
        int c; // char literal
        char *str; // str literal
        unsigned int *bitmap; // char class, utf8 class
        int group; // subexpr start
    };
    int pc; // program counter index, for NFA -> VM
    union {
        int len; // string length, to allow embedded \0 chars
        struct {
            unsigned short flags; // char any, match, utf8 class, char any dotall, subexpr
            short sub_index;
        };
    };
    regex_ptrlist_t *ptrlist;
    regex_token_t *out_a;
    regex_token_t *out_b;
    regex_token_t *next;
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

typedef struct regex_subroutine_s regex_subroutine_t;
struct regex_subroutine_s {
    int id;
    uint32_t crc;
    char *name;
    int infixed;
    regex_token_t *tokens;
    regex_subroutine_t *next;
};

typedef struct regex_vm_build_s regex_vm_build_t;
struct regex_vm_build_s {
    regex_vm_t *vm;
    regex_token_t *tokens;
    regex_subroutine_t *subroutine_index;
    regex_vm_pc_patch_t *patch_list;
    regex_vm_gen_path_t *gen_list;
    regex_vm_gen_path_t *subroutine_list;
    regex_vm_gen_path_t *gen_pool;
    unsigned int flags;
    int groups;
    int pc;
};

int regexVMGroupTableEntryAdd(regex_vm_build_t *build, const char *group,
                              int len, int index);

/////////////////////////////////////////////////////////////////////////////
// Token meta attribute helper functions
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

typedef void (*regexTokenDetailPrint_t)(FILE *fp, regex_token_t *token);
typedef void (*regexTokenVMInstrPrint_t)(FILE *fp, regex_vm_t *vm, unsigned int oper_a, unsigned int oper_b);

void regexEmitCharClassBitmap(FILE *fp, const unsigned int *bitmap);
void regexEmitEscapedStr(FILE *fp, const char *str, int len);

const unsigned int *regexVMClassTableEntryGet(regex_vm_t *vm, int class_table_id);
const char *regexVMStringTableEntryGet(regex_vm_t *vm, int string_table_id, int *len);
const unsigned int *regexVMUtf8TableEntryGet(regex_vm_t *vm, int utf8_table_id);
const char *regexVMGroupNameFromIndex(regex_vm_t *vm, int index);

void regexPrintVMCharClass(FILE *fp, regex_vm_t *vm, int class_idx) {
    const unsigned int *bitmap;

    if((bitmap = regexVMClassTableEntryGet(vm, class_idx)) == NULL) {
        fprintf(fp, "index %d was null", class_idx);
    } else {
        regexEmitCharClassBitmap(fp, bitmap);
    }
}

void regexPrintVMStringLiteralEntry(FILE *fp, regex_vm_t *vm, int str_idx) {
    const char *str;
    int len;

    str = regexVMStringTableEntryGet(vm, str_idx, &len);
    regexEmitEscapedStr(fp, str, len);
}

void regexPrintVMUtf8ClassEntry(FILE *fp, regex_vm_t *vm, int utf8_idx) {
    const unsigned int *bitmap;

    bitmap = regexVMUtf8TableEntryGet(vm, utf8_idx);
    if(bitmap == NULL) {
        fprintf(fp, "NULL");
    } else {
        fprintf(fp, "0x%8.8X%8.8X", bitmap[0], bitmap[1]);
    }
}

// Token detail printers

void regexPrinter_eTokenCharLiteral(FILE *fp, regex_token_t *token) {
    fprintf(fp, "(%c:%d)", (((token->c < 32) || (token->c > 127)) ? ' ' : token->c), token->c);
}

void regexPrinter_eTokenCharClass(FILE *fp, regex_token_t *token) {
    fputc('[', fp);
    regexEmitCharClassBitmap(stdout, token->bitmap);
    fputc(']', fp);
}

void regexPrinter_eTokenStringLiteral(FILE *fp, regex_token_t *token) {
    fputc('\"', fp);
    regexEmitEscapedStr(stdout, token->str, token->len);
    fputc('\"', fp);
}

void regexPrinter_eTokenUtf8Class(FILE *fp, regex_token_t *token) {
    fputc('[', fp);
    regexEmitCharClassBitmap(fp, token->bitmap);
    fputc(']', fp);
}

void regexPrinter_eTokenCall(FILE *fp, regex_token_t *token) {
    fprintf(fp, "%d", token->c);
}

void regexPrinter_eTokenSubExprStart(FILE *fp, regex_token_t *token) {
    regex_subroutine_t *sub;

    if(token->flags & REGEX_TOKEN_FLAG_SUBROUTINE) {
        fprintf(fp, "subroutine %d", token->sub_index);
    } else {
        fprintf(fp, "group %d", token->group);
    }
    if(token->flags & REGEX_TOKEN_FLAG_COMPOUND) { fputs(", compound", fp); }
    if(token->flags & REGEX_TOKEN_FLAG_NOCAPTURE) { fputs(", no capture", fp); }
    if(token->flags & REGEX_TOKEN_FLAG_CASEINS) { fputs(", case insensitive", fp); }
}

// Token VM instruction printers

void regexPrintVM_eTokenCharLiteral(FILE *fp, regex_vm_t *vm, unsigned int oper_a, unsigned int oper_b) {
    fprintf(fp, "char (%c:%d)%s", (((oper_a < 32) || (oper_a > 127)) ? '_' : oper_a),
            oper_a, (oper_b ? " inverted" : ""));
}

void regexPrintVM_eTokenStringLiteral(FILE *fp, regex_vm_t *vm, unsigned int oper_a, unsigned int oper_b) {
    fprintf(fp, "string (%d) \"", oper_a);
    regexPrintVMStringLiteralEntry(fp, vm, (int)oper_a);
    fputc('\"', fp);
}

void regexPrintVM_eTokenCharClass(FILE *fp, regex_vm_t *vm, unsigned int oper_a, unsigned int oper_b) {
    fprintf(fp, "class (%d)[", oper_a);
    regexPrintVMCharClass(fp, vm, (int) oper_a);
    fputc(']', fp);
}

void regexPrintVM_eTokenCharAny(FILE *fp, regex_vm_t *vm, unsigned int oper_a, unsigned int oper_b) {
    fprintf(fp, "anychar");
}

void regexPrintVM_eTokenSave(FILE *fp, regex_vm_t *vm, unsigned int oper_a, unsigned int oper_b) {
    const char *str;

    str = regexVMGroupNameFromIndex(vm, (int)oper_a);

    fprintf(fp, "save %d", oper_a);
    if(str != NULL) {
        fprintf(fp, " (%s)", str);
    }
}

void regexPrintVM_eTokenSplit(FILE *fp, regex_vm_t *vm, unsigned int oper_a, unsigned int oper_b) {
    fprintf(fp, "split %d, %d", oper_a, oper_b);
}

void regexPrintVM_eTokenMatch(FILE *fp, regex_vm_t *vm, unsigned int oper_a, unsigned int oper_b) {
    fprintf(fp, "match");
}

void regexPrintVM_eTokenJmp(FILE *fp, regex_vm_t *vm, unsigned int oper_a, unsigned int oper_b) {
    fprintf(fp, "jmp %d", oper_a);
}

void regexPrintVM_eTokenUtf8Class(FILE *fp, regex_vm_t *vm, unsigned int oper_a, unsigned int oper_b) {
    fprintf(fp, "utf8 (%d)[", oper_a);
    regexPrintVMUtf8ClassEntry(fp, vm, (int)oper_a);
    fputc(']', fp);
}

void regexPrintVM_eTokenCharAnyDotAll(FILE *fp, regex_vm_t *vm, unsigned int oper_a, unsigned int oper_b) {
    fprintf(fp, "anybyte");
}

void regexPrintVM_eTokenCall(FILE *fp, regex_vm_t *vm, unsigned int oper_a, unsigned int oper_b) {
    fprintf(fp, "call %d", oper_a);
}

void regexPrintVM_eTokenReturn(FILE *fp, regex_vm_t *vm, unsigned int oper_a, unsigned int oper_b) {
    fprintf(fp, "return");
}

void regexPrintVM_eTokenByte(FILE *fp, regex_vm_t *vm, unsigned int oper_a, unsigned int oper_b) {
    fprintf(fp, "byte");
}

void regexPrintVM_eTokenUnknown(FILE *fp, regex_vm_t *vm, unsigned int oper_a, unsigned int oper_b) {
    fprintf(fp, "UNKNOWN [0x%4.4X] [0x%4.4X]", oper_a & 0x3FFFu, oper_b & 0x3FFFu);
}

typedef struct regex_token_detail_s regex_token_detail_t;
struct regex_token_detail_s {
    eRegexToken_t token;
    const char *name;
    regexTokenDetailPrint_t detail_print;
    regexTokenVMInstrPrint_t vm_print;
    eRegexTokenPriority_t priority;
    eRegexTokenTerminality_t terminal;
    int arity;
};

#define RE_TOK_DETAIL_P_(token,printer,priority,terminal,arity) \
        {token, #token, regexPrinter_ ## token, regexPrintVM_eTokenUnknown, priority, terminal, arity}
#define RE_TOK_DETAIL_PV(token,printer,priority,terminal,arity) \
        {token, #token, regexPrinter_ ## token, regexPrintVM_ ## token, priority, terminal, arity}
#define RE_TOK_DETAIL_N_(token,priority,terminal,arity) \
        {token, #token, NULL, regexPrintVM_eTokenUnknown, priority, terminal, arity}
#define RE_TOK_DETAIL_NV(token,priority,terminal,arity) \
        {token, #token, NULL, regexPrintVM_ ## token, priority, terminal, arity}

#define RE_TOK_DETAIL_END       RE_TOK_DETAIL_N_(eTokenUnknown, ePriorityNone, eReTokNotTerminal, 0)

regex_token_detail_t _regexTokenDetails[] = {
    RE_TOK_DETAIL_N_(eTokenNone, ePriorityNone, eReTokNotTerminal, 0),
    RE_TOK_DETAIL_PV(eTokenCharLiteral, regexTokenDetailCharLiteral, ePriorityNone, eReTokTerminal, 0),
    RE_TOK_DETAIL_PV(eTokenCharClass, regexTokenDetailCharClass, ePriorityNone, eReTokTerminal, 0),
    RE_TOK_DETAIL_PV(eTokenStringLiteral, regexTokenDetailStringLiteral, ePriorityNone, eReTokTerminal, 0),
    RE_TOK_DETAIL_NV(eTokenCharAny, ePriorityNone, eReTokTerminal, 0),
    RE_TOK_DETAIL_NV(eTokenMatch, ePriorityNone, eReTokNotTerminal, 0),
    RE_TOK_DETAIL_NV(eTokenSplit, ePriorityNone, eReTokNotTerminal, 0),
    RE_TOK_DETAIL_NV(eTokenJmp, ePriorityNone, eReTokNotTerminal, 0),
    RE_TOK_DETAIL_NV(eTokenSave, ePriorityNone, eReTokNotTerminal, 0),
    RE_TOK_DETAIL_PV(eTokenUtf8Class, regexTokenDetailUtf8Class, ePriorityNone, eReTokTerminal, 0),
    RE_TOK_DETAIL_NV(eTokenCharAnyDotAll, ePriorityNone, eReTokNotTerminal, 0),
    RE_TOK_DETAIL_PV(eTokenCall, regexTokenDetailCall, ePriorityNone, eReTokTerminal, 0),
    RE_TOK_DETAIL_NV(eTokenReturn, ePriorityNone, eReTokTerminal, 0),
    RE_TOK_DETAIL_NV(eTokenByte, ePriorityNone, eReTokTerminal, 0),
    RE_TOK_DETAIL_N_(eTokenConcatenation, ePriorityMedium, eReTokNotTerminal, 2),
    RE_TOK_DETAIL_N_(eTokenAlternative, ePriorityMedium, eReTokNotTerminal, 2),
    RE_TOK_DETAIL_N_(eTokenZeroOrOne, ePriorityHigh, eReTokPreceeding, 1),
    RE_TOK_DETAIL_N_(eTokenZeroOrMany, ePriorityHigh, eReTokPreceeding, 1),
    RE_TOK_DETAIL_N_(eTokenOneOrMany, ePriorityHigh, eReTokPreceeding, 1),
    RE_TOK_DETAIL_P_(eTokenSubExprStart, regexTokenDetailSubExprStart, ePriorityLow, eReTokNotPreceeding, 0),
    RE_TOK_DETAIL_N_(eTokenSubExprEnd, ePriorityLow, eReTokPreceeding, 0),
    RE_TOK_DETAIL_END
};

int regexVerifyTokenDetails(void) {
    int k;
    for(k = 0; ((_regexTokenDetails[k].token != eTokenUnknown) &&
                (_regexTokenDetails[k].token == k)); k++);
    return ((_regexTokenDetails[k].token == eTokenUnknown) &&
            (_regexTokenDetails[k].token == k));
}

void regexEmitTokenStr(FILE *fp, regex_token_t *token) {
    if(token == NULL) {
        fprintf(fp, "token: NULL\n");
    } else if((token->tokenType <= eTokenNone) || (token->tokenType >= eTokenUnknown)) {
        fprintf(fp, "token: INVALID(%d)\n", token->tokenType);
    } else {
        fprintf(fp, "token: %s ", _regexTokenDetails[token->tokenType].name);
        if(token->pc == VM_PC_UNVISITED) {
            fprintf(fp, "UNVISITED ");
        } else {
            fprintf(fp, "PC:%d ", token->pc);
        }
        if(_regexTokenDetails[token->tokenType].detail_print != NULL) {
            fputc(' ', fp);
            _regexTokenDetails[token->tokenType].detail_print(fp, token);
        }
        fputc('\n', fp);
    }
}

void regexEmitTokenStrStack(FILE *fp, regex_token_t *token) {
    for(; token != NULL; token = token->next) {
        regexEmitTokenStr(fp, token);
    }
}

void regexEmitVMInstr(FILE *fp, regex_vm_t *vm, int pc) {
    eRegexToken_t token;
    unsigned int oper_a, oper_b, instr;

    instr = vm->program[pc];
    token = (eRegexToken_t)(instr & 0xFu);
    oper_a = (instr >> 4u) & 0x3FFFu;
    oper_b = (instr >> 18u) & 0x3FFFu;

    fprintf(fp, "%4d ", pc);
    _regexTokenDetails[token].vm_print(fp, vm, oper_a, oper_b);
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

/////////////////////////////////////////////////////////////////////////////
// Unicode char class registration functions
/////////////////////////////////////////////////////////////////////////////

// Forward declarations of the unicode property classes
const char unicode_combining_marks[];
const char unicode_numeric[];
const char unicode_punctuation[];
const char unicode_whitespace[];
const char unicode_uppercase[];
const char unicode_lowercase[];
const char unicode_letter[];

typedef struct regex_unicode_charclass_entry_s regex_unicode_charclass_entry_t;
struct regex_unicode_charclass_entry_s {
    int dynamic;
    char *id;
    const char *class_str;
    regex_unicode_charclass_entry_t *next;
};

regex_unicode_charclass_entry_t _utf8_class_M_ = {
        .dynamic = 0,
        .id = "M",
        .class_str = unicode_combining_marks,
        .next = NULL
};

regex_unicode_charclass_entry_t _utf8_class_N_ = {
        .dynamic = 0,
        .id = "N",
        .class_str = unicode_numeric,
        .next = &_utf8_class_M_
};

regex_unicode_charclass_entry_t _utf8_class_P_ = {
        .dynamic = 0,
        .id = "P",
        .class_str = unicode_punctuation,
        .next = &_utf8_class_N_
};

regex_unicode_charclass_entry_t _utf8_class_Z_ = {
        .dynamic = 0,
        .id = "Z",
        .class_str = unicode_whitespace,
        .next = &_utf8_class_P_
};

regex_unicode_charclass_entry_t _utf8_class_Lu = {
        .dynamic = 0,
        .id = "Lu",
        .class_str = unicode_uppercase,
        .next = &_utf8_class_Z_
};

regex_unicode_charclass_entry_t _utf8_class_Ll = {
        .dynamic = 0,
        .id = "Ll",
        .class_str = unicode_lowercase,
        .next = &_utf8_class_Lu
};

regex_unicode_charclass_entry_t _utf8_class_L_ = {
        .dynamic = 0,
        .id = "L",
        .class_str = unicode_letter,
        .next = &_utf8_class_Ll
};

regex_unicode_charclass_entry_t *_regex_unicode_charclass_registry = &_utf8_class_L_;

int regexRegUnicodeCharClassAdd(const char *classId, const char *classStr) {
    regex_unicode_charclass_entry_t *entry, *walk;

    if((classId[0] == '\0') || ((classId[1] != '\0') && (classId[2] != '\0'))) {
        return 0;
    }

    if((entry = _regexAlloc(sizeof(regex_unicode_charclass_entry_t), _regexMemContext)) == NULL) {
        return 0;
    }
    entry->dynamic = 1;
    entry->id = _regexStrdup(classId, -1);
    entry->class_str = classStr;
    if(_regex_unicode_charclass_registry == NULL) {
        _regex_unicode_charclass_registry = entry;
    } else {
        for(walk = _regex_unicode_charclass_registry; walk->next != NULL; walk = walk->next);
        walk->next = entry;
    }
    return 1;
}

const char *regexRegUnicodeCharClassGet(const char *classId) {
    regex_unicode_charclass_entry_t *entry;

    for(entry = _regex_unicode_charclass_registry; entry != NULL; entry = entry->next) {
        if((classId[0] == entry->id[0]) && (classId[1] == entry->id[1])) {
            return entry->class_str;
        }
    }
    return NULL;
}

void regexRegUnicodeCharClassRemove(const char *classId) {
    regex_unicode_charclass_entry_t *entry, *last = NULL;

    for(entry = _regex_unicode_charclass_registry; entry != NULL; entry = entry->next) {
        if(!strcmp(classId, entry->id)) {
            break;
        }
        last = entry;
    }
    if(entry == NULL) {
        return;
    }
    if(last == NULL) {
        _regex_unicode_charclass_registry = entry->next;
    } else {
        last->next = entry->next;
    }
    if(entry->dynamic) {
        _regexDealloc(entry->id, _regexMemContext);
        _regexDealloc(entry, _regexMemContext);
    }
}

/////////////////////////////////////////////////////////////////////////////
// Token management functions
/////////////////////////////////////////////////////////////////////////////

regex_ptrlist_t *regexPtrlistCreate(regex_token_t *token, eRePtrListType_t type) {
    regex_ptrlist_t *entry;

    if((entry = _regexAlloc(sizeof(regex_ptrlist_t), _regexMemContext)) == NULL) {
        return NULL;
    }
    memset(entry, 0, sizeof(regex_ptrlist_t));
    entry->token = token;
    entry->type = type;
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
        if(list->type == eRePtrOutA) {
            list->token->out_a = token;
        } else {
            list->token->out_b = token;
        }
    }
}

void regexPtrListFree(regex_ptrlist_t *list) {
    regex_ptrlist_t *next = NULL;
    for(; list != NULL; list = next) {
        next = list->next;
        _regexDealloc(list, _regexMemContext);
    }
}

regex_token_t *regexAllocToken(eRegexToken_t tokenType, int c, char *str, int sizeOrFlags, int sub_index) {
    regex_token_t *token;

    if((token = _regexAlloc(sizeof(regex_token_t), _regexMemContext)) == NULL) {
        return NULL;
    }
    memset(token, 0, sizeof(regex_token_t));
    token->tokenType = tokenType;
    token->pc = VM_PC_UNVISITED;
    if(str != NULL) {
        token->str = str;
        token->len = sizeOrFlags;
    } else {
        token->c = c;
        token->flags = sizeOrFlags;
        token->sub_index = (short)sub_index;
    }
    return token;
}

int regexTokenCreate(regex_token_t **list, eRegexToken_t tokenType, int c, char *str, int sizeOrFlags, int sub_index) {
    regex_token_t *token, *walk;

    if((token = regexAllocToken(tokenType, c, str, sizeOrFlags, sub_index)) == NULL) {
        return 0;
    }

    if(*list == NULL) {
        *list = token;
    } else {
        for(walk = *list; walk->next != NULL; walk = walk->next);
        if(regexTokenIsTerminal(token, 0) && regexTokenIsTerminal(walk, 1)) {
            // Two adjacent terminals have an implicit concatenation
            if((walk->next = regexAllocToken(eTokenConcatenation, 0, NULL, 0, 0)) == NULL) {
                _regexDealloc(token, _regexMemContext);
                return 0;
            }
            walk = walk->next;
        }
        walk->next = token;
    }

    return 1;
}

void regexTokenDestroy(regex_token_t *token, int stack) {
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
                    _regexDealloc(token->str, _regexMemContext);
                }
                break;
            default:
                break;
        }
        regexPtrListFree(token->ptrlist);
        _regexDealloc(token, _regexMemContext);
        if(!stack) {
            break;
        }
    }
}

/////////////////////////////////////////////////////////////////////////////
// Subroutine index table
/////////////////////////////////////////////////////////////////////////////

// CRC32 routines public domain from http://home.thep.lu.se/~bjorn/crc/

uint32_t calcCrc32ForByte(uint32_t r) {
    for(int j = 0; j < 8; ++j) {
        r = (r & 1 ? 0 : (uint32_t) 0xEDB88320L) ^ r >> 1;
    }
    return r ^ (uint32_t)0xFF000000L;
}

void calcCrc32(const void *data, size_t n_bytes, uint32_t* crc) {
    static uint32_t table[0x100];
    if(!*table) {
        for(size_t i = 0; i < 0x100; ++i) {
            table[i] = calcCrc32ForByte(i);
        }
    }
    for(size_t i = 0; i < n_bytes; ++i) {
        *crc = table[((unsigned char)(*crc)) ^ ((unsigned char *)data)[i]] ^ (*crc >> 8);
    }
}

regex_subroutine_t *regexSubroutineIdxCreate(regex_vm_build_t *build) {
    regex_subroutine_t *entry, *walk;

    if((entry = _regexAlloc(sizeof(regex_subroutine_t), _regexMemContext)) == NULL) {
        return NULL;
    }
    entry->id = 1;
    if(build->subroutine_index != NULL) {
        for(walk = build->subroutine_index; walk != NULL; walk = walk->next) {
            if(entry->id <= walk->id) {
                entry->id = walk->id + 1;
            }
        }
    }
    entry->next = build->subroutine_index;
    build->subroutine_index = entry;
    return entry;
}

regex_subroutine_t *regexSubroutineIdxGet(regex_vm_build_t *build, int id) {
    regex_subroutine_t *entry;

    for(entry = build->subroutine_index; entry != NULL; entry = entry->next) {
        if(entry->id == id) {
            return entry;
        }
    }
    return NULL;
}

eRegexCompileStatus_t regexTokenizePattern(const char *pattern,
                                           int *pos,
                                           regex_token_t **tokens,
                                           regex_vm_build_t *build);

regex_subroutine_t *regexSubroutineIdxGetPattern(regex_vm_build_t *build, const char *pattern) {
    regex_subroutine_t *walk;
    uint32_t crc = 0;

    calcCrc32(pattern, strlen(pattern), &crc);
    if(crc == 0) {
        return NULL;
    }

    for(walk = build->subroutine_index; walk != NULL; walk = walk->next) {
        if(walk->crc == crc) {
            return walk;
        }
    }
    return NULL;
}

regex_subroutine_t *regexSubroutineIdxGetName(regex_vm_build_t *build, const char *name, int len) {
    regex_subroutine_t *walk;

    for(walk = build->subroutine_index; walk != NULL; walk = walk->next) {
        if((walk->name != NULL) && (!strncmp(walk->name, name, len))) {
            return walk;
        }
    }
    return NULL;
}

void regexSubroutineIdxFree(regex_vm_build_t *build) {
    regex_subroutine_t *walk, *next;

    for(walk = build->subroutine_index; walk != NULL; walk = next) {
        next = walk->next;
        if(walk->name != NULL) {
            _regexDealloc(walk->name, _regexMemContext);
        }
        _regexDealloc(walk, _regexMemContext);
    }
    build->subroutine_index = NULL;
}

eRegexCompileStatus_t regexSubroutineGenerateFromPattern(regex_token_t **tokens,
                                                         regex_token_t **subroutine,
                                                         regex_vm_build_t *build,
                                                         const char *pattern,
                                                         const char *name,
                                                         int nameLen,
                                                         int *sub_id) {
    int pos;
    eRegexCompileStatus_t status;
    regex_subroutine_t *entry = NULL;

    printf("1\n");
    // Is there already a subroutine index entry?
    if((sub_id != NULL) && (*sub_id > 0)) {
        if((entry = regexSubroutineIdxGet(build, *sub_id)) == NULL) {
            return eCompileInternalError;
        }
    } else {
        if((pattern != NULL) && (entry = regexSubroutineIdxGetPattern(build, pattern)) == NULL) {
            if(name != NULL) {
                entry = regexSubroutineIdxGetName(build, name, nameLen);
            }
        }
    }

    printf("2\n");
    if(entry == NULL) {
        // We're creating a new entry
        if((pattern == NULL) && (subroutine == NULL) && ((name == NULL) || (nameLen <= 0))) {
            // There's nothing here to identify the new entry!
            return eCompileInternalError;
        }
        if((entry = regexSubroutineIdxCreate(build)) == NULL) {
            return eCompileOutOfMem;
        }
    }

    printf("3\n");
    if((subroutine != NULL) && (entry->tokens == NULL)) {
        if((*subroutine == NULL) && (pattern != NULL)) {
            if((status = regexTokenizePattern(pattern, &pos, subroutine, build)) != eCompileOk) {
                // TODO - retain sub-compilation status
                return status;
            }
        }
        entry->tokens = *subroutine;
    }
    if((pattern != NULL) && (entry->crc == 0)) {
        calcCrc32(pattern, strlen(pattern), &(entry->crc));
    }
    if((name != NULL) && (entry->name == NULL) && (nameLen >= 0)) {
        if((entry->name = _regexStrdup(name, nameLen + 1)) == NULL) {
            return 0;
        }
        entry->name[nameLen] = '\0';
    }
    printf("4\n");
    if(tokens != NULL) {
        if(!regexTokenCreate(tokens, eTokenCall, entry->id, NULL, 0, 0)) {
            return eCompileOutOfMem;
        }
    }
    printf("5\n");
    if(sub_id != NULL) {
        *sub_id = entry->id;
    }
    return eCompileOk;
}

/////////////////////////////////////////////////////////////////////////////
// Character parsing, assists with escaped chars
/////////////////////////////////////////////////////////////////////////////

int parseIsHexdigit(char c) {
    return (((c >= 'a') && (c <= 'f')) ||       // a - f
            ((c >= 'A') && (c <= 'F')) ||       // A - F
            ((c >= '0') && (c <= '9')));        // 0 - 9
}

int parseGetHexValue(char c) {
    if ((c >= 'a') && (c <= 'f')) {
        return c - 'a' + 10;
    } else if((c >= 'A') && (c <= 'F')) {
        return c - 'A' + 10;
    } else if((c >= '0') && (c <= '9')) {
        return c - '0';
    }
    return 0; // This should never be reached if c is a hex digit
}

typedef enum {
    eRegexPatternEnd = 0,
    eRegexPatternChar,
    eRegexPatternUnicode,
    eRegexPatternInvalid,
    eRegexPatternInvalidEscape,
    eRegexPatternMetaChar,
    eRegexPatternMetaClass,
    eRegexPatternUnicodeMetaClass,
    eRegexPatternEscapedChar
} eRegexPatternCharState_t;

typedef struct parseChar_s parseChar_t;
struct parseChar_s {
    eRegexPatternCharState_t state;
    int c;
};

int parsePatternIsValid(parseChar_t *pc) {
    if((pc->state == eRegexPatternEnd) || (pc->state == eRegexPatternInvalid) ||
       (pc->state == eRegexPatternInvalidEscape)) {
        return 0;
    }
    return 1;
}

parseChar_t parseGetNextPatternChar(const char **pattern) {
    parseChar_t result = {
        .c = 0,
        .state = eRegexPatternInvalid
    };
    int k;
    char classId[3];

    if(**pattern == '\0') {
        result.state = eRegexPatternEnd;
        return result;
    }

    if(**pattern == '\\') {
        switch(*(*pattern + 1)) {
            case '\0': // End of pattern string
                result.state = eRegexPatternInvalid;
                return result;

            case 'p': // unicode class
            case 'P': // unicode class (inverted)
                if(*(*pattern + 2) == '{') {
                    classId[0] = *(*pattern + 3);
                    if((classId[0] == '\0') || (classId[0] == '}')) {
                        return result;
                    }
                    classId[1] = *(*pattern + 4);
                    if(!((classId[1] == '}') || (*(*pattern + 5) == '}'))) {
                        return result;
                    }
                    if(classId[1] == '}') {
                        classId[1] = '\0';
                    } else {
                        classId[2] = '\0';
                    }
                    if(regexRegUnicodeCharClassGet(classId) == NULL) {
                        result.state = eRegexPatternInvalidEscape;
                    } else {
                        result.state = eRegexPatternUnicodeMetaClass;
                        result.c = classId[0] | (classId[1] << 8);
                        if(*(*pattern + 1) == 'P') {
                            result.c |= 0x10000;
                        }
                    }
                    return result;
                }
                return result;

            case 'd': // digit
            case 'D': // non digit
            case 's': // whitespace
            case 'S': // non whitespace
            case 'w': // word character
            case 'W': // non word character
            case 'B': // explicit byte match
            case 'X': // full unicode glyph (base + markers)
            case 'R': // Subroutine call
                result.state = eRegexPatternMetaClass;
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

            case 'u': // unicode codepoint (0 - 0xFFFF)
                for(k = 2, result.c = 0; k <= 5; k++) {
                    result.c *= 16;
                    if(parseIsHexdigit(*(*pattern + k))) {
                        result.c += parseGetHexValue(*(*pattern + k));
                    } else {
                        return result;
                    }
                }
                result.state = eRegexPatternUnicode;
                return result;

            case 'U': // unicode codepoint (0x010000 - 0x10FFFF)
                for(k = 2, result.c = 0; k < 7; k++) {
                    result.c *= 16;
                    if(parseIsHexdigit(*(*pattern + k))) {
                        result.c += parseGetHexValue(*(*pattern + k));
                    } else {
                        return result;
                    }
                }
                result.state = eRegexPatternUnicode;
                return result;

            case 'x': // hexidecimal encoded byte
                for(k = 0, result.c = 0; k < 2; k++) {
                    (*pattern)++;
                    result.c *= 16;
                    if(parseIsHexdigit(**pattern)) {
                        result.c += parseGetHexValue(**pattern);
                    } else {
                        return result;
                    }
                }
                result.state = eRegexPatternEscapedChar;
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
            case '$':   // literal dollar sign
                result.state = eRegexPatternEscapedChar;
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
        case '^':   // start of string assertion
        case '+':   // one or more
        case '(':   // subexpression start
        case '[':   // character class start
        case ')':   // subexpression end
        case '$':   // end of string assertion
            result.c = **pattern;
            result.state = eRegexPatternMetaChar;
            return result;

        default:
            result.c = **pattern;
            result.state = eRegexPatternChar;
            return result;
    }
}

void parsePatternCharAdvance(const char **pattern) {
    if(**pattern == '\\') {
        if((*(*pattern + 1) == 'x') && (*(*pattern + 2) != '\0') && (*(*pattern + 3) != '\0')) {
            *pattern += 4; // \x##
        } else if((*(*pattern + 1) == 'u') && (*(*pattern + 2) != '\0') && (*(*pattern + 3) != '\0') &&
                  (*(*pattern + 4) != '\0') && (*(*pattern + 5) != '\0')) {
            *pattern += 6; // \u####
        } else if((*(*pattern + 1) == 'U') && (*(*pattern + 2) != '\0') && (*(*pattern + 3) != '\0') &&
                  (*(*pattern + 4) != '\0') && (*(*pattern + 5) != '\0') && (*(*pattern + 6) != '\0') &&
                  (*(*pattern + 7) != '\0')) {
            *pattern += 8; // \U######
        } else if((*(*pattern + 1) == 'p') && (*(*pattern + 2) == '{')) {
            if(*(*pattern + 4) == '}') {
                *pattern += 5; // \p{_}
            } else if(*(*pattern + 5) == '}') {
                *pattern += 6; // \p{__}
            }
        } else if((*(*pattern + 1) != '\0')) {
            *pattern += 2;
        }
    } else if(**pattern != '\0'){
        (*pattern)++;
    }
}

int parseIsAlnum(char c) {
    return (((c >= 'a') && (c <= 'z')) ||       // a - z
            ((c >= 'A') && (c <= 'Z')) ||       // A - Z
            ((c >= '0') && (c <= '9')));        // 0 - 9
}

int parseCheckNextPatternChar(const char **pattern, char c) {
    parseChar_t pc;

    pc = parseGetNextPatternChar(pattern);
    if(!parsePatternIsValid(&pc)) {
        return 0;
    }
    if((pc.c == '\0') || (pc.c != c)) {
        return 0;
    }
    parsePatternCharAdvance(pattern);
    return 1;
}

/////////////////////////////////////////////////////////////////////////////
// String literal parsing handlers
/////////////////////////////////////////////////////////////////////////////

int parseUtf8EncodingByteLen(int c) {
    if(c > 65535) {
        return 4;
    } else if(c >2047) {
        return 3;
    } else if(c > 127) {
        return 2;
    }
    return 1;
}

// bytes should be a 5 byte sequence, and c a unicode codepoint
// bytes will be filled with the byte count, and the utf8 encoding bytes
// from high to low
void parseUtf8EncodeSequence(unsigned char *bytes, int c) {
    if(c <= 127) { // 1 byte
        bytes[0] = 1;
        bytes[1] = (unsigned int)c;
    } else if(c <= 2047) { // 2 byte (5 prefix bits, 0xC0)
        bytes[0] = 2;
        bytes[1] = (((unsigned int)c & 0x7C0u) >> 6u);
        bytes[2] = ((unsigned int)c & 0x3Fu);
    } else if(c <= 65535) { // 3 byte (4 prefix bits, 0xE0)
        bytes[0] = 3;
        bytes[1] = (((unsigned int)c & 0xF000u) >> 12u);
        bytes[2] = (((unsigned int)c & 0xFC0u) >> 6u);
        bytes[3] = ((unsigned int)c & 0x3Fu);
    } else { // 4 byte (3 prefix bits, 0xF0)
        bytes[0] = 4;
        bytes[1] = (((unsigned int)c & 0x7000000u) >> 18u);
        bytes[2] = (((unsigned int)c & 0x3F000u) >> 12u);
        bytes[3] = (((unsigned int)c & 0xFC0u) >> 6u);
        bytes[4] = ((unsigned int)c & 0x3Fu);
    }
}

#define UTF8_FOUR_BYTE_PREFIX   0xF0
#define UTF8_FOUR_BYTE_MASK     0xF8
#define UTF8_FOUR_BYTE_BITMASK  0x07
#define UTF8_THREE_BYTE_PREFIX  0xE0
#define UTF8_THREE_BYTE_MASK    0xF0
#define UTF8_THREE_BYTE_BITMASK 0x0F
#define UTF8_TWO_BYTE_PREFIX    0xC0
#define UTF8_TWO_BYTE_MASK      0xE0
#define UTF8_TWO_BYTE_BITMASK   0x1F
#define UTF8_LOW_BYTE_PREFIX    0x80
#define UTF8_LOW_BYTE_MASK      0xC0
#define UTF8_LOW_BYTE_BITMASK   0x3F

int parseUtf8EncodedHighByte(char c) {
    if((c & UTF8_TWO_BYTE_MASK) == UTF8_TWO_BYTE_PREFIX) {
        return 2;
    }
    if((c & UTF8_THREE_BYTE_MASK) == UTF8_THREE_BYTE_PREFIX) {
        return 3;
    }
    if((c & UTF8_FOUR_BYTE_MASK) == UTF8_FOUR_BYTE_PREFIX) {
        return 4;
    }
    return 1;
}

int parseUtf8DecodeSequence(const char *str) {
    if((str[0] & UTF8_TWO_BYTE_MASK) == UTF8_TWO_BYTE_PREFIX) {
        if((str[1] & UTF8_LOW_BYTE_MASK) == UTF8_LOW_BYTE_PREFIX) {
            // two byte utf8
            return ((str[0] & UTF8_TWO_BYTE_BITMASK) << 6) | (str[1] & UTF8_LOW_BYTE_BITMASK);
        }
    } else if((str[0] & UTF8_THREE_BYTE_MASK) == UTF8_THREE_BYTE_PREFIX) {
        if((str[1] & UTF8_LOW_BYTE_MASK) == UTF8_LOW_BYTE_PREFIX) {
            if((str[2] & UTF8_LOW_BYTE_MASK) == UTF8_LOW_BYTE_PREFIX) {
                // three byte utf8
                return ((str[0] & UTF8_THREE_BYTE_BITMASK) << 12) | ((str[1] & UTF8_LOW_BYTE_BITMASK) << 6) |
                        (str[2] & UTF8_LOW_BYTE_BITMASK);
            }
        }
    } else if((str[0] & UTF8_FOUR_BYTE_MASK) == UTF8_FOUR_BYTE_PREFIX) {
        if((str[1] & UTF8_LOW_BYTE_MASK) == UTF8_LOW_BYTE_PREFIX) {
            if((str[2] & UTF8_LOW_BYTE_MASK) == UTF8_LOW_BYTE_PREFIX) {
                if((str[3] & UTF8_LOW_BYTE_MASK) == UTF8_LOW_BYTE_PREFIX) {
                    // four byte utf8
                    return ((str[0] & UTF8_FOUR_BYTE_BITMASK) << 18) | ((str[1] & UTF8_LOW_BYTE_BITMASK) << 12) |
                            ((str[2] & UTF8_LOW_BYTE_BITMASK) << 6) | (str[3] & UTF8_LOW_BYTE_BITMASK);
                }
            }
        }
    }
    return -1;
}

// parseGetPatternStrLen determines the length in characters AND bytes to
// represent a string in the pattern. The low word is the character count, and
// the high word is the number of additional bytes needed to accommodate any
// utf8 encoded characters within the string.

#define PARSE_DEC_UTF8_COUNT(count)         (((unsigned int)count >> 16u) && 0xFFu)
#define PARSE_DEC_CHAR_COUNT(count)         ((unsigned int)count & 0xFFFFFFu)

int parseGetPatternStrLen(const char **pattern) {
    const char *ptr = *pattern;
    unsigned int count = 0;
    unsigned int utf8 = 0; // additional bytes needed to accommodate utf8 chars in the str
    parseChar_t c;

    for(;;) {
        c = parseGetNextPatternChar(&ptr);
        switch(c.state) {
            case eRegexPatternEnd:
            case eRegexPatternMetaChar:
            case eRegexPatternUnicodeMetaClass:
            case eRegexPatternMetaClass:
                return (int)(utf8 ? ((utf8 << 16u) | count) : count);

            case eRegexPatternChar:
            case eRegexPatternEscapedChar:
                count++;
                break;

            case eRegexPatternUnicode:
                count++;
                utf8 += parseUtf8EncodingByteLen(c.c) - 1;
                break;

            case eRegexPatternInvalid:
            case eRegexPatternInvalidEscape:
                return -1;

        }
        parsePatternCharAdvance(&ptr);
    }
}

// Allocates len + 1 bytes and decodes a string from pattern into the buffer.
// Note: this function expects the len to have been pre-validated, and does NOT
// recognize the null character, to allow it to be used as an actual value
// within the string.
char *parseGetPatternStr(const char **pattern, int len, int size) {
    char *str, *ptr;
    int k;
    parseChar_t c;

    if((str = _regexAlloc(size + 1, _regexMemContext)) == NULL) {
        return NULL;
    }
    ptr = str;

    for(; len; len--) {
        c = parseGetNextPatternChar(pattern);
        parsePatternCharAdvance(pattern);
        if(c.state == eRegexPatternUnicode) {
            for(k = parseUtf8EncodingByteLen(c.c); k; k--) {
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

int regexGetEscapedStrLen(const char *str, int len) {
    int k, out = 0, codepoint;

    for(k = 0; k < len; k++) {
        // Check for utf8 encoding
        if((codepoint = parseUtf8DecodeSequence(str + k)) != -1) {
            if(codepoint > 0xFFFF) {
                out += 8; // \U######
                k += 3;
            } else if(codepoint > 2047) {
                out += 6; // \u####
                k += 2;
            } else if(codepoint > 127) {
                out += 6; // \u####
                k += 1;
            }
            if(codepoint > 127) {
                continue;
            }
        }
        switch(str[k]) {
            case '\0':
            case '\a':
            case '\b':
            case '\x1b':
            case '\f':
            case '\n':
            case '\r':
            case '\t':
            case '\v':
                out += 2;
                break;

            default:
                if((str[k] >= ' ') && (str[k] <= 127)) {
                    out++;
                } else {
                    out += 4; // \x##
                }
                break;
        }
    }
    return out;
}

void regexEmitEscapedStr(FILE *fp, const char *str, int len) {
    int k;
    int codepoint;

    if(str == NULL) {
        fputs("NULL", fp);
    }
    for(k = 0; k < len; k++) {
        // Check for utf8 encoding
        if((codepoint = parseUtf8DecodeSequence(str + k)) != -1) {
            if(codepoint > 0xFFFF) {
                k += 3;
                fprintf(fp, "\\U%6.6X", codepoint);
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

typedef struct utf8_charclass_midlow_byte_s utf8_charclass_midlow_byte_t;
struct utf8_charclass_midlow_byte_s {
    unsigned char prefix;
    unsigned int bitmap[2];
    utf8_charclass_midlow_byte_t *next;
};

typedef struct utf8_charclass_midhigh_byte_s utf8_charclass_midhigh_byte_t;
struct utf8_charclass_midhigh_byte_s {
    unsigned char prefix;
    utf8_charclass_midlow_byte_t *midlow_byte;
    utf8_charclass_midhigh_byte_t *next;
};

typedef struct utf8_charclass_high_byte_s utf8_charclass_high_byte_t;
struct utf8_charclass_high_byte_s {
    unsigned char prefix;
    utf8_charclass_midhigh_byte_t *midhigh_byte;
    utf8_charclass_high_byte_t *next;
};

typedef struct utf8_charclass_tree_s utf8_charclass_tree_t;
struct utf8_charclass_tree_s {
    utf8_charclass_high_byte_t *four_byte;
    utf8_charclass_midhigh_byte_t *three_byte;
    utf8_charclass_midlow_byte_t *two_byte;
    unsigned int one_byte[8];
};

utf8_charclass_midlow_byte_t *parseUtf8GetMidLowByte(utf8_charclass_midlow_byte_t **base,
                                                     unsigned char prefix) {
    utf8_charclass_midlow_byte_t *midlow, *ml_last = NULL;

    for(midlow = *base; midlow != NULL; midlow = midlow->next) {
        ml_last = midlow;
        if(prefix == midlow->prefix) {
            return midlow;
        }
    }
    if((midlow = _regexAlloc(sizeof(utf8_charclass_midlow_byte_t), _regexMemContext)) == NULL) {
        return NULL;
    }
    midlow->prefix = prefix;
    if(ml_last == NULL) {
        *base = midlow;
    } else {
        ml_last->next = midlow;
    }
    return midlow;
}

utf8_charclass_midhigh_byte_t *parseUtf8GetMidHighByte(utf8_charclass_midhigh_byte_t **base,
                                                       unsigned char prefix) {
    utf8_charclass_midhigh_byte_t *midhigh, *mh_last = NULL;

    for(midhigh = *base; midhigh != NULL; midhigh = midhigh->next) {
        mh_last = midhigh;
        if(prefix == midhigh->prefix) {
            return midhigh;
        }
    }
    if((midhigh = _regexAlloc(sizeof(utf8_charclass_midlow_byte_t), _regexMemContext)) == NULL) {
        return NULL;
    }
    midhigh->prefix = prefix;
    if(mh_last == NULL) {
        *base = midhigh;
    } else {
        mh_last->next = midhigh;
    }
    return midhigh;
}

utf8_charclass_high_byte_t *parseUtf8GetHighByte(utf8_charclass_high_byte_t **base,
                                                    unsigned char prefix) {
    utf8_charclass_high_byte_t *high, *last = NULL;

    for(high = *base; high != NULL; high = high->next) {
        last = high;
        if(prefix == high->prefix) {
            return high;
        }
    }
    if((high = _regexAlloc(sizeof(utf8_charclass_midlow_byte_t), _regexMemContext)) == NULL) {
        return NULL;
    }
    high->prefix = prefix;
    if(last == NULL) {
        *base = high;
    } else {
        last->next = high;
    }
    return high;
}

unsigned int *parseUtf8TreeGetLowByte(utf8_charclass_tree_t *tree, unsigned char *bytes) {
    utf8_charclass_midlow_byte_t *midlow;
    utf8_charclass_midhigh_byte_t *midhigh;
    utf8_charclass_high_byte_t *high;

    switch(bytes[0]) {
        default:
        case 1:
            return tree->one_byte;

        case 2:
            if((midlow = parseUtf8GetMidLowByte(&(tree->two_byte), bytes[1])) == NULL) {
                return NULL;
            }
            return midlow->bitmap;

        case 3:
            if((midhigh = parseUtf8GetMidHighByte(&(tree->three_byte), bytes[1])) == NULL) {
                return NULL;
            }
            if((midlow = parseUtf8GetMidLowByte(&(midhigh->midlow_byte), bytes[2])) == NULL) {
                return NULL;
            }
            return midlow->bitmap;

        case 4:
            if((high = parseUtf8GetHighByte(&(tree->four_byte), bytes[1])) == NULL) {
                return NULL;
            }
            if((midhigh = parseUtf8GetMidHighByte(&(high->midhigh_byte), bytes[2])) == NULL) {
                return NULL;
            }
            if((midlow = parseUtf8GetMidLowByte(&(midhigh->midlow_byte), bytes[3])) == NULL) {
                return NULL;
            }
            return midlow->bitmap;
    }
}

unsigned int *charClassBitmapCopy(unsigned int *bitmap) {
    unsigned int *copy;

    if((copy = _regexAlloc(32, _regexMemContext)) == NULL) {
        return NULL;
    }
    memcpy(copy, bitmap, 32);
    return copy;
}

unsigned int *utf8ClassBitmapCopy(unsigned int *bitmap, int invert) {
    unsigned int *copy;
    int k;

    if((copy = _regexAlloc(2, _regexMemContext)) == NULL) {
        return NULL;
    }
    memcpy(copy, bitmap, 8);
    if(invert) {
        for(k = 0; k < 2; k++) {
            copy[k] ^= 0xFFFFFFFF;
        }
    }
    return copy;
}

int regexTokenUtf8ClassMidlowCreate(regex_token_t **tokens, utf8_charclass_midlow_byte_t *midlow,
                                    int first, int nested, int invert) {
    unsigned int *bitmap;

    if(!first) {
        if(!regexTokenCreate(tokens, eTokenAlternative, 0, NULL, 0,0)) {
            return 0;
        }
    }
    if(!regexTokenCreate(tokens, eTokenSubExprStart, 0, NULL, REGEX_TOKEN_FLAG_NOCAPTURE, 0)) {
        return 0;
    }
    if(!regexTokenCreate(tokens, eTokenCharLiteral,
                         midlow->prefix | (nested ? UTF8_LOW_BYTE_PREFIX : UTF8_TWO_BYTE_PREFIX),
                         NULL, (invert ? REGEX_TOKEN_FLAG_INVERT : 0), 0)) {
        return 0;
    }
    if((bitmap = utf8ClassBitmapCopy(midlow->bitmap, invert)) == NULL) {
        return 0;
    }
    if(!regexTokenCreate(tokens, eTokenUtf8Class, 0, (char *)bitmap, 0, 0)) {
        return 0;
    }
    if(!regexTokenCreate(tokens, eTokenSubExprEnd, 0, NULL, 0, 0)) {
        return 0;
    }
    return 1;
}

int regexTokenUtf8ClassMidhighCreate(regex_token_t **tokens, utf8_charclass_midhigh_byte_t *midhigh,
                                     int first, int nested, int invert) {
    utf8_charclass_midlow_byte_t *midlow;

    if(!first) {
        if(!regexTokenCreate(tokens, eTokenAlternative, 0, NULL, 0, 0)) {
            return 0;
        }
    }
    if(!regexTokenCreate(tokens, eTokenSubExprStart, 0, NULL, REGEX_TOKEN_FLAG_NOCAPTURE, 0)) {
        return 0;
    }
    if(!regexTokenCreate(tokens, eTokenCharLiteral,
                         midhigh->prefix | (nested ? UTF8_LOW_BYTE_PREFIX : UTF8_THREE_BYTE_PREFIX),
                         NULL, (invert ? REGEX_TOKEN_FLAG_INVERT : 0), 0)) {
        return 0;
    }
    first = 1;
    for(midlow = midhigh->midlow_byte; midlow != NULL; midlow = midlow->next) {
        if(!regexTokenUtf8ClassMidlowCreate(tokens, midlow, first, 1, invert)) {
            return 0;
        }
        first = 0;
    }
    if(!regexTokenCreate(tokens, eTokenSubExprEnd, 0, NULL, 0, 0)) {
        return 0;
    }
    return 1;
}

int regexTokenUtf8ClassHighCreate(regex_token_t **tokens, utf8_charclass_high_byte_t *high, int first, int invert) {
    utf8_charclass_midhigh_byte_t *midhigh;

    if(!first) {
        if(!regexTokenCreate(tokens, eTokenAlternative, 0, NULL, 0, 0)) {
            return 0;
        }
    }
    if(!regexTokenCreate(tokens, eTokenSubExprStart, 0, NULL, REGEX_TOKEN_FLAG_NOCAPTURE, 0)) {
        return 0;
    }
    if(!regexTokenCreate(tokens, eTokenCharLiteral,
                         high->prefix | UTF8_FOUR_BYTE_PREFIX,
                         NULL, (invert ? REGEX_TOKEN_FLAG_INVERT : 0), 0)) {
        return 0;
    }
    first = 1;
    for(midhigh = high->midhigh_byte; midhigh != NULL; midhigh = midhigh->next) {
        if(!regexTokenUtf8ClassMidhighCreate(tokens, midhigh, first, 1, invert)) {
            return 0;
        }
        first = 0;
    }
    if(!regexTokenCreate(tokens, eTokenSubExprEnd, 0, NULL, 0, 0)) {
        return 0;
    }
    return 1;
}

int regexTokenUtf8ClassCreate(regex_vm_build_t *build, eRegexCompileStatus_t *status,
                              regex_token_t **tokens, utf8_charclass_tree_t *tree,
                              int invert, const char *pattern, const char *classId) {
    regex_token_t *utf8class = NULL;
    utf8_charclass_midlow_byte_t *midlow;
    utf8_charclass_midhigh_byte_t *midhigh;
    utf8_charclass_high_byte_t *high;
    unsigned int *bitmap;
    int k, first = 1;

    // Preset to OoM, change to eCompileOk on success
    *status = eCompileOutOfMem;

    if(((classId != NULL) && (regexSubroutineIdxGetName(build, classId, strlen(classId)) == NULL)) ||
       ((classId == NULL) && (regexSubroutineIdxGetPattern(build, pattern) == NULL))) {
        if(!regexTokenCreate(&utf8class, eTokenSubExprStart, 0, NULL, REGEX_TOKEN_FLAG_NOCAPTURE, 0)) {
            return 0;
        }

        // Single byte character encoding
        // Are low bytes set?
        for(k = 0; k < 32; k++) {
            if(tree->one_byte[k] != 0) {
                break;
            }
        }
        if(k < 32) {
            // The single byte encoding has a glyph set
            if(invert) {
                for(k = 0; k < 32; k++) {
                    tree->one_byte[k] ^= 0xFFu;
                }
            }
            if((bitmap = charClassBitmapCopy(tree->one_byte)) == NULL) {
                *status = eCompileOutOfMem;
                return 0;
            }
            if(!regexTokenCreate(&utf8class, eTokenCharClass, 0, (char *) bitmap, 0, 0)) {
                *status = eCompileOutOfMem;
                return 0;
            }
            first = 0;
        }

        if(tree->two_byte != NULL) {
            if(!first) {
                if(!regexTokenCreate(&utf8class, eTokenAlternative, 0, NULL, 0, 0)) {
                    return 0;
                }
            }
            if(!regexTokenCreate(&utf8class, eTokenSubExprStart, 0, NULL, REGEX_TOKEN_FLAG_NOCAPTURE, 0)) {
                return 0;
            }
            first = 1;
            for(midlow = tree->two_byte; midlow != NULL; midlow = midlow->next) {
                if(!regexTokenUtf8ClassMidlowCreate(&utf8class, midlow, first, 0, invert)) {
                    return 0;
                }
                first = 0;
            }
            if(!regexTokenCreate(&utf8class, eTokenSubExprEnd, 0, NULL, 0, 0)) {
                return 0;
            }
        }

        if(tree->three_byte != NULL) {
            if(!first) {
                if(!regexTokenCreate(&utf8class, eTokenAlternative, 0, NULL, 0, 0)) {
                    return 0;
                }
            }
            if(!regexTokenCreate(&utf8class, eTokenSubExprStart, 0, NULL, REGEX_TOKEN_FLAG_NOCAPTURE, 0)) {
                return 0;
            }
            first = 1;
            for(midhigh = tree->three_byte; midhigh != NULL; midhigh = midhigh->next) {
                if(!regexTokenUtf8ClassMidhighCreate(&utf8class, midhigh, first, 0, invert)) {
                    return 0;
                }
                first = 0;
            }
            if(!regexTokenCreate(&utf8class, eTokenSubExprEnd, 0, NULL, 0, 0)) {
                return 0;
            }
        }

        if(tree->four_byte != NULL) {
            if(!first) {
                if(!regexTokenCreate(&utf8class, eTokenAlternative, 0, NULL, 0, 0)) {
                    return 0;
                }
            }
            if(!regexTokenCreate(&utf8class, eTokenSubExprStart, 0, NULL, REGEX_TOKEN_FLAG_NOCAPTURE, 0)) {
                return 0;
            }
            first = 1;
            for(high = tree->four_byte; high != NULL; high = high->next) {
                if(!regexTokenUtf8ClassHighCreate(&utf8class, high, first, invert)) {
                    return 0;
                }
                first = 0;
            }
            if(!regexTokenCreate(&utf8class, eTokenSubExprEnd, 0, NULL, 0, 0)) {
                return 0;
            }
        }

        if(!regexTokenCreate(&utf8class, eTokenSubExprEnd, 0, NULL, 0, 0)) {
            return 0;
        }
    }

    if((*status = regexSubroutineGenerateFromPattern(tokens, &utf8class, build, pattern, classId, 3, NULL)) != eCompileOk) {
        return 0;
    }

    *status = eCompileOk;
    return 1;
}

#define UTF8_ENC_LOW_BYTE_FULL_RANGE        0x3F // 10xxxxxx
#define UTF8_ENC_ONE_BYTE_FULL_RANGE        0x7F // 0xxxxxxx
#define UTF8_ENC_HIGH_TWO_BYTE_FULL_RANGE   0x1F // 110xxxxx
#define UTF8_ENC_HIGH_THREE_BYTE_FULL_RANGE 0x0F // 1110xxxx
#define UTF8_ENC_HIGH_FOUR_BYTE_FULL_RANGE  0x07 // 11110xxx

int parseUtf8CharClassCodepoint(utf8_charclass_tree_t *tree, int codepoint) {
    unsigned int *bitmap;
    unsigned char bytes[5];
    int byte;

    parseUtf8EncodeSequence(bytes, codepoint);
    if((bitmap = parseUtf8TreeGetLowByte(tree, bytes)) == NULL) {
        return 0;
    }
    switch(bytes[0]) {
        default:
        case 1: byte = bytes[1]; break;
        case 2: byte = bytes[2]; break;
        case 3: byte = bytes[3]; break;
        case 4: byte = bytes[4]; break;
    }
    bitmap[byte / 32] |= 0x1u << (byte % 32u);
    return 1;
}

void parseUtf8SetCharClassBitmapRange(unsigned int *bitmap, int start, int end) {
    int k;

    for(k = start; k <= end; k++) {
        bitmap[k / 32] |= 0x1u << (k % 32u);
    }
}

int parseUtf8SetTreeClassBitmapRange(utf8_charclass_tree_t *tree, int count,
                                     int high, int midhigh, int midlow,
                                     int start, int end) {
    unsigned int *bitmap;
    unsigned char bytes[5];

    bytes[0] = count;
    switch(count) {
        case 2:
            bytes[1] = midlow;
            break;
        case 3:
            bytes[1] = midhigh;
            bytes[2] = midlow;
            break;
        case 4:
            bytes[1] = high;
            bytes[2] = midhigh;
            bytes[3] = midlow;
            break;
        default:
            break;
    }
    if((bitmap = parseUtf8TreeGetLowByte(tree, bytes)) == NULL) {
        return 0;
    }
    parseUtf8SetCharClassBitmapRange(bitmap, start, end);
    return 1;
}

int parseUtf8TreeGenerateRange(utf8_charclass_tree_t *tree,
                               unsigned char *start, unsigned char *end) {
    // Byte order for documenting byte encodings: DCBA
    // a_ references the start, b_ references the end
    // i___ references an iterated intermediary value between two points:
    //     _x__ - ABCD, the byte identifier
    //     __x_ - ab0, range base, a, b, or zero
    //     ___x - abHL, range end, a (exclusive), b (exclusive),
    //                             high byte max value (inclusive),
    //                             low byte max value (inclusive)
    // H references the max value for the high byte
    // L references the max value for the low bytes

    int mh, ml, h;

    switch(start[0]) {
        case 1:
            // aA - bA
            if(!parseUtf8SetTreeClassBitmapRange(tree, 1, 0, 0, 0, start[1], end[1])) { return 0; }
            break;

        case 2:
            if(start[1] == end[1]) {
                // aA - bA
                if(!parseUtf8SetTreeClassBitmapRange(tree, 2, 0, 0, start[1], start[2], end[2])) { return 0; }
            } else {
                // aB, aA - aL
                if(!parseUtf8SetTreeClassBitmapRange(tree, 2, 0, 0, start[1], start[2], UTF8_ENC_LOW_BYTE_FULL_RANGE)) { return 0; }
                // iBab, 0 - L
                for(ml = start[1] + 1; ml < end[1]; ml++) {
                    if(!parseUtf8SetTreeClassBitmapRange(tree, 2, 0, 0, ml, 0, UTF8_ENC_LOW_BYTE_FULL_RANGE)) { return 0; }
                }
                // bB, 0 - bA
                if(!parseUtf8SetTreeClassBitmapRange(tree, 2, 0, 0, end[1], 0, end[2])) { return 0; }
            }
            break;

        case 3:
            if(start[1] == end[1]) {
                if(start[2] == end[2]) {
                    // aC, aB, aA - bA
                    if(!parseUtf8SetTreeClassBitmapRange(tree, 2, 0, start[1], start[2], start[3], end[3])) { return 0; }
                } else {
                    // aC, aB, aA - L
                    if(!parseUtf8SetTreeClassBitmapRange(tree, 2, 0, start[1], start[2], start[3], UTF8_ENC_LOW_BYTE_FULL_RANGE)) { return 0; }
                    // aC, iBab, 0 - L
                    for(ml = start[1] + 1; ml < end[1]; ml++) {
                        if(!parseUtf8SetTreeClassBitmapRange(tree, 2, 0, start[1], ml, 0, UTF8_ENC_LOW_BYTE_FULL_RANGE)) { return 0; }
                    }
                    // aC, bB, 0 - bA
                    if(!parseUtf8SetTreeClassBitmapRange(tree, 2, 0, start[1], end[2], 0, end[3])) { return 0; }
                }
            } else {
                // aC, aB, aA - L
                if(!parseUtf8SetTreeClassBitmapRange(tree, 2, 0, start[1], start[2], start[3], UTF8_ENC_LOW_BYTE_FULL_RANGE)) { return 0; }
                // aC, iBaL, 0 - L
                for(ml = start[2] + 1; ml <= UTF8_ENC_LOW_BYTE_FULL_RANGE; ml++) {
                    if(!parseUtf8SetTreeClassBitmapRange(tree, 2, 0, start[1], ml, 0, UTF8_ENC_LOW_BYTE_FULL_RANGE)) { return 0; }
                }
                // iCab, iB0L, 0 - L
                for(mh = start[1] + 1; mh < end[1]; mh++) {
                    for(ml = 0; ml <= UTF8_ENC_LOW_BYTE_FULL_RANGE; ml++) {
                        if(!parseUtf8SetTreeClassBitmapRange(tree, 2, 0, mh, ml, 0, UTF8_ENC_LOW_BYTE_FULL_RANGE)) { return 0; }
                    }
                }
                // bC, iB0b, 0 - L
                for(ml = 0; ml < end[2]; ml++) {
                    if(!parseUtf8SetTreeClassBitmapRange(tree, 2, 0, end[1], ml, 0, UTF8_ENC_LOW_BYTE_FULL_RANGE)) { return 0; }
                }
                // bC, bB, 0 - bA
                if(!parseUtf8SetTreeClassBitmapRange(tree, 2, 0, end[1], end[2], 0, end[3])) { return 0; }
            }
            break;

        case 4:
            if(start[1] == end[1]) {
                if(start[2] == end[2]) {
                    if(start[3] == end[3]) {
                        // aD, aC, aB, aA - bA
                        if(!parseUtf8SetTreeClassBitmapRange(tree, 2, start[1], start[2], start[3], start[4], end[4])) { return 0; }
                    } else {
                        // aD, aC, aB, aA - L
                        if(!parseUtf8SetTreeClassBitmapRange(tree, 2, start[1], start[2], start[3], start[4], UTF8_ENC_LOW_BYTE_FULL_RANGE)) { return 0; }
                        // aD, aC, iBab, 0 - L
                        for(ml = start[3] + 1; ml < end[3]; ml++) {
                            if(!parseUtf8SetTreeClassBitmapRange(tree, 2, start[1], start[2], ml, 0, UTF8_ENC_LOW_BYTE_FULL_RANGE)) { return 0; }
                        }
                        // aD, aC, bB, 0 - bA
                        if(!parseUtf8SetTreeClassBitmapRange(tree, 2, start[1], start[2], end[3], 0, end[4])) { return 0; }
                    }
                } else {
                    // aD, aC, aB, aA - L
                    if(!parseUtf8SetTreeClassBitmapRange(tree, 2, start[1], start[2], start[3], start[4], UTF8_ENC_LOW_BYTE_FULL_RANGE)) { return 0; }
                    // aD, aC, iBaL, 0 - L
                    for(ml = start[3] + 1; ml <= UTF8_ENC_LOW_BYTE_FULL_RANGE; ml++) {
                        if(!parseUtf8SetTreeClassBitmapRange(tree, 2, start[1], start[2], ml, 0, UTF8_ENC_LOW_BYTE_FULL_RANGE)) { return 0; }
                    }
                    // aD, iCab, iB0L, 0 - L
                    for(mh = start[2] + 1; mh < end[2]; mh++) {
                        for(ml = 0; ml <= UTF8_ENC_LOW_BYTE_FULL_RANGE; ml++) {
                            if(!parseUtf8SetTreeClassBitmapRange(tree, 2, start[1], mh, ml, 0, UTF8_ENC_LOW_BYTE_FULL_RANGE)) { return 0; }
                        }
                    }
                    // aD, bC, iB0b, 0 - L
                    for(ml = 0; ml < end[3]; ml++) {
                        if(!parseUtf8SetTreeClassBitmapRange(tree, 2, start[1], end[2], ml, 0, UTF8_ENC_LOW_BYTE_FULL_RANGE)) { return 0; }
                    }
                    // aD, bC, bB, 0 - bA
                    if(!parseUtf8SetTreeClassBitmapRange(tree, 2, start[1], end[2], end[3], 0, end[4])) { return 0; }
                }
            } else {
                // aD, aC, aB, aA - L
                if(!parseUtf8SetTreeClassBitmapRange(tree, 2, start[1], start[2], start[3], start[4], UTF8_ENC_LOW_BYTE_FULL_RANGE)) { return 0; }
                // aD, aC, iBaL, 0 - L
                for(ml = start[3] + 1; ml <= UTF8_ENC_LOW_BYTE_FULL_RANGE; ml++) {
                    if(!parseUtf8SetTreeClassBitmapRange(tree, 2, start[1], start[2], ml, 0, UTF8_ENC_LOW_BYTE_FULL_RANGE)) { return 0; }
                }
                // aD, iCab, iB0L, 0 - L
                for(mh = start[2] + 1; mh < end[2]; mh++) {
                    for(ml = 0; ml <= UTF8_ENC_LOW_BYTE_FULL_RANGE; ml++) {
                        if(!parseUtf8SetTreeClassBitmapRange(tree, 2, start[1], mh, ml, 0, UTF8_ENC_LOW_BYTE_FULL_RANGE)) { return 0; }
                    }
                }
                // iDab, iC0L, iB0L, 0 - L
                for(h = start[1] + 1; h < end[1]; h++) {
                    for(mh = 0; mh < UTF8_ENC_LOW_BYTE_FULL_RANGE; mh++) {
                        for(ml = 0; ml < UTF8_ENC_LOW_BYTE_FULL_RANGE; ml++) {
                            if(!parseUtf8SetTreeClassBitmapRange(tree, 2, h, mh, ml, 0, UTF8_ENC_LOW_BYTE_FULL_RANGE)) { return 0; }
                        }
                    }
                }
                // bD, iC0b, iB0L, 0 - L
                for(mh = 0; mh < end[2]; mh++) {
                    for(ml = 0; ml < UTF8_ENC_LOW_BYTE_FULL_RANGE; ml++) {
                        if(!parseUtf8SetTreeClassBitmapRange(tree, 2, end[1], mh, ml, 0, UTF8_ENC_LOW_BYTE_FULL_RANGE)) { return 0; }
                    }
                }
                // bD, bC, iB0b, 0 - L
                for(ml = 0; ml < end[3]; ml++) {
                    if(!parseUtf8SetTreeClassBitmapRange(tree, 2, end[1], end[2], ml, 0, UTF8_ENC_LOW_BYTE_FULL_RANGE)) { return 0; }
                }
                // bD, bC, bB, 0 - bA
                if(!parseUtf8SetTreeClassBitmapRange(tree, 2, end[1], end[2], end[3], 0, end[4])) { return 0; }
            }
            break;
    }
    return 1;
}

int parseUtf8CharClassRangeSet(utf8_charclass_tree_t *tree, int code_a, int code_b) {
    unsigned char b_a[5], b_b[5], b_r[5], b_r2[5];
    int k;

    parseUtf8EncodeSequence(b_a, code_a);
    parseUtf8EncodeSequence(b_b, code_b);

    if(b_a[0] == b_b[0]) {
        // The utf8 encoding byte length in the range is equal
        return parseUtf8TreeGenerateRange(tree, b_a, b_b);
    }
    // The range crosses a utf8 encoding byte length boundary

    // Fill to the end of this start of range utf8 byte length encoding
    if(b_a[0] != 4) {
        b_r[0] = b_a[0];
        b_r[2] = UTF8_ENC_LOW_BYTE_FULL_RANGE;
        b_r[3] = UTF8_ENC_LOW_BYTE_FULL_RANGE;
        switch(b_a[0]) {
            case 1: b_r[1] = UTF8_ENC_ONE_BYTE_FULL_RANGE; break;
            case 2: b_r[1] = UTF8_ENC_HIGH_TWO_BYTE_FULL_RANGE; break;
            case 3: b_r[1] = UTF8_ENC_HIGH_THREE_BYTE_FULL_RANGE; break;
        }
        if(!parseUtf8TreeGenerateRange(tree, b_a, b_r)) {
            return 0;
        }
    }

    // Fill intermediary utf8 byte length encoding ranges
    for(k = b_a[0] + 1; k < b_b[0]; k++) {
        b_r[0] = k;
        b_r[1] = 0;
        b_r[2] = 0;
        b_r[3] = 0;
        b_r2[0] = k;
        b_r2[2] = UTF8_ENC_LOW_BYTE_FULL_RANGE;
        b_r2[3] = UTF8_ENC_LOW_BYTE_FULL_RANGE;
        if(k == 2) {
            b_r2[1] = UTF8_ENC_HIGH_TWO_BYTE_FULL_RANGE;
        } else if(k == 3) {
            b_r2[1] = UTF8_ENC_HIGH_THREE_BYTE_FULL_RANGE;
        }
        if(!parseUtf8TreeGenerateRange(tree, b_r, b_r2)) {
            return 0;
        }
    }

    // Fill to the end of range utf8 byte length encoding
    b_r[0] = b_b[0];
    b_r[1] = 0;
    b_r[2] = 0;
    b_r[3] = 0;
    b_r[4] = 0;
    if(!parseUtf8TreeGenerateRange(tree, b_r, b_b)) {
        return 0;
    }

    return 1;
}

void regexCharClassUtf8TreeFree(utf8_charclass_tree_t *tree) {
    utf8_charclass_midlow_byte_t *midlow, *ml_next;
    utf8_charclass_midhigh_byte_t *midhigh, *mh_next;
    utf8_charclass_high_byte_t *high, *h_next;

    for(high = tree->four_byte; high != NULL; high = h_next) {
        h_next = high->next;
        for(midhigh = high->midhigh_byte; midhigh != NULL; midhigh = mh_next) {
            mh_next = midhigh->next;
            for(midlow = midhigh->midlow_byte; midlow != NULL; midlow = ml_next) {
                ml_next = midlow->next;
                _regexDealloc(midlow, _regexMemContext);
            }
            _regexDealloc(midhigh, _regexMemContext);
        }
        _regexDealloc(high, _regexMemContext);
    }
    for(midhigh = tree->three_byte; midhigh != NULL; midhigh = mh_next) {
        mh_next = midhigh->next;
        for(midlow = midhigh->midlow_byte; midlow != NULL; midlow = ml_next) {
            ml_next = midlow->next;
            _regexDealloc(midlow, _regexMemContext);
        }
        _regexDealloc(midhigh, _regexMemContext);
    }
    for(midlow = tree->two_byte; midlow != NULL; midlow = ml_next) {
        ml_next = midlow->next;
        _regexDealloc(midlow, _regexMemContext);
    }
    _regexDealloc(tree, _regexMemContext);
}

int charClassBitmapCheck(const unsigned int *bitmap, int pos) {
    return (int)(bitmap[pos / 32u] & (1u << pos % 32u));
}

int utf8ClassBitmapCheck(const unsigned int *bitmap, int pos) {
    if((pos & UTF8_LOW_BYTE_MASK) != UTF8_LOW_BYTE_PREFIX) {
        return 0;
    }
    return (int)(bitmap[pos / 32u] & (1u << pos % 32u));
}

/////////////////////////////////////////////////////////////////////////////
//
//    For the psuedo unicode char class:
//
//        "\u000a\u0bcd-\u0bce\u0bfg\uhijk-\uhijl\uhmno"
//
//    A tree is generated:
//
//        1 byte: a
//        3 byte: b->c->d
//                b->c->e
//                b->f->g
//        4 byte: h->i->j->k
//                h->i->j->l
//                h->m->n->o
//
//    And a sub sequence pattern is generated:
//
//        (?:[a]|b(?:c(?:[d]|[e])|f[g])|h(?:ij(?:[k]|[l])|mn[o]))
//
/////////////////////////////////////////////////////////////////////////////

int parseCharClassBitmapSet(utf8_charclass_tree_t *tree, unsigned int *bitmap, int c) {
    if((c > 127) && (tree != NULL)) {
        return parseUtf8CharClassCodepoint(tree, c);
    }
    bitmap[c / 32u] |= (1u << (c % 32u));
    return 1;
}

int parseCharClassBitmapRangeSet(utf8_charclass_tree_t *tree, unsigned int *bitmap, int a, int b) {
    if((b > 127) && (tree != NULL)) {
        return parseUtf8CharClassRangeSet(tree, a, b);
    }
    for(; a <= b; a++) {
        bitmap[a / 32u] |= (1u << (a % 32u));
    }
    return 1;
}

// The embedded flag indicates whether this class pattern is within the context
// of an actual pattern expression, or is just the content of the pattern, from
// a secodary source. An embedded pattern ends at the required ']' delimiter,
// whereas a non embedded pattern does not contain a trailing delimiter.
int parseCharClassAndCreateToken(eRegexCompileStatus_t *status, regex_vm_build_t *build,
                                 const char **pattern, const char *classId, int embedded, regex_token_t **tokens) {
    unsigned int bitmap[8], *ptr;
    const char *orig_pattern = *pattern;
    parseChar_t c;
    int invert = 0;
    int range = 0;
    int last = 0;
    int k;
    utf8_charclass_tree_t *utf8tree = NULL;

    memset(bitmap, 0, 32);

    if((classId != NULL) && (classId[0] == '^')) {
        invert = 1;
    }

    c = parseGetNextPatternChar(pattern);
    if(parsePatternIsValid(&c)) {
        parsePatternCharAdvance(pattern);
    }
    if(c.state == eRegexPatternEnd) {
        *status = eCompileCharClassIncomplete;
        return 0;
    }

    if(c.state == eRegexPatternMetaChar && c.c == '^') {
        invert = 1;
        c = parseGetNextPatternChar(pattern);
        if(parsePatternIsValid(&c)) {
            parsePatternCharAdvance(pattern);
        }
    }

    for(;;) {
        switch(c.state) {
            case eRegexPatternInvalid:
                *status = eCompileEscapeCharIncomplete;
                return 0;

            case eRegexPatternInvalidEscape:
                *status = eCompileInvalidEscapeChar;
                return 0;

            case eRegexPatternUnicode:
                if(utf8tree == NULL) {
                    if((utf8tree = _regexAlloc(sizeof(utf8_charclass_tree_t), _regexMemContext)) == NULL) {
                        *status = eCompileOutOfMem;
                        return 0;
                    }
                    memset(utf8tree, 0, sizeof(utf8_charclass_tree_t));
                }
                // intentional fall through

            case eRegexPatternChar:
                if(c.c == ']') {
                    if(range == 2) {
                        *status = eCompileCharClassRangeIncomplete;
                        return 0;
                    }
                    goto parseClassCompleted;
                }
                // intentional fall through

            case eRegexPatternMetaChar:
            case eRegexPatternEscapedChar:
                if(range == 0) {
                    last = c.c;
                    if(!parseCharClassBitmapSet(utf8tree, bitmap, last)) {
                        *status = eCompileOutOfMem;
                        return 0;
                    }
                    range = 1;
                } else if(range == 1) {
                    if(c.state != eRegexPatternEscapedChar && c.c == '-') {
                        range = 2;
                    } else {
                        last = c.c;
                        if(!parseCharClassBitmapSet(utf8tree, bitmap, last)) {
                            *status = eCompileOutOfMem;
                            return 0;
                        }
                    }
                } else {
                    if(!parseCharClassBitmapRangeSet(utf8tree, bitmap, last, c.c)) {
                        *status = eCompileOutOfMem;
                        return 0;
                    }
                    range = 0;
                }
                break;

            case eRegexPatternMetaClass:
            case eRegexPatternUnicodeMetaClass:
                *status = eCompileInvalidEscapeChar;
                return 0;

            case eRegexPatternEnd:
                if(range == 2) {
                    *status = eCompileCharClassRangeIncomplete;
                    return 0;
                }
                if(!embedded) {
                    goto parseClassCompleted;
                }
                *status = eCompileCharClassIncomplete;
                return 0;
        }
        c = parseGetNextPatternChar(pattern);
        if(parsePatternIsValid(&c)) {
            parsePatternCharAdvance(pattern);
        }
    }

parseClassCompleted:

    *status = eCompileOk;

    if(utf8tree != NULL) {
        // Merge any entries in the single byte bitmap into the utf8 tree
        for(k = 0; k < 256; k++) {
            if(bitmap[k / 32u] & (1u << k % 32u)) {
                utf8tree->one_byte[k / 32u] |= (1u << (k % 32u));
            }
        }

        if(!regexTokenUtf8ClassCreate(build, status, tokens, utf8tree, invert, orig_pattern, classId)) {
            regexCharClassUtf8TreeFree(utf8tree);
            return 0;
        }

        regexCharClassUtf8TreeFree(utf8tree);
    } else {
        // Strictly single byte, use a regular char class only
        if(invert) {
            for(k = 0; k < 8; k++) {
                bitmap[k] ^= (unsigned int)0xFFFFFFFFu;
            }
        }

        if((ptr = charClassBitmapCopy(bitmap)) == NULL) {
            *status = eCompileOutOfMem;
            return 0;
        }

        if(!regexTokenCreate(tokens, eTokenCharClass, 0, (char *)ptr, 0, 0)) {
            *status = eCompileOutOfMem;
            return 0;
        }
    }
    return 1;
}

int parseUnicodeClassAndCreateToken(eRegexCompileStatus_t *status, regex_vm_build_t *build,
                                    int unicodeClass, regex_token_t **tokens) {
    char classId[4];
    const char *classStr;
    int invert;
    regex_token_t *utf8class;

    classId[0] = ' ';
    classId[1] = (char)((unsigned int)unicodeClass & 0xFFu);
    classId[2] = (char)(((unsigned int)unicodeClass & 0xFF00u) >> 8u);
    classId[3] = '\0';

    invert = ((unicodeClass & 0x10000) ? 1 : 0);
    if(invert) {
        classId[0] = '^';
    }


    if((classStr = regexRegUnicodeCharClassGet(classId + 1)) == NULL) {
        *status = eCompileUnknownUnicodeClass;
        return 0;
    }

    if(regexSubroutineIdxGetName(build, classId, 3) != NULL) {
        if((*status = regexSubroutineGenerateFromPattern(tokens, NULL, build, NULL, classId, 2, NULL)) != eCompileOk) {
            return 0;
        }
        return 1;
    }
    return parseCharClassAndCreateToken(status, build, &classStr, classId, 0, tokens);
}

void regexEmitCharClassBitmap(FILE *fp, const unsigned int *bitmap) {
    int k;
    int run;

    for(k = 0; k < 256; k++) {
        if(charClassBitmapCheck(bitmap, k)) {
            for(run = k + 1; run < 256 && charClassBitmapCheck(bitmap, run); run++);
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
// Subexpression name table
/////////////////////////////////////////////////////////////////////////////

// The subexpression group number is derived from it's pattern order. This
// group number can be used to derive the expression number, via the following
// utility functions.

int regexSubexprStartFromGroup(int group) {
    return (group - 1) * 2;
}

int regexSubexprEndFromGroup(int group) {
    return ((group - 1) * 2) + 1;
}

// Parses the subexpression name pointed to by pattern, creates a
// subexpression name lookup entry, and adds it to the subexpression name list.
// Returns 1 on success, 0 on out of memory, and -1 if the name is malformed.
int regexSubexprLookupEntryCreate(regex_vm_build_t *build, const char **pattern, int index) {
    int len;

    if(pattern == NULL) {
        if(!regexVMGroupTableEntryAdd(build, NULL, 0, index)) {
            return 0;
        }
        return 1;
    }

    for(len = 0; (*pattern)[len] != '>' && (*pattern)[len] != '\0'; len++) {
        if(!parseIsAlnum((*pattern)[len])) {
            return -1;
        }
    }
    if(len == 0) {
        return -1;
    }

    if(!regexVMGroupTableEntryAdd(build, *pattern, len, index)) {
        return 0;
    }
    *pattern += len;

    // Discard the trailing '>' delimiter
    (*pattern)++;

    return 1;
}

// Parses the subroutine name pointed to by pattern, creates a subroutine index
// entry, and adds it to the subroutine index. Returns the subroutine id (>0) on
// success, 0 on out of memory, and -1 if the name is malformed.
int regexSubroutineIdxEntryCreate(regex_vm_build_t *build, const char **pattern) {
    int len, index = 0;

    for(len = 0; (*pattern)[len] != '>' && (*pattern)[len] != '\0'; len++) {
        if(!parseIsAlnum((*pattern)[len])) {
            return -1;
        }
    }
    if(len == 0) {
        return -1;
    }

    if(regexSubroutineGenerateFromPattern(NULL, NULL, build, NULL, *pattern, len, &index) != eCompileOk) {
        return 0;
    }

    *pattern += len;

    // Discard the trailing '>' delimiter
    (*pattern)++;

    return index;
}

// Parses the subroutine name pointed to by pattern, looks up that name within
// the subroutine index, and returns the subroutine id (>0) on success, 0 if the
// entry was not found, and -1 if the name is malformed.
int regexSubroutineIdxCall(regex_vm_build_t *build, const char **pattern) {
    int len;
    regex_subroutine_t *subroutine;

    for(len = 0; (*pattern)[len] != '}' && (*pattern)[len] != '\0'; len++) {
        if(!parseIsAlnum((*pattern)[len])) {
            return -1;
        }
    }
    if(len == 0) {
        return -1;
    }

    if((subroutine = regexSubroutineIdxGetName(build, *pattern, len)) == NULL) {
        return 0;
    }

    *pattern += len;

    // Discard the trailing '>' delimiter
    (*pattern)++;

    return subroutine->id;
}

/////////////////////////////////////////////////////////////////////////////
// Lexer - tokenizes the pattern into a token list (infix form)
/////////////////////////////////////////////////////////////////////////////

#define SET_RESULT(stat)  result = stat; goto compileFailure;

eRegexCompileStatus_t regexTokenizePattern(const char *pattern,
                                           int *pos,
                                           regex_token_t **tokens,
                                           regex_vm_build_t *build) {
    eRegexCompileStatus_t result;
    const char *start = pattern;
    const char *class;
    parseChar_t c;
    int response;
    int len;
    char *str;
    unsigned int flags;
    regex_token_t *subroutine;
    eRegexCompileStatus_t status;
    int subexpr = 0;
    int named;
    int index;

    // Parse the regex pattern into a sequence of tokens (operators and operands)
    // The output of this stage is a sequence of lexical tokens in infix form

    // Loop through the pattern until we've handled it all
    for(; *pattern != '\0';) {
        // Get the next character in the pattern. The helper function assists
        // in disambiguating escaped characters.
        c = parseGetNextPatternChar(&pattern);
        if(parsePatternIsValid(&c)) {
            parsePatternCharAdvance(&pattern);
        }

        switch(c.state) {
            case eRegexPatternEnd:
                goto endOfPattern;

            case eRegexPatternInvalid:
                SET_RESULT(eCompileEscapeCharIncomplete);

            case eRegexPatternInvalidEscape:
                SET_RESULT(eCompileInvalidEscapeChar);

            case eRegexPatternMetaChar:
                switch(c.c) {
                    case '.':
                        // Operand, the meta "ANY" char
                        if(!regexTokenCreate(tokens, eTokenCharAny, 0, 0, 0, 0)) {
                            SET_RESULT(eCompileOutOfMem);
                        }
                        continue;

                    case '[':
                        if(!parseCharClassAndCreateToken(&status, build, &pattern, NULL, 1, tokens)) {
                            SET_RESULT(status);
                        }
                        continue;

                    case '?':
                        if(!regexTokenCreate(tokens, eTokenZeroOrOne, 0, 0, 0, 0)) {
                            SET_RESULT(eCompileOutOfMem);
                        }
                        continue;

                    case '*':
                        if(!regexTokenCreate(tokens, eTokenZeroOrMany, 0, 0, 0, 0)) {
                            SET_RESULT(eCompileOutOfMem);
                        }
                        continue;

                    case '+':
                        if(!regexTokenCreate(tokens, eTokenOneOrMany, 0, 0, 0, 0)) {
                            SET_RESULT(eCompileOutOfMem);
                        }
                        continue;

                    case '|':
                        if(!regexTokenCreate(tokens, eTokenAlternative, 0, 0, 0, 0)) {
                            SET_RESULT(eCompileOutOfMem);
                        }
                        continue;

                    case '(':
                        // Grouped subexpression, complex operator, resolves to a compound operand
                        // Speculative subexpression number. If non-capturing, will NOT actually
                        // increase the group count.
                        subexpr = build->groups + 1;
                        flags = 0;
                        named = 0;
                        index = 0;
                        // Check for group meta modifiers
                        while(*pattern == '?') {
                            pattern++;
                            if(parseCheckNextPatternChar(&pattern, 'i')) {
                                // Case insensitive matching
                                flags |= REGEX_TOKEN_FLAG_CASEINS;
                            } else if(parseCheckNextPatternChar(&pattern, 'P')) {
                                // Named sub expression
                                if(named || (flags & (REGEX_TOKEN_FLAG_SUBROUTINE | REGEX_TOKEN_FLAG_NOCAPTURE))) {
                                    SET_RESULT(eCompileConflictingAttrs);
                                }
                                if(!parseCheckNextPatternChar(&pattern, '<')) {
                                    SET_RESULT(eCompileMalformedSubExprName);
                                }
                                if((response = regexSubexprLookupEntryCreate(build, &pattern, subexpr)) != 1) {
                                    SET_RESULT((response == 0 ? eCompileOutOfMem : eCompileMalformedSubExprName));
                                }
                                named = 1;
                            } else if(parseCheckNextPatternChar(&pattern, ':')) {
                                // Non-capturing subexpression
                                if(named || (flags & (REGEX_TOKEN_FLAG_COMPOUND))) {
                                    SET_RESULT(eCompileConflictingAttrs);
                                }
                                flags |= REGEX_TOKEN_FLAG_NOCAPTURE;
                            } else if(parseCheckNextPatternChar(&pattern, '*')) {
                                // Compound capturing subexpression
                                if(flags & (REGEX_TOKEN_FLAG_NOCAPTURE | REGEX_TOKEN_FLAG_SUBROUTINE)) {
                                    SET_RESULT(eCompileConflictingAttrs);
                                }
                                flags |= REGEX_TOKEN_FLAG_COMPOUND;
                            } else if(parseCheckNextPatternChar(&pattern, 'R')) {
                                // Named subroutine
                                if(named || (flags & REGEX_TOKEN_FLAG_COMPOUND)) {
                                    SET_RESULT(eCompileConflictingAttrs);
                                }
                                if(!parseCheckNextPatternChar(&pattern, '<')) {
                                    SET_RESULT(eCompileMalformedSubExprName);
                                }
                                if((index = regexSubroutineIdxEntryCreate(build, &pattern)) <= 0) {
                                    SET_RESULT((index == 0 ? eCompileOutOfMem : eCompileMalformedSubExprName));
                                }
                                flags |= REGEX_TOKEN_FLAG_SUBROUTINE | REGEX_TOKEN_FLAG_NOCAPTURE;
                            } else {
                                SET_RESULT(eCompileUnsupportedMeta);
                            }
                        }
                        if(!named) {
                            if((response = regexSubexprLookupEntryCreate(build, NULL, subexpr)) != 1) {
                                SET_RESULT((response == 0 ? eCompileOutOfMem : eCompileMalformedSubExprName));
                            }
                        }
                        if(!(flags & (REGEX_TOKEN_FLAG_NOCAPTURE | REGEX_TOKEN_FLAG_SUBROUTINE))) {
                            build->groups++;
                        }
                        if(!regexTokenCreate(tokens, eTokenSubExprStart, subexpr, NULL, (int)flags, index)) {
                            SET_RESULT(eCompileOutOfMem);
                        }
                        continue;

                    case ')':
                        // End of grouped subexpression
                        if(!regexTokenCreate(tokens, eTokenSubExprEnd, 0, 0, 0, 0)) {
                            SET_RESULT(eCompileOutOfMem);
                        }
                        continue;

                    default:
                        // Unexpected meta character
                        SET_RESULT(eCompileInternalError);
                }
                continue;

            case eRegexPatternMetaClass:
                switch(c.c) {
                    case 'R': // subroutine call
                        if(!parseCheckNextPatternChar(&pattern, '{')) {
                            SET_RESULT(eCompileMalformedSubExprName);
                        }
                        if((index = regexSubroutineIdxCall(build, &pattern)) <= 0) {
                            SET_RESULT((index == 0 ? eCompileOutOfMem : eCompileMalformedSubExprName));
                        }
                        if(!regexTokenCreate(tokens, eTokenCall, index, NULL, 0, 0)) {
                            SET_RESULT(eCompileOutOfMem);
                        }
                        continue;
                    case 'X': // full unicode glyph (base + markers)
                        // Represented by a micro NFA
                        subroutine = NULL;
                        if((status = regexSubroutineGenerateFromPattern(tokens, &subroutine, build, "\\p{M}\\p{M}*", NULL, 0, NULL)) != eCompileOk) {
                            SET_RESULT(status);
                        }
                        continue;
                    case 'B': // explicit byte
                        if(!regexTokenCreate(tokens, eTokenByte, 0, 0, 0, 0)) {
                            SET_RESULT(eCompileOutOfMem);
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
                }
                if(!parseCharClassAndCreateToken(&status, build, &class, NULL, 0, tokens)) {
                    SET_RESULT(status);
                }
                continue;

            case eRegexPatternUnicodeMetaClass:
                if(!parseUnicodeClassAndCreateToken(&status, build, c.c, tokens)) {
                    SET_RESULT(status);
                }
                continue;

            case eRegexPatternChar:
            case eRegexPatternUnicode:
            case eRegexPatternEscapedChar:
                // Operand, either character literal or string literal
                if((len = parseGetPatternStrLen(&pattern)) == -1) {
                    SET_RESULT(eCompileEscapeCharIncomplete);
                }
                if(PARSE_DEC_CHAR_COUNT(len) == 0) {
                    if(!regexTokenCreate(tokens, eTokenCharLiteral, (char) (c.c), 0, 0, 0)) {
                        SET_RESULT(eCompileOutOfMem);
                    }
                } else {
                    len++;
                    if(c.state == eRegexPatternEscapedChar) {
                        pattern--;
                    } else if(c.state == eRegexPatternUnicode) {
                        // Increment the utf8 byte accommodation
                        len += (int)((unsigned int)(parseUtf8EncodingByteLen(c.c) - 1) << 16u);
                        pattern -= 5;
                    }
                    pattern--;
                    if((str = parseGetPatternStr(&pattern, PARSE_DEC_CHAR_COUNT(len), PARSE_DEC_UTF8_COUNT(len) + PARSE_DEC_CHAR_COUNT(len))) == NULL) {
                        SET_RESULT(eCompileOutOfMem);
                    }
                    if(!regexTokenCreate(tokens, eTokenStringLiteral, 0, str, PARSE_DEC_UTF8_COUNT(len) + PARSE_DEC_CHAR_COUNT(len), 0)) {
                        SET_RESULT(eCompileOutOfMem);
                    }
                }
                continue;
        }
    }

endOfPattern:
    SET_RESULT(eCompileOk);

compileFailure:
    *pos = (int)(pattern - start);

    return result;
}

/////////////////////////////////////////////////////////////////////////////
// Shunting yard implementation - convert infix to postfix to an NFA
/////////////////////////////////////////////////////////////////////////////

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
        return eTokenNone;
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

int regexOperatorLiteralCreate(regex_token_t **stack, regex_token_t *token) {
    if((token->ptrlist = regexPtrlistCreate(token, eRePtrOutA)) == NULL) {
        return 0;
    }
    regexTokenStackPush(stack, token);
    return 1;
}

int regexOperatorSubexprCreate(regex_token_t **stack, regex_token_t *token) {
    token->tokenType = eTokenSave;
    if((token->ptrlist = regexPtrlistCreate(token, eRePtrOutA)) == NULL) {
        return 0;
    }
    regexTokenStackPush(stack, token);
    return 1;
}

int regexOperatorConcatenationCreate(regex_token_t **stack, regex_token_t *token) {
    regex_token_t *e1, *e2;

    regexTokenDestroy(token, 1);
    if(((e2 = regexTokenStackPop(stack)) == NULL) ||
       ((e1 = regexTokenStackPop(stack)) == NULL)) {
        return 0;
    }
    regexPtrlistPatch(e1->ptrlist, e2);
    e1->ptrlist = e2->ptrlist;
    e2->ptrlist = NULL;
    regexTokenStackPush(stack, e1);

    return 1;
}

int regexOperatorAlternationCreate(regex_token_t **stack, regex_token_t *token) {
    regex_token_t *e1, *e2;
    regex_token_t *jmp = NULL;

    if(((e2 = regexTokenStackPop(stack)) == NULL) ||
       ((e1 = regexTokenStackPop(stack)) == NULL)) {
        return 0;
    }

    if(!regexTokenCreate(&jmp, eTokenJmp, 0, NULL, 0, 0)) {
        return 0;
    }
    regexPtrlistPatch(e1->ptrlist, jmp);
    if((token->ptrlist = regexPtrlistCreate(jmp, eRePtrOutA)) == NULL) {
        return 0;
    }

    token->tokenType = eTokenSplit;
    token->out_a = e1;
    token->out_b = e2;
    token->ptrlist = regexPtrlistAppend(token->ptrlist, e2->ptrlist);
    e2->ptrlist = NULL;
    regexTokenStackPush(stack, token);

    return 1;
}

int regexOperatorZeroOrOneCreate(regex_token_t **stack, regex_token_t *token) {
    regex_token_t *e;

    if((e = regexTokenStackPop(stack)) == NULL) {
        return 0;
    }
    token->tokenType = eTokenSplit;
    token->out_a = e;
    if((token->ptrlist = regexPtrlistCreate(token, eRePtrOutB)) == NULL) {
        return 0;
    }
    token->ptrlist = regexPtrlistAppend(e->ptrlist, token->ptrlist);
    e->ptrlist = NULL;
    regexTokenStackPush(stack, token);

    return 1;
}

int regexOperatorZeroOrMoreCreate(regex_token_t **stack, regex_token_t *token) {
    regex_token_t *e;
    regex_token_t *jmp = NULL;

    if((e = regexTokenStackPop(stack)) == NULL) {
        return 0;
    }
    token->tokenType = eTokenSplit;
    token->out_a = e;
    if((token->ptrlist = regexPtrlistCreate(token, eRePtrOutB)) == NULL) {
        return 0;
    }
    if(!regexTokenCreate(&jmp, eTokenJmp, 0, NULL, 0, 0)) {
        return 0;
    }
    jmp->out_a = token;
    regexPtrlistPatch(e->ptrlist, jmp);
    regexTokenStackPush(stack, token);

    return 1;
}

int regexOperatorOneOrMoreCreate(regex_token_t **stack, regex_token_t *token) {
    regex_token_t *e;

    if((e = regexTokenStackPop(stack)) == NULL) {
        return 0;
    }
    token->tokenType = eTokenSplit;
    token->out_a = e;
    regexPtrlistPatch(e->ptrlist, token);
    if((e->ptrlist = regexPtrlistCreate(token, eRePtrOutB)) == NULL) {
        return 0;
    }
    regexTokenStackPush(stack, e);
    return 1;
}

int regexOperatorMatchCreate(regex_token_t **stack) {
    regex_token_t *e;
    regex_token_t *token = NULL;

    if((e = regexTokenStackPop(stack)) == NULL) {
        return 0;
    }
    if(!regexTokenCreate(&token, eTokenMatch, 0, NULL, 0, 0)) {
        return 0;
    }
    regexPtrlistPatch(e->ptrlist, token);
    regexTokenStackPush(stack, e);

    return 1;
}

int regexOperatorReturnCreate(regex_token_t **stack) {
    regex_token_t *e;
    regex_token_t *token = NULL;

    if((e = regexTokenStackPop(stack)) == NULL) {
        return 0;
    }
    if(!regexTokenCreate(&token, eTokenReturn, 0, NULL, 0, 0)) {
        return 0;
    }
    regexPtrlistPatch(e->ptrlist, token);
    regexTokenStackPush(stack, e);

    return 1;
}

int regexHasSufficientOperands(regex_token_t *fragments, int arity) {
    for(; ((fragments != NULL) && (arity)); arity--, fragments = fragments->next);
    return arity == 0;
}

eRegexCompileStatus_t regexOperatorApply(regex_token_t **operators, eRegexOpApply apply, eRegexToken_t tokenType, regex_token_t **operands) {
    regex_token_t *operator;

    while((apply == OP_ALL && *operators != NULL) ||
          (apply == OP_GREATER_OR_EQUAL && regexStackTypeGreaterOrEqualToToken(*operators, tokenType))) {
        operator = regexTokenStackPop(operators);
        if(!regexHasSufficientOperands(*operands, regexGetOperatorArity(operator))) {
            return eCompileMissingOperand;
        }
        switch(operator->tokenType) {
            case eTokenZeroOrOne:
                if(!regexOperatorZeroOrOneCreate(operands, operator)) {
                    return eCompileOutOfMem;
                }
                break;

            case eTokenZeroOrMany:
                if(!regexOperatorZeroOrMoreCreate(operands, operator)) {
                    return eCompileOutOfMem;
                }
                break;

            case eTokenOneOrMany:
                if(!regexOperatorOneOrMoreCreate(operands, operator)) {
                    return eCompileOutOfMem;
                }
                break;

            case eTokenConcatenation:
                if(!regexOperatorConcatenationCreate(operands, operator)) {
                    return eCompileOutOfMem;
                }
                break;

            case eTokenAlternative:
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

#define REGEX_SHUNTING_YARD_NO_PARENT   -1
#define REGEX_SHUNTING_YARD_NO_CAPTURE  -2

#define SET_YARD_RESULT(res)    status = res; goto ShuntingYardFailure;

eRegexCompileStatus_t regexShuntingYardFragment(regex_vm_build_t *build, regex_token_t **tokens, regex_token_t **root_stack,
                                                int sub_expression, unsigned int group_flags) {
    regex_token_t *token = NULL, *routine, *operators = NULL;
    regex_token_t *operands = NULL, *subexpr;
    regex_subroutine_t *subindex;
    eRegexCompileStatus_t status;
    int group_num;

    if((sub_expression >= 0) || (sub_expression == REGEX_SHUNTING_YARD_NO_CAPTURE)) {
        operands = *root_stack;
        if(operands != NULL) {
            if(!regexTokenCreate(&token, eTokenConcatenation, 0, NULL, 0, 0)) {
                return eCompileOutOfMem;
            }
            regexTokenStackPush(&operators, token);
        }
    }

    while((token = regexTokenStackPop(tokens)) != NULL) {
        switch(token->tokenType) {
            default:
                return eCompileInternalError;
            case eTokenCharLiteral:
            case eTokenCharClass:
            case eTokenStringLiteral:
            case eTokenUtf8Class:
            case eTokenCharAny:
            case eTokenByte:
            case eTokenReturn:
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
                if((status = regexOperatorApply(&operators, OP_GREATER_OR_EQUAL,
                                                token->tokenType, &operands)) != eCompileOk) {
                    goto ShuntingYardFailure;
                }
                regexTokenStackPush(&operators, token);
                break;

            case eTokenCall:
                // Generate the sub sequence pattern, and tie it into the current pattern
                // with an eTokenCall
                subexpr = NULL;
                if((subindex = regexSubroutineIdxGet(build, token->c)) == NULL) {
                    SET_YARD_RESULT(eCompileInternalError);
                }
                if(!subindex->infixed) {
                    routine = subindex->tokens;
                    if((status = regexShuntingYardFragment(build, &(routine), &subexpr,
                                                           REGEX_SHUNTING_YARD_NO_CAPTURE,
                                                           REGEX_TOKEN_FLAG_SUBROUTINE)) != eCompileOk) {
                        SET_YARD_RESULT(status);
                    }
                    subindex->tokens = subexpr;
                    subindex->infixed = 1;
                }
                token->out_b = subindex->tokens;
                if(!regexOperatorLiteralCreate(&operands, token)) {
                    SET_YARD_RESULT(eCompileOutOfMem);
                }
                break;

            case eTokenSubExprStart:
                // TODO - handle case insensitive propogation
                subexpr = NULL;
                if(token->flags & REGEX_TOKEN_FLAG_NOCAPTURE) {
                    group_num = REGEX_SHUNTING_YARD_NO_CAPTURE;
                } else {
                    group_num = token->group;
                    token->group = regexSubexprStartFromGroup(token->group);
                    if(!regexOperatorSubexprCreate(&subexpr, token)) {
                        SET_YARD_RESULT(eCompileOutOfMem);
                    }
                }
                if(token->flags & REGEX_TOKEN_FLAG_SUBROUTINE) {
                    if((subindex = regexSubroutineIdxGet(build, token->sub_index)) == NULL) {
                        SET_YARD_RESULT(eCompileInternalError);
                    }
                    if(!subindex->infixed) {
                        if((status = regexShuntingYardFragment(build, tokens, &subexpr,
                                                               group_num, token->flags)) != eCompileOk) {
                            SET_YARD_RESULT(status);
                        }
                        subindex->tokens = subexpr;
                        subindex->infixed = 1;
                    }
                    token->out_b = subindex->tokens;
                    token->tokenType = eTokenCall;
                    if(!regexOperatorLiteralCreate(&operands, token)) {
                        SET_YARD_RESULT(eCompileOutOfMem);
                    }
                } else {
                    if((status = regexShuntingYardFragment(build, tokens, &subexpr,
                                                           group_num, token->flags)) != eCompileOk) {
                        SET_YARD_RESULT(status);
                    }
                    regexTokenStackPush(&operands, subexpr);
                }
                break;

            case eTokenSubExprEnd:
                if(sub_expression == REGEX_SHUNTING_YARD_NO_PARENT) {
                    SET_YARD_RESULT(eCompileMissingSubexprStart);
                }
                if(sub_expression != REGEX_SHUNTING_YARD_NO_CAPTURE) {
                    token->group = regexSubexprEndFromGroup(sub_expression);
                    token->flags = group_flags;
                    if(!regexOperatorSubexprCreate(&operands, token)) {
                        SET_YARD_RESULT(eCompileOutOfMem);
                    }
                    token = NULL;
                    if(!regexTokenCreate(&token, eTokenConcatenation, 0, NULL, 0, 0)) {
                        return eCompileOutOfMem;
                    }
                    regexTokenStackPush(&operators, token);
                }
                if((status = regexOperatorApply(&operators, OP_ALL, 0, &operands)) != eCompileOk) {
                    goto ShuntingYardFailure;
                }
                if(operands->next != NULL) {
                    SET_YARD_RESULT(eCompileInternalError);
                }
                if(group_flags & REGEX_TOKEN_FLAG_SUBROUTINE) {
                    if(!regexOperatorReturnCreate(&operands)) {
                        return eCompileOutOfMem;
                    }
                }
                *root_stack = operands;
                return eCompileOk;
        }
    }

    if((status = regexOperatorApply(&operators, OP_ALL, 0, &operands)) != eCompileOk) {
        goto ShuntingYardFailure;
    }

    if(group_flags & REGEX_TOKEN_FLAG_SUBROUTINE) {
        if(!regexOperatorReturnCreate(&operands)) {
            SET_YARD_RESULT(eCompileOutOfMem);
        }
    } else {
        if(sub_expression != REGEX_SHUNTING_YARD_NO_PARENT) {
            SET_YARD_RESULT(eCompileInternalError);
        }

        // Complete the sequence with a match
        if(!regexOperatorMatchCreate(&operands)) {
            SET_YARD_RESULT(eCompileOutOfMem);
        }
    }

    *root_stack = operands;

    return status;

ShuntingYardFailure:
    regexTokenDestroy(operators, 1);
    operands = NULL;

    return status;
}

eRegexCompileStatus_t regexShuntingYard(regex_vm_build_t *build, regex_token_t **tokens) {
    regex_token_t *stack = NULL;
    eRegexCompileStatus_t status;

    status = regexShuntingYardFragment(build, tokens, &stack, REGEX_SHUNTING_YARD_NO_PARENT, 0);
    if(status != eCompileOk) {
        regexTokenDestroy(stack, 1);
        return status;
    }

    *tokens = stack;

    return status;
}

/////////////////////////////////////////////////////////////////////////////
// Regex VM builder and evaluator
/////////////////////////////////////////////////////////////////////////////

// String table /////////////////////////////////////////////////////////////

int regexVMStringTableEntryAdd(regex_vm_build_t *build, const char *str, int len) {
    return regexStrTableEntryAdd(&(build->vm->string_table),
                                 &(build->vm->string_tbl_size),
                                 len, str, &(build->vm->string_tbl_len),
                                 REGEX_TABLE_DEDUPE, NULL);
}

const char *regexVMStringTableEntryGet(regex_vm_t *vm, int string_table_id, int *len) {
    return regexStrTableEntryGet(vm->string_table, vm->string_tbl_size,
                                 vm->string_tbl_len, string_table_id, len);
}

void regexVMStringTableFree(regex_vm_t *vm) {
    regexStrTableFreeAll(&(vm->string_table), &(vm->string_tbl_size),
                         &(vm->string_tbl_len));
}

// Class table //////////////////////////////////////////////////////////////

int regexVMClassTableEntryAdd(regex_vm_build_t *build, const unsigned int *bitmap) {
    return regexStrTableEntryAdd((char ***)&(build->vm->class_table),
                                 &(build->vm->class_tbl_size),
                                32, (char *)bitmap, NULL,
                                REGEX_TABLE_DEDUPE | REGEX_TABLE_KEEP_PTR, NULL);
}

const unsigned int *regexVMClassTableEntryGet(regex_vm_t *vm, int class_table_id) {
    return (unsigned int *)regexStrTableEntryGet((char **)(vm->class_table),
                                                 vm->class_tbl_size, NULL,
                                                 class_table_id, NULL);
}

void regexVMClassTableFree(regex_vm_t *vm) {
    regexStrTableFreeAll((char ***)&(vm->class_table), &(vm->class_tbl_size),
                         NULL);
}

// utf8 table ///////////////////////////////////////////////////////////////

int regexVMUtf8TableEntryAdd(regex_vm_build_t *build, const unsigned int *bitmap) {
    return regexStrTableEntryAdd((char ***)&(build->vm->utf8_class_table),
                                 &(build->vm->utf8_tbl_size),
                                 8, (char *)bitmap, NULL,
                                 REGEX_TABLE_KEEP_PTR | REGEX_TABLE_DEDUPE, NULL);
}

const unsigned int *regexVMUtf8TableEntryGet(regex_vm_t *vm, int utf8_table_id) {
    return (unsigned int *)regexStrTableEntryGet((char **)(vm->utf8_class_table),
                                                 vm->utf8_tbl_size, NULL,
                                                 utf8_table_id, NULL);
}

void regexVMUtf8TableFree(regex_vm_t *vm) {
    regexStrTableFreeAll((char ***)&(vm->utf8_class_table), &(vm->utf8_tbl_size),
                         NULL);
}

// Group table //////////////////////////////////////////////////////////////

int regexVMGroupTableEntryAdd(regex_vm_build_t *build, const char *group, int len, int index) {
    int k;

    for(k = build->vm->group_tbl_size; k < index; k++) {
        if(regexStrTableEntryAdd(&(build->vm->group_table),
                                 &(build->vm->group_tbl_size), 0,
                                 NULL, NULL,
                                 REGEX_TABLE_KEEP_PTR, NULL) == -1) {
            return 0;
        }
    }
    return regexStrTableEntryAdd(&(build->vm->group_table),
                                 &(build->vm->group_tbl_size),
                                 len, group, NULL, REGEX_TABLE_NULL_TERMINATE, NULL);
}

void regexVMGroupTableFree(regex_vm_t *vm) {
    regexStrTableFreeAll(&(vm->group_table), &(vm->group_tbl_size), NULL);
}

const char *regexVMGroupNameFromIndex(regex_vm_t *vm, int index) {
    if((index < 1) || (index > vm->group_tbl_size)) {
        return NULL;
    }
    return vm->group_table[index];
}

int regexVMGroupNameLookup(regex_vm_t *vm, const char *name) {
    for(int k = 1; k < vm->group_tbl_size; k++) {
        if(!strcmp(vm->group_table[k], name)) {
            return k;
        }
    }
    return -1;
}

// VM Patch list management functions ///////////////////////////////////////

#define REGEX_VM_INSTR_DECODE(var,vm,pc)   (var)[0] = (vm)->program[(pc)] & 0xFU; \
                                            (var)[1] = ((vm)->program[(pc)] & 0x3FFF0U) >> 4U; \
                                            (var)[2] = ((vm)->program[(pc)] & 0xFFFC0000U) >> 18U;

#define REGEX_VM_INSTR_ENCODE(vm,pc,instr,opa,opb) (vm)->program[(pc)] = (instr & 0xFU) | ((opa & 0x3FFFU) << 4U) | ((opb & 0x3FFFU) << 18U)

int regexAddPCPatchEntry(regex_vm_build_t *build, regex_token_t *token, int operand) {
    regex_vm_pc_patch_t *entry;

    if((entry = _regexAlloc(sizeof(regex_vm_pc_patch_t), _regexMemContext)) == NULL) {
        return 0;
    }
    memset(entry, 0, sizeof(regex_vm_pc_patch_t));
    entry->pc = build->pc;
    entry->operand = operand;
    entry->token = token;
    entry->next = build->patch_list;
    build->patch_list = entry;
    return 1;
}

void regexVMPatchListFree(regex_vm_pc_patch_t *patch_list) {
    regex_vm_pc_patch_t *next;

    for(; patch_list != NULL; patch_list = next) {
        next = patch_list->next;
        _regexDealloc(patch_list, _regexMemContext);
    }
}

void regexVMPatchJumps(regex_vm_build_t *build, regex_vm_pc_patch_t **patch_list) {
    regex_vm_pc_patch_t *patch, *next;
    unsigned int instr[3];

    for(patch = *patch_list; patch != NULL; patch = next) {
        next = patch->next;
        REGEX_VM_INSTR_DECODE(instr, build->vm, patch->pc);
        if(patch->operand == 1) {
            instr[1] = patch->token->pc;
        } else {
            instr[2] = patch->token->pc;
        }
        REGEX_VM_INSTR_ENCODE(build->vm, patch->pc, instr[0], instr[1], instr[2]);
        _regexDealloc(patch, _regexMemContext);
    }
    *patch_list = NULL;
}

int regexVMGenPathSubroutineExists(regex_vm_build_t *build, regex_token_t *token) {
    regex_vm_gen_path_t *entry;

    for(entry = build->subroutine_list; entry != NULL; entry = entry->next) {
        if(entry->token == token) {
            return 1;
        }
    }
    return 0;
}

int regexVMGenPathCreate(regex_vm_build_t *build, regex_token_t *token, int subroutine) {
    regex_vm_gen_path_t *entry;

    if(build->gen_pool != NULL) {
        entry = build->gen_pool;
        build->gen_pool = entry->next;
    } else {
        if((entry = _regexAlloc(sizeof(regex_vm_gen_path_t), _regexMemContext)) == NULL) {
            return 0;
        }
    }
    entry->token = token;
    if(subroutine) {
        entry->next = build->subroutine_list;
        build->subroutine_list = entry;
    } else {
        entry->next = build->gen_list;
        build->gen_list = entry;
    }
    return 1;
}

regex_token_t *regexVMGenPathGetNext(regex_vm_build_t *build, int subroutine) {
    regex_vm_gen_path_t *entry;
    regex_vm_gen_path_t **base = (subroutine ? &(build->subroutine_list) : &(build->gen_list));

    if((entry = *base) == NULL) {
        return NULL;
    }
    *base = entry->next;
    entry->next = build->gen_pool;
    build->gen_pool = entry;
    return entry->token;
}

void regexVMGenPathFree(regex_vm_build_t *build) {
    regex_vm_gen_path_t *walk, *next;

    for(walk = build->gen_list; walk != NULL; walk = next) {
        next = walk->next;
        free(walk);
    }
    build->gen_list = NULL;

    for(walk = build->subroutine_list; walk != NULL; walk = next) {
        next = walk->next;
        free(walk);
    }
    build->subroutine_list = NULL;

    for(walk = build->gen_pool; walk != NULL; walk = next) {
        next = walk->next;
        free(walk);
    }
    build->gen_pool = NULL;
}

// VM bytecode generator ////////////////////////////////////////////////////

int regexVMProgramAdd(regex_vm_build_t *build, eRegexToken_t opcode, unsigned int arg1, unsigned int arg2) {
    if((build->vm->size - build->pc) <= 0) {
        if((build->vm->program = _regexRealloc(build->vm->program,
                                               (build->vm->size * sizeof(int)),
                                               (build->vm->size + DEF_VM_SIZE_INC) * sizeof(int),
                                               _regexMemContext)) == NULL) {
            return 0;
        }
        memset(build->vm->program + build->vm->size, 0, DEF_VM_SIZE_INC * sizeof(int));
        build->vm->size += DEF_VM_SIZE_INC;
    }

    REGEX_VM_INSTR_ENCODE(build->vm, build->pc, opcode, arg1, arg2);
    build->pc++;

    return 1;
}

int regexVMProgramGenerateInstr(regex_vm_build_t *build, regex_token_t *token, regex_token_t **next) {
    int idx_a = 0, idx_b = 0;

    if(token == NULL) {
        *next = NULL;
        return 1;
    }
    if(token->pc != VM_PC_UNVISITED) {
        *next = NULL;
        return 1;
    }
    token->pc = build->pc;

    switch(token->tokenType) {
        case eTokenCharLiteral:
            idx_a = token->c;
            idx_b = (int)(token->flags & REGEX_TOKEN_FLAG_INVERT);
            break;
        case eTokenStringLiteral:
            if((idx_a = regexVMStringTableEntryAdd(build, token->str, token->len)) == -1) {
                return 0;
            }
            break;
        case eTokenCharClass:
            if((idx_a = regexVMClassTableEntryAdd(build, token->bitmap)) == -1) {
                return 0;
            }
            break;
        case eTokenUtf8Class:
            if((idx_a = regexVMUtf8TableEntryAdd(build, token->bitmap)) == -1) {
                return 0;
            }
            break;
        case eTokenCall:
            if((idx_a = token->out_b->pc) == VM_PC_UNVISITED) {
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
            idx_b = (int)(token->flags & REGEX_TOKEN_FLAG_COMPOUND);
            break;
        case eTokenSplit:
            if((idx_a = token->out_a->pc) == VM_PC_UNVISITED) {
                if(!regexAddPCPatchEntry(build, token->out_a, 1)) {
                    return 0;
                }
            }
            if((idx_b = token->out_b->pc) == VM_PC_UNVISITED) {
                if(!regexAddPCPatchEntry(build, token->out_b, 2)) {
                    return 0;
                }
            }
            break;
        case eTokenJmp:
            if((idx_a = token->out_a->pc) == VM_PC_UNVISITED) {
                if(!regexAddPCPatchEntry(build, token->out_a, 1)) {
                    return 0;
                }
            }
            break;
        default:
            break;
    }

    if(!regexVMProgramAdd(build, token->tokenType, idx_a, idx_b)) {
        return 0;
    }

    if(token->tokenType == eTokenSplit) {
        if(!regexVMGenPathCreate(build, token->out_b, 0)) {
            return 0;
        }
    }
    *next = token->out_a;
    return 1;
}

int regexVMProgramGenerate(regex_vm_build_t *build, regex_token_t *token) {
    do {
        for(;token != NULL;) {
            if(!regexVMProgramGenerateInstr(build, token, &token)) {
                return 0;
            }
        }
    } while((token = regexVMGenPathGetNext(build, 0)) != NULL);

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

    regexVMPatchJumps(build, &(build->patch_list));
    regexVMPatchListFree(build->patch_list);

    return 1;
}

int regexVMBuildInit(regex_vm_build_t *build) {
    memset(build, 0, sizeof(regex_vm_build_t));
    if((build->vm = _regexAlloc(sizeof(regex_vm_t), _regexMemContext)) == NULL) {
        return 0;
    }
    build->vm->vm_version = REGEX_VM_MACHINE_VERSION;
    return 1;
}

void regexVMFree(regex_vm_t *vm) {
    regexVMStringTableFree(vm);
    regexVMClassTableFree(vm);
    regexVMUtf8TableFree(vm);
    regexVMGroupTableFree(vm);
    _regexDealloc(vm, _regexMemContext);
}

void regexVMBuildDestroy(regex_vm_build_t *build) {
    regexVMFree(build->vm);
    regexVMGenPathFree(build);
    regexVMPatchListFree(build->patch_list);
    regexTokenDestroy(build->tokens, 1);
}

/////////////////////////////////////////////////////////////////////////////
// VM program source code generation functions
/////////////////////////////////////////////////////////////////////////////

#ifdef MOJO_REGEX_VM_SOURCE_GENERATION

void regexVMGenerateDeclaration(regex_vm_t *vm, const char *symbol, FILE *fp) {
    fprintf(fp, "extern regex_vm_t *%s;\n", symbol);
}

#define MAX_LINE_LEN    80
#define LINE_PADDING    6   // 4 leading spaces, 2 trailing

int hexStrLen(unsigned int val) {
    unsigned int k;

    for(k = 7; k > 2; k-= 1) {
        if(val & (0xFu << (k * 4))) {
            return (int)k + 3; // 3 = '0x' + nibble
        }
    }
    return 4; // 0x##
}

void generateHexValueTable(FILE *fp, unsigned int *table, int size) {
    int k, len, line, first = 1;

    line = MAX_LINE_LEN - LINE_PADDING;
    for(k = 0; k < size; k++) {
        len = hexStrLen(table[k]) + 1;   // value, comma
        if(!first) {
            len++; // padding space between values
        }
        line -= len;
        if(line < 0) {
            fputs("\n    ", fp);
            line = MAX_LINE_LEN - LINE_PADDING;
            first = 1;
        }
        fprintf(fp, "%s0x%0.2X%s", (!first ? " " : ""), table[k],
                (k + 1 != size ? "," : ""));
        first = 0;
    }
}

int intStrLen(int val) {
    int check = (val < 0 ? -val : val);
    int k, len;

    for(len = 0, k = 9; k <= check; len++, k = (k * 10) + 9);
    if(val < 0) {
        len++;
    }
    return len;
}

void generateIntValueTable(FILE *fp, int *table, int size) {
    int k, len, line, first = 1;

    line = MAX_LINE_LEN - LINE_PADDING;
    for(k = 0; k < size; k++) {
        len = intStrLen(table[k]) + 1;   // value, comma
        if(!first) {
            len++; // padding space between values
        }
        line -= len;
        if(line < 0) {
            fputs("\n    ", fp);
            line = MAX_LINE_LEN - LINE_PADDING;
            first = 1;
        }
        fprintf(fp, "%s%d%s", (!first ? " " : ""), table[k],
                (k + 1 != size ? "," : ""));
        first = 0;
    }
}

void generateStringValueTable(FILE *fp, char **table, const int *lengths, int size) {
    int k, len, pad, line, baselen, first = 1;

    line = MAX_LINE_LEN - LINE_PADDING;
    for(k = 0; k < size; k++) {
        if(table[k] == NULL) {
            len = 4; // NULL
            pad = 1; // comma
        } else {
            baselen = (int)(lengths != NULL ? lengths[k] : strlen(table[k]));
            len = regexGetEscapedStrLen(table[k], baselen);
            pad = 3; // quotes, comma
        }
        if(!first) {
            pad++; // padding space between values
        }
        line -= len + pad;
        if((line < 0) && (!first)) {
            fputs("\n    ", fp);
            line = MAX_LINE_LEN - LINE_PADDING;
            first = 1;
        }
        if(table[k] == NULL) {
            fprintf(fp, "%sNULL%s", (!first ? " " : ""), (k + 1 != size ? "," : ""));
        } else {
            fprintf(fp, "%s\"", (!first ? " " : ""));
            regexEmitEscapedStr(fp, table[k], baselen);
            fprintf(fp, "\"%s", (k + 1 != size ? "," : ""));
        }
        first = 0;
    }
}

void regexVMGenerateDefinition(regex_vm_t *vm, const char *symbol, FILE *fp) {
    int k, j, len, line, first = 1;

    // Program bytecode
    line = MAX_LINE_LEN - LINE_PADDING;
    fprintf(fp, "unsigned int _%s_program[] = {\n    ", symbol);
    generateHexValueTable(fp, vm->program, vm->size);
    fprintf(fp, "\n};\n\n");

    // String literal table
    fprintf(fp, "char *_%s_string_table[] = {\n    ", symbol);
    generateStringValueTable(fp, vm->string_table, vm->string_tbl_len, vm->string_tbl_size);
    fprintf(fp, "\n};\n\n");

    fprintf(fp, "int _%s_string_tbl_len[] = {\n    ", symbol);
    generateIntValueTable(fp, vm->string_tbl_len, vm->string_tbl_size);
    fprintf(fp, "\n};\n\n");

    // Character class table
    for(k = 0; k < vm->class_tbl_size; k++) {
        if(vm->class_table[k] == NULL) {
            continue;
        }
        fprintf(fp, "unsigned int _%s_class_entry_%d[] = {\n    ", symbol, k);
        generateHexValueTable(fp, vm->class_table[k], 8);
        fprintf(fp, "\n};\n\n");
    }

    fprintf(fp, "unsigned int *_%s_class_table[] = {\n", symbol);
    for(k = 0; k < vm->class_tbl_size; k++) {
        if(vm->class_table[k] == NULL) {
            fprintf(fp, "    NULL,\n");
        } else {
            fprintf(fp, "    _%s_class_entry_%d,\n", symbol, k);
        }
    }
    fprintf(fp, "};\n\n");

    // Utf8 character class table
    for(k = 0; k < vm->utf8_tbl_size; k++) {
        if(vm->utf8_class_table[k] == NULL) {
            continue;
        }
        fprintf(fp, "unsigned int _%s_utf8_class_entry_%d[] = {\n    ", symbol, k);
        generateHexValueTable(fp, vm->utf8_class_table[k], 2);
        fprintf(fp, "\n};\n\n");
    }

    fprintf(fp, "unsigned int *_%s_utf_class_table[] = {\n    ", symbol);
    for(k = 0; k < vm->utf8_tbl_size; k++) {
        fprintf(fp, "    _%s_class_entry_%d,\n", symbol, k);
    }
    fprintf(fp, "};\n\n");

    // Group table
    fprintf(fp, "char *_%s_group_table[] = {\n    ", symbol);
    generateStringValueTable(fp, vm->group_table, NULL, vm->group_tbl_size);
    fprintf(fp, "\n};\n\n");

    fprintf(fp, "regex_vm_t _%s = {\n", symbol);
    fprintf(fp, "    .vm_version = %d,\n", vm->vm_version);
    fprintf(fp, "    .program = _%s_program,\n", symbol);
    fprintf(fp, "    .size = %d,\n", vm->size);
    fprintf(fp, "    .string_table = _%s_string_table,\n", symbol);
    fprintf(fp, "    .string_tbl_size = %d,\n", vm->string_tbl_size);
    fprintf(fp, "    .string_tbl_len = _%s_string_tbl_len,\n", symbol);
    fprintf(fp, "    .class_table = _%s_class_table,\n", symbol);
    fprintf(fp, "    .class_tbl_size = %d,\n", vm->class_tbl_size);
    fprintf(fp, "    .utf8_class_table = _%s_utf8_class_table,\n", symbol); // TODO
    fprintf(fp, "    .utf8_tbl_size = %d,\n", vm->utf8_tbl_size); // TODO
    fprintf(fp, "    .group_table = _%s_group_table,\n", symbol);
    fprintf(fp, "    .group_tbl_size = %d\n", vm->group_tbl_size);
    fprintf(fp, "};\n\n");
    fprintf(fp, "regex_vm_t *%s = &_%s;\n", symbol, symbol);
}

void regexVMPrintProgram(FILE *fp, regex_vm_t *vm) {
    int pc;

    for(pc = 0; pc < vm->size; pc++) {
        regexEmitVMInstr(fp, vm, pc);
    }
}

#endif // MOJO_REGEX_VM_SOURCE_GENERATION

/////////////////////////////////////////////////////////////////////////////
// Pattern compilation
/////////////////////////////////////////////////////////////////////////////

const char *regexGetCompileStatusStr(eRegexCompileStatus_t status) {
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
        default: return "unknown failure";
    }
}

void regexGetCompileResult(FILE *fp, regex_compile_ctx_t *ctx) {
    fprintf(fp, "Result: %s\n", regexGetCompileStatusStr(ctx->status));
    if(ctx->status != eCompileOk) {
        fprintf(fp, "Pattern: %s\n", ctx->pattern);
        fprintf(fp, "(@%-4d)  %*.*s^", ctx->position, ctx->position, ctx->position, "");
    }
}

// Return a compiled regex, or an error and position within the pattern
eRegexCompileStatus_t regexCompile(regex_compile_ctx_t *ctx, const char *pattern,
                                   unsigned int flags) {
    regex_vm_build_t build;

    if(!regexVerifyTokenDetails()) {
        ctx->status = eCompileInternalError;
        return ctx->status;
    }

    memset(ctx, 0, sizeof(regex_compile_ctx_t));
    ctx->pattern = pattern;
    ctx->status = eCompileOk;

    if(!regexVMBuildInit(&build)) {
        ctx->status = eCompileOutOfMem;
        return ctx->status;
    }
    build.flags = flags;

    // Parse the regex pattern into a sequence of tokens (operators and operands)
    // The output of this stage is a sequence of lexical tokens in infix form

    if((ctx->status = regexTokenizePattern(ctx->pattern, &(ctx->position),
                                           &(build.tokens), &build)) != eCompileOk) {
        regexTokenDestroy(build.tokens, 1);
        build.tokens = NULL;
        regexVMBuildDestroy(&build);
        ctx->vm = NULL;
        return ctx->status;
    }

    // Next, convert the infix form of the regular expression to postfix form,
    // and derive an NFA representation. We accomplish this using the shunting
    // yard algorithm. This is then converted into a VM bytecode sequence, for
    // runtime evaluation.

    if((ctx->status = regexShuntingYard(&build, &(build.tokens))) != eCompileOk) {
        regexVMBuildDestroy(&build);
        ctx->vm = NULL;
        return ctx->status;
    }

    if(!regexVMProgramGenerate(&build, build.tokens)) {
        regexVMBuildDestroy(&build);
        regexSubroutineIdxFree(&build);
        ctx->vm = NULL;
        ctx->status = eCompileOutOfMem;
        return ctx->status;
    }
    regexSubroutineIdxFree(&build);

    // Fixup the lookup tables
    build.vm->group_tbl_size = build.groups;

    build.vm->size = build.pc;
    ctx->vm = build.vm;

    return eCompileOk;
}

#endif // MOJO_REGEX_COMPILE_IMPLEMENTATION

/////////////////////////////////////////////////////////////////////////////
// Pattern evaluation structures and functions
/////////////////////////////////////////////////////////////////////////////

#ifdef MOJO_REGEX_EVALUATE_IMPLEMENTATION

typedef struct regex_compound_s regex_compound_t;
struct regex_compound_s {
    int subexpr;
    const char *start;
    const char *end;
    regex_compound_t *next;
};

typedef struct regex_thread_s regex_thread_t;
struct regex_thread_s {
    unsigned int pc;
    int pos;
    int callstack[REGEX_THREAD_CALLSTACK_MAX_DEPTH];
    regex_thread_t *next;
    regex_compound_t *compound;
    const char *subexprs[0];
};

typedef struct regex_eval_s regex_eval_t;
struct regex_eval_s {
    const char *sp;
    int len;
    int pos;
    regex_thread_t *thread;
    regex_thread_t *queue;
    regex_vm_t *vm;
    const char **subexprs;
    regex_compound_t *compound_pool;
    regex_thread_t *pool;
#ifdef MOJO_REGEX_VM_DEBUG
    FILE *debug;
#endif // MOJO_REGEX_VM_DEBUG
};

regex_compound_t *regexThreadCompoundCreate(regex_eval_t *eval, regex_thread_t *thread, int append) {
    regex_compound_t *entry, *walk;

    if(eval->compound_pool != NULL) {
        entry = eval->compound_pool;
        eval->compound_pool = entry->next;
    } else {
        if((entry = _regexAlloc(sizeof(regex_compound_t), _regexMemContext)) == NULL) {
            return NULL;
        }
    }
    memset(entry, 0, sizeof(regex_compound_t));
    if(append) {
        // Used during copy, to retain entry ordering
        if(thread->compound == NULL) {
            thread->compound = entry;
        } else {
            for(walk = thread->compound; walk->next != NULL; walk = walk->next);
            walk->next = entry;
        }
    } else {
        entry->next = thread->compound;
        thread->compound = entry;
    }
    return entry;
}

void regexThreadCompoundDestroy(regex_eval_t *eval, regex_compound_t *compound) {
    regex_compound_t *next;

    if(eval == NULL) {
        for(; compound != NULL; compound = next) {
            next = compound->next;
            _regexDealloc(compound, _regexMemContext);
        }
    } else {
        for(; compound != NULL; compound = next) {
            next = compound->next;
            compound->next = eval->compound_pool;
            eval->compound_pool = compound;
        }
    }
}

int regexThreadCompoundCopy(regex_eval_t *eval, regex_thread_t *dest, regex_thread_t *src) {
    regex_compound_t *walk, *entry;

    for(walk = src->compound; walk != NULL; walk = walk->next) {
        if((entry = regexThreadCompoundCreate(eval, dest, 1)) == NULL) {
            return 0;
        }
        entry->subexpr = walk->subexpr;
        entry->start = walk->start;
        entry->end = walk->end;
    }
    return 1;
}

int regexThreadCompoundCount(regex_compound_t *compound) {
    int k = 0;

    for(; compound != NULL; k++, compound = compound->next);

    return k;
}

void regexThreadCompoundReverse(regex_compound_t **compound) {
    regex_compound_t *base = NULL, *walk, *next;

    for(walk = *compound; walk != NULL; walk = next) {
        next = walk->next;
        walk->next = base;
        base = walk;
    }
    *compound = base;
}

size_t regexThreadCompoundCalcMatchBufferSize(regex_vm_t *vm, regex_thread_t *thread) {
    size_t size;

    // an entry for each group
    size = sizeof(const char ***) * vm->group_tbl_size;

    // a null entry to terminate each group
    size += sizeof(const char **) * vm->group_tbl_size;

    // 2 pointers for each compound match (start, end)
    size += sizeof(const char **) * (2 * regexThreadCompoundCount(thread->compound));

    return size;
}

// Note: Assumes that match has preallocated sufficient space and provides a
// pointer via baseMem
void regexThreadCompoundStoreInMatch(regex_match_t *match, const char *baseMem, regex_thread_t *thread) {
    int grp, cnt;
    regex_compound_t *walk;

    regexThreadCompoundReverse(&(thread->compound));

    match->compound = (const char ***)baseMem;
    baseMem += match->vm->group_tbl_size * sizeof(const char ***);
    for(grp = 0; grp < match->vm->group_tbl_size; grp++) {
        for(cnt = 0, walk = thread->compound; walk != NULL; walk = walk->next) {
            if((walk->subexpr / 2) == grp) {
                cnt++;
            }
        }
        match->compound[grp] = (const char **)baseMem;
        if(cnt == 0) {
            match->compound[grp][0] = NULL;
            baseMem += sizeof(char **);
        } else {
            baseMem += sizeof(char **) * (((cnt  + 1) * 2) + 1);
            cnt = 0;
            for(walk = thread->compound; walk != NULL; walk = walk->next) {
                if((walk->subexpr / 2) == grp) {
                    match->compound[grp][cnt * 2] = walk->start;
                    match->compound[grp][(cnt * 2) + 1] = walk->end;
                    cnt++;
                }
            }
            match->compound[grp][cnt * 2] = NULL;
        }
    }
}

int regexThreadCompoundStart(regex_eval_t *eval, regex_thread_t *thread, int subexpr, const char *ptr) {
    regex_compound_t *entry;

    if((entry = regexThreadCompoundCreate(eval, thread, 0)) == NULL) {
        return 0;
    }
    entry->subexpr = subexpr;
    entry->start = ptr;
    return 1;
}

int regexThreadCompoundEnd(regex_thread_t *thread, const char *ptr) {
    if(thread->compound == NULL) {
        return 0;
    }
    thread->compound->end = ptr;
    return 1;
}

void regexThreadCopySubexprs(int count, regex_thread_t *dest, regex_thread_t *src) {
    int k;

    for(k = 0; k < count; k++) {
        dest->subexprs[k] = src->subexprs[k];
    }
}

regex_eval_t *regexEvalCreate(regex_vm_t *vm, const char *pattern, int len) {
    regex_eval_t *eval;

    if((eval = _regexAlloc(sizeof(regex_eval_t), _regexMemContext)) == NULL) {
        return NULL;
    }
    memset(eval, 0, sizeof(regex_eval_t));

    if((eval->subexprs = _regexAlloc(sizeof(char *) * 2 * vm->group_tbl_size, _regexMemContext)) == NULL) {
        _regexDealloc(eval, _regexMemContext);
        return NULL;
    }
    memset(eval->subexprs, 0, sizeof(char *) * 2 * vm->group_tbl_size);

    eval->vm = vm;
    eval->sp = pattern;
    if(len == REGEX_STR_NULL_TERMINATED) {
        eval->len = strlen(pattern);
    } else {
        eval->len = len;
    }

    return eval;
}

int regexThreadCreate(regex_eval_t *eval, regex_thread_t *parent, unsigned int pc, int queued) {
    regex_thread_t *thread;
    int k;

    if(eval->pool != NULL) {
        thread = eval->pool;
        eval->pool = thread->next;
        thread->next = NULL;
    } else {
        if((thread = _regexAlloc(sizeof(regex_thread_t) + (eval->vm->group_tbl_size * 2 * sizeof(char *)), _regexMemContext)) == NULL) {
            return 0;
        }
    }
    memset(thread, 0, sizeof(regex_thread_t) + (eval->vm->group_tbl_size * 2 * sizeof(char *)));
    if(parent != NULL) {
        regexThreadCopySubexprs(eval->vm->group_tbl_size * 2, thread, parent);
        regexThreadCompoundCopy(eval, thread, parent);
        memcpy(thread->callstack, parent->callstack, sizeof(int) * REGEX_THREAD_CALLSTACK_MAX_DEPTH);
    } else {
        for(k = 0; k < REGEX_THREAD_CALLSTACK_MAX_DEPTH; k++) {
            thread->callstack[k] = -1;
        }
    }
    thread->pc = pc;
    thread->pos = -1;

    if(queued) {
        thread->next = eval->queue;
        eval->queue = thread;
    } else {
        thread->next = eval->thread;
        eval->thread = thread;
    }

    return 1;
}

void regexThreadFree(regex_eval_t *eval, regex_thread_t *thread) {
    regex_thread_t *next;

    if(thread == NULL) {
        return;
    }
    if(eval != NULL) {
        thread->next = eval->pool;
        eval->pool = thread;
        return;
    }
    for(; thread != NULL; thread = next) {
        next = thread->next;
        regexThreadCompoundDestroy(NULL, thread->compound);
        _regexDealloc(thread, _regexMemContext);
    }
}

void regexEvalFree(regex_eval_t *eval) {
    regexThreadFree(NULL, eval->thread);
    regexThreadFree(NULL, eval->queue);
    regexThreadFree(NULL, eval->pool);
    if(eval->subexprs != NULL) {
        _regexDealloc(eval->subexprs, _regexMemContext);
        eval->subexprs = NULL;
    }
    regexThreadCompoundDestroy(NULL, eval->compound_pool);
    _regexDealloc(eval, _regexMemContext);
}

int regexVMNoTextAdvance(unsigned int instr) {
    switch(instr & 0xFU) {
        case eTokenMatch:
        case eTokenJmp:
        case eTokenSplit:
        case eTokenSave:
        case eTokenCall:
        case eTokenReturn:
            return 1;
        default:
            return 0;
    }
}

typedef enum {
    eEvalStackOverrun = -3,
    eEvalInternalError = -2,
    eEvalOutOfMem = -1,
    eEvalNoMatch = 0,
    eEvalMatch = 1,
    eEvalContinue = 2
} eRegexEvalResult;

eRegexEvalResult regexThreadProcess(regex_eval_t *eval, regex_thread_t *thread, int complete) {
    const unsigned int *bitmap;
    const char *str;
    int len;
    int k;
    unsigned int instr[3];
    unsigned char c = *eval->sp;
#ifdef MOJO_REGEX_VM_DEBUG
    int first = 1;
#endif // MOJO_REGEX_VM_DEBUG

    while(regexVMNoTextAdvance(eval->vm->program[thread->pc])) {
#ifdef MOJO_REGEX_VM_DEBUG
        if(eval->debug != NULL) {
            if(!first) {
                fprintf(eval->debug, "             ");
            }
            regexEmitVMInstr(eval->debug, eval->vm, thread->pc);
            first = 0;
        }
#endif // MOJO_REGEX_VM_DEBUG
        REGEX_VM_INSTR_DECODE(instr, eval->vm, thread->pc);
        switch(instr[0]) {
            case eTokenSave:
                if(instr[1] % 2) {
                    // Group end
                    thread->subexprs[instr[1]] = eval->sp;
                    if(instr[2]) {
                        if(!regexThreadCompoundEnd(thread, eval->sp)) {
                            return eEvalOutOfMem;
                        }
                    }
                } else {
                    // Group start
                    if(thread->subexprs[instr[1]] == NULL) {
                        thread->subexprs[instr[1]] = eval->sp;
                    }
                    if(instr[2]) {
                        if(!regexThreadCompoundStart(eval, thread, instr[1], eval->sp)) {
                            return eEvalOutOfMem;
                        }
                    }
                }
                thread->pc++;
                break;
            case eTokenSplit:
                if(!regexThreadCreate(eval, thread, instr[1], 0)) {
                    return eEvalOutOfMem;
                }
                thread->pc = instr[2];
                continue;
            case eTokenMatch:
                if((complete || instr[1]) && eval->pos == eval->len) {
                    return eEvalMatch;
                }
                if(!complete && !instr[1]) {
                    return eEvalMatch;
                }
                return eEvalNoMatch;
            case eTokenJmp:
                thread->pc = instr[1];
                continue;
            case eTokenCall:
                for(k = 0; k < REGEX_THREAD_CALLSTACK_MAX_DEPTH; k++) {
                    if(thread->callstack[k] == -1) {
                        thread->callstack[k] = thread->pc + 1;
                        thread->pc = instr[1];
                        break;
                    }
                }
                if(k == REGEX_THREAD_CALLSTACK_MAX_DEPTH) {
                    return eEvalStackOverrun;
                }
                break;
            case eTokenReturn:
                for(k = REGEX_THREAD_CALLSTACK_MAX_DEPTH - 1; k >= 0; k--) {
                    if(thread->callstack[k] != -1) {
                        thread->pc = thread->callstack[k];
                        thread->callstack[k] = -1;
                        break;
                    }
                }
                if(k < 0) {
                    return eEvalInternalError;
                }
                break;
            default:
                return eEvalInternalError;
        }
    }
#ifdef MOJO_REGEX_VM_DEBUG
    if(eval->debug) {
        if(!first) {
            fprintf(eval->debug, "             ");
        }
        regexEmitVMInstr(eval->debug, eval->vm, thread->pc);
        first = 0;
    }
#endif // MOJO_REGEX_VM_DEBUG
    REGEX_VM_INSTR_DECODE(instr, eval->vm, thread->pc);
    switch(instr[0]) {
        case eTokenCharLiteral:
            if((instr[2] && instr[1] == c) || (instr[1] != c)) {
                return eEvalNoMatch;
            } else {
                thread->pc++;
            }
            return eEvalMatch;
        case eTokenStringLiteral:
            if((str = regexVMStringTableEntryGet(eval->vm, instr[1], &len)) == NULL) {
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
        case eTokenCharClass:
            if((bitmap = regexVMClassTableEntryGet(eval->vm, instr[1])) == NULL) {
                return eEvalInternalError;
            }
            if(!charClassBitmapCheck(bitmap, c)) {
                return eEvalNoMatch;
            }
            thread->pc++;
            return eEvalMatch;
        case eTokenUtf8Class:
            if((bitmap = regexVMUtf8TableEntryGet(eval->vm, instr[1])) == NULL) {
                return eEvalInternalError;
            }
            if(!utf8ClassBitmapCheck(bitmap, c)) {
                return eEvalNoMatch;
            }
            thread->pc++;
            return eEvalMatch;
        case eTokenCharAny:
            if(*eval->sp == '\n') {
                return eEvalNoMatch;
            }
            // intentional fall through
        case eTokenCharAnyDotAll:
            if(1) { // unicode mode
                if(thread->pos == -1) {
                    thread->pos = parseUtf8EncodedHighByte(c);
                } else if((c & UTF8_LOW_BYTE_MASK) != UTF8_LOW_BYTE_PREFIX) {
                    return eEvalNoMatch;
                }
                thread->pos--;
                if(thread->pos == 0) {
                    thread->pc++;
                    thread->pos = -1;
                    return eEvalMatch;
                }
                return eEvalContinue;
            } else {
                thread->pc++;
            }
            return eEvalMatch;
        case eTokenByte:
            thread->pc++;
            return eEvalMatch;
        default:
            return eEvalInternalError;
    }
}

#ifdef MOJO_REGEX_VM_DEBUG
regex_match_t *regexMatch(regex_vm_t *vm, const char *text, int len, int anchored, FILE *debug) {
#else // !MOJO_REGEX_VM_DEBUG
regex_match_t *regexMatch(regex_vm_t *vm, const char *text, int len, int anchored) {
#endif // MOJO_REGEX_VM_DEBUG
    regex_match_t *match;
    regex_eval_t *eval;
    regex_thread_t *thread;
    size_t capture_size;
    size_t compound_size;
    int k;

    if(vm->vm_version != REGEX_VM_MACHINE_VERSION) {
        return NULL;
    }

    if((eval = regexEvalCreate(vm, text, len)) == NULL) {
        return NULL;
    }

#ifdef MOJO_REGEX_VM_DEBUG
    eval->debug = debug;
#endif // MOJO_REGEX_VM_DEBUG

    if(!regexThreadCreate(eval, NULL, 0, 1)) {
        regexEvalFree(eval);
        return NULL;
    }

    for(; eval->pos != eval->len; eval->sp++, eval->pos++) {
#ifdef MOJO_REGEX_VM_DEBUG
        if(eval->debug != NULL) {
            fprintf(eval->debug, "[%2d] (%c:%3d)\n", eval->pos, *eval->sp, *eval->sp);
        }
#endif // MOJO_REGEX_VM_DEBUG
        if(eval->queue == NULL) {
            break;
        }
        eval->thread = eval->queue;
        eval->queue = NULL;
        for(thread = eval->thread; thread != NULL; thread = eval->thread) {
#ifdef MOJO_REGEX_VM_DEBUG
            if(eval->debug != NULL) {
                fprintf(eval->debug, "    %p ", thread);
            }
#endif // MOJO_REGEX_VM_DEBUG
            eval->thread = thread->next;
            switch(regexThreadProcess(eval, thread, anchored)) {
                default:
                case eEvalStackOverrun:
                case eEvalInternalError:
                case eEvalOutOfMem:
#ifdef MOJO_REGEX_VM_DEBUG
                    if(eval->debug) {
                        fprintf(eval->debug, "    %p --- thread failure\n", thread);
                    }
#endif // MOJO_REGEX_VM_DEBUG
                    regexEvalFree(eval);
                    return NULL;
                case eEvalMatch:
                case eEvalContinue:
                    if(eval->vm->program[thread->pc] == eTokenMatch) {
                        if((anchored && (eval->pos == eval->len)) || (!anchored)) {
                            goto FoundMatch;
                        }
                    }
                    thread->next = eval->queue;
                    eval->queue = thread;
                    break;
                case eEvalNoMatch:
#ifdef MOJO_REGEX_VM_DEBUG
                    if(eval->debug) {
                        fprintf(eval->debug, "    %p --- thread exit\n", thread);
                    }
#endif // MOJO_REGEX_VM_DEBUG
                    regexThreadFree(eval, thread);
                    break;
            }
        }
    }

    for(thread = eval->thread; thread != NULL; thread = thread->next) {
        if((eval->vm->program[thread->pc] & 0xFU) == eTokenMatch) {
            goto FoundMatch;
        }
    }
    for(thread = eval->queue; thread != NULL; thread = thread->next) {
        if((eval->vm->program[thread->pc] & 0xFU) == eTokenMatch) {
            goto FoundMatch;
        }
    }

    regexEvalFree(eval);

    return NULL;

FoundMatch:

    // Expects "thread" to be the success thread
    capture_size = sizeof(char *) * 2 * vm->group_tbl_size;
    compound_size = regexThreadCompoundCalcMatchBufferSize(vm, thread);
    if((match = _regexAlloc(sizeof(regex_match_t) + capture_size + compound_size, _regexMemContext)) == NULL) {
        return NULL;
    }
    memset(match, 0, sizeof(regex_match_t));
    match->subexprs = (const char **) match->_buffer;
    match->vm = vm;
    match->text = text;
    match->len = eval->len;
    match->pos = eval->sp;
    for(k = 0; k < (vm->group_tbl_size * 2); k++) {
        match->subexprs[k] = thread->subexprs[k];
    }
    regexThreadCompoundStoreInMatch(match, match->_buffer + capture_size, thread);
    thread->compound = NULL;
    regexEvalFree(eval);

    return match;
}

void regexMatchFree(regex_match_t *match) {
    _regexDealloc(match, _regexMemContext);
}

int regexGroupCountGet(regex_vm_t *vm) {
    return vm->group_tbl_size;
}

const char *regexGroupNameLookup(regex_vm_t *vm, int group) {
    return regexVMGroupNameFromIndex(vm, group);
}

int regexGroupIndexLookup(regex_vm_t *vm, const char *name) {
    return regexVMGroupNameLookup(vm, name);
}

const char *regexGroupValueGet(regex_match_t *match, int group, int *len) {
    int start, end;

    if(len != NULL) {
        *len = 0;
    }

    if((group < 0) || (group > match->vm->group_tbl_size)) {
        return NULL;
    }

    if(group == 0) {
        // Special group (the complete matched text)
        if(match->pos == NULL) {
            return NULL;
        }
        if(len != NULL) {
            *len = match->pos - match->text + 1;
        }
        return match->text;
    }

    start = regexSubexprStartFromGroup(group);
    end = regexSubexprEndFromGroup(group);
    if((match->subexprs[start] == NULL) || (match->subexprs[end] == NULL)) {
        if(len != NULL) {
            *len = 0;
        }
        return NULL;
    }

    if(len != NULL) {
        *len = match->subexprs[end] - match->subexprs[start];
    }

    return match->subexprs[start];
}

const char *regexGroupValueGetByName(regex_match_t *match, const char *name, int *len) {
    return regexGroupValueGet(match, regexGroupIndexLookup(match->vm, name), len);
}

int regexGroupCompoundCountGet(regex_match_t *match, int group) {
    int cnt;
    if((group < 1) || (group > match->vm->group_tbl_size)) {
        return 0;
    }
    group--;

    for(cnt = 0; match->compound[group][cnt] != NULL; cnt++);

    return cnt / 2;
}

const char *regexGroupCompoundValueGet(regex_match_t *match, int group, int num, int *len) {
    int cnt;

    if(len != NULL) {
        *len = 0;
    }
    if((group < 1) || (group > match->vm->group_tbl_size)) {
        return NULL;
    }
    group--;
    num *= 2;
    for(cnt = 0; ((cnt < num) && (match->compound[group][cnt] != NULL)); cnt++);
    if(match->compound[group][cnt] == NULL) {
        return NULL;
    }
    if(len != NULL) {
        *len = match->compound[group][cnt + 1] - match->compound[group][cnt];
    }
    return match->compound[group][cnt];
}

#endif // MOJO_REGEX_EVALUATE_IMPLEMENTATION

/////////////////////////////////////////////////////////////////////////////
// Regex dev test functions
/////////////////////////////////////////////////////////////////////////////

#ifdef MOJO_REGEX_TEST_MAIN

void regexDumpMatch(regex_match_t *match) {
    int k, m, n, len;
    const char *ptr;

    if(match == NULL) {
        printf("No match\n");
        return;
    }
    printf("Match found\n");
    printf("    %d groups\n", regexGroupCountGet(match->vm));
    for(k = 0; k <= regexGroupCountGet(match->vm); k++) {
        if((ptr = regexGroupValueGet(match, k, &len)) == NULL) {
            printf("        %d [%s]: no match\n", k, regexGroupNameLookup(match->vm, k));
        } else {
            printf("        %d [%s]: [", k, regexGroupNameLookup(match->vm, k));
            regexEmitEscapedStr(stdout, ptr, len);
            printf("]\n");
            if((m = regexGroupCompoundCountGet(match, k)) > 0) {
                for(n = 0; n < m; n++) {
                    ptr = regexGroupCompoundValueGet(match, k, n, &len);
                    printf("            %d [", n);
                    regexEmitEscapedStr(stdout, ptr, len);
                    printf("]\n");

                }
            }
        }
    }
}

int main(int argc, char **argv) {
    regex_compile_ctx_t result;
    regex_match_t *match;

    if(argc > 1) {
        if(regexCompile(&result, argv[1], 0) != eCompileOk) {
            //printf("Compile failed: %s", regexGetCompileStatusStr(result.status));
            regexGetCompileResult(stdout, &result);
            return 1;
        }
        if(argc > 2) {
            regexVMPrintProgram(stdout, result.vm);
            printf("-------------------------\n");

            printf("Evaluating [%s]\n", argv[2]);
            printf(" (escaped) [");
            regexEmitEscapedStr(stdout, argv[2], (int)strlen(argv[2]));
            printf("]\n");
#ifdef MOJO_REGEX_VM_DEBUG
            if((match = regexMatch(result.vm, argv[2], REGEX_STR_NULL_TERMINATED, 0, stdout)) == NULL) {
#else // !MOJO_REGEX_VM_DEBUG
            if((match = regexMatch(result.vm, argv[2], REGEX_STR_NULL_TERMINATED, 0)) == NULL) {
#endif // MOJO_REGEX_VM_DEBUG
                printf("No match\n");
                //return 1;
            } else {
                regexDumpMatch(match);
                regexMatchFree(match);
            }
        }
        //regexVMGenerateDeclaration(result.vm, "myparser", stdout);
        //regexVMGenerateDefinition(result.vm, "myparser", stdout);
    }

    return 0;
}

#endif // MOJO_REGEX_TEST_MAIN

/////////////////////////////////////////////////////////////////////////////
// Unicode property class definitions
/////////////////////////////////////////////////////////////////////////////

#ifdef MOJO_REGEX_UNICODE

// These property classes were generated from the unicode database by the
// extract_unicode_props.py script

const char unicode_combining_marks[] =
    "\\u05BF\\u05C7\\u0670\\u0711\\u09D7\\u0A51\\u0A75\\u0B82\\u0BD7\\u0D57"\
    "\\u0DD6\\u0E31\\u0EB1\\u108F\\u18A9\\uA802\\uA80B\\uA8FF\\uA9E5\\uAA43"\
    "\\uAAB0\\uFB1E\\U01123E\\U011357\\U0119E4\\U011D3A\\U011D47\\U016F4F"\
    "\\u05BF\\u05C7\\u0670\\u0711\\u07FD\\u09BC\\u09FE\\u0A3C\\u0A51\\u0A75"\
    "\\u0ABC\\u0B3C\\u0B82\\u0CBC\\u0DCA\\u0DD6\\u0E31\\u0EB1\\u0F35\\u0F37"\
    "\\u0F39\\u0FC6\\u17DD\\u18A9\\u1A7F\\u1CED\\u1CF4\\u2D7F\\uA802\\uA806"\
    "\\uA80B\\uA8FF\\uA9E5\\uAA43\\uAAB0\\uAAC1\\uFB1E\\U0101FD\\U0102E0"\
    "\\U010A3F\\U011173\\U01123E\\U01145E\\U011A47\\U011D3A\\U011D47"\
    "\\U016F4F\\U01DA75\\U01DA84\\u05BF\\u05C7\\u0670\\u0711\\u07FD\\u09BC"\
    "\\u09D7\\u09FE\\u0A3C\\u0A51\\u0A75\\u0ABC\\u0B3C\\u0B82\\u0BD7\\u0CBC"\
    "\\u0D57\\u0DCA\\u0DD6\\u0E31\\u0EB1\\u0F35\\u0F37\\u0F39\\u0FC6\\u108F"\
    "\\u17DD\\u18A9\\u1A7F\\u1CED\\u1CF4\\u2D7F\\uA802\\uA806\\uA80B\\uA8FF"\
    "\\uA9E5\\uAA43\\uAAB0\\uAAC1\\uFB1E\\U0101FD\\U0102E0\\U010A3F"\
    "\\U011173\\U01123E\\U011357\\U01145E\\U0119E4\\U011A47\\U011D3A"\
    "\\U011D47\\U016F4F\\U01DA75\\U01DA84\\u05BF\\u05C7\\u0670\\u0711"\
    "\\u07FD\\u09BC\\u09D7\\u09FE\\u0A3C\\u0A51\\u0A75\\u0ABC\\u0B3C\\u0B82"\
    "\\u0BD7\\u0CBC\\u0D57\\u0DCA\\u0DD6\\u0E31\\u0EB1\\u0F35\\u0F37\\u0F39"\
    "\\u0FC6\\u108F\\u17DD\\u18A9\\u1A7F\\u1CED\\u1CF4\\u2D7F\\uA802\\uA806"\
    "\\uA80B\\uA8FF\\uA9E5\\uAA43\\uAAB0\\uAAC1\\uFB1E\\U0101FD\\U0102E0"\
    "\\U010A3F\\U011173\\U01123E\\U011357\\U01145E\\U0119E4\\U011A47"\
    "\\U011D3A\\U011D47\\U016F4F\\U01DA75\\U01DA84\\u05BF\\u05C7\\u0670"\
    "\\u0711\\u07FD\\u09BC\\u09D7\\u09FE\\u0A3C\\u0A51\\u0A75\\u0ABC\\u0B3C"\
    "\\u0B82\\u0BD7\\u0CBC\\u0D57\\u0DCA\\u0DD6\\u0E31\\u0EB1\\u0F35\\u0F37"\
    "\\u0F39\\u0FC6\\u17DD\\u18A9\\u1A7F\\u1CED\\u1CF4\\u2D7F\\uA802\\uA806"\
    "\\uA80B\\uA8FF\\uA9E5\\uAA43\\uAAB0\\uAAC1\\uFB1E\\U0101FD\\U0102E0"\
    "\\U010A3F\\U011173\\U01123E\\U011357\\U01145E\\U011A47\\U011D3A"\
    "\\U011D47\\U016F4F\\U01DA75\\U01DA84\\u108F\\U0119E4\\u05BF\\u05C7"\
    "\\u0670\\u0711\\u07FD\\u09BC\\u09D7\\u09FE\\u0A3C\\u0A51\\u0A75\\u0ABC"\
    "\\u0B3C\\u0B82\\u0BD7\\u0CBC\\u0D57\\u0DCA\\u0DD6\\u0E31\\u0EB1\\u0F35"\
    "\\u0F37\\u0F39\\u0FC6\\u108F\\u17DD\\u18A9\\u1A7F\\u1CED\\u1CF4\\u2D7F"\
    "\\uA802\\uA806\\uA80B\\uA8FF\\uA9E5\\uAA43\\uAAB0\\uAAC1\\uFB1E"\
    "\\U0101FD\\U0102E0\\U010A3F\\U011173\\U01123E\\U011357\\U01145E"\
    "\\U0119E4\\U011A47\\U011D3A\\U011D47\\U016F4F\\U01DA75\\U01DA84"\
    "\\u20D0-\\u20F0\\u0591-\\u05BD\\u05C1-\\u05C2\\u05C4-\\u05C5"\
    "\\u0610-\\u061A\\u064B-\\u065F\\u06D6-\\u06DC\\u06DF-\\u06E4"\
    "\\u06E7-\\u06E8\\u0730-\\u074A\\u07A6-\\u07B0\\u0816-\\u0819"\
    "\\u081B-\\u0823\\u0825-\\u0827\\u0829-\\u082D\\u08D3-\\u08E1"\
    "\\u08E3-\\u0903\\u093A-\\u093C\\u093E-\\u094F\\u0951-\\u0957"\
    "\\u0962-\\u0963\\u0981-\\u0983\\u09BE-\\u09C4\\u09C7-\\u09C8"\
    "\\u09CB-\\u09CD\\u09E2-\\u09E3\\u0A01-\\u0A03\\u0A3E-\\u0A42"\
    "\\u0A47-\\u0A48\\u0A4B-\\u0A4D\\u0A70-\\u0A71\\u0A81-\\u0A83"\
    "\\u0ABE-\\u0AC5\\u0AC7-\\u0AC9\\u0ACB-\\u0ACD\\u0AE2-\\u0AE3"\
    "\\u0AFA-\\u0AFF\\u0B01-\\u0B03\\u0B3E-\\u0B44\\u0B47-\\u0B48"\
    "\\u0B4B-\\u0B4D\\u0B56-\\u0B57\\u0B62-\\u0B63\\u0BBE-\\u0BC2"\
    "\\u0BC6-\\u0BC8\\u0BCA-\\u0BCD\\u0C00-\\u0C04\\u0C3E-\\u0C44"\
    "\\u0C46-\\u0C48\\u0C4A-\\u0C4D\\u0C55-\\u0C56\\u0C62-\\u0C63"\
    "\\u0C81-\\u0C83\\u0CBE-\\u0CC4\\u0CC6-\\u0CC8\\u0CCA-\\u0CCD"\
    "\\u0CD5-\\u0CD6\\u0CE2-\\u0CE3\\u0D00-\\u0D03\\u0D3E-\\u0D44"\
    "\\u0D46-\\u0D48\\u0D4A-\\u0D4D\\u0D62-\\u0D63\\u0D82-\\u0D83"\
    "\\u0DCF-\\u0DD4\\u0DD8-\\u0DDF\\u0DF2-\\u0DF3\\u0E34-\\u0E3A"\
    "\\u0EB4-\\u0EBC\\u0F71-\\u0F84\\u0F8D-\\u0F97\\u0F99-\\u0FBC"\
    "\\u102B-\\u103E\\u1056-\\u1059\\u105E-\\u1060\\u1062-\\u1064"\
    "\\u1067-\\u106D\\u1071-\\u1074\\u1082-\\u108D\\u109A-\\u109D"\
    "\\u1712-\\u1714\\u1732-\\u1734\\u1752-\\u1753\\u1772-\\u1773"\
    "\\u1885-\\u1886\\u1920-\\u192B\\u1930-\\u193B\\u1A17-\\u1A1B"\
    "\\u1A55-\\u1A5E\\u1A60-\\u1A7C\\u1B00-\\u1B04\\u1B34-\\u1B44"\
    "\\u1B80-\\u1B82\\u1BA1-\\u1BAD\\u1BE6-\\u1BF3\\u1C24-\\u1C37"\
    "\\u1DC0-\\u1DF9\\u2DE0-\\u2DFF\\uA674-\\uA67D\\uA69E-\\uA69F"\
    "\\uA823-\\uA827\\uA880-\\uA881\\uA8B4-\\uA8C5\\uA926-\\uA92D"\
    "\\uA947-\\uA953\\uA980-\\uA983\\uA9B3-\\uA9C0\\uAA29-\\uAA36"\
    "\\uAA4C-\\uAA4D\\uAA7B-\\uAA7D\\uAAB2-\\uAAB4\\uAAB7-\\uAAB8"\
    "\\uAAEB-\\uAAEF\\uABE3-\\uABEA\\U010376-\\U01037A\\U010A01-\\U010A03"\
    "\\U010A05-\\U010A06\\U010A0C-\\U010A0F\\U010D24-\\U010D27"\
    "\\U011000-\\U011002\\U011038-\\U011046\\U0110B0-\\U0110BA"\
    "\\U011100-\\U011102\\U011127-\\U011134\\U011145-\\U011146"\
    "\\U011180-\\U011182\\U0111B3-\\U0111C0\\U0112DF-\\U0112EA"\
    "\\U011300-\\U011303\\U01133E-\\U011344\\U011347-\\U011348"\
    "\\U01134B-\\U01134D\\U011362-\\U011363\\U011435-\\U011446"\
    "\\U0114B0-\\U0114C3\\U0115AF-\\U0115B5\\U0115B8-\\U0115C0"\
    "\\U0115DC-\\U0115DD\\U011630-\\U011640\\U0116AB-\\U0116B7"\
    "\\U01171D-\\U01172B\\U01182C-\\U01183A\\U0119D1-\\U0119D7"\
    "\\U0119DA-\\U0119E0\\U011A01-\\U011A0A\\U011A33-\\U011A39"\
    "\\U011A3B-\\U011A3E\\U011A51-\\U011A5B\\U011A8A-\\U011A99"\
    "\\U011C2F-\\U011C36\\U011C38-\\U011C3F\\U011C92-\\U011CA7"\
    "\\U011CA9-\\U011CB6\\U011D31-\\U011D36\\U011D3C-\\U011D3D"\
    "\\U011D3F-\\U011D45\\U011D8A-\\U011D8E\\U011D90-\\U011D91"\
    "\\U011D93-\\U011D97\\U011EF3-\\U011EF6\\U016F51-\\U016F87"\
    "\\U016F8F-\\U016F92\\U01E000-\\U01E006\\U01E008-\\U01E018"\
    "\\U01E01B-\\U01E021\\U01E023-\\U01E024\\U01E026-\\U01E02A"\
    "\\u0300-\\u036F\\u0483-\\u0489\\u06EA-\\u06ED\\u07EB-\\u07F3"\
    "\\u0859-\\u085B\\u0D3B-\\u0D3C\\u0E47-\\u0E4E\\u0EC8-\\u0ECD"\
    "\\u0F18-\\u0F19\\u0F86-\\u0F87\\u135D-\\u135F\\u17B4-\\u17D3"\
    "\\u180B-\\u180D\\u1AB0-\\u1ABE\\u1B6B-\\u1B73\\u1CD0-\\u1CD2"\
    "\\u1CD4-\\u1CE8\\u1CF7-\\u1CF9\\u1DFB-\\u1DFF\\u2CEF-\\u2CF1"\
    "\\u302A-\\u302F\\u3099-\\u309A\\uA66F-\\uA672\\uA6F0-\\uA6F1"\
    "\\uA8E0-\\uA8F1\\uAABE-\\uAABF\\uAAF5-\\uAAF6\\uFE00-\\uFE0F"\
    "\\uFE20-\\uFE2F\\U010A38-\\U010A3A\\U010AE5-\\U010AE6"\
    "\\U010F46-\\U010F50\\U01107F-\\U011082\\U0111C9-\\U0111CC"\
    "\\U01122C-\\U011237\\U01133B-\\U01133C\\U011366-\\U01136C"\
    "\\U011370-\\U011374\\U016AF0-\\U016AF4\\U016B30-\\U016B36"\
    "\\U01BC9D-\\U01BC9E\\U01D165-\\U01D169\\U01D17B-\\U01D182"\
    "\\U01D185-\\U01D18B\\U01D1AA-\\U01D1AD\\U01D242-\\U01D244"\
    "\\U01DA00-\\U01DA36\\U01DA3B-\\U01DA6C\\U01DA9B-\\U01DA9F"\
    "\\U01DAA1-\\U01DAAF\\U01E130-\\U01E136\\U01E2EC-\\U01E2EF"\
    "\\U01E8D0-\\U01E8D6\\U01E944-\\U01E94A\\U0E0100-\\U0E01EF"\
    "\\u0F3E-\\u0F3F\\uABEC-\\uABED\\U01D16D-\\U01D172";

const char unicode_numeric[] =
    "\\u3007\\U010341\\U01034A\\u3007\\U010341\\U01034A\\u3007\\U010341"\
    "\\U01034A\\u3007\\U010341\\U01034A\\u3007\\U010341\\U01034A\\u00B9"\
    "\\u2070\\u2CFD\\u3007\\U010341\\U01034A\\u00B9\\u2070\\u2CFD\\u3007"\
    "\\U010341\\U01034A\\U01D7CE-\\U01D7FF\\u16EE-\\u16F0\\u2150-\\u2182"\
    "\\u2185-\\u2189\\u3021-\\u3029\\u3038-\\u303A\\uA6E6-\\uA6EF"\
    "\\U010140-\\U010178\\U0103D1-\\U0103D5\\U012400-\\U01246E"\
    "\\u0030-\\u0039\\u0660-\\u0669\\u06F0-\\u06F9\\u07C0-\\u07C9"\
    "\\u0966-\\u096F\\u09E6-\\u09EF\\u0A66-\\u0A6F\\u0AE6-\\u0AEF"\
    "\\u0B66-\\u0B6F\\u0BE6-\\u0BF2\\u0C66-\\u0C6F\\u0CE6-\\u0CEF"\
    "\\u0D66-\\u0D78\\u0DE6-\\u0DEF\\u0E50-\\u0E59\\u0ED0-\\u0ED9"\
    "\\u0F20-\\u0F33\\u1040-\\u1049\\u1090-\\u1099\\u1369-\\u137C"\
    "\\u17E0-\\u17E9\\u1810-\\u1819\\u1946-\\u194F\\u19D0-\\u19DA"\
    "\\u1A80-\\u1A89\\u1A90-\\u1A99\\u1B50-\\u1B59\\u1BB0-\\u1BB9"\
    "\\u1C40-\\u1C49\\u1C50-\\u1C59\\uA620-\\uA629\\uA8D0-\\uA8D9"\
    "\\uA900-\\uA909\\uA9D0-\\uA9D9\\uA9F0-\\uA9F9\\uAA50-\\uAA59"\
    "\\uABF0-\\uABF9\\uFF10-\\uFF19\\U0104A0-\\U0104A9\\U010D30-\\U010D39"\
    "\\U011052-\\U01106F\\U0110F0-\\U0110F9\\U011136-\\U01113F"\
    "\\U0111D0-\\U0111D9\\U0112F0-\\U0112F9\\U011450-\\U011459"\
    "\\U0114D0-\\U0114D9\\U011650-\\U011659\\U0116C0-\\U0116C9"\
    "\\U011730-\\U01173B\\U0118E0-\\U0118F2\\U011C50-\\U011C6C"\
    "\\U011D50-\\U011D59\\U011DA0-\\U011DA9\\U016A60-\\U016A69"\
    "\\U016B50-\\U016B59\\U01E140-\\U01E149\\U01E2F0-\\U01E2F9"\
    "\\U01E950-\\U01E959\\u00B2-\\u00B3\\u00BC-\\u00BE\\u09F4-\\u09F9"\
    "\\u0B72-\\u0B77\\u0C78-\\u0C7E\\u0D58-\\u0D5E\\u17F0-\\u17F9"\
    "\\u2074-\\u2079\\u2080-\\u2089\\u2460-\\u249B\\u24EA-\\u24FF"\
    "\\u2776-\\u2793\\u3192-\\u3195\\u3220-\\u3229\\u3248-\\u324F"\
    "\\u3251-\\u325F\\u3280-\\u3289\\u32B1-\\u32BF\\uA830-\\uA835"\
    "\\U010107-\\U010133\\U01018A-\\U01018B\\U0102E1-\\U0102FB"\
    "\\U010320-\\U010323\\U010858-\\U01085F\\U010879-\\U01087F"\
    "\\U0108A7-\\U0108AF\\U0108FB-\\U0108FF\\U010916-\\U01091B"\
    "\\U0109BC-\\U0109BD\\U0109C0-\\U0109CF\\U0109D2-\\U0109FF"\
    "\\U010A40-\\U010A48\\U010A7D-\\U010A7E\\U010A9D-\\U010A9F"\
    "\\U010AEB-\\U010AEF\\U010B58-\\U010B5F\\U010B78-\\U010B7F"\
    "\\U010BA9-\\U010BAF\\U010CFA-\\U010CFF\\U010E60-\\U010E7E"\
    "\\U010F1D-\\U010F26\\U010F51-\\U010F54\\U0111E1-\\U0111F4"\
    "\\U011FC0-\\U011FD4\\U016B5B-\\U016B61\\U016E80-\\U016E96"\
    "\\U01D2E0-\\U01D2F3\\U01D360-\\U01D378\\U01E8C7-\\U01E8CF"\
    "\\U01EC71-\\U01ECAB\\U01ECAD-\\U01ECAF\\U01ECB1-\\U01ECB4"\
    "\\U01ED01-\\U01ED2D\\U01ED2F-\\U01ED3D\\U01F100-\\U01F10C";

const char unicode_punctuation[] =
    "\\uFE63\\uFE68\\u0387\\u005F\\u0387\\uFF3F\\u005F\\u0387\\uFF3F\\u005F"\
    "\\u007B\\u007D\\u00A1\\u00A7\\u00AB\\u00BB\\u00BF\\u037E\\u0387\\u05BE"\
    "\\u05C0\\u05C3\\u05C6\\u061B\\u06D4\\u085E\\u0970\\u09FD\\u0A76\\u0AF0"\
    "\\u0C77\\u0C84\\u0DF4\\u0E4F\\u0F14\\u0F85\\u10FB\\u1400\\u166E\\u1CD3"\
    "\\u2D70\\u3030\\u303D\\u30A0\\u30FB\\uA673\\uA67E\\uA8FC\\uA95F\\uABEB"\
    "\\uFE63\\uFE68\\uFF3F\\uFF5B\\uFF5D\\U01039F\\U0103D0\\U01056F"\
    "\\U010857\\U01091F\\U01093F\\U010A7F\\U0111CD\\U0111DB\\U0112A9"\
    "\\U01145B\\U01145D\\U0114C6\\U01183B\\U0119E2\\U011FFF\\U016AF5"\
    "\\U016B44\\U016FE2\\U01BC9F\\u005F\\u007B\\u007D\\u00A1\\u00A7\\u00AB"\
    "\\u00BB\\u00BF\\u037E\\u0387\\u05BE\\u05C0\\u05C3\\u05C6\\u061B\\u06D4"\
    "\\u085E\\u0970\\u09FD\\u0A76\\u0AF0\\u0C77\\u0C84\\u0DF4\\u0E4F\\u0F14"\
    "\\u0F85\\u10FB\\u1400\\u166E\\u1CD3\\u2D70\\u3030\\u303D\\u30A0\\u30FB"\
    "\\uA673\\uA67E\\uA8FC\\uA95F\\uABEB\\uFE63\\uFE68\\uFF3F\\uFF5B\\uFF5D"\
    "\\U01039F\\U0103D0\\U01056F\\U010857\\U01091F\\U01093F\\U010A7F"\
    "\\U0111CD\\U0111DB\\U0112A9\\U01145B\\U01145D\\U0114C6\\U01183B"\
    "\\U0119E2\\U011FFF\\U016AF5\\U016B44\\U016FE2\\U01BC9F\\u207D-\\u207E"\
    "\\u208D-\\u208E\\u2308-\\u230B\\u27C5-\\u27C6\\u27E6-\\u27EF"\
    "\\u2983-\\u2998\\u29D8-\\u29DB\\u29FC-\\u29FD\\u2030-\\u2043"\
    "\\uFE30-\\uFE52\\u0021-\\u0023\\u0025-\\u002A\\u002C-\\u002F"\
    "\\u003A-\\u003B\\u003F-\\u0040\\u005B-\\u005D\\u00B6-\\u00B7"\
    "\\u055A-\\u055F\\u0589-\\u058A\\u05F3-\\u05F4\\u0609-\\u060A"\
    "\\u060C-\\u060D\\u061E-\\u061F\\u066A-\\u066D\\u0700-\\u070D"\
    "\\u07F7-\\u07F9\\u0830-\\u083E\\u0964-\\u0965\\u0E5A-\\u0E5B"\
    "\\u0F04-\\u0F12\\u0F3A-\\u0F3D\\u0FD0-\\u0FD4\\u0FD9-\\u0FDA"\
    "\\u104A-\\u104F\\u1360-\\u1368\\u169B-\\u169C\\u16EB-\\u16ED"\
    "\\u1735-\\u1736\\u17D4-\\u17D6\\u17D8-\\u17DA\\u1800-\\u180A"\
    "\\u1944-\\u1945\\u1A1E-\\u1A1F\\u1AA0-\\u1AA6\\u1AA8-\\u1AAD"\
    "\\u1B5A-\\u1B60\\u1BFC-\\u1BFF\\u1C3B-\\u1C3F\\u1C7E-\\u1C7F"\
    "\\u1CC0-\\u1CC7\\u2010-\\u2027\\u2045-\\u2051\\u2053-\\u205E"\
    "\\u2329-\\u232A\\u2768-\\u2775\\u2CF9-\\u2CFC\\u2CFE-\\u2CFF"\
    "\\u2E00-\\u2E2E\\u2E30-\\u2E4F\\u3001-\\u3003\\u3008-\\u3011"\
    "\\u3014-\\u301F\\uA4FE-\\uA4FF\\uA60D-\\uA60F\\uA6F2-\\uA6F7"\
    "\\uA874-\\uA877\\uA8CE-\\uA8CF\\uA8F8-\\uA8FA\\uA92E-\\uA92F"\
    "\\uA9C1-\\uA9CD\\uA9DE-\\uA9DF\\uAA5C-\\uAA5F\\uAADE-\\uAADF"\
    "\\uAAF0-\\uAAF1\\uFD3E-\\uFD3F\\uFE10-\\uFE19\\uFE54-\\uFE61"\
    "\\uFE6A-\\uFE6B\\uFF01-\\uFF03\\uFF05-\\uFF0A\\uFF0C-\\uFF0F"\
    "\\uFF1A-\\uFF1B\\uFF1F-\\uFF20\\uFF3B-\\uFF3D\\uFF5F-\\uFF65"\
    "\\U010100-\\U010102\\U010A50-\\U010A58\\U010AF0-\\U010AF6"\
    "\\U010B39-\\U010B3F\\U010B99-\\U010B9C\\U010F55-\\U010F59"\
    "\\U011047-\\U01104D\\U0110BB-\\U0110BC\\U0110BE-\\U0110C1"\
    "\\U011140-\\U011143\\U011174-\\U011175\\U0111C5-\\U0111C8"\
    "\\U0111DD-\\U0111DF\\U011238-\\U01123D\\U01144B-\\U01144F"\
    "\\U0115C1-\\U0115D7\\U011641-\\U011643\\U011660-\\U01166C"\
    "\\U01173C-\\U01173E\\U011A3F-\\U011A46\\U011A9A-\\U011A9C"\
    "\\U011A9E-\\U011AA2\\U011C41-\\U011C45\\U011C70-\\U011C71"\
    "\\U011EF7-\\U011EF8\\U012470-\\U012474\\U016A6E-\\U016A6F"\
    "\\U016B37-\\U016B3B\\U016E97-\\U016E9A\\U01DA87-\\U01DA8B"\
    "\\U01E95E-\\U01E95F";

const char unicode_whitespace[] =
    "\\u0020\\u00A0\\u1680\\u202F\\u205F\\u3000\\u0020\\u00A0\\u1680\\u202F"\
    "\\u205F\\u3000\\u2000-\\u200A\\u2028-\\u2029";

const char unicode_letter[] =
    "\\u2102\\u2107\\u2115\\u2124\\u2128\\U01D4A2\\U01D4BB\\U01D546"\
    "\\U01EE24\\U01EE27\\U01EE39\\U01EE3B\\U01EE42\\U01EE47\\U01EE49"\
    "\\U01EE4B\\U01EE54\\U01EE57\\U01EE59\\U01EE5B\\U01EE5D\\U01EE5F"\
    "\\U01EE64\\U01EE7E\\u00AA\\u00B5\\u00BA\\u02EC\\u02EE\\u037F\\u0386"\
    "\\u038C\\u0559\\u06D5\\u06FF\\u0710\\u07B1\\u07FA\\u081A\\u0824\\u0828"\
    "\\u093D\\u0950\\u09B2\\u09BD\\u09CE\\u09FC\\u0A5E\\u0ABD\\u0AD0\\u0AF9"\
    "\\u0B3D\\u0B71\\u0B83\\u0B9C\\u0BD0\\u0C3D\\u0C80\\u0CBD\\u0CDE\\u0D3D"\
    "\\u0D4E\\u0DBD\\u0E84\\u0EA5\\u0EBD\\u0EC6\\u0F00\\u103F\\u1061\\u108E"\
    "\\u10C7\\u10CD\\u1258\\u12C0\\u17D7\\u17DC\\u18AA\\u1AA7\\u1CFA\\u1F59"\
    "\\u1F5B\\u1F5D\\u1FBE\\u2071\\u207F\\u2102\\u2107\\u2115\\u2124\\u2126"\
    "\\u2128\\u214E\\u2D27\\u2D2D\\u2D6F\\u2E2F\\uA8FB\\uA9CF\\uAA7A\\uAAB1"\
    "\\uAAC0\\uAAC2\\uFB1D\\uFB3E\\U010808\\U01083C\\U010A00\\U010F27"\
    "\\U011144\\U011176\\U0111DA\\U0111DC\\U011288\\U01133D\\U011350"\
    "\\U01145F\\U0114C7\\U011644\\U0116B8\\U0118FF\\U0119E1\\U0119E3"\
    "\\U011A00\\U011A3A\\U011A50\\U011A9D\\U011C40\\U011D46\\U011D98"\
    "\\U016F50\\U016FE3\\U01D4A2\\U01D4BB\\U01D546\\U01E14E\\U01E94B"\
    "\\U01EE24\\U01EE27\\U01EE39\\U01EE3B\\U01EE42\\U01EE47\\U01EE49"\
    "\\U01EE4B\\U01EE54\\U01EE57\\U01EE59\\U01EE5B\\U01EE5D\\U01EE5F"\
    "\\U01EE64\\U01EE7E\\u00AA\\u00B5\\u00BA\\u1FBE\\u2071\\u207F\\u214E"\
    "\\u2D27\\u2D2D\\U01D4BB\\u037F\\u0386\\u038C\\u10C7\\u10CD\\u1F59"\
    "\\u1F5B\\u1F5D\\u2102\\u2107\\u2115\\u2124\\u2126\\u2128\\U01D4A2"\
    "\\U01D546\\u00AA\\u00B5\\u00BA\\u037F\\u0386\\u038C\\u10C7\\u10CD"\
    "\\u1F59\\u1F5B\\u1F5D\\u1FBE\\u2071\\u207F\\u2102\\u2107\\u2115\\u2124"\
    "\\u2126\\u2128\\u214E\\u2D27\\u2D2D\\U01D4A2\\U01D4BB\\U01D546\\u02EC"\
    "\\u02EE\\u0559\\u07FA\\u081A\\u0824\\u0828\\u0EC6\\u17D7\\u1AA7\\u2071"\
    "\\u207F\\u2D6F\\u2E2F\\uA9CF\\U016FE3\\U01E94B\\u037F\\u0386\\u038C"\
    "\\u10C7\\u10CD\\u1F59\\u1F5B\\u1F5D\\u2126\\u00B5\\u1FBE\\u214E\\u2D27"\
    "\\u2D2D\\u00B5\\u1FBE\\u214E\\u2D27\\u2D2D\\u00B5\\u037F\\u0386\\u038C"\
    "\\u10C7\\u10CD\\u1F59\\u1F5B\\u1F5D\\u2126\\u00B5\\u037F\\u0386\\u038C"\
    "\\u10C7\\u10CD\\u1F59\\u1F5B\\u1F5D\\u1FBE\\u2126\\u214E\\u2D27\\u2D2D"\
    "\\u00AA\\u00B5\\u00BA\\u02EC\\u02EE\\u037F\\u0386\\u038C\\u0559\\u06D5"\
    "\\u06FF\\u0710\\u07B1\\u07FA\\u081A\\u0824\\u0828\\u093D\\u0950\\u09B2"\
    "\\u09BD\\u09CE\\u09FC\\u0A5E\\u0ABD\\u0AD0\\u0AF9\\u0B3D\\u0B71\\u0B83"\
    "\\u0B9C\\u0BD0\\u0C3D\\u0C80\\u0CBD\\u0CDE\\u0D3D\\u0D4E\\u0DBD\\u0E84"\
    "\\u0EA5\\u0EBD\\u0EC6\\u0F00\\u103F\\u1061\\u108E\\u10C7\\u10CD\\u1258"\
    "\\u12C0\\u17D7\\u17DC\\u18AA\\u1AA7\\u1CFA\\u1F59\\u1F5B\\u1F5D\\u1FBE"\
    "\\u2071\\u207F\\u2102\\u2107\\u2115\\u2124\\u2126\\u2128\\u214E\\u2D27"\
    "\\u2D2D\\u2D6F\\uA8FB\\uA9CF\\uAA7A\\uAAB1\\uAAC0\\uAAC2\\uFB1D\\uFB3E"\
    "\\U010808\\U01083C\\U010A00\\U010F27\\U011144\\U011176\\U0111DA"\
    "\\U0111DC\\U011288\\U01133D\\U011350\\U01145F\\U0114C7\\U011644"\
    "\\U0116B8\\U0118FF\\U0119E1\\U0119E3\\U011A00\\U011A3A\\U011A50"\
    "\\U011A9D\\U011C40\\U011D46\\U011D98\\U016F50\\U016FE3\\U01D4A2"\
    "\\U01D4BB\\U01D546\\U01E14E\\U01E94B\\U01EE24\\U01EE27\\U01EE39"\
    "\\U01EE3B\\U01EE42\\U01EE47\\U01EE49\\U01EE4B\\U01EE54\\U01EE57"\
    "\\U01EE59\\U01EE5B\\U01EE5D\\U01EE5F\\U01EE64\\U01EE7E\\u00AA\\u00B5"\
    "\\u00BA\\u02EC\\u02EE\\u037F\\u0386\\u038C\\u0559\\u06D5\\u06FF\\u0710"\
    "\\u07B1\\u07FA\\u081A\\u0824\\u0828\\u093D\\u0950\\u09B2\\u09BD\\u09CE"\
    "\\u09FC\\u0A5E\\u0ABD\\u0AD0\\u0AF9\\u0B3D\\u0B71\\u0B83\\u0B9C\\u0BD0"\
    "\\u0C3D\\u0C80\\u0CBD\\u0CDE\\u0D3D\\u0D4E\\u0DBD\\u0E84\\u0EA5\\u0EBD"\
    "\\u0EC6\\u0F00\\u103F\\u1061\\u108E\\u10C7\\u10CD\\u1258\\u12C0\\u17D7"\
    "\\u17DC\\u18AA\\u1AA7\\u1CFA\\u1F59\\u1F5B\\u1F5D\\u1FBE\\u2071\\u207F"\
    "\\u2102\\u2107\\u2115\\u2124\\u2126\\u2128\\u214E\\u2D27\\u2D2D\\u2D6F"\
    "\\uA8FB\\uA9CF\\uAA7A\\uAAB1\\uAAC0\\uAAC2\\uFB1D\\uFB3E\\U010808"\
    "\\U01083C\\U010A00\\U010F27\\U011144\\U011176\\U0111DA\\U0111DC"\
    "\\U011288\\U01133D\\U011350\\U01145F\\U0114C7\\U011644\\U0116B8"\
    "\\U0118FF\\U0119E1\\U0119E3\\U011A00\\U011A3A\\U011A50\\U011A9D"\
    "\\U011C40\\U011D46\\U011D98\\U016F50\\U016FE3\\U01D4A2\\U01D4BB"\
    "\\U01D546\\U01E14E\\U01E94B\\U01EE24\\U01EE27\\U01EE39\\U01EE3B"\
    "\\U01EE42\\U01EE47\\U01EE49\\U01EE4B\\U01EE54\\U01EE57\\U01EE59"\
    "\\U01EE5B\\U01EE5D\\U01EE5F\\U01EE64\\U01EE7E\\u00AA\\u00B5\\u00BA"\
    "\\u02EC\\u02EE\\u037F\\u0386\\u038C\\u0559\\u06D5\\u06FF\\u0710\\u07B1"\
    "\\u07FA\\u081A\\u0824\\u0828\\u093D\\u0950\\u09B2\\u09BD\\u09CE\\u09FC"\
    "\\u0A5E\\u0ABD\\u0AD0\\u0AF9\\u0B3D\\u0B71\\u0B83\\u0B9C\\u0BD0\\u0C3D"\
    "\\u0C80\\u0CBD\\u0CDE\\u0D3D\\u0D4E\\u0DBD\\u0E84\\u0EA5\\u0EBD\\u0EC6"\
    "\\u0F00\\u103F\\u1061\\u108E\\u10C7\\u10CD\\u1258\\u12C0\\u17D7\\u17DC"\
    "\\u18AA\\u1AA7\\u1CFA\\u1F59\\u1F5B\\u1F5D\\u1FBE\\u2071\\u207F\\u2102"\
    "\\u2107\\u2115\\u2124\\u2126\\u2128\\u214E\\u2D27\\u2D2D\\u2D6F\\uA8FB"\
    "\\uA9CF\\uAA7A\\uAAB1\\uAAC0\\uAAC2\\uFB1D\\uFB3E\\U010808\\U01083C"\
    "\\U010A00\\U010F27\\U011144\\U011176\\U0111DA\\U0111DC\\U011288"\
    "\\U01133D\\U011350\\U01145F\\U0114C7\\U011644\\U0116B8\\U0118FF"\
    "\\U0119E1\\U0119E3\\U011A00\\U011A3A\\U011A50\\U011A9D\\U011C40"\
    "\\U011D46\\U011D98\\U016F50\\U016FE3\\U01D4A2\\U01D4BB\\U01D546"\
    "\\U01E14E\\U01E94B\\U01EE24\\U01EE27\\U01EE39\\U01EE3B\\U01EE42"\
    "\\U01EE47\\U01EE49\\U01EE4B\\U01EE54\\U01EE57\\U01EE59\\U01EE5B"\
    "\\U01EE5D\\U01EE5F\\U01EE64\\U01EE7E\\u00AA\\u00B5\\u00BA\\u02EC"\
    "\\u02EE\\u037F\\u0386\\u038C\\u0559\\u06D5\\u06FF\\u0710\\u07B1\\u07FA"\
    "\\u081A\\u0824\\u0828\\u093D\\u0950\\u09B2\\u09BD\\u09CE\\u09FC\\u0A5E"\
    "\\u0ABD\\u0AD0\\u0AF9\\u0B3D\\u0B71\\u0B83\\u0B9C\\u0BD0\\u0C3D\\u0C80"\
    "\\u0CBD\\u0CDE\\u0D3D\\u0D4E\\u0DBD\\u0E84\\u0EA5\\u0EBD\\u0EC6\\u0F00"\
    "\\u103F\\u1061\\u108E\\u10C7\\u10CD\\u1258\\u12C0\\u17D7\\u17DC\\u18AA"\
    "\\u1AA7\\u1CFA\\u1F59\\u1F5B\\u1F5D\\u1FBE\\u2071\\u207F\\u2102\\u2107"\
    "\\u2115\\u2124\\u2126\\u2128\\u214E\\u2D27\\u2D2D\\u2D6F\\uA8FB\\uA9CF"\
    "\\uAA7A\\uAAB1\\uAAC0\\uAAC2\\uFB1D\\uFB3E\\U010808\\U01083C\\U010A00"\
    "\\U010F27\\U011144\\U011176\\U0111DA\\U0111DC\\U011288\\U01133D"\
    "\\U011350\\U01145F\\U0114C7\\U011644\\U0116B8\\U0118FF\\U0119E1"\
    "\\U0119E3\\U011A00\\U011A3A\\U011A50\\U011A9D\\U011C40\\U011D46"\
    "\\U011D98\\U016F50\\U016FE3\\U01D4A2\\U01D4BB\\U01D546\\U01E14E"\
    "\\U01E94B\\U01EE24\\U01EE27\\U01EE39\\U01EE3B\\U01EE42\\U01EE47"\
    "\\U01EE49\\U01EE4B\\U01EE54\\U01EE57\\U01EE59\\U01EE5B\\U01EE5D"\
    "\\U01EE5F\\U01EE64\\U01EE7E\\u00AA\\u00B5\\u00BA\\u02EC\\u02EE\\u037F"\
    "\\u0386\\u038C\\u0559\\u06D5\\u06FF\\u0710\\u07B1\\u07FA\\u081A\\u0824"\
    "\\u0828\\u093D\\u0950\\u09B2\\u09BD\\u09CE\\u09FC\\u0A5E\\u0ABD\\u0AD0"\
    "\\u0AF9\\u0B3D\\u0B71\\u0B83\\u0B9C\\u0BD0\\u0C3D\\u0C80\\u0CBD\\u0CDE"\
    "\\u0D3D\\u0D4E\\u0DBD\\u0E84\\u0EA5\\u0EBD\\u0EC6\\u0F00\\u103F\\u1061"\
    "\\u108E\\u10C7\\u10CD\\u1258\\u12C0\\u17D7\\u17DC\\u18AA\\u1AA7\\u1CFA"\
    "\\u1F59\\u1F5B\\u1F5D\\u1FBE\\u2071\\u207F\\u2102\\u2107\\u2115\\u2124"\
    "\\u2126\\u2128\\u214E\\u2D27\\u2D2D\\u2D6F\\u2E2F\\uA8FB\\uA9CF\\uAA7A"\
    "\\uAAB1\\uAAC0\\uAAC2\\uFB1D\\uFB3E\\U010808\\U01083C\\U010A00"\
    "\\U010F27\\U011144\\U011176\\U0111DA\\U0111DC\\U011288\\U01133D"\
    "\\U011350\\U01145F\\U0114C7\\U011644\\U0116B8\\U0118FF\\U0119E1"\
    "\\U0119E3\\U011A00\\U011A3A\\U011A50\\U011A9D\\U011C40\\U011D46"\
    "\\U011D98\\U016F50\\U016FE3\\U01D4A2\\U01D4BB\\U01D546\\U01E14E"\
    "\\U01E94B\\U01EE24\\U01EE27\\U01EE39\\U01EE3B\\U01EE42\\U01EE47"\
    "\\U01EE49\\U01EE4B\\U01EE54\\U01EE57\\U01EE59\\U01EE5B\\U01EE5D"\
    "\\U01EE5F\\U01EE64\\U01EE7E\\u00AA\\u00B5\\u00BA\\u02EC\\u02EE\\u037F"\
    "\\u0386\\u038C\\u0559\\u06D5\\u06FF\\u0710\\u07B1\\u07FA\\u081A\\u0824"\
    "\\u0828\\u093D\\u0950\\u09B2\\u09BD\\u09CE\\u09FC\\u0A5E\\u0ABD\\u0AD0"\
    "\\u0AF9\\u0B3D\\u0B71\\u0B83\\u0B9C\\u0BD0\\u0C3D\\u0C80\\u0CBD\\u0CDE"\
    "\\u0D3D\\u0D4E\\u0DBD\\u0E84\\u0EA5\\u0EBD\\u0EC6\\u0F00\\u103F\\u1061"\
    "\\u108E\\u10C7\\u10CD\\u1258\\u12C0\\u17D7\\u17DC\\u18AA\\u1AA7\\u1CFA"\
    "\\u1F59\\u1F5B\\u1F5D\\u1FBE\\u2071\\u207F\\u2102\\u2107\\u2115\\u2124"\
    "\\u2126\\u2128\\u214E\\u2D27\\u2D2D\\u2D6F\\u2E2F\\uA8FB\\uA9CF\\uAA7A"\
    "\\uAAB1\\uAAC0\\uAAC2\\uFB1D\\uFB3E\\U010808\\U01083C\\U010A00"\
    "\\U010F27\\U011144\\U011176\\U0111DA\\U0111DC\\U011288\\U01133D"\
    "\\U011350\\U01145F\\U0114C7\\U011644\\U0116B8\\U0118FF\\U0119E1"\
    "\\U0119E3\\U011A00\\U011A3A\\U011A50\\U011A9D\\U011C40\\U011D46"\
    "\\U011D98\\U016F50\\U016FE3\\U01D4A2\\U01D4BB\\U01D546\\U01E14E"\
    "\\U01E94B\\U01EE24\\U01EE27\\U01EE39\\U01EE3B\\U01EE42\\U01EE47"\
    "\\U01EE49\\U01EE4B\\U01EE54\\U01EE57\\U01EE59\\U01EE5B\\U01EE5D"\
    "\\U01EE5F\\U01EE64\\U01EE7E\\u03A3-\\u03F5\\u210A-\\u2113"\
    "\\u2119-\\u211D\\u212A-\\u212D\\u212F-\\u2139\\u213C-\\u213F"\
    "\\u2145-\\u2149\\U01D400-\\U01D454\\U01D456-\\U01D49C"\
    "\\U01D49E-\\U01D49F\\U01D4A5-\\U01D4A6\\U01D4A9-\\U01D4AC"\
    "\\U01D4AE-\\U01D4B9\\U01D4BD-\\U01D4C3\\U01D4C5-\\U01D505"\
    "\\U01D507-\\U01D50A\\U01D50D-\\U01D514\\U01D516-\\U01D51C"\
    "\\U01D51E-\\U01D539\\U01D53B-\\U01D53E\\U01D540-\\U01D544"\
    "\\U01D54A-\\U01D550\\U01D552-\\U01D6A5\\U01D6A8-\\U01D6C0"\
    "\\U01D6C2-\\U01D6DA\\U01D6DC-\\U01D6FA\\U01D6FC-\\U01D714"\
    "\\U01D716-\\U01D734\\U01D736-\\U01D74E\\U01D750-\\U01D76E"\
    "\\U01D770-\\U01D788\\U01D78A-\\U01D7A8\\U01D7AA-\\U01D7C2"\
    "\\U01D7C4-\\U01D7CB\\U01EE00-\\U01EE03\\U01EE05-\\U01EE1F"\
    "\\U01EE21-\\U01EE22\\U01EE29-\\U01EE32\\U01EE34-\\U01EE37"\
    "\\U01EE4D-\\U01EE4F\\U01EE51-\\U01EE52\\U01EE61-\\U01EE62"\
    "\\U01EE67-\\U01EE6A\\U01EE6C-\\U01EE72\\U01EE74-\\U01EE77"\
    "\\U01EE79-\\U01EE7C\\U01EE80-\\U01EE89\\U01EE8B-\\U01EE9B"\
    "\\U01EEA1-\\U01EEA3\\U01EEA5-\\U01EEA9\\U01EEAB-\\U01EEBB"\
    "\\u0041-\\u005A\\u0061-\\u007A\\u00C0-\\u00D6\\u00D8-\\u00F6"\
    "\\u00F8-\\u02C1\\u02C6-\\u02D1\\u02E0-\\u02E4\\u0370-\\u0374"\
    "\\u0376-\\u0377\\u037A-\\u037D\\u0388-\\u038A\\u038E-\\u03A1"\
    "\\u03F7-\\u0481\\u048A-\\u052F\\u0531-\\u0556\\u0560-\\u0588"\
    "\\u05D0-\\u05EA\\u05EF-\\u05F2\\u0620-\\u064A\\u066E-\\u066F"\
    "\\u0671-\\u06D3\\u06E5-\\u06E6\\u06EE-\\u06EF\\u06FA-\\u06FC"\
    "\\u0712-\\u072F\\u074D-\\u07A5\\u07CA-\\u07EA\\u07F4-\\u07F5"\
    "\\u0800-\\u0815\\u0840-\\u0858\\u0860-\\u086A\\u08A0-\\u08B4"\
    "\\u08B6-\\u08BD\\u0904-\\u0939\\u0958-\\u0961\\u0971-\\u0980"\
    "\\u0985-\\u098C\\u098F-\\u0990\\u0993-\\u09A8\\u09AA-\\u09B0"\
    "\\u09B6-\\u09B9\\u09DC-\\u09DD\\u09DF-\\u09E1\\u09F0-\\u09F1"\
    "\\u0A05-\\u0A0A\\u0A0F-\\u0A10\\u0A13-\\u0A28\\u0A2A-\\u0A30"\
    "\\u0A32-\\u0A33\\u0A35-\\u0A36\\u0A38-\\u0A39\\u0A59-\\u0A5C"\
    "\\u0A72-\\u0A74\\u0A85-\\u0A8D\\u0A8F-\\u0A91\\u0A93-\\u0AA8"\
    "\\u0AAA-\\u0AB0\\u0AB2-\\u0AB3\\u0AB5-\\u0AB9\\u0AE0-\\u0AE1"\
    "\\u0B05-\\u0B0C\\u0B0F-\\u0B10\\u0B13-\\u0B28\\u0B2A-\\u0B30"\
    "\\u0B32-\\u0B33\\u0B35-\\u0B39\\u0B5C-\\u0B5D\\u0B5F-\\u0B61"\
    "\\u0B85-\\u0B8A\\u0B8E-\\u0B90\\u0B92-\\u0B95\\u0B99-\\u0B9A"\
    "\\u0B9E-\\u0B9F\\u0BA3-\\u0BA4\\u0BA8-\\u0BAA\\u0BAE-\\u0BB9"\
    "\\u0C05-\\u0C0C\\u0C0E-\\u0C10\\u0C12-\\u0C28\\u0C2A-\\u0C39"\
    "\\u0C58-\\u0C5A\\u0C60-\\u0C61\\u0C85-\\u0C8C\\u0C8E-\\u0C90"\
    "\\u0C92-\\u0CA8\\u0CAA-\\u0CB3\\u0CB5-\\u0CB9\\u0CE0-\\u0CE1"\
    "\\u0CF1-\\u0CF2\\u0D05-\\u0D0C\\u0D0E-\\u0D10\\u0D12-\\u0D3A"\
    "\\u0D54-\\u0D56\\u0D5F-\\u0D61\\u0D7A-\\u0D7F\\u0D85-\\u0D96"\
    "\\u0D9A-\\u0DB1\\u0DB3-\\u0DBB\\u0DC0-\\u0DC6\\u0E01-\\u0E30"\
    "\\u0E32-\\u0E33\\u0E40-\\u0E46\\u0E81-\\u0E82\\u0E86-\\u0E8A"\
    "\\u0E8C-\\u0EA3\\u0EA7-\\u0EB0\\u0EB2-\\u0EB3\\u0EC0-\\u0EC4"\
    "\\u0EDC-\\u0EDF\\u0F40-\\u0F47\\u0F49-\\u0F6C\\u0F88-\\u0F8C"\
    "\\u1000-\\u102A\\u1050-\\u1055\\u105A-\\u105D\\u1065-\\u1066"\
    "\\u106E-\\u1070\\u1075-\\u1081\\u10A0-\\u10C5\\u10D0-\\u10FA"\
    "\\u10FC-\\u1248\\u124A-\\u124D\\u1250-\\u1256\\u125A-\\u125D"\
    "\\u1260-\\u1288\\u128A-\\u128D\\u1290-\\u12B0\\u12B2-\\u12B5"\
    "\\u12B8-\\u12BE\\u12C2-\\u12C5\\u12C8-\\u12D6\\u12D8-\\u1310"\
    "\\u1312-\\u1315\\u1318-\\u135A\\u1380-\\u138F\\u13A0-\\u13F5"\
    "\\u13F8-\\u13FD\\u1401-\\u166C\\u166F-\\u167F\\u1681-\\u169A"\
    "\\u16A0-\\u16EA\\u16F1-\\u16F8\\u1700-\\u170C\\u170E-\\u1711"\
    "\\u1720-\\u1731\\u1740-\\u1751\\u1760-\\u176C\\u176E-\\u1770"\
    "\\u1780-\\u17B3\\u1820-\\u1878\\u1880-\\u1884\\u1887-\\u18A8"\
    "\\u18B0-\\u18F5\\u1900-\\u191E\\u1950-\\u196D\\u1970-\\u1974"\
    "\\u1980-\\u19AB\\u19B0-\\u19C9\\u1A00-\\u1A16\\u1A20-\\u1A54"\
    "\\u1B05-\\u1B33\\u1B45-\\u1B4B\\u1B83-\\u1BA0\\u1BAE-\\u1BAF"\
    "\\u1BBA-\\u1BE5\\u1C00-\\u1C23\\u1C4D-\\u1C4F\\u1C5A-\\u1C7D"\
    "\\u1C80-\\u1C88\\u1C90-\\u1CBA\\u1CBD-\\u1CBF\\u1CE9-\\u1CEC"\
    "\\u1CEE-\\u1CF3\\u1CF5-\\u1CF6\\u1D00-\\u1DBF\\u1E00-\\u1F15"\
    "\\u1F18-\\u1F1D\\u1F20-\\u1F45\\u1F48-\\u1F4D\\u1F50-\\u1F57"\
    "\\u1F5F-\\u1F7D\\u1F80-\\u1FB4\\u1FB6-\\u1FBC\\u1FC2-\\u1FC4"\
    "\\u1FC6-\\u1FCC\\u1FD0-\\u1FD3\\u1FD6-\\u1FDB\\u1FE0-\\u1FEC"\
    "\\u1FF2-\\u1FF4\\u1FF6-\\u1FFC\\u2090-\\u209C\\u2183-\\u2184"\
    "\\u2C00-\\u2C2E\\u2C30-\\u2C5E\\u2C60-\\u2CE4\\u2CEB-\\u2CEE"\
    "\\u2CF2-\\u2CF3\\u2D00-\\u2D25\\u2D30-\\u2D67\\u2D80-\\u2D96"\
    "\\u2DA0-\\u2DA6\\u2DA8-\\u2DAE\\u2DB0-\\u2DB6\\u2DB8-\\u2DBE"\
    "\\u2DC0-\\u2DC6\\u2DC8-\\u2DCE\\u2DD0-\\u2DD6\\u2DD8-\\u2DDE"\
    "\\u3005-\\u3006\\u3031-\\u3035\\u303B-\\u303C\\u3041-\\u3096"\
    "\\u309D-\\u309F\\u30A1-\\u30FA\\u30FC-\\u30FF\\u3105-\\u312F"\
    "\\u3131-\\u318E\\u31A0-\\u31BA\\u31F0-\\u31FF\\u3400-\\u4DB5"\
    "\\u4E00-\\u9FEF\\uA000-\\uA48C\\uA4D0-\\uA4FD\\uA500-\\uA60C"\
    "\\uA610-\\uA61F\\uA62A-\\uA62B\\uA640-\\uA66E\\uA67F-\\uA69D"\
    "\\uA6A0-\\uA6E5\\uA717-\\uA71F\\uA722-\\uA788\\uA78B-\\uA7BF"\
    "\\uA7C2-\\uA7C6\\uA7F7-\\uA801\\uA803-\\uA805\\uA807-\\uA80A"\
    "\\uA80C-\\uA822\\uA840-\\uA873\\uA882-\\uA8B3\\uA8F2-\\uA8F7"\
    "\\uA8FD-\\uA8FE\\uA90A-\\uA925\\uA930-\\uA946\\uA960-\\uA97C"\
    "\\uA984-\\uA9B2\\uA9E0-\\uA9E4\\uA9E6-\\uA9EF\\uA9FA-\\uA9FE"\
    "\\uAA00-\\uAA28\\uAA40-\\uAA42\\uAA44-\\uAA4B\\uAA60-\\uAA76"\
    "\\uAA7E-\\uAAAF\\uAAB5-\\uAAB6\\uAAB9-\\uAABD\\uAADB-\\uAADD"\
    "\\uAAE0-\\uAAEA\\uAAF2-\\uAAF4\\uAB01-\\uAB06\\uAB09-\\uAB0E"\
    "\\uAB11-\\uAB16\\uAB20-\\uAB26\\uAB28-\\uAB2E\\uAB30-\\uAB5A"\
    "\\uAB5C-\\uAB67\\uAB70-\\uABE2\\uAC00-\\uD7A3\\uD7B0-\\uD7C6"\
    "\\uD7CB-\\uD7FB\\uF900-\\uFA6D\\uFA70-\\uFAD9\\uFB00-\\uFB06"\
    "\\uFB13-\\uFB17\\uFB1F-\\uFB28\\uFB2A-\\uFB36\\uFB38-\\uFB3C"\
    "\\uFB40-\\uFB41\\uFB43-\\uFB44\\uFB46-\\uFBB1\\uFBD3-\\uFD3D"\
    "\\uFD50-\\uFD8F\\uFD92-\\uFDC7\\uFDF0-\\uFDFB\\uFE70-\\uFE74"\
    "\\uFE76-\\uFEFC\\uFF21-\\uFF3A\\uFF41-\\uFF5A\\uFF66-\\uFFBE"\
    "\\uFFC2-\\uFFC7\\uFFCA-\\uFFCF\\uFFD2-\\uFFD7\\uFFDA-\\uFFDC"\
    "\\U010000-\\U01000B\\U01000D-\\U010026\\U010028-\\U01003A"\
    "\\U01003C-\\U01003D\\U01003F-\\U01004D\\U010050-\\U01005D"\
    "\\U010080-\\U0100FA\\U010280-\\U01029C\\U0102A0-\\U0102D0"\
    "\\U010300-\\U01031F\\U01032D-\\U010340\\U010342-\\U010349"\
    "\\U010350-\\U010375\\U010380-\\U01039D\\U0103A0-\\U0103C3"\
    "\\U0103C8-\\U0103CF\\U010400-\\U01049D\\U0104B0-\\U0104D3"\
    "\\U0104D8-\\U0104FB\\U010500-\\U010527\\U010530-\\U010563"\
    "\\U010600-\\U010736\\U010740-\\U010755\\U010760-\\U010767"\
    "\\U010800-\\U010805\\U01080A-\\U010835\\U010837-\\U010838"\
    "\\U01083F-\\U010855\\U010860-\\U010876\\U010880-\\U01089E"\
    "\\U0108E0-\\U0108F2\\U0108F4-\\U0108F5\\U010900-\\U010915"\
    "\\U010920-\\U010939\\U010980-\\U0109B7\\U0109BE-\\U0109BF"\
    "\\U010A10-\\U010A13\\U010A15-\\U010A17\\U010A19-\\U010A35"\
    "\\U010A60-\\U010A7C\\U010A80-\\U010A9C\\U010AC0-\\U010AC7"\
    "\\U010AC9-\\U010AE4\\U010B00-\\U010B35\\U010B40-\\U010B55"\
    "\\U010B60-\\U010B72\\U010B80-\\U010B91\\U010C00-\\U010C48"\
    "\\U010C80-\\U010CB2\\U010CC0-\\U010CF2\\U010D00-\\U010D23"\
    "\\U010F00-\\U010F1C\\U010F30-\\U010F45\\U010FE0-\\U010FF6"\
    "\\U011003-\\U011037\\U011083-\\U0110AF\\U0110D0-\\U0110E8"\
    "\\U011103-\\U011126\\U011150-\\U011172\\U011183-\\U0111B2"\
    "\\U0111C1-\\U0111C4\\U011200-\\U011211\\U011213-\\U01122B"\
    "\\U011280-\\U011286\\U01128A-\\U01128D\\U01128F-\\U01129D"\
    "\\U01129F-\\U0112A8\\U0112B0-\\U0112DE\\U011305-\\U01130C"\
    "\\U01130F-\\U011310\\U011313-\\U011328\\U01132A-\\U011330"\
    "\\U011332-\\U011333\\U011335-\\U011339\\U01135D-\\U011361"\
    "\\U011400-\\U011434\\U011447-\\U01144A\\U011480-\\U0114AF"\
    "\\U0114C4-\\U0114C5\\U011580-\\U0115AE\\U0115D8-\\U0115DB"\
    "\\U011600-\\U01162F\\U011680-\\U0116AA\\U011700-\\U01171A"\
    "\\U011800-\\U01182B\\U0118A0-\\U0118DF\\U0119A0-\\U0119A7"\
    "\\U0119AA-\\U0119D0\\U011A0B-\\U011A32\\U011A5C-\\U011A89"\
    "\\U011AC0-\\U011AF8\\U011C00-\\U011C08\\U011C0A-\\U011C2E"\
    "\\U011C72-\\U011C8F\\U011D00-\\U011D06\\U011D08-\\U011D09"\
    "\\U011D0B-\\U011D30\\U011D60-\\U011D65\\U011D67-\\U011D68"\
    "\\U011D6A-\\U011D89\\U011EE0-\\U011EF2\\U012000-\\U012399"\
    "\\U012480-\\U012543\\U013000-\\U01342E\\U014400-\\U014646"\
    "\\U016800-\\U016A38\\U016A40-\\U016A5E\\U016AD0-\\U016AED"\
    "\\U016B00-\\U016B2F\\U016B40-\\U016B43\\U016B63-\\U016B77"\
    "\\U016B7D-\\U016B8F\\U016E40-\\U016E7F\\U016F00-\\U016F4A"\
    "\\U016F93-\\U016F9F\\U016FE0-\\U016FE1\\U017000-\\U0187F7"\
    "\\U018800-\\U018AF2\\U01B000-\\U01B11E\\U01B150-\\U01B152"\
    "\\U01B164-\\U01B167\\U01B170-\\U01B2FB\\U01BC00-\\U01BC6A"\
    "\\U01BC70-\\U01BC7C\\U01BC80-\\U01BC88\\U01BC90-\\U01BC99"\
    "\\U01E100-\\U01E12C\\U01E137-\\U01E13D\\U01E2C0-\\U01E2EB"\
    "\\U01E800-\\U01E8C4\\U01E900-\\U01E943\\U020000-\\U02A6D6"\
    "\\U02A700-\\U02B734\\U02B740-\\U02B81D\\U02B820-\\U02CEA1"\
    "\\U02CEB0-\\U02EBE0\\U02F800-\\U02FA1D";

const char unicode_uppercase[] =
    "\\u0100\\u0102\\u0104\\u0106\\u0108\\u010A\\u010C\\u010E\\u0110\\u0112"\
    "\\u0114\\u0116\\u0118\\u011A\\u011C\\u011E\\u0120\\u0122\\u0124\\u0126"\
    "\\u0128\\u012A\\u012C\\u012E\\u0130\\u0132\\u0134\\u0136\\u0139\\u013B"\
    "\\u013D\\u013F\\u0141\\u0143\\u0145\\u0147\\u014A\\u014C\\u014E\\u0150"\
    "\\u0152\\u0154\\u0156\\u0158\\u015A\\u015C\\u015E\\u0160\\u0162\\u0164"\
    "\\u0166\\u0168\\u016A\\u016C\\u016E\\u0170\\u0172\\u0174\\u0176\\u017B"\
    "\\u017D\\u0184\\u01A2\\u01A4\\u01A9\\u01AC\\u01B5\\u01BC\\u01C4\\u01C7"\
    "\\u01CA\\u01CD\\u01CF\\u01D1\\u01D3\\u01D5\\u01D7\\u01D9\\u01DB\\u01DE"\
    "\\u01E0\\u01E2\\u01E4\\u01E6\\u01E8\\u01EA\\u01EC\\u01EE\\u01F1\\u01F4"\
    "\\u01FA\\u01FC\\u01FE\\u0200\\u0202\\u0204\\u0206\\u0208\\u020A\\u020C"\
    "\\u020E\\u0210\\u0212\\u0214\\u0216\\u0218\\u021A\\u021C\\u021E\\u0220"\
    "\\u0222\\u0224\\u0226\\u0228\\u022A\\u022C\\u022E\\u0230\\u0232\\u0241"\
    "\\u0248\\u024A\\u024C\\u024E\\u0370\\u0372\\u0376\\u037F\\u0386\\u038C"\
    "\\u03CF\\u03D8\\u03DA\\u03DC\\u03DE\\u03E0\\u03E2\\u03E4\\u03E6\\u03E8"\
    "\\u03EA\\u03EC\\u03EE\\u03F4\\u03F7\\u0460\\u0462\\u0464\\u0466\\u0468"\
    "\\u046A\\u046C\\u046E\\u0470\\u0472\\u0474\\u0476\\u0478\\u047A\\u047C"\
    "\\u047E\\u0480\\u048A\\u048C\\u048E\\u0490\\u0492\\u0494\\u0496\\u0498"\
    "\\u049A\\u049C\\u049E\\u04A0\\u04A2\\u04A4\\u04A6\\u04A8\\u04AA\\u04AC"\
    "\\u04AE\\u04B0\\u04B2\\u04B4\\u04B6\\u04B8\\u04BA\\u04BC\\u04BE\\u04C3"\
    "\\u04C5\\u04C7\\u04C9\\u04CB\\u04CD\\u04D0\\u04D2\\u04D4\\u04D6\\u04D8"\
    "\\u04DA\\u04DC\\u04DE\\u04E0\\u04E2\\u04E4\\u04E6\\u04E8\\u04EA\\u04EC"\
    "\\u04EE\\u04F0\\u04F2\\u04F4\\u04F6\\u04F8\\u04FA\\u04FC\\u04FE\\u0500"\
    "\\u0502\\u0504\\u0506\\u0508\\u050A\\u050C\\u050E\\u0510\\u0512\\u0514"\
    "\\u0516\\u0518\\u051A\\u051C\\u051E\\u0520\\u0522\\u0524\\u0526\\u0528"\
    "\\u052A\\u052C\\u052E\\u10C7\\u10CD\\u1E00\\u1E02\\u1E04\\u1E06\\u1E08"\
    "\\u1E0A\\u1E0C\\u1E0E\\u1E10\\u1E12\\u1E14\\u1E16\\u1E18\\u1E1A\\u1E1C"\
    "\\u1E1E\\u1E20\\u1E22\\u1E24\\u1E26\\u1E28\\u1E2A\\u1E2C\\u1E2E\\u1E30"\
    "\\u1E32\\u1E34\\u1E36\\u1E38\\u1E3A\\u1E3C\\u1E3E\\u1E40\\u1E42\\u1E44"\
    "\\u1E46\\u1E48\\u1E4A\\u1E4C\\u1E4E\\u1E50\\u1E52\\u1E54\\u1E56\\u1E58"\
    "\\u1E5A\\u1E5C\\u1E5E\\u1E60\\u1E62\\u1E64\\u1E66\\u1E68\\u1E6A\\u1E6C"\
    "\\u1E6E\\u1E70\\u1E72\\u1E74\\u1E76\\u1E78\\u1E7A\\u1E7C\\u1E7E\\u1E80"\
    "\\u1E82\\u1E84\\u1E86\\u1E88\\u1E8A\\u1E8C\\u1E8E\\u1E90\\u1E92\\u1E94"\
    "\\u1E9E\\u1EA0\\u1EA2\\u1EA4\\u1EA6\\u1EA8\\u1EAA\\u1EAC\\u1EAE\\u1EB0"\
    "\\u1EB2\\u1EB4\\u1EB6\\u1EB8\\u1EBA\\u1EBC\\u1EBE\\u1EC0\\u1EC2\\u1EC4"\
    "\\u1EC6\\u1EC8\\u1ECA\\u1ECC\\u1ECE\\u1ED0\\u1ED2\\u1ED4\\u1ED6\\u1ED8"\
    "\\u1EDA\\u1EDC\\u1EDE\\u1EE0\\u1EE2\\u1EE4\\u1EE6\\u1EE8\\u1EEA\\u1EEC"\
    "\\u1EEE\\u1EF0\\u1EF2\\u1EF4\\u1EF6\\u1EF8\\u1EFA\\u1EFC\\u1EFE\\u1F59"\
    "\\u1F5B\\u1F5D\\u1F5F\\u2102\\u2107\\u2115\\u2124\\u2126\\u2128\\u2145"\
    "\\u2183\\u2C60\\u2C67\\u2C69\\u2C6B\\u2C72\\u2C75\\u2C82\\u2C84\\u2C86"\
    "\\u2C88\\u2C8A\\u2C8C\\u2C8E\\u2C90\\u2C92\\u2C94\\u2C96\\u2C98\\u2C9A"\
    "\\u2C9C\\u2C9E\\u2CA0\\u2CA2\\u2CA4\\u2CA6\\u2CA8\\u2CAA\\u2CAC\\u2CAE"\
    "\\u2CB0\\u2CB2\\u2CB4\\u2CB6\\u2CB8\\u2CBA\\u2CBC\\u2CBE\\u2CC0\\u2CC2"\
    "\\u2CC4\\u2CC6\\u2CC8\\u2CCA\\u2CCC\\u2CCE\\u2CD0\\u2CD2\\u2CD4\\u2CD6"\
    "\\u2CD8\\u2CDA\\u2CDC\\u2CDE\\u2CE0\\u2CE2\\u2CEB\\u2CED\\u2CF2\\uA640"\
    "\\uA642\\uA644\\uA646\\uA648\\uA64A\\uA64C\\uA64E\\uA650\\uA652\\uA654"\
    "\\uA656\\uA658\\uA65A\\uA65C\\uA65E\\uA660\\uA662\\uA664\\uA666\\uA668"\
    "\\uA66A\\uA66C\\uA680\\uA682\\uA684\\uA686\\uA688\\uA68A\\uA68C\\uA68E"\
    "\\uA690\\uA692\\uA694\\uA696\\uA698\\uA69A\\uA722\\uA724\\uA726\\uA728"\
    "\\uA72A\\uA72C\\uA72E\\uA732\\uA734\\uA736\\uA738\\uA73A\\uA73C\\uA73E"\
    "\\uA740\\uA742\\uA744\\uA746\\uA748\\uA74A\\uA74C\\uA74E\\uA750\\uA752"\
    "\\uA754\\uA756\\uA758\\uA75A\\uA75C\\uA75E\\uA760\\uA762\\uA764\\uA766"\
    "\\uA768\\uA76A\\uA76C\\uA76E\\uA779\\uA77B\\uA780\\uA782\\uA784\\uA786"\
    "\\uA78B\\uA78D\\uA790\\uA792\\uA796\\uA798\\uA79A\\uA79C\\uA79E\\uA7A0"\
    "\\uA7A2\\uA7A4\\uA7A6\\uA7A8\\uA7B6\\uA7B8\\uA7BA\\uA7BC\\uA7BE\\uA7C2"\
    "\\U01D49C\\U01D4A2\\U01D546\\U01D7CA\\u0041-\\u005A\\u00C0-\\u00D6"\
    "\\u00D8-\\u00DE\\u0178-\\u0179\\u0181-\\u0182\\u0186-\\u0187"\
    "\\u0189-\\u018B\\u018E-\\u0191\\u0193-\\u0194\\u0196-\\u0198"\
    "\\u019C-\\u019D\\u019F-\\u01A0\\u01A6-\\u01A7\\u01AE-\\u01AF"\
    "\\u01B1-\\u01B3\\u01B7-\\u01B8\\u01F6-\\u01F8\\u023A-\\u023B"\
    "\\u023D-\\u023E\\u0243-\\u0246\\u0388-\\u038A\\u038E-\\u038F"\
    "\\u0391-\\u03A1\\u03A3-\\u03AB\\u03D2-\\u03D4\\u03F9-\\u03FA"\
    "\\u03FD-\\u042F\\u04C0-\\u04C1\\u0531-\\u0556\\u10A0-\\u10C5"\
    "\\u13A0-\\u13F5\\u1C90-\\u1CBA\\u1CBD-\\u1CBF\\u1F08-\\u1F0F"\
    "\\u1F18-\\u1F1D\\u1F28-\\u1F2F\\u1F38-\\u1F3F\\u1F48-\\u1F4D"\
    "\\u1F68-\\u1F6F\\u1FB8-\\u1FBB\\u1FC8-\\u1FCB\\u1FD8-\\u1FDB"\
    "\\u1FE8-\\u1FEC\\u1FF8-\\u1FFB\\u210B-\\u210D\\u2110-\\u2112"\
    "\\u2119-\\u211D\\u212A-\\u212D\\u2130-\\u2133\\u213E-\\u213F"\
    "\\u2C00-\\u2C2E\\u2C62-\\u2C64\\u2C6D-\\u2C70\\u2C7E-\\u2C80"\
    "\\uA77D-\\uA77E\\uA7AA-\\uA7AE\\uA7B0-\\uA7B4\\uA7C4-\\uA7C6"\
    "\\uFF21-\\uFF3A\\U010400-\\U010427\\U0104B0-\\U0104D3"\
    "\\U010C80-\\U010CB2\\U0118A0-\\U0118BF\\U016E40-\\U016E5F"\
    "\\U01D400-\\U01D419\\U01D434-\\U01D44D\\U01D468-\\U01D481"\
    "\\U01D49E-\\U01D49F\\U01D4A5-\\U01D4A6\\U01D4A9-\\U01D4AC"\
    "\\U01D4AE-\\U01D4B5\\U01D4D0-\\U01D4E9\\U01D504-\\U01D505"\
    "\\U01D507-\\U01D50A\\U01D50D-\\U01D514\\U01D516-\\U01D51C"\
    "\\U01D538-\\U01D539\\U01D53B-\\U01D53E\\U01D540-\\U01D544"\
    "\\U01D54A-\\U01D550\\U01D56C-\\U01D585\\U01D5A0-\\U01D5B9"\
    "\\U01D5D4-\\U01D5ED\\U01D608-\\U01D621\\U01D63C-\\U01D655"\
    "\\U01D670-\\U01D689\\U01D6A8-\\U01D6C0\\U01D6E2-\\U01D6FA"\
    "\\U01D71C-\\U01D734\\U01D756-\\U01D76E\\U01D790-\\U01D7A8"\
    "\\U01E900-\\U01E921";

const char unicode_lowercase[] =
    "\\u00B5\\u0101\\u0103\\u0105\\u0107\\u0109\\u010B\\u010D\\u010F\\u0111"\
    "\\u0113\\u0115\\u0117\\u0119\\u011B\\u011D\\u011F\\u0121\\u0123\\u0125"\
    "\\u0127\\u0129\\u012B\\u012D\\u012F\\u0131\\u0133\\u0135\\u013A\\u013C"\
    "\\u013E\\u0140\\u0142\\u0144\\u0146\\u014B\\u014D\\u014F\\u0151\\u0153"\
    "\\u0155\\u0157\\u0159\\u015B\\u015D\\u015F\\u0161\\u0163\\u0165\\u0167"\
    "\\u0169\\u016B\\u016D\\u016F\\u0171\\u0173\\u0175\\u0177\\u017A\\u017C"\
    "\\u0183\\u0185\\u0188\\u0192\\u0195\\u019E\\u01A1\\u01A3\\u01A5\\u01A8"\
    "\\u01AD\\u01B0\\u01B4\\u01B6\\u01C6\\u01C9\\u01CC\\u01CE\\u01D0\\u01D2"\
    "\\u01D4\\u01D6\\u01D8\\u01DA\\u01DF\\u01E1\\u01E3\\u01E5\\u01E7\\u01E9"\
    "\\u01EB\\u01ED\\u01F3\\u01F5\\u01F9\\u01FB\\u01FD\\u01FF\\u0201\\u0203"\
    "\\u0205\\u0207\\u0209\\u020B\\u020D\\u020F\\u0211\\u0213\\u0215\\u0217"\
    "\\u0219\\u021B\\u021D\\u021F\\u0221\\u0223\\u0225\\u0227\\u0229\\u022B"\
    "\\u022D\\u022F\\u0231\\u023C\\u0242\\u0247\\u0249\\u024B\\u024D\\u0371"\
    "\\u0373\\u0377\\u0390\\u03D9\\u03DB\\u03DD\\u03DF\\u03E1\\u03E3\\u03E5"\
    "\\u03E7\\u03E9\\u03EB\\u03ED\\u03F5\\u03F8\\u0461\\u0463\\u0465\\u0467"\
    "\\u0469\\u046B\\u046D\\u046F\\u0471\\u0473\\u0475\\u0477\\u0479\\u047B"\
    "\\u047D\\u047F\\u0481\\u048B\\u048D\\u048F\\u0491\\u0493\\u0495\\u0497"\
    "\\u0499\\u049B\\u049D\\u049F\\u04A1\\u04A3\\u04A5\\u04A7\\u04A9\\u04AB"\
    "\\u04AD\\u04AF\\u04B1\\u04B3\\u04B5\\u04B7\\u04B9\\u04BB\\u04BD\\u04BF"\
    "\\u04C2\\u04C4\\u04C6\\u04C8\\u04CA\\u04CC\\u04D1\\u04D3\\u04D5\\u04D7"\
    "\\u04D9\\u04DB\\u04DD\\u04DF\\u04E1\\u04E3\\u04E5\\u04E7\\u04E9\\u04EB"\
    "\\u04ED\\u04EF\\u04F1\\u04F3\\u04F5\\u04F7\\u04F9\\u04FB\\u04FD\\u04FF"\
    "\\u0501\\u0503\\u0505\\u0507\\u0509\\u050B\\u050D\\u050F\\u0511\\u0513"\
    "\\u0515\\u0517\\u0519\\u051B\\u051D\\u051F\\u0521\\u0523\\u0525\\u0527"\
    "\\u0529\\u052B\\u052D\\u052F\\u1E01\\u1E03\\u1E05\\u1E07\\u1E09\\u1E0B"\
    "\\u1E0D\\u1E0F\\u1E11\\u1E13\\u1E15\\u1E17\\u1E19\\u1E1B\\u1E1D\\u1E1F"\
    "\\u1E21\\u1E23\\u1E25\\u1E27\\u1E29\\u1E2B\\u1E2D\\u1E2F\\u1E31\\u1E33"\
    "\\u1E35\\u1E37\\u1E39\\u1E3B\\u1E3D\\u1E3F\\u1E41\\u1E43\\u1E45\\u1E47"\
    "\\u1E49\\u1E4B\\u1E4D\\u1E4F\\u1E51\\u1E53\\u1E55\\u1E57\\u1E59\\u1E5B"\
    "\\u1E5D\\u1E5F\\u1E61\\u1E63\\u1E65\\u1E67\\u1E69\\u1E6B\\u1E6D\\u1E6F"\
    "\\u1E71\\u1E73\\u1E75\\u1E77\\u1E79\\u1E7B\\u1E7D\\u1E7F\\u1E81\\u1E83"\
    "\\u1E85\\u1E87\\u1E89\\u1E8B\\u1E8D\\u1E8F\\u1E91\\u1E93\\u1E9F\\u1EA1"\
    "\\u1EA3\\u1EA5\\u1EA7\\u1EA9\\u1EAB\\u1EAD\\u1EAF\\u1EB1\\u1EB3\\u1EB5"\
    "\\u1EB7\\u1EB9\\u1EBB\\u1EBD\\u1EBF\\u1EC1\\u1EC3\\u1EC5\\u1EC7\\u1EC9"\
    "\\u1ECB\\u1ECD\\u1ECF\\u1ED1\\u1ED3\\u1ED5\\u1ED7\\u1ED9\\u1EDB\\u1EDD"\
    "\\u1EDF\\u1EE1\\u1EE3\\u1EE5\\u1EE7\\u1EE9\\u1EEB\\u1EED\\u1EEF\\u1EF1"\
    "\\u1EF3\\u1EF5\\u1EF7\\u1EF9\\u1EFB\\u1EFD\\u1FBE\\u210A\\u2113\\u212F"\
    "\\u2134\\u2139\\u214E\\u2184\\u2C61\\u2C68\\u2C6A\\u2C6C\\u2C71\\u2C81"\
    "\\u2C83\\u2C85\\u2C87\\u2C89\\u2C8B\\u2C8D\\u2C8F\\u2C91\\u2C93\\u2C95"\
    "\\u2C97\\u2C99\\u2C9B\\u2C9D\\u2C9F\\u2CA1\\u2CA3\\u2CA5\\u2CA7\\u2CA9"\
    "\\u2CAB\\u2CAD\\u2CAF\\u2CB1\\u2CB3\\u2CB5\\u2CB7\\u2CB9\\u2CBB\\u2CBD"\
    "\\u2CBF\\u2CC1\\u2CC3\\u2CC5\\u2CC7\\u2CC9\\u2CCB\\u2CCD\\u2CCF\\u2CD1"\
    "\\u2CD3\\u2CD5\\u2CD7\\u2CD9\\u2CDB\\u2CDD\\u2CDF\\u2CE1\\u2CEC\\u2CEE"\
    "\\u2CF3\\u2D27\\u2D2D\\uA641\\uA643\\uA645\\uA647\\uA649\\uA64B\\uA64D"\
    "\\uA64F\\uA651\\uA653\\uA655\\uA657\\uA659\\uA65B\\uA65D\\uA65F\\uA661"\
    "\\uA663\\uA665\\uA667\\uA669\\uA66B\\uA66D\\uA681\\uA683\\uA685\\uA687"\
    "\\uA689\\uA68B\\uA68D\\uA68F\\uA691\\uA693\\uA695\\uA697\\uA699\\uA69B"\
    "\\uA723\\uA725\\uA727\\uA729\\uA72B\\uA72D\\uA733\\uA735\\uA737\\uA739"\
    "\\uA73B\\uA73D\\uA73F\\uA741\\uA743\\uA745\\uA747\\uA749\\uA74B\\uA74D"\
    "\\uA74F\\uA751\\uA753\\uA755\\uA757\\uA759\\uA75B\\uA75D\\uA75F\\uA761"\
    "\\uA763\\uA765\\uA767\\uA769\\uA76B\\uA76D\\uA76F\\uA77A\\uA77C\\uA77F"\
    "\\uA781\\uA783\\uA785\\uA787\\uA78C\\uA78E\\uA791\\uA797\\uA799\\uA79B"\
    "\\uA79D\\uA79F\\uA7A1\\uA7A3\\uA7A5\\uA7A7\\uA7A9\\uA7AF\\uA7B5\\uA7B7"\
    "\\uA7B9\\uA7BB\\uA7BD\\uA7BF\\uA7C3\\uA7FA\\U01D4BB\\U01D7CB"\
    "\\u0061-\\u007A\\u00DF-\\u00F6\\u00F8-\\u00FF\\u0137-\\u0138"\
    "\\u0148-\\u0149\\u017E-\\u0180\\u018C-\\u018D\\u0199-\\u019B"\
    "\\u01AA-\\u01AB\\u01B9-\\u01BA\\u01BD-\\u01BF\\u01DC-\\u01DD"\
    "\\u01EF-\\u01F0\\u0233-\\u0239\\u023F-\\u0240\\u024F-\\u0293"\
    "\\u0295-\\u02AF\\u037B-\\u037D\\u03AC-\\u03CE\\u03D0-\\u03D1"\
    "\\u03D5-\\u03D7\\u03EF-\\u03F3\\u03FB-\\u03FC\\u0430-\\u045F"\
    "\\u04CE-\\u04CF\\u0560-\\u0588\\u10D0-\\u10FA\\u10FD-\\u10FF"\
    "\\u13F8-\\u13FD\\u1C80-\\u1C88\\u1D00-\\u1D2B\\u1D6B-\\u1D77"\
    "\\u1D79-\\u1D9A\\u1E95-\\u1E9D\\u1EFF-\\u1F07\\u1F10-\\u1F15"\
    "\\u1F20-\\u1F27\\u1F30-\\u1F37\\u1F40-\\u1F45\\u1F50-\\u1F57"\
    "\\u1F60-\\u1F67\\u1F70-\\u1F7D\\u1F80-\\u1F87\\u1F90-\\u1F97"\
    "\\u1FA0-\\u1FA7\\u1FB0-\\u1FB4\\u1FB6-\\u1FB7\\u1FC2-\\u1FC4"\
    "\\u1FC6-\\u1FC7\\u1FD0-\\u1FD3\\u1FD6-\\u1FD7\\u1FE0-\\u1FE7"\
    "\\u1FF2-\\u1FF4\\u1FF6-\\u1FF7\\u210E-\\u210F\\u213C-\\u213D"\
    "\\u2146-\\u2149\\u2C30-\\u2C5E\\u2C65-\\u2C66\\u2C73-\\u2C74"\
    "\\u2C76-\\u2C7B\\u2CE3-\\u2CE4\\u2D00-\\u2D25\\uA72F-\\uA731"\
    "\\uA771-\\uA778\\uA793-\\uA795\\uAB30-\\uAB5A\\uAB60-\\uAB67"\
    "\\uAB70-\\uABBF\\uFB00-\\uFB06\\uFB13-\\uFB17\\uFF41-\\uFF5A"\
    "\\U010428-\\U01044F\\U0104D8-\\U0104FB\\U010CC0-\\U010CF2"\
    "\\U0118C0-\\U0118DF\\U016E60-\\U016E7F\\U01D41A-\\U01D433"\
    "\\U01D44E-\\U01D454\\U01D456-\\U01D467\\U01D482-\\U01D49B"\
    "\\U01D4B6-\\U01D4B9\\U01D4BD-\\U01D4C3\\U01D4C5-\\U01D4CF"\
    "\\U01D4EA-\\U01D503\\U01D51E-\\U01D537\\U01D552-\\U01D56B"\
    "\\U01D586-\\U01D59F\\U01D5BA-\\U01D5D3\\U01D5EE-\\U01D607"\
    "\\U01D622-\\U01D63B\\U01D656-\\U01D66F\\U01D68A-\\U01D6A5"\
    "\\U01D6C2-\\U01D6DA\\U01D6DC-\\U01D6E1\\U01D6FC-\\U01D714"\
    "\\U01D716-\\U01D71B\\U01D736-\\U01D74E\\U01D750-\\U01D755"\
    "\\U01D770-\\U01D788\\U01D78A-\\U01D78F\\U01D7AA-\\U01D7C2"\
    "\\U01D7C4-\\U01D7C9\\U01E922-\\U01E943";

#endif // MOJO_REGEX_UNICODE
#endif // MOJO_REGEX_IMPLEMENTATION
#endif // _MOJO_REGEX_HEADER_
