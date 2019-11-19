#ifndef _MOJO_REGEX_HEADER_
#define _MOJO_REGEX_HEADER_

#define MOJO_REGEX_TEST_MAIN
#define MOJO_REGEX_COMPILE_IMPLEMENTATION
#define MOJO_REGEX_VM_SOURCE_GENERATION
#define MOJO_REGEX_EVALUATE_IMPLEMENTATION
#define MOJO_UNICODE_SUPPORT
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

    Note on utf8 support:
        clang follows sane expectations and converts \u#### into utf8 encoding
        within a char string. gcc illogically converts \u#### into utf16
        encoding, even within a char string, and does work properly with the
        evaluator. For gcc, you will need to manually encode utf8 characters.

    CURRENT:
        utf8 classes are incomplete. DFA is buildable, but some lingering issues
        in the subroutine mechanism need further work. Additionally, the shunting
        yard output has many redundant jump instructions.
        
    TODO
        \X full unicode letter glyph (may be multiple chars, includes marker glyphs)
            "\P{M}\p{M}*"
        ^  start of string (assertion, non consuming)
        $  end of string (assertion, non consuming) [see eTokenMatch]
        \< match start of word (assertion, non consuming, ASCII only)
        \> match end of word (assertion, non consuming, ASCII only)
        \P inverts unicode category class
        extend subexpression notation for explicit subroutines
        surface subroutine errors during compilation
        unicode utf8 classes
            build NFA from class tree
        create utf8 class table
        output utf8 class table

    (?P<name>...)  named subexpressions
    (?:...) non capturing subexpressions
    (?*...) compound subexpressions
    (?i) case insensitive match - TODO

    multiple subexpression meta prefixes may be defined, but must be at the
    head of the subexpression. (?P<name>...) and (?:...) are mutually exclusive
    (why name something unused?). (?:...) and (?*...) are mutually exclusive
    (compound non-captures?). Additional meta's must include their own unique
    '?' prefix, ie.: (?:?P<name>...)

    TODO
        unicode character classes
        unicode compilation toggle - do NOT generate unicode char classes in non
            unicode mode

    TODO: Future work (refining, not MVP critical)
        tag meta char class -> char class -> vm path, for better runtime debug
        update unicode parser script to parse arbitrary property sets/groups
        refactor unicode char class registration to a single struct type
            current has independent class id and class str values
        Shunting yard creates a valid, yet inefficient, DFA. There are various
            redundencies, particularly with regards to chained and unneccessary
            jumps.


Regex VM Bytecode (v5)

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

    At runtime, if a partial match is requested, exceution begins at 0. If a
    full match is requested, execution begins at 3.

Test: a(?P<foolio>bcd(?iefg*[hijk]foo)?)[0-9]+cat abcdefgefgjfoo8167287catfoo
*/

/////////////////////////////////////////////////////////////////////////////

#define REGEX_VM_MACHINE_VERSION    6

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

regex_match_t *regexMatch(regex_vm_t *vm, const char *text, int len, int anchored);
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

char *_regexStrdup(const char *str) {
    char *ptr;

    if((ptr = _regexAlloc(strlen(str) + 1, _regexMemContext)) == NULL) {
        return NULL;
    }
    memcpy(ptr, str, strlen(str) + 1);
    return ptr;
}

/////////////////////////////////////////////////////////////////////////////
// String table management and data reproduction
/////////////////////////////////////////////////////////////////////////////

#define REGEX_TABLE_DEDUPE      0x01u
#define REGEX_TABLE_KEEP_PTR    0x02u

int tableEntryAdd(void ***base, int *table_size, size_t entry_size, void *entry_data,
                  unsigned int flags, int *deduped) {
    int k;

    if(flags & REGEX_TABLE_DEDUPE) {
        for(k = 0; k < *table_size; k++) {
            if(((entry_data == NULL) && ((*base)[k] == NULL)) ||
               ((entry_data != NULL) && (!memcmp((*base)[k], entry_data, entry_size)))) {
                if(deduped != NULL) {
                    *deduped = 1;
                }
                return k;
            }
        }
        if(deduped != NULL) {
            *deduped = 0;
        }
    }

    if((*base = _regexRealloc(*base, ((*table_size) * sizeof(void *)),
                              (((*table_size) + 1) * sizeof(void *)),
                              _regexMemContext)) == NULL) {
        return -1;
    }
    if(flags & REGEX_TABLE_KEEP_PTR) {
        (*base)[*table_size] = entry_data;
    } else {
        if(entry_data == NULL) {
            (*base)[*table_size] = entry_data;
        } else {
            if(((*base)[*table_size] = _regexAlloc(entry_size, _regexMemContext)) == NULL) {
                return -1;
            }
            memcpy((*base)[*table_size], entry_data, entry_size);
        }
    }

    (*table_size)++;
    return (*table_size) - 1;
}

void *tableEntryGet(void **base, int table_size, int index) {
    if(index >= table_size) {
        return NULL;
    }
    return base[index];
}

void tableEntryFree(void ***base, int *table_size, int free_data) {
    int k;
    if((*base) != NULL) {
        if(free_data) {
            for(k = 0; k < *table_size; k++) {
                if((*base)[k] != NULL) {
                    _regexDealloc((*base)[k], _regexMemContext);
                }
            }
        }
        _regexDealloc((*base), _regexMemContext);
    }
    (*base) = NULL;
    *table_size = 0;
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
    eTokenUtf8Literal,
    eTokenUtf8AnyChar,
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

typedef enum {
    ePriorityNone,
    ePriorityLow,
    ePriorityMedium,
    ePriorityHigh
} eRegexTokenPriority_t;

// Token attribute flags ///////////////////////////
// Subexpression flags
#define REGEX_TOKEN_FLAG_CASEINS    0x1u
#define REGEX_TOKEN_FLAG_NOCAPTURE  0x2u
#define REGEX_TOKEN_FLAG_COMPOUND   0x4u

// Shunting yard subroutine flag
#define REGEX_TOKEN_FLAG_SUBROUTINE 0x8u

// char literal
#define REGEX_TOKEN_FLAG_INVERT     0x1u

typedef struct regex_token_s regex_token_t;
struct regex_token_s {
    eRegexToken_t tokenType;
    union {
        int c;
        char *str;
        unsigned int *bitmap;
        int group;
        regex_token_t *sub_sequence;
    };
    int pc; // program counter index, for NFA -> VM
    union {
        int len; // string length, to allow embedded \0 chars
        unsigned int flags;
    };
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

typedef enum {
    eReSubroutineIdCrc,
    eReSubroutineIdPattern,
    eReSubroutineIdClass,
    eReSubroutineIdName
} eReSubroutineIdType_t;

typedef struct regex_subroutine_s regex_subroutine_t;
struct regex_subroutine_s {
    union {
        uint32_t crc;
        char *pattern;
        char *charClass;
        char *name;
    };
    eReSubroutineIdType_t type;
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

int regexVMGroupTableEntryAdd(regex_vm_build_t *build, const char *group, int len, int index);

/////////////////////////////////////////////////////////////////////////////
// Token meta attribute helper functions
/////////////////////////////////////////////////////////////////////////////

// Determines whether a given token is a terminal operand. Used when comparing
// two adjacent tokens, the preceeding flag indicates whether the token be
// checked is leftmost.
int regexTokenIsTerminal(regex_token_t *token, int preceeding) {
    switch(token->tokenType) {
        case eTokenCharLiteral:
        case eTokenStringLiteral:
        case eTokenCharClass:
        case eTokenByte:
        case eTokenUtf8Class:
        case eTokenCall:
        case eTokenReturn:
        case eTokenCharAny:
            return 1;
        case eTokenZeroOrOne:
        case eTokenZeroOrMany:
        case eTokenOneOrMany:
            return preceeding;
        case eTokenSubExprEnd:
            return preceeding;
        case eTokenSubExprStart:
            return !preceeding;
        default:
            return 0;
    }
}

// Token priority when applying operators to operands in the shunting yard
eRegexTokenPriority_t regexGetTokenTypePriority(eRegexToken_t tokenType) {
    switch(tokenType) {
        case eTokenCharLiteral:
        case eTokenCharClass:
        case eTokenStringLiteral:
        case eTokenCharAny:
        case eTokenByte:
        case eTokenUtf8Class:
        case eTokenCall:
        case eTokenReturn:
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

// Operator arity (number of tokens that the operator functions on) in the
// shunting yard
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
    entry->id = _regexStrdup(classId);
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

regex_token_t *regexAllocToken(eRegexToken_t tokenType, int c, char *str, int sizeOrFlags) {
    regex_token_t *token;

    if((token = _regexAlloc(sizeof(regex_token_t), _regexMemContext)) == NULL) {
        return NULL;
    }
    memset(token, 0, sizeof(regex_token_t));
    token->tokenType = tokenType;
    token->pc = VM_PC_UNVISITED;
    if(tokenType == eTokenCall) {
        token->sub_sequence = (regex_token_t *)str;
        token->out_b = (regex_token_t *)str;
    } else if(str != NULL) {
        token->str = str;
        token->len = sizeOrFlags;
    } else {
        token->c = c;
        token->flags = sizeOrFlags;
    }
    return token;
}

int regexTokenCreate(regex_token_t **list, eRegexToken_t tokenType, int c, char *str, int sizeOrFlags) {
    regex_token_t *token, *walk;

    if((token = regexAllocToken(tokenType, c, str, sizeOrFlags)) == NULL) {
        return 0;
    }

    if(*list == NULL) {
        *list = token;
    } else {
        for(walk = *list; walk->next != NULL; walk = walk->next);
        if(regexTokenIsTerminal(token, 0) && regexTokenIsTerminal(walk, 1)) {
            // Two adjacent terminals have an implicit concatenation
            if((walk->next = regexAllocToken(eTokenConcatenation, 0, NULL, 0)) == NULL) {
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
        *crc = table[(uint8_t) *crc ^ ((uint8_t *) data)[i]] ^ *crc >> 8;
    }
}

regex_token_t *regexSubroutineIdxCheckCrc(regex_vm_build_t *build, uint32_t check) {
    regex_subroutine_t *walk;

    for(walk = build->subroutine_index; walk != NULL; walk = walk->next) {
        if(walk->crc == check) {
            return walk->tokens;
        }
    }
    return NULL;
}

regex_token_t *regexSubroutineIdxCheck(regex_vm_build_t *build, const char *pattern) {
    uint32_t check = 0;

    calcCrc32(pattern, strlen(pattern), &check);
    return regexSubroutineIdxCheckCrc(build, check);
}

int regexSubroutineIdxAddCrc(regex_vm_build_t *build, uint32_t crc, regex_token_t *token) {
    regex_subroutine_t *entry;

    if((entry = _regexAlloc(sizeof(regex_subroutine_t), _regexMemContext)) == NULL) {
        return 0;
    }
    entry->tokens = token;
    entry->crc = crc;
    entry->next = build->subroutine_index;
    build->subroutine_index = entry;
    return 1;
}

int regexSubroutineIdxAdd(regex_vm_build_t *build, const char *pattern, regex_token_t *token) {
    uint32_t crc = 0;

    calcCrc32(pattern, strlen(pattern), &crc);
    return regexSubroutineIdxAddCrc(build, crc, token);
}

void regexSubroutineIdxFree(regex_vm_build_t *build) {
    regex_subroutine_t *walk, *next;

    for(walk = build->subroutine_index; walk != NULL; walk = next) {
        next = walk->next;
        _regexDealloc(walk, _regexMemContext);
    }
}

eRegexCompileStatus_t regexTokenizePattern(const char *pattern,
                                           int *pos,
                                           regex_token_t **tokens,
                                           regex_vm_build_t *build);

eRegexCompileStatus_t regexTokenCreateSubroutine(regex_token_t **tokens, regex_vm_build_t *build, const char *pattern) {
    regex_token_t *subroutine = NULL;
    int pos;
    eRegexCompileStatus_t status;

    if((subroutine = regexSubroutineIdxCheck(build, pattern)) == NULL) {
        if((status = regexTokenizePattern(pattern, &pos, &subroutine, build)) != eCompileOk) {
            // TODO - retain sub-compilation status
            return status;
        }

        if(!regexSubroutineIdxAdd(build, pattern, subroutine)) {
            return eCompileOutOfMem;
        }
    }

    if(!regexTokenCreate(tokens, eTokenCall, 0, (void *)subroutine, 0)) {
        return eCompileOutOfMem;
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
    char classId[3], c;

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
        if(!regexTokenCreate(tokens, eTokenAlternative, 0, NULL, 0)) {
            return 0;
        }
    }
    if(!regexTokenCreate(tokens, eTokenSubExprStart, 0, NULL, REGEX_TOKEN_FLAG_NOCAPTURE)) {
        return 0;
    }
    if(!regexTokenCreate(tokens, eTokenCharLiteral,
                         midlow->prefix | (nested ? UTF8_LOW_BYTE_PREFIX : UTF8_TWO_BYTE_PREFIX),
                         NULL, (invert ? REGEX_TOKEN_FLAG_INVERT : 0))) {
        return 0;
    }
    if((bitmap = utf8ClassBitmapCopy(midlow->bitmap, invert)) == NULL) {
        return 0;
    }
    if(!regexTokenCreate(tokens, eTokenUtf8Class, 0, (char *)bitmap, 0)) {
        return 0;
    }
    if(!regexTokenCreate(tokens, eTokenSubExprEnd, 0, NULL, 0)) {
        return 0;
    }
    return 1;
}

int regexTokenUtf8ClassMidhighCreate(regex_token_t **tokens, utf8_charclass_midhigh_byte_t *midhigh,
                                     int first, int nested, int invert) {
    utf8_charclass_midlow_byte_t *midlow;

    if(!first) {
        if(!regexTokenCreate(tokens, eTokenAlternative, 0, NULL, 0)) {
            return 0;
        }
    }
    if(!regexTokenCreate(tokens, eTokenSubExprStart, 0, NULL, REGEX_TOKEN_FLAG_NOCAPTURE)) {
        return 0;
    }
    if(!regexTokenCreate(tokens, eTokenCharLiteral,
                         midhigh->prefix | (nested ? UTF8_LOW_BYTE_PREFIX : UTF8_THREE_BYTE_PREFIX),
                         NULL, (invert ? REGEX_TOKEN_FLAG_INVERT : 0))) {
        return 0;
    }
    first = 1;
    for(midlow = midhigh->midlow_byte; midlow != NULL; midlow = midlow->next) {
        if(!regexTokenUtf8ClassMidlowCreate(tokens, midlow, first, 1, invert)) {
            return 0;
        }
        first = 0;
    }
    if(!regexTokenCreate(tokens, eTokenSubExprEnd, 0, NULL, 0)) {
        return 0;
    }
    return 1;
}

int regexTokenUtf8ClassHighCreate(regex_token_t **tokens, utf8_charclass_high_byte_t *high, int first, int invert) {
    utf8_charclass_midhigh_byte_t *midhigh;

    if(!first) {
        if(!regexTokenCreate(tokens, eTokenAlternative, 0, NULL, 0)) {
            return 0;
        }
    }
    if(!regexTokenCreate(tokens, eTokenSubExprStart, 0, NULL, REGEX_TOKEN_FLAG_NOCAPTURE)) {
        return 0;
    }
    if(!regexTokenCreate(tokens, eTokenCharLiteral,
                         high->prefix | UTF8_FOUR_BYTE_PREFIX,
                         NULL, (invert ? REGEX_TOKEN_FLAG_INVERT : 0))) {
        return 0;
    }
    first = 1;
    for(midhigh = high->midhigh_byte; midhigh != NULL; midhigh = midhigh->next) {
        if(!regexTokenUtf8ClassMidhighCreate(tokens, midhigh, first, 1, invert)) {
            return 0;
        }
        first = 0;
    }
    if(!regexTokenCreate(tokens, eTokenSubExprEnd, 0, NULL, 0)) {
        return 0;
    }
    return 1;
}

uint32_t regexCalUtf8ClassCrc(utf8_charclass_tree_t *tree) {
    utf8_charclass_midlow_byte_t *midlow;
    utf8_charclass_midhigh_byte_t *midhigh;
    utf8_charclass_high_byte_t *high;
    uint32_t crc = 0;

    calcCrc32(tree->one_byte, 32, &crc);
    for(midlow = tree->two_byte; midlow != NULL; midlow = midlow->next) {
        calcCrc32(&(midlow->prefix), 1, &crc);
        calcCrc32(midlow->bitmap, 8, &crc);
    }
    for(midhigh = tree->three_byte; midhigh != NULL; midhigh = midhigh->next) {
        calcCrc32(&(midhigh->prefix), 1, &crc);
        for(midlow = midhigh->midlow_byte; midlow != NULL; midlow = midlow->next) {
            calcCrc32(&(midlow->prefix), 1, &crc);
            calcCrc32(midlow->bitmap, 8, &crc);
        }
    }
    for(high = tree->four_byte; high != NULL; high = high->next) {
        calcCrc32(&(high->prefix), 1, &crc);
        for(midhigh = high->midhigh_byte; midhigh != NULL; midhigh = midhigh->next) {
            calcCrc32(&(midhigh->prefix), 1, &crc);
            for(midlow = midhigh->midlow_byte; midlow != NULL; midlow = midlow->next) {
                calcCrc32(&(midlow->prefix), 1, &crc);
                calcCrc32(midlow->bitmap, 8, &crc);
            }
        }
    }
    return crc;
}

int regexTokenUtf8ClassCreate(regex_vm_build_t *build, eRegexCompileStatus_t *status,
                              regex_token_t **tokens, utf8_charclass_tree_t *tree, int invert) {
    regex_token_t *utf8class = NULL;
    utf8_charclass_midlow_byte_t *midlow;
    utf8_charclass_midhigh_byte_t *midhigh;
    utf8_charclass_high_byte_t *high;
    unsigned int *bitmap;
    uint32_t crc;
    int k, first = 1;

    // Preset to OoM, change to eCompileOk on success
    *status = eCompileOutOfMem;

    crc = regexCalUtf8ClassCrc(tree);
    if((utf8class = regexSubroutineIdxCheckCrc(build, crc)) == NULL) {
        if(!regexTokenCreate(&utf8class, eTokenSubExprStart, 0, NULL, REGEX_TOKEN_FLAG_NOCAPTURE)) {
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
            if(!regexTokenCreate(&utf8class, eTokenCharClass, 0, (char *) bitmap, 0)) {
                *status = eCompileOutOfMem;
                return 0;
            }
            first = 0;
        }

        if(tree->two_byte != NULL) {
            if(!first) {
                if(!regexTokenCreate(&utf8class, eTokenAlternative, 0, NULL, 0)) {
                    return 0;
                }
            }
            if(!regexTokenCreate(&utf8class, eTokenSubExprStart, 0, NULL, REGEX_TOKEN_FLAG_NOCAPTURE)) {
                return 0;
            }
            first = 1;
            for(midlow = tree->two_byte; midlow != NULL; midlow = midlow->next) {
                if(!regexTokenUtf8ClassMidlowCreate(&utf8class, midlow, first, 0, invert)) {
                    return 0;
                }
                first = 0;
            }
            if(!regexTokenCreate(&utf8class, eTokenSubExprEnd, 0, NULL, 0)) {
                return 0;
            }
        }

        if(tree->three_byte != NULL) {
            if(!first) {
                if(!regexTokenCreate(&utf8class, eTokenAlternative, 0, NULL, 0)) {
                    return 0;
                }
            }
            if(!regexTokenCreate(&utf8class, eTokenSubExprStart, 0, NULL, REGEX_TOKEN_FLAG_NOCAPTURE)) {
                return 0;
            }
            first = 1;
            for(midhigh = tree->three_byte; midhigh != NULL; midhigh = midhigh->next) {
                if(!regexTokenUtf8ClassMidhighCreate(&utf8class, midhigh, first, 0, invert)) {
                    return 0;
                }
                first = 0;
            }
            if(!regexTokenCreate(&utf8class, eTokenSubExprEnd, 0, NULL, 0)) {
                return 0;
            }
        }

        if(tree->four_byte != NULL) {
            if(!first) {
                if(!regexTokenCreate(&utf8class, eTokenAlternative, 0, NULL, 0)) {
                    return 0;
                }
            }
            if(!regexTokenCreate(&utf8class, eTokenSubExprStart, 0, NULL, REGEX_TOKEN_FLAG_NOCAPTURE)) {
                return 0;
            }
            first = 1;
            for(high = tree->four_byte; high != NULL; high = high->next) {
                if(!regexTokenUtf8ClassHighCreate(&utf8class, high, first, invert)) {
                    return 0;
                }
                first = 0;
            }
            if(!regexTokenCreate(&utf8class, eTokenSubExprEnd, 0, NULL, 0)) {
                return 0;
            }
        }

        if(!regexTokenCreate(&utf8class, eTokenSubExprEnd, 0, NULL, 0)) {
            return 0;
        }

        if(!regexSubroutineIdxAddCrc(build, crc, utf8class)) {
            return eCompileOutOfMem;
        }
    }

    if(!regexTokenCreate(tokens, eTokenCall, 0, (void *)utf8class, 0)) {
        return eCompileOutOfMem;
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
                                 const char **pattern, int embedded, regex_token_t **tokens) {
    unsigned int bitmap[8], *ptr;
    parseChar_t c;
    int invert = 0;
    int range = 0;
    int last = 0;
    int k;
    utf8_charclass_tree_t *utf8tree = NULL;

    memset(bitmap, 0, 32);

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

        if(!regexTokenUtf8ClassCreate(build, status, tokens, utf8tree, invert)) {
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

        if(!regexTokenCreate(tokens, eTokenCharClass, 0, (char *)ptr, 0)) {
            *status = eCompileOutOfMem;
            return 0;
        }
    }
    return 1;
}

int parseUnicodeClassAndCreateToken(eRegexCompileStatus_t *status, regex_vm_build_t *build,
                                    int unicodeClass, regex_token_t **tokens) {
    char classId[3];
    const char *classStr;

    classId[0] = (char)((unsigned int)unicodeClass & 0xFFu);
    classId[1] = (char)(((unsigned int)unicodeClass & 0xFF00u) >> 8u);
    classId[2] = '\0';

    if((classStr = regexRegUnicodeCharClassGet(classId)) == NULL) {
        *status = eCompileUnknownUnicodeClass;
        return 0;
    }

    return parseCharClassAndCreateToken(status, build, &classStr, 0, tokens);
}

void regexPrintCharClassToFP(FILE *fp, unsigned int *bitmap) {
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
    eRegexCompileStatus_t status;
    int subexpr = 0;
    int named;

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
                        if(!regexTokenCreate(tokens, eTokenCharAny, 0, 0, 0)) {
                            SET_RESULT(eCompileOutOfMem);
                        }
                        continue;

                    case '[':
                        if(!parseCharClassAndCreateToken(&status, build, &pattern, 1, tokens)) {
                            SET_RESULT(status);
                        }
                        continue;

                    case '?':
                        if(!regexTokenCreate(tokens, eTokenZeroOrOne, 0, 0, 0)) {
                            SET_RESULT(eCompileOutOfMem);
                        }
                        continue;

                    case '*':
                        if(!regexTokenCreate(tokens, eTokenZeroOrMany, 0, 0, 0)) {
                            SET_RESULT(eCompileOutOfMem);
                        }
                        continue;

                    case '+':
                        if(!regexTokenCreate(tokens, eTokenOneOrMany, 0, 0, 0)) {
                            SET_RESULT(eCompileOutOfMem);
                        }
                        continue;

                    case '|':
                        if(!regexTokenCreate(tokens, eTokenAlternative, 0, 0, 0)) {
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
                        // Check for group meta modifiers
                        while(*pattern == '?') {
                            pattern++;
                            if(parseCheckNextPatternChar(&pattern, 'i')) {
                                // Case insensitive matching
                                flags |= REGEX_TOKEN_FLAG_CASEINS;
                            } else if(parseCheckNextPatternChar(&pattern, 'P')) {
                                // Named sub expression
                                if(named) {
                                    // This sub expression was already named
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
                                if(named) {
                                    // Cannot name a non-capturing subexpression
                                    SET_RESULT(eCompileConflictingAttrs);
                                }
                                if(flags & REGEX_TOKEN_FLAG_NOCAPTURE) {
                                    // Redundant non-capturing definition
                                    SET_RESULT(eCompileConflictingAttrs);
                                }
                                flags |= REGEX_TOKEN_FLAG_NOCAPTURE;
                            } else if(parseCheckNextPatternChar(&pattern, '*')) {
                                // Compound capturing subexpression
                                if(flags & REGEX_TOKEN_FLAG_NOCAPTURE) {
                                    // Non-sensical compound no-capture
                                    SET_RESULT(eCompileConflictingAttrs);
                                }
                                if(flags & REGEX_TOKEN_FLAG_COMPOUND) {
                                    // Redundant compound sub expression definition
                                    SET_RESULT(eCompileConflictingAttrs);
                                }
                                flags |= REGEX_TOKEN_FLAG_COMPOUND;
                            } else {
                                SET_RESULT(eCompileUnsupportedMeta);
                            }
                        }
                        if(!named) {
                            if((response = regexSubexprLookupEntryCreate(build, NULL, subexpr)) != 1) {
                                SET_RESULT((response == 0 ? eCompileOutOfMem : eCompileMalformedSubExprName));
                            }
                        }
                        if(!(flags & REGEX_TOKEN_FLAG_NOCAPTURE)) {
                            build->groups++;
                        }
                        if(!regexTokenCreate(tokens, eTokenSubExprStart, subexpr, NULL, flags)) {
                            SET_RESULT(eCompileOutOfMem);
                        }
                        continue;

                    case ')':
                        // End of grouped subexpression
                        if(!regexTokenCreate(tokens, eTokenSubExprEnd, 0, 0, 0)) {
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
                    case 'X': // full unicode glyph (base + markers)
                        // Represented by a micro NFA
                        // TODO
                        continue;
                    case 'B': // explicit byte
                        if(!regexTokenCreate(tokens, eTokenByte, 0, 0, 0)) {
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
                if(!parseCharClassAndCreateToken(&status, build, &class, 0, tokens)) {
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
                    if(!regexTokenCreate(tokens, eTokenCharLiteral, (char) (c.c), 0, 0)) {
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
                    if(!regexTokenCreate(tokens, eTokenStringLiteral, 0, str, PARSE_DEC_UTF8_COUNT(len) + PARSE_DEC_CHAR_COUNT(len))) {
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
// Subroutine support functions
//
// Since subroutines may be called from multiple places, multiple positions
// in the pattern may reference the same subroutine token stream. We use an
// index to keep track of a subroutine when it is converted to an NFA so that
// we only need to generate the NFA once, and subsequent references can then
// refer to the same NFA.
/////////////////////////////////////////////////////////////////////////////

typedef struct regex_sub_index_s regex_sub_index_t;
struct regex_sub_index_s {
    regex_token_t *sequence;
    regex_fragment_t *fragment;
    regex_sub_index_t *next;
};

regex_fragment_t *regexSubroutineGet(regex_sub_index_t *index, regex_token_t *token) {
    for(; index != NULL; index = index->next) {
        if(index->sequence == token) {
            return index->fragment;
        }
    }
    return NULL;
}

int regexSubroutineStore(regex_sub_index_t **index, regex_token_t *token, regex_fragment_t *fragment) {
    regex_sub_index_t *entry;

    if((entry = _regexAlloc(sizeof(regex_sub_index_t), _regexMemContext)) == NULL) {
        return 0;
    }
    memset(entry, 0, sizeof(regex_sub_index_t));
    entry->sequence = token;
    entry->fragment = fragment;
    entry->next = *index;
    *index = entry;
    return 1;
}

void regexSubroutineFree(regex_sub_index_t **index) {
    regex_sub_index_t *walk, *next;

    for(walk = *index; walk != NULL; walk = next) {
        next = walk->next;
        _regexDealloc(walk, _regexMemContext);
    }
}

/////////////////////////////////////////////////////////////////////////////
// NFA form regex support
/////////////////////////////////////////////////////////////////////////////

regex_fragment_t *regexFragmentCreate(regex_token_t *token, regex_ptrlist_t *list) {
    regex_fragment_t *fragment;

    if((fragment = _regexAlloc(sizeof(regex_fragment_t), _regexMemContext)) == NULL) {
        return NULL;
    }
    memset(fragment, 0, sizeof(regex_fragment_t));
    fragment->token = token;
    fragment->ptrlist = list;
    return fragment;
}

regex_ptrlist_t *regexPtrlistCreate(regex_token_t **state) {
    regex_ptrlist_t *entry;

    if((entry = _regexAlloc(sizeof(regex_ptrlist_t), _regexMemContext)) == NULL) {
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
        _regexDealloc(list, _regexMemContext);
    }
}

void regexFragmentFree(regex_fragment_t *fragment) {
    regex_fragment_t *next;

    while(fragment != NULL) {
        next = fragment->next;
        if(fragment->token != NULL) {
            regexTokenDestroy(fragment->token, 0);
            fragment->token = NULL;
        }
        if(fragment->ptrlist != NULL) {
            regexPtrListFree(fragment->ptrlist);
            fragment->ptrlist = NULL;
        }
        _regexDealloc(fragment, _regexMemContext);
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

    regexTokenDestroy(token, 1);
    if(((e2 = regexFragmentStackPop(stack)) == NULL) ||
       ((e1 = regexFragmentStackPop(stack)) == NULL)) {
        return 0;
    }
    regexPtrlistPatch(e1->ptrlist, e2->token);
    if((fragment = regexFragmentCreate(e1->token, e2->ptrlist)) == NULL) {
        return 0;
    }
    regexFragmentStackPush(stack, fragment);

    regexPtrListFree(e1->ptrlist);
    e1->ptrlist = NULL;
    e1->token = NULL;
    regexFragmentFree(e1);
    e2->token = NULL;
    e2->ptrlist = NULL;
    regexFragmentFree(e2);

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

    if(!regexTokenCreate(&jmp, eTokenJmp, 0, NULL, 0)) {
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

    regexPtrListFree(e1->ptrlist);
    e1->ptrlist = NULL;
    e1->token = NULL;
    regexFragmentFree(e1);
    e2->ptrlist = NULL;
    e2->token = NULL;
    regexFragmentFree(e2);

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

    e->ptrlist = NULL;
    e->token = NULL;
    regexFragmentFree(e);

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
    if(!regexTokenCreate(&jmp, eTokenJmp, 0, NULL, 0)) {
        return 0;
    }
    jmp->out_a = token;
    regexPtrlistPatch(e->ptrlist, jmp);
    if((fragment = regexFragmentCreate(token, list)) == NULL) {
        return 0;
    }
    regexFragmentStackPush(stack, fragment);

    e->ptrlist = NULL;
    e->token = NULL;
    regexFragmentFree(e);

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
        _regexDealloc(list, _regexMemContext);
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
    if(!regexTokenCreate(&token, eTokenMatch, 0, NULL, 0)) {
        return 0;
    }
    regexPtrlistPatch(e->ptrlist, token);
    regexFragmentStackPush(stack, e);

    return 1;
}

int regexOperatorReturnCreate(regex_fragment_t **stack) {
    regex_fragment_t *e;
    regex_token_t *token = NULL;

    if((e = regexFragmentStackPop(stack)) == NULL) {
        return 0;
    }
    if(!regexTokenCreate(&token, eTokenReturn, 0, NULL, 0)) {
        return 0;
    }
    regexPtrlistPatch(e->ptrlist, token);
    regexFragmentStackPush(stack, e);

    return 1;
}

int regexHasSufficientOperands(regex_fragment_t *fragments, int arity) {
    for(; ((fragments != NULL) && (arity)); arity--, fragments = fragments->next);
    return arity == 0;
}

eRegexCompileStatus_t regexOperatorApply(regex_token_t **operators, eRegexOpApply apply, eRegexToken_t tokenType, regex_fragment_t **operands) {
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

eRegexCompileStatus_t regexShuntingYardFragment(regex_token_t **tokens, regex_fragment_t **root_stack,
                                                regex_sub_index_t **sub_index, int sub_expression,
                                                unsigned int group_flags) {
    regex_token_t *token = NULL, *routine, *operators = NULL;
    regex_fragment_t *operands = NULL, *subexpr;
    eRegexCompileStatus_t status;
    int group_num;

    if((sub_expression >= 0) || (sub_expression == REGEX_SHUNTING_YARD_NO_CAPTURE)) {
        operands = *root_stack;
        if(operands != NULL) {
            if(!regexTokenCreate(&token, eTokenConcatenation, 0, NULL, 0)) {
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
                if((subexpr = regexSubroutineGet(*sub_index, token->sub_sequence)) == NULL) {
                    routine = token->sub_sequence;
                    if((status = regexShuntingYardFragment(&(token->sub_sequence), &subexpr, sub_index,
                                                           REGEX_SHUNTING_YARD_NO_CAPTURE,
                                                           REGEX_TOKEN_FLAG_SUBROUTINE)) != eCompileOk) {
                        SET_YARD_RESULT(status);
                    }
                    if(!regexSubroutineStore(sub_index, routine, subexpr)) {
                        SET_YARD_RESULT(eCompileOutOfMem);
                    }
                }
                token->out_b = subexpr->token;
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
                if((status = regexShuntingYardFragment(tokens, &subexpr, sub_index,
                                                       group_num, token->flags)) != eCompileOk) {
                    SET_YARD_RESULT(status);
                }
                regexFragmentStackPush(&operands, subexpr);
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
                    if(!regexTokenCreate(&token, eTokenConcatenation, 0, NULL, 0)) {
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
                *root_stack = operands;
                return eCompileOk;
        }
    }

    if((status = regexOperatorApply(&operators, OP_ALL, 0, &operands)) != eCompileOk) {
        goto ShuntingYardFailure;
    }

    if(group_flags & REGEX_TOKEN_FLAG_SUBROUTINE) {
        printf("Creating return instruction\n");
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
    regexFragmentFree(operands);
    regexTokenDestroy(operators, 1);
    operands = NULL;

    return status;
}

eRegexCompileStatus_t regexShuntingYard(regex_token_t **tokens) {
    regex_fragment_t *stack = NULL;
    eRegexCompileStatus_t status;
    regex_sub_index_t *sub_index = NULL;

    status = regexShuntingYardFragment(tokens, &stack, &sub_index, REGEX_SHUNTING_YARD_NO_PARENT, 0);
    regexSubroutineFree(&sub_index);
    if(status != eCompileOk) {
        regexFragmentFree(stack);
        return status;
    }

    *tokens = stack->token;
    stack->token = NULL;
    regexFragmentFree(stack);

    return status;
}

/////////////////////////////////////////////////////////////////////////////
// Regex VM builder and evaluator
/////////////////////////////////////////////////////////////////////////////

// String table /////////////////////////////////////////////////////////////

int regexVMStringTableEntryAdd(regex_vm_build_t *build, const char *str, int len) {
    int index, count = build->vm->string_tbl_size, deduped;

    if((index = tableEntryAdd((void ***)&(build->vm->string_table),
                              &(build->vm->string_tbl_size),
                              len + 1, (void *)str, REGEX_TABLE_DEDUPE,
                              &deduped)) == -1) {
        return -1;
    }
    if(!deduped) {
        build->vm->string_table[index][len] = '\0';
        return tableEntryAdd((void ***)&(build->vm->string_tbl_len),
                             &count, 0, (void *)len,
                             REGEX_TABLE_KEEP_PTR, NULL);
    }
    return index;
}

const char *regexVMStringTableEntryGet(regex_vm_t *vm, int string_table_id, int *len) {
    if(string_table_id >= vm->string_tbl_size) {
        return NULL;
    }
    *len = vm->string_tbl_len[string_table_id];
    return vm->string_table[string_table_id];
}

void regexVMStringTableFree(regex_vm_t *vm) {
    int count = vm->string_tbl_size;

    tableEntryFree((void ***)&(vm->string_table), &(vm->string_tbl_size), 1);
    tableEntryFree((void ***)&(vm->string_tbl_len), &count, 0);
}

// Class table //////////////////////////////////////////////////////////////

int regexVMClassTableEntryAdd(regex_vm_build_t *build, const unsigned int *bitmap) {
    return tableEntryAdd((void ***)&(build->vm->class_table),
                         &(build->vm->class_tbl_size), 32, (void *)bitmap,
                         REGEX_TABLE_DEDUPE | REGEX_TABLE_KEEP_PTR, NULL);
}

const unsigned int *regexVMClassTableEntryGet(regex_vm_t *vm, int class_table_id) {
    return (unsigned int *)tableEntryGet((void **)vm->class_table, vm->class_tbl_size, class_table_id);
}

void regexVMClassTableFree(regex_vm_t *vm) {
    tableEntryFree((void ***)&(vm->class_table), &(vm->class_tbl_size), 1);
}

// utf8 table ///////////////////////////////////////////////////////////////

int regexVMUtf8TableEntryAdd(regex_vm_build_t *build, const unsigned int *bitmap) {
    return tableEntryAdd((void ***)&(build->vm->utf8_class_table),
                         &(build->vm->utf8_tbl_size), 8, (void *)bitmap,
                         REGEX_TABLE_KEEP_PTR | REGEX_TABLE_DEDUPE, NULL);
}

const unsigned int *regexVMUtf8TableEntryGet(regex_vm_t *vm, int utf8_table_id) {
    if(utf8_table_id >= vm->utf8_tbl_size) {
        return NULL;
    }
    return vm->utf8_class_table[utf8_table_id];
}

void regexVMUtf8TableFree(regex_vm_t *vm) {
    tableEntryFree((void ***)&(vm->utf8_class_table), &(vm->utf8_tbl_size), 1);
}

// Group table //////////////////////////////////////////////////////////////

int regexVMGroupTableEntryAdd(regex_vm_build_t *build, const char *group, int len, int index) {
    int k;

    for(k = build->vm->group_tbl_size; k < index; k++) {
        if(tableEntryAdd((void ***)&(build->vm->group_table), &(build->vm->group_tbl_size),
                         0, NULL, REGEX_TABLE_KEEP_PTR, NULL) == -1) {
            return 0;
        }
    }
    if(group == NULL) {
        if((tableEntryAdd((void ***)&(build->vm->group_table),
                          &(build->vm->group_tbl_size),
                          0, NULL, REGEX_TABLE_KEEP_PTR, NULL)) != index) {
            return 0;
        }
    } else {
        if((tableEntryAdd((void ***)&(build->vm->group_table),
                          &(build->vm->group_tbl_size),
                          len + 1, (void *)group, 0, NULL)) != index) {
            return 0;
        }
        build->vm->group_table[index][len] = '\0';
    }
    return 1;
}

void regexVMGroupTableFree(regex_vm_t *vm) {
    tableEntryFree((void ***)&(vm->group_table), &(vm->group_tbl_size), 1);
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

    fprintf(fp, "unsigned int *_%s_utf_class_table[] = {\n    ", symbol, k);
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
    unsigned int instr[3];
    const char *name;

    for(pc = 0; pc < vm->size; pc++) {
        REGEX_VM_INSTR_DECODE(instr, vm, pc);
        fprintf(fp, "%3.3d  ", pc);
        switch(instr[0]) {
            case eTokenCharLiteral:
                fprintf(fp, "char (%c:%d)%s\n",
                        ((instr[1] < 32 || instr[1] > 127) ? '-' : instr[1]), instr[1],
                        (instr[2] ? " inverted" : ""));
                break;
            case eTokenStringLiteral:
                fputs("string(\"", fp);
                regexEmitEscapedStr(fp, vm->string_table[instr[1]], vm->string_tbl_len[instr[1]]);
                fputs("\")\n", fp);
                break;
            case eTokenCharClass:
                fprintf(fp, "class([");
                regexPrintCharClassToFP(fp, vm->class_table[instr[1]]);
                fprintf(fp, "])\n");
                break;
            case eTokenCharAny:
                fprintf(fp, "anychar\n");
                break;
            case eTokenSave:
                fprintf(fp, "save %d", instr[1]);
                if((name = regexVMGroupNameFromIndex(vm, (((int)instr[1])/2) + 1)) != NULL) {
                    fprintf(fp, " [%s]", name);
                }
                fprintf(fp, "%s\n", (instr[2] ? " compound" : ""));
                break;
            case eTokenSplit:
                fprintf(fp, "split %d, %d\n", instr[1], instr[2]);
                break;
            case eTokenMatch:
                fprintf(fp, "match\n");
                break;
            case eTokenJmp:
                fprintf(fp, "jmp %d\n", instr[1]);
                break;
            case eTokenUtf8Class:
                fprintf(fp, "utf8 [%d]\n", instr[1]);
                break;
            case eTokenCharAnyDotAll:
                fprintf(fp, "anybyte\n");
                break;
            case eTokenCall:
                fprintf(fp, "call %d\n", instr[1]);
                break;
            case eTokenReturn:
                fprintf(fp, "return\n");
                break;
            case eTokenByte:
                fprintf(fp, "byte\n");
                break;
            default:
                fprintf(fp, "UNKNOWN [%d]!\n", instr[0]);
                return;
        }
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

// Return a compiled regex, or an error and position within the pattern
eRegexCompileStatus_t regexCompile(regex_compile_ctx_t *ctx, const char *pattern,
                                   unsigned int flags) {
    regex_vm_build_t build;

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
        regexVMBuildDestroy(&build);
        ctx->vm = NULL;
        return ctx->status;
    }
    regexSubroutineIdxFree(&build);

    // Next, convert the infix form of the regular expression to postfix form,
    // and derive an NFA representation. We accomplish this using the shunting
    // yard algorithm. This is then converted into a VM bytecode sequence, for
    // runtime evaluation.

    if((ctx->status = regexShuntingYard(&(build.tokens))) != eCompileOk) {
        regexVMBuildDestroy(&build);
        ctx->vm = NULL;
        return ctx->status;
    }

    if(!regexVMProgramGenerate(&build, build.tokens)) {
        regexVMBuildDestroy(&build);
        ctx->vm = NULL;
        ctx->status = eCompileOutOfMem;
        return ctx->status;
    }

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

    while(regexVMNoTextAdvance(eval->vm->program[thread->pc])) {
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

regex_match_t *regexMatch(regex_vm_t *vm, const char *text, int len, int anchored) {
    regex_match_t *match;
    regex_eval_t *eval;
    regex_thread_t *thread;
    size_t capture_size;
    size_t compound_size;
    unsigned char c;
    int k;

    if(vm->vm_version != REGEX_VM_MACHINE_VERSION) {
        return NULL;
    }

    if((eval = regexEvalCreate(vm, text, len)) == NULL) {
        return NULL;
    }

    if(!regexThreadCreate(eval, NULL, 0, 1)) {
        regexEvalFree(eval);
        return NULL;
    }

    for(; eval->pos != eval->len; eval->sp++, eval->pos++) {
        c = *eval->sp;
        if(eval->queue == NULL) {
            break;
        }
        eval->thread = eval->queue;
        eval->queue = NULL;
        for(thread = eval->thread; thread != NULL; thread = eval->thread) {
            eval->thread = thread->next;
            switch(regexThreadProcess(eval, thread, anchored)) {
                default:
                case eEvalStackOverrun:
                case eEvalInternalError:
                case eEvalOutOfMem:
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

void printEscapedStr(FILE *fp, const char *text, int len) {
    int k;
    int codepoint;
    char fmt[10];

    for(k = 0; k < len; k++) {
        // Check for utf8 encoding
        if((codepoint = parseUtf8DecodeSequence(text + k)) != -1) {
            if(codepoint > 0xFFFF) {
                k += 3;
                fprintf(fp, "\\U%6.6X", codepoint);
            } else {
                if(codepoint > 2047) {
                    k += 2;
                } else if(codepoint > 127) {
                    k += 1;
                }
                snprintf(fmt, sizeof(fmt) - 1, "\\u%4.4X", codepoint);
                fmt[6] = '\0';
                fputs(fmt, fp);
            }
        } else {
            switch(text[k]) {
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
                    if((text[k] >= ' ') && (text[k] <= 127)) {
                        fputc(text[k], fp);
                    } else {
                        fprintf(fp, "\\x%2.2X", text[k]);
                    }
                    break;
            }
        }
    }
}

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
            printEscapedStr(stdout, ptr, len);
            printf("]\n");
            if((m = regexGroupCompoundCountGet(match, k)) > 0) {
                for(n = 0; n < m; n++) {
                    ptr = regexGroupCompoundValueGet(match, k, n, &len);
                    printf("            %d [", n);
                    printEscapedStr(stdout, ptr, len);
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
            printf("Compile failed: %s", regexGetCompileStatusStr(result.status));
            return 1;
        }
        if(argc > 2) {
            regexVMPrintProgram(stdout, result.vm);
            printf("-------------------------\n");

            printf("Evaluating [%s]\n", argv[2]);
            printf(" (escaped) [");
            printEscapedStr(stdout, argv[2], strlen(argv[2]));
            printf("]\n");
            if((match = regexMatch(result.vm, argv[2], REGEX_STR_NULL_TERMINATED, 0)) == NULL) {
                printf("No match\n");
                return 1;
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

// TODO - integrate directly
#include "unicode_props.c"

#endif // MOJO_REGEX_IMPLEMENTATION
#endif // _MOJO_REGEX_HEADER_
