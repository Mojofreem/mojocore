#ifndef _MOJO_REGEX_HEADER_
#define _MOJO_REGEX_HEADER_

#define MOJO_REGEX_COMPILE_IMPLEMENTATION
#define MOJO_REGEX_EVALUATE_IMPLEMENTATION
#define MOJO_REGEX_IMPLEMENTATION

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "unicode_props.c"

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
        \d [0-9]
        \D [^0-9]
        \s [ \t\f\v\r\n]
        \S [^ \t\f\v\r\n]
        \w [a-zA-Z0-9_] - TODO adapt for unicode utf8 sequences
        \W [^a-zA-Z0-9_] - TODO adapt for unicode utf8 sequences
        \p{M} - combining mark (M_)
        \p{N} - numeric digit (N_)
        \p{P} - punctuation (P_)
        \p{Z} - any unicode whitespace (Z_)
        \p{L} - any unicode letter (L_)
        \p{Lu} - uppercase unicode letter (Lu)
        \p{Ll} - lowercase unicode letter (Ll)
        \B - match a byte (differs from . in that it always matches a single byte, even '\n')

    TODO
        \X full unicode glyph (may be multiple chars)
        ^  start of string (assertion, non consuming)
        $  end of string (assertion, non consuming)
        \< match start of word (assertion, non consuming)
        \> match end of word (assertion, non consuming)
        eTokenUnicodeLiteral
        eTokenAnyCharDotAll
        unicode utf8 classes
            build NFA from class tree
        eTokenCharAny - prevent match on newline without DOTALL flag
        eTokenCharAny - handle multibyte utf8
        eTokenCharLiteral - handle inverse
        eTokenCharAnyDotAll
        eTokenCall
        eTokenReturn

    (?P<name>...)  named subexpressions
    (?:...) non capturing subexpressions - TODO
    (?*...) compound subexpressions - TODO
    (?i) case insensitive match - TODO

    TODO
        subexpression meta prefix rework
        multiple prefixes may be defined, but must be at the head of the subexpression
        (?P<name>...) and (?:...) are mutually exclusive (why name something unused?)
        (?:...) and (?*...) are mutually exclusive (compound non-captures?)
        unicode character classes
        multipass char class parsing, to separate ascii and unicode handling
        unicode compilation toggle - do NOT generate unicode char classes in non
            unicode mode

    TODO: Future work (refining, not MVP critical)
        mark predefined unicode char classes to prevent attempte dealloc
        allow unregistering unicode char classes
        tag meta char class -> char class -> vm path, for better runtime debug

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
        eTokenMatch             5
        eTokenSplit             6       program counter         program counter
        eTokenJmp               7       program counter
        eTokenSave              8       subexpression number
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

create utf8 class table
output utf8 class table
*/



/////////////////////////////////////////////////////////////////////////////

#define REGEX_VM_MACHINE_VERSION    5

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
typedef void *(*regexMemReallocator_t)(void *ptr, size_t size, void *ctx);

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
    eCompileInternalError
} eRegexCompileStatus;

#define REGEX_UNICODE           0x01
#define REGEX_CASE_INSENSITIVE  0x02
#define REGEX_NO_CAPTURE        0x04
#define REGEX_DOTALL            0x08

#define REGEX_STR_NULL_TERMINATED   -1

typedef struct regex_compile_ctx_s regex_compile_ctx_t;
struct regex_compile_ctx_s {
    eRegexCompileStatus status;
    const char *pattern;
    int position;
    regex_vm_t *vm;
};

const char *regexGetCompileStatusStr(eRegexCompileStatus status);
eRegexCompileStatus regexCompile(regex_compile_ctx_t *ctx, const char *pattern,
                                 unsigned int flags);

#ifndef uint32_t
typedef unsigned int uint32_t;
#endif // uint32_t

#ifndef uint8_t
typedef unsigned char uint8_t;
#endif // uint8_t

#endif // MOJO_REGEX_COMPILE_IMPLEMENTATION

#ifdef MOJO_REGEX_EVALUATE_IMPLEMENTATION

typedef struct regex_match_s regex_match_t;
struct regex_match_s {
    const char *text;
    int len;
    const char *pos;
    regex_vm_t *vm;
    const char *subexprs[0];
};

regex_match_t *regexMatch(regex_vm_t *vm, const char *text, int len, int complete);
void regexMatchFree(regex_match_t *match);
const char *regexGroupValueGet(regex_match_t *match, int group, int *len);
const char *regexGroupValueGetByName(regex_match_t *match, const char *name, int *len);

#endif // MOJO_REGEX_EVALUATE_IMPLEMENTATION

void regexVMFree(regex_vm_t *vm);
void regexVMGenerateDeclaration(regex_vm_t *vm, const char *symbol, FILE *fp);
void regexVMGenerateDefinition(regex_vm_t *vm, const char *symbol, FILE *fp);
void regexVMPrintProgram(FILE *fp, regex_vm_t *vm);

int regexGroupCountGet(regex_vm_t *vm);
const char *regexGroupNameLookup(regex_vm_t *vm, int group);
int regexGroupIndexLookup(regex_vm_t *vm, const char *name);

/////////////////////////////////////////////////////////////////////////////
#if defined(MOJO_REGEX_IMPLEMENTATION)
/////////////////////////////////////////////////////////////////////////////

// Delegated memory allocation handlers /////////////////////////////////////

static void *regexDefMemAllocator(size_t size, void *ctx) {
    return malloc(size);
}

static void regexDefMemDeallocator(void *ptr, void *ctx) {
    free(ptr);
}

static void *regexDefMemReallocator(void *ptr, size_t size, void *ctx) {
    return realloc(ptr, size);
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

// CRC calculation functions ////////////////////////////////////////////////

// Public domain from http://home.thep.lu.se/~bjorn/crc/

uint32_t crc32_for_byte(uint32_t r) {
    for(int j = 0; j < 8; ++j)
        r = (r & 1? 0: (uint32_t)0xEDB88320L) ^ r >> 1;
    return r ^ (uint32_t)0xFF000000L;
}

void crc32(const void *data, size_t n_bytes, uint32_t* crc) {
    static uint32_t table[0x100];
    if(!*table)
        for(size_t i = 0; i < 0x100; ++i)
            table[i] = crc32_for_byte(i);
    for(size_t i = 0; i < n_bytes; ++i)
        *crc = table[(uint8_t)*crc ^ ((uint8_t*)data)[i]] ^ *crc >> 8;
}

// Data structures and function declarations ////////////////////////////////

#define DEF_VM_SIZE_INC 1024
#define DEF_VM_STRTBL_INC 32

#define VM_PC_UNVISITED     ((int)-1)
#define VM_PC_PENDING_ASSIGNMENT    ((int)-2)

#define META_DIGITS_PATTERN     "0-9"
#define META_WHITESPACE_PATTERN " \\t\\f\\v\\r\\n"
#define META_WORD_PATTERN       "a-zA-Z0-9_"

#define META_CLASS(pattern)     pattern
#define META_CLASS_INV(pattern) "^" pattern

typedef enum {
    // Maps directly to VM opcodes
    eTokenNone,
    eTokenCharLiteral,
    eTokenCharClass,
    eTokenStringLiteral,
    eTokenCharAny,
    eTokenMatch,
    eTokenSplit,
    eTokenJmp,
    eTokenSave,
    eTokenUtf8Class,
    eTokenCharAnyDotAll,
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
    eTokenOneOrMany,
    eTokenSubExprStart,
    eTokenSubExprEnd
} eRegexToken;

typedef enum {
    ePriorityNone,
    ePriorityLow,
    ePriorityMedium,
    ePriorityHigh
} eRegexTokenPriority;

typedef struct regex_token_s regex_token_t;
struct regex_token_s {
    eRegexToken tokenType;
    union {
        int c;
        char *str;
        unsigned int *bitmap;
        int group;
        regex_token_t *sub_sequence;
    };
    int pc; // program counter index, for NFA -> VM
    int len; // string length, to allow embedded \0 chars
    regex_token_t *out_a;
    regex_token_t *out_b;
    regex_token_t *next;
};

typedef struct regex_vm_build_s regex_vm_build_t;
struct regex_vm_build_s {
    regex_vm_t *vm;
    regex_token_t *tokens;
    unsigned int flags;
    int string_tbl_count;
    int class_tbl_count;
    int utf8_tbl_count;
    int groups;
    int pc;
};

int regexVMGroupTableEntryAdd(regex_vm_build_t *build, const char *group, int len, int index);

/////////////////////////////////////////////////////////////////////////////
// Unicode char class registration functions
/////////////////////////////////////////////////////////////////////////////

typedef struct regex_unicode_charclass_entry_s regex_unicode_charclass_entry_t;
struct regex_unicode_charclass_entry_s {
    char id[3];
    const char *class_str;
    regex_unicode_charclass_entry_t *next;
};

regex_unicode_charclass_entry_t _utf8_class_M_ = {
        .id = "M",
        .class_str = unicode_combining_marks,
        .next = NULL
};

regex_unicode_charclass_entry_t _utf8_class_N_ = {
        .id = "N",
        .class_str = unicode_numeric,
        .next = &_utf8_class_M_
};

regex_unicode_charclass_entry_t _utf8_class_P_ = {
        .id = "P",
        .class_str = unicode_punctuation,
        .next = &_utf8_class_N_
};

regex_unicode_charclass_entry_t _utf8_class_Z_ = {
        .id = "Z",
        .class_str = unicode_whitespace,
        .next = &_utf8_class_P_
};

regex_unicode_charclass_entry_t _utf8_class_Lu = {
        .id = "Lu",
        .class_str = unicode_uppercase,
        .next = &_utf8_class_Z_
};

regex_unicode_charclass_entry_t _utf8_class_Ll = {
        .id = "Ll",
        .class_str = unicode_lowercase,
        .next = &_utf8_class_Lu
};

regex_unicode_charclass_entry_t _utf8_class_L_ = {
        .id = "L",
        .class_str = unicode_letter,
        .next = &_utf8_class_Ll
};

regex_unicode_charclass_entry_t *_regex_unicode_charclass_registry = &_utf8_class_L_;

int regexRegUnicodeCharClass(const char *classId, const char *classStr) {
    regex_unicode_charclass_entry_t *entry, *walk;

    if((classId[0] == '\0') || ((classId[1] != '\0') && (classId[2] != '\0'))) {
        return 0;
    }

    if((entry = _regexAlloc(sizeof(regex_unicode_charclass_entry_t), _regexMemContext)) == NULL) {
        return 0;
    }
    memset(entry, 0, sizeof(regex_unicode_charclass_entry_t));
    entry->id[0] = classId[0];
    entry->id[1] = classId[1];
    entry->id[2] = '\0';
    entry->class_str = classStr;
    if(_regex_unicode_charclass_registry == NULL) {
        _regex_unicode_charclass_registry = entry;
    } else {
        for(walk = _regex_unicode_charclass_registry; walk->next != NULL; walk = walk->next);
        walk->next = entry;
    }
    return 1;
}

const char *regexGetUnicodeCharClass(const char *classId) {
    regex_unicode_charclass_entry_t *entry;

    for(entry = _regex_unicode_charclass_registry; entry != NULL; entry = entry->next) {
        if((classId[0] == entry->id[0]) && (classId[1] == entry->id[1])) {
            return entry->class_str;
        }
    }
    return NULL;
}

/////////////////////////////////////////////////////////////////////////////
// Token management functions
/////////////////////////////////////////////////////////////////////////////

int regexTokenIsTerminal(regex_token_t *token, int preceeding) {
    switch(token->tokenType) {
        case eTokenCharLiteral:
        case eTokenStringLiteral:
        case eTokenCharClass:
        case eTokenByte:
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

regex_token_t *regexAllocToken(eRegexToken tokenType, int c, char *str, int size) {
    regex_token_t *token;

    if((token = _regexAlloc(sizeof(regex_token_t), _regexMemContext)) == NULL) {
        return NULL;
    }
    memset(token, 0, sizeof(regex_token_t));
    token->tokenType = tokenType;
    if(str != NULL) {
        token->str = str;
        token->len = size;
    } else {
        token->c = c;
    }
    return token;
}

int regexTokenCreate(regex_token_t **list, eRegexToken tokenType, int c, char *str, int size) {
    regex_token_t *token, *walk;

    if((token = regexAllocToken(tokenType, c, str, size)) == NULL) {
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

    if(stack) {
        for(; token != NULL; token = next) {
            next = token->next;
            switch(token->tokenType) {
                case eTokenCharClass:
                case eTokenStringLiteral:
                    if(token->str != NULL) {
                        _regexDealloc(token->str, _regexMemContext);
                    }
                    break;
                default:
                    break;
            }
            _regexDealloc(token, _regexMemContext);
        }
    } else {
        if(token->pc != VM_PC_UNVISITED) {
            return;
        }
        token->pc = VM_PC_UNVISITED;
        regexTokenDestroy(token->out_a, 0);
        regexTokenDestroy(token->out_b, 0);
    }
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
} eRegexPatternCharState;

typedef struct parseChar_s parseChar_t;
struct parseChar_s {
    eRegexPatternCharState state;
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
                    if(regexGetUnicodeCharClass(classId) == NULL) {
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
                for(k = 0, result.c = 0; k < 4; k++) {
                    (*pattern)++;
                    result.c *= 16;
                    if(parseIsHexdigit(**pattern)) {
                        result.c += parseGetHexValue(**pattern);
                    } else {
                        return result;
                    }
                }
                result.state = eRegexPatternUnicode;
                return result;

            case 'U': // unicode codepoint (0x010000 - 0x10FFFF)
                for(k = 0, result.c = 0; k < 6; k++) {
                    (*pattern)++;
                    result.c *= 16;
                    if(parseIsHexdigit(**pattern)) {
                        result.c += parseGetHexValue(**pattern);
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

unsigned int parseUtf8EncodeCodepoint(int c) {
    if(c <= 127) { // 1 byte
        return (unsigned int)c;
    } else if(c <= 2047) { // 2 byte (5 prefix bits)
        return 0xC080u | ((unsigned int)c & 0x3Fu) | (((unsigned int)c & 0x7C0u) << 2u);
    } else if(c <= 65535) { // 3 byte (4 prefix bits)
        return 0xE08080u | ((unsigned int)c & 0x3Fu) | (((unsigned int)c & 0xFC0u) << 2u) | (((unsigned int)c & 0xF000u) << 4u);
    } else { // 4 byte (3 prefix bits)
        return 0xF0808080u | ((unsigned int)c & 0x3Fu) | (((unsigned int)c & 0xFC0u) << 2u) | (((unsigned int)c & 0x3F000u) << 4u) | ((unsigned int)c & 0x7000000u);
    }
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
    unsigned char bitmap[8];
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
    unsigned char one_byte[32];
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
    memset(midlow, 0, sizeof(utf8_charclass_midlow_byte_t));
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
    memset(midhigh, 0, sizeof(utf8_charclass_midlow_byte_t));
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
    memset(high, 0, sizeof(utf8_charclass_midlow_byte_t));
    high->prefix = prefix;
    if(last == NULL) {
        *base = high;
    } else {
        last->next = high;
    }
    return high;
}

unsigned char *parseUtf8TreeGetLowByte(utf8_charclass_tree_t *tree, unsigned char *bytes) {
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

#define UTF8_ENC_LOW_BYTE_FULL_RANGE        0x3F // 10xxxxxx
#define UTF8_ENC_ONE_BYTE_FULL_RANGE        0x7F // 0xxxxxxx
#define UTF8_ENC_HIGH_TWO_BYTE_FULL_RANGE   0x1F // 110xxxxx
#define UTF8_ENC_HIGH_THREE_BYTE_FULL_RANGE 0x0F // 1110xxxx
#define UTF8_ENC_HIGH_FOUR_BYTE_FULL_RANGE  0x07 // 11110xxx

int parseUtf8CharClassCodepoint(utf8_charclass_tree_t *tree, int codepoint) {
    unsigned char *bitmap;
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
    bitmap[byte / 8] |= 0x1u << (byte % 8u);
    return 1;
}

void parseUtf8SetCharClassBitmapRange(unsigned char *bitmap, int start, int end) {
    int k;

    for(k = start; k <= end; k++) {
        bitmap[k / 8] |= 0x1u << (k % 8u);
    }
}

int parseUtf8SetTreeClassBitmapRange(utf8_charclass_tree_t *tree, int count,
                                     int high, int midhigh, int midlow,
                                     int start, int end) {
    unsigned char *bitmap;
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

    unsigned char *bitmap;
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

int parseUtf8CharClassRange(utf8_charclass_tree_t *tree, int code_a, int code_b) {
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

utf8_charclass_tree_t *regexCharClassUtf8TreeCreate(void) {
    utf8_charclass_tree_t *tree;

    if((tree = _regexAlloc(sizeof(utf8_charclass_tree_t), _regexMemContext)) == NULL) {
        return NULL;
    }
    memset(tree, 0, sizeof(utf8_charclass_tree_t));
    return tree;
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

void charClassBitmapSet(unsigned int *bitmap, int pos) {
    unsigned int idx = pos / 32u;
    unsigned int bit = pos % 32u;

    bitmap[idx] |= (1u << bit);
}

int charClassBitmapCheck(const unsigned int *bitmap, int pos) {
    unsigned int idx = pos / 32u;
    unsigned int bit = pos % 32u;

    return (int)(bitmap[idx] & (1u << bit));
}

void charClassBitmapInvert(unsigned int *bitmap) {
    for(int k = 0; k < 8; k++) {
        bitmap[k] ^= (unsigned int)0xFFFFFFFFu;
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

/*
For the psuedo unicode char class:

    "\u000a\u0bcd-\u0bce\u0bfg\uhijk-\uhijl\uhmno"

A tree is generated:

    1 byte: a
    3 byte: b->c->d
            b->c->e
            b->f->g
    4 byte: h->i->j->k
            h->i->j->l
            h->m->n->o

And a sub sequence pattern is generated:

    (?:[a]|b(?:c(?:[d]|[e])|f[g])|h(?:ij(?:[k]|[l])|mn[o]))
*/

// The embedded flag indicates whether this class pattern is within the context
// of an actual pattern expression, or is just the content of the pattern, from
// a secodary source. An embedded pattern ends at the required ']' delimiter,
// whereas a non embedded pattern does not contain a trailing delimiter.
int parseCharClassAndCreateToken2(eRegexCompileStatus *status, const char **pattern, int embedded, regex_token_t **tokens) {
    unsigned int bitmap[8], *ptr;
    parseChar_t c;
    int invert = 0;
    int range = 0;
    int last = 0;
    int next;
    int unicode = 0; // flag indicating unicode characters in the class
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
                    unicode = 1;
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
                    charClassBitmapSet(bitmap, last);
                    range = 1;
                } else if(range == 1) {
                    if(c.state != eRegexPatternEscapedChar && c.c == '-') {
                        range = 2;
                    } else {
                        last = c.c;
                        charClassBitmapSet(bitmap, last);
                    }
                } else {
                    next = c.c;
                    for(; last <= next; last++) {
                        charClassBitmapSet(bitmap, last);
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

    if(unicode) {
        // TODO: Check bitmap for unicode overlap. If none, include BOTH utf8
        //       class AND regular char class. If overlap, transfer char class
        //       content to utf8 class
    } else {
        // Strictly single byte, use a regular char class only
        if(invert) {
            charClassBitmapInvert(bitmap);
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

int parseUnicodeClassAndCreateToken(eRegexCompileStatus *status, int unicodeClass, regex_token_t **tokens) {
    char classId[3];
    const char *classStr;

    classId[0] = (char)((unsigned int)unicodeClass & 0xFFu);
    classId[1] = (char)(((unsigned int)unicodeClass & 0xFF00u) >> 8u);
    classId[2] = '\0';

    if((classStr = regexGetUnicodeCharClass(classId)) == NULL) {
        *status = eCompileUnknownUnicodeClass;
        return 0;
    }

    return parseCharClassAndCreateToken2(status, &classStr, 0, tokens);
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

eRegexCompileStatus regexTokenizePattern(const char *pattern,
                                         int *pos,
                                         regex_token_t **tokens,
                                         regex_vm_build_t *build) {
    eRegexCompileStatus result;
    const char *start = pattern;
    const char *class;
    parseChar_t c;
    int response;
    int len;
    char *str;
    eRegexCompileStatus status;
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
                        if(!parseCharClassAndCreateToken2(&status, &pattern, 1, tokens)) {
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
                        build->groups++;
                        subexpr = build->groups;
                        named = 0;
                        if(parseCheckNextPatternChar(&pattern, '?')) {
                            // Meta character subexpression modifier
                            if(parseCheckNextPatternChar(&pattern, 'i')) {
                                // Case insensitive matching (NOT an actual sub expression)
                                // TODO
                            } else if(parseCheckNextPatternChar(&pattern, 'P')) {
                                // Named sub expression
                                if(!parseCheckNextPatternChar(&pattern, '<')) {
                                    SET_RESULT(eCompileMalformedSubExprName);
                                }
                                if((response = regexSubexprLookupEntryCreate(build, &pattern, subexpr)) != 1) {
                                    SET_RESULT((response == 0 ? eCompileOutOfMem : eCompileMalformedSubExprName));
                                }
                                named = 1;
                            } else if(parseCheckNextPatternChar(&pattern, ':')) {
                                // Non-capturing subexpression
                                // TODO
                            } else if(parseCheckNextPatternChar(&pattern, '*')) {
                                // Compound capturing subexpression
                                // TODO
                            } else {
                                SET_RESULT(eCompileUnsupportedMeta);
                            }
                        }
                        if(!named) {
                            if((response = regexSubexprLookupEntryCreate(build, NULL, subexpr)) != 1) {
                                SET_RESULT((response == 0 ? eCompileOutOfMem : eCompileMalformedSubExprName));
                            }
                        }
                        if(!regexTokenCreate(tokens, eTokenSubExprStart, subexpr, NULL, 0)) {
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
                        // TODO: derive from the unicode DB
                        class = META_CLASS(META_WORD_PATTERN);
                        break;

                    case 'W': // non word character
                        // TODO: derive from the unicode DB
                        class = META_CLASS_INV(META_WORD_PATTERN);
                        break;
                }
                if(!parseCharClassAndCreateToken2(&status, &class, 0, tokens)) {
                    SET_RESULT(status);
                }
                continue;

            case eRegexPatternUnicodeMetaClass:
                printf("utf8 class: %c%c\n", (c.c & 0xFF00) >> 8, c.c & 0xFF);
                if(!parseUnicodeClassAndCreateToken(&status, c.c, tokens)) {
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
        case eTokenByte:
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

int regexHasSufficientOperands(regex_fragment_t *fragments, int arity) {
    for(; ((fragments != NULL) && (arity)); arity--, fragments = fragments->next);
    return arity == 0;
}

eRegexCompileStatus regexOperatorApply(regex_token_t **operators, eRegexOpApply apply, eRegexToken tokenType, regex_fragment_t **operands) {
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

#define SET_YARD_RESULT(res)    status = res; goto ShuntingYardFailure;

eRegexCompileStatus regexShuntingYardFragment(regex_token_t **tokens, regex_fragment_t **root_stack, int sub_expression) {
    regex_token_t *token = NULL, *operators = NULL;
    regex_fragment_t *operands = NULL, *subexpr;
    eRegexCompileStatus status;
    int group_num;

    if(sub_expression >= 0) {
        operands = *root_stack;
        if(!regexTokenCreate(&token, eTokenConcatenation, 0, NULL, 0)) {
            return eCompileOutOfMem;
        }
        regexTokenStackPush(&operators, token);
    }

    while((token = regexTokenStackPop(tokens)) != NULL) {
        switch(token->tokenType) {
            default:
                return eCompileInternalError;
            case eTokenCharLiteral:
            case eTokenCharClass:
            case eTokenStringLiteral:
            case eTokenCharAny:
            case eTokenByte:
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
                token->group = regexSubexprStartFromGroup(token->group);
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
                token->group = regexSubexprEndFromGroup(sub_expression);
                if(!regexOperatorSubexprCreate(&operands, token)) {
                    SET_YARD_RESULT(eCompileOutOfMem);
                }
                token = NULL;
                if(!regexTokenCreate(&token, eTokenConcatenation, 0, NULL, 0)) {
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
    regexTokenDestroy(operators, 1);
    operands = NULL;

    return status;
}

eRegexCompileStatus regexShuntingYard(regex_token_t **tokens) {
    regex_fragment_t *stack = NULL;
    eRegexCompileStatus status;

    status = regexShuntingYardFragment(tokens, &stack, -1);
    if(status != eCompileOk) {
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

void regexVMStringTableFree(regex_vm_t *vm) {
    int k;
    if(vm->string_table != NULL) {
        for(k = 0; k < vm->string_tbl_size; k++) {
            if(vm->string_table[k] != NULL) {
                _regexDealloc(vm->string_table[k], _regexMemContext);
            }
        }
        _regexDealloc(vm->string_table, _regexMemContext);
    }
    if(vm->string_tbl_len != NULL) {
        _regexDealloc(vm->string_tbl_len, _regexMemContext);
    }
    vm->string_table = NULL;
    vm->string_tbl_size = 0;
}

int regexVMStringTableEntryAdd(regex_vm_build_t *build, const char *str, int len) {
    int index;
    char *dup;

    for(index = 0; index < build->string_tbl_count; index++) {
        if((build->vm->string_tbl_len[index] == len) &&
           (!memcmp(build->vm->string_table[index], str, len))) {
            return index;
        }
    }

    if(build->vm->string_tbl_size <= build->string_tbl_count) {
        if((build->vm->string_table = _regexRealloc(build->vm->string_table, (build->vm->string_tbl_size + DEF_VM_STRTBL_INC) * sizeof(char *), _regexMemContext)) == NULL) {
            return -1;
        }
        memset(build->vm->string_table + build->vm->string_tbl_size, 0, DEF_VM_STRTBL_INC * sizeof(char *));
        if((build->vm->string_tbl_len = _regexRealloc(build->vm->string_tbl_len, (build->vm->string_tbl_size + DEF_VM_STRTBL_INC) * sizeof(int), _regexMemContext)) == NULL) {
            return -1;
        }
        memset(build->vm->string_tbl_len + build->vm->string_tbl_size, 0, DEF_VM_STRTBL_INC * sizeof(int));
        build->vm->string_tbl_size += DEF_VM_STRTBL_INC;
    }
    if((dup = _regexAlloc(len + 1, _regexMemContext)) == NULL) {
        return -1;
    }
    memcpy(dup, str, len);
    dup[len] = '\0';
    build->vm->string_table[build->string_tbl_count] = dup;
    build->vm->string_tbl_len[build->string_tbl_count] = len;
    index = build->string_tbl_count;
    build->string_tbl_count++;
    return index;
}

const char *regexVMStringTableEntryGet(regex_vm_t *vm, int string_table_id, int *len) {
    if(string_table_id >= vm->string_tbl_size) {
        return NULL;
    }
    *len = vm->string_tbl_len[string_table_id];
    return vm->string_table[string_table_id];
}

// Class table //////////////////////////////////////////////////////////////

void regexVMClassTableFree(regex_vm_t *vm) {
    int k;
    if(vm->class_table != NULL) {
        for(k = 0; k < vm->class_tbl_size; k++) {
            if(vm->class_table[k] != NULL) {
                _regexDealloc(vm->class_table[k], _regexMemContext);
            }
        }
        _regexDealloc(vm->class_table, _regexMemContext);
    }
    vm->class_table = NULL;
    vm->class_tbl_size = 0;
}

int regexVMClassTableEntryAdd(regex_vm_build_t *build, const unsigned int *bitmap) {
    int index;

    for(index = 0; index < build->class_tbl_count; index++) {
        if(!memcmp(build->vm->class_table[index], bitmap, 32)) {
            return index;
        }
    }

    if(build->vm->class_tbl_size <= build->class_tbl_count ) {
        if((build->vm->class_table = _regexRealloc(build->vm->class_table, (build->vm->class_tbl_size + DEF_VM_STRTBL_INC) * sizeof(unsigned int *), _regexMemContext)) == NULL) {
            return -1;
        }
        memset(build->vm->class_table + build->vm->class_tbl_size, 0, DEF_VM_STRTBL_INC * sizeof(unsigned int *));
        build->vm->class_tbl_size += DEF_VM_STRTBL_INC;
    }

    if((build->vm->class_table[build->class_tbl_count] = _regexAlloc(32, _regexMemContext)) == NULL) {
        return -1;
    }
    memcpy(build->vm->class_table[build->class_tbl_count], bitmap, 32);
    index = build->class_tbl_count;
    build->class_tbl_count++;
    return index;
}

const unsigned int *regexVMClassTableEntryGet(regex_vm_t *vm, int class_table_id) {
    if(class_table_id >= vm->class_tbl_size) {
        return NULL;
    }
    return vm->class_table[class_table_id];
}

// Class table //////////////////////////////////////////////////////////////

void regexVMUtf8TableFree(regex_vm_t *vm) {
    int k;
    if(vm->utf8_class_table != NULL) {
        for(k = 0; k < vm->utf8_tbl_size; k++) {
            if(vm->utf8_class_table[k] != NULL) {
                _regexDealloc(vm->utf8_class_table[k], _regexMemContext);
            }
        }
        _regexDealloc(vm->utf8_class_table, _regexMemContext);
    }
    vm->utf8_class_table = NULL;
    vm->utf8_tbl_size = 0;
}

int regexVMUtf8TableEntryAdd(regex_vm_build_t *build, const unsigned int *bitmap) {
    int index;

    for(index = 0; index < build->utf8_tbl_count; index++) {
        if(!memcmp(build->vm->utf8_class_table[index], bitmap, 8)) {
            return index;
        }
    }

    if(build->vm->utf8_tbl_size <= build->utf8_tbl_count ) {
        if((build->vm->utf8_class_table = _regexRealloc(build->vm->utf8_class_table, (build->vm->utf8_tbl_size + DEF_VM_STRTBL_INC) * sizeof(unsigned int *), _regexMemContext)) == NULL) {
            return -1;
        }
        memset(build->vm->utf8_class_table + build->vm->utf8_tbl_size, 0, DEF_VM_STRTBL_INC * sizeof(unsigned int *));
        build->vm->utf8_tbl_size += DEF_VM_STRTBL_INC;
    }

    if((build->vm->utf8_class_table[build->utf8_tbl_count] = _regexAlloc(8, _regexMemContext)) == NULL) {
        return -1;
    }
    memcpy(build->vm->utf8_class_table[build->utf8_tbl_count], bitmap, 8);
    index = build->utf8_tbl_count;
    build->utf8_tbl_count++;
    return index;
}

const unsigned int *regexVMUtf8TableEntryGet(regex_vm_t *vm, int utf8_table_id) {
    if(utf8_table_id >= vm->utf8_tbl_size) {
        return NULL;
    }
    return vm->utf8_class_table[utf8_table_id];
}

// Group table //////////////////////////////////////////////////////////////

void regexVMGroupTableFree(regex_vm_t *vm) {
    int k;
    if(vm->group_table != NULL) {
        for(k = 0; k < vm->group_tbl_size; k++) {
            if(vm->group_table[k] != NULL) {
                _regexDealloc(vm->group_table[k], _regexMemContext);
            }
        }
        _regexDealloc(vm->group_table, _regexMemContext);
    }
    vm->group_table = NULL;
    vm->group_tbl_size = 0;
}

const char *regexVMGroupNameFromIndex(regex_vm_t *vm, int index);

int regexVMGroupTableEntryAdd(regex_vm_build_t *build, const char *group, int len, int index) {
    int add;

    add = index - build->vm->group_tbl_size;
    if((build->vm->group_table = _regexRealloc(build->vm->group_table, (build->vm->group_tbl_size + add) * sizeof(char *), _regexMemContext)) == NULL) {
        return 0;
    }
    memset(build->vm->group_table + build->vm->group_tbl_size, 0, add * sizeof(char *));
    build->vm->group_tbl_size += add;

    if(group != NULL) {
        if((build->vm->group_table[index - 1] = _regexAlloc(len + 1, _regexMemContext)) == NULL) {
            return 0;
        }
        strncpy(build->vm->group_table[index - 1], group, len);
        build->vm->group_table[index - 1][len] = '\0';
    } else {
        build->vm->group_table[index - 1] = NULL;
    }

    return 1;
}

const char *regexVMGroupNameFromIndex(regex_vm_t *vm, int index) {
    if((index < 1) || (index > vm->group_tbl_size)) {
        return NULL;
    }
    return vm->group_table[index - 1];
}

int regexVMGroupNameLookup(regex_vm_t *vm, const char *name) {
    int k;

    for(k = 0; k < vm->group_tbl_size; k++) {
        if(!strcmp(vm->group_table[k], name)) {
            return k + 1;
        }
    }
    return -1;
}

// VM Patch list management functions ///////////////////////////////////////

#define REGEX_VM_INSTR_DECODE2(var,vm,pc)   (var)[0] = (vm)->program[(pc)] & 0xFU; \
                                            (var)[1] = ((vm)->program[(pc)] & 0x3FFF0U) >> 4U; \
                                            (var)[2] = ((vm)->program[(pc)] & 0xFFFC0000U) >> 18U;
#define REGEX_VM_INSTR_DECODE1(var,vm,pc)   (var)[0] = (vm)->program[(pc)] & 0xFU; \
                                            (var)[1] = ((vm)->program[(pc)] & 0x3FFF0U) >> 4U;
#define REGEX_VM_INSTR_DECODE0(var,vm,pc)   (var)[0] = (vm)->program[(pc)] & 0xFU;

#define REGEX_VM_INSTR_ENCODE2(vm,pc,instr,opa,opb) (vm)->program[(pc)] = (instr & 0xFU) | ((opa & 0x3FFFU) << 4U) | ((opb & 0x3FFFU) << 18U)
#define REGEX_VM_INSTR_ENCODE1(vm,pc,instr,opa)     (vm)->program[(pc)] = (instr & 0xFU) | ((opa & 0x3FFFU) << 4U)
#define REGEX_VM_INSTR_ENCODE0(vm,pc,instr)         (vm)->program[(pc)] = (instr & 0xFU)

typedef struct regex_vm_pc_patch_s regex_vm_pc_patch_t;
struct regex_vm_pc_patch_s {
    regex_token_t *token;
    int pc;
    int operand;
    regex_vm_pc_patch_t *next;
};

int regexAddPCPatchEntry(regex_vm_pc_patch_t **patch_list, regex_token_t *token, int index, int operand) {
    regex_vm_pc_patch_t *entry;

    if((entry = _regexAlloc(sizeof(regex_vm_pc_patch_t), _regexMemContext)) == NULL) {
        return 0;
    }
    memset(entry, 0, sizeof(regex_vm_pc_patch_t));
    entry->pc = index;
    entry->operand = operand;
    entry->token = token;
    entry->next = *patch_list;
    *patch_list = entry;
    return 1;
}

void regexVMPatchListFree(regex_vm_pc_patch_t *patch_list) {
    regex_vm_pc_patch_t *next;

    for(; patch_list != NULL; patch_list = next) {
        next = patch_list;
        _regexDealloc(patch_list, _regexMemContext);
    }
}

void regexVMPatchJumps(regex_vm_build_t *build, regex_vm_pc_patch_t **patch_list) {
    regex_vm_pc_patch_t *patch, *next;
    unsigned int instr[3];

    for(patch = *patch_list; patch != NULL; patch = next) {
        next = patch->next;
        REGEX_VM_INSTR_DECODE2(instr, build->vm, patch->pc);
        if(patch->operand == 1) {
            instr[1] = patch->token->pc;
        } else {
            instr[2] = patch->token->pc;
        }
        REGEX_VM_INSTR_ENCODE2(build->vm, patch->pc, instr[0], instr[1], instr[2]);
        _regexDealloc(patch, _regexMemContext);
    }
    *patch_list = NULL;
}

/////////////////////////////////////////////////////////////////////////////

int regexVMProgramAdd(regex_vm_build_t *build, eRegexToken opcode, unsigned int arg1, unsigned int arg2) {
    if((build->vm->size - build->pc) <= 0) {
        if((build->vm->program = _regexRealloc(build->vm->program, (build->vm->size + DEF_VM_SIZE_INC) * sizeof(int), _regexMemContext)) == NULL) {
            return 0;
        }
        memset(build->vm->program + build->vm->size, 0, DEF_VM_SIZE_INC * sizeof(int));
        build->vm->size += DEF_VM_SIZE_INC;
    }

    REGEX_VM_INSTR_ENCODE2(build->vm, build->pc, opcode, arg1, arg2);
    build->pc++;

    return 1;
}

void regexWalkAllTokens(regex_token_t *token, int value) {
    if(token != NULL) {
        if(token->pc == value) {
            return;
        }
        token->pc = value;
        if(token->out_a != NULL) {
            regexWalkAllTokens(token->out_a, value);
        }
        if(token->out_b != NULL) {
            regexWalkAllTokens(token->out_b, value);
        }
    }
}

// VM bytecode generator ////////////////////////////////////////////////////

int regexVMProgramGenerate(regex_vm_build_t *build, regex_vm_pc_patch_t **patch_list, regex_token_t *token) {
    int idx_a = 0, idx_b = 0;

    if(token == NULL) {
        return 1;
    }
    if(token->pc != VM_PC_UNVISITED) {
        return 1;
    }
    token->pc = build->pc;

    switch(token->tokenType) {
        case eTokenCharLiteral:
            idx_a = token->c;
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
        case eTokenSave:
            idx_a = token->group;
            break;
        case eTokenSplit:
            if((idx_a = token->out_a->pc) == VM_PC_UNVISITED) {
                if(!regexAddPCPatchEntry(patch_list, token->out_a, build->pc, 1)) {
                    return 0;
                }
            }
            if((idx_b = token->out_b->pc) == VM_PC_UNVISITED) {
                if(!regexAddPCPatchEntry(patch_list, token->out_b, build->pc, 2)) {
                    return 0;
                }
            }
            break;
        case eTokenJmp:
            if((idx_a = token->out_a->pc) == VM_PC_UNVISITED) {
                if(!regexAddPCPatchEntry(patch_list, token->out_a, build->pc, 1)) {
                    return 0;
                }
            }
            break;
        default:
            // Fall through
            break;
    }

    if(!regexVMProgramAdd(build, token->tokenType, idx_a, idx_b)) {
        return 0;
    }

    if(token->tokenType == eTokenSplit) {
        if(!regexVMProgramGenerate(build, patch_list, token->out_a)) {
            return 0;
        }
        return regexVMProgramGenerate(build, patch_list, token->out_b);
    }

    return regexVMProgramGenerate(build, patch_list, token->out_a);

}

int regexVMBuildInit(regex_vm_build_t *build) {
    memset(build, 0, sizeof(regex_vm_build_t));
    if((build->vm = _regexAlloc(sizeof(regex_vm_t), _regexMemContext)) == NULL) {
        return 0;
    }
    memset(build->vm, 0, sizeof(regex_vm_t));
    build->vm->vm_version = REGEX_VM_MACHINE_VERSION;
    return 1;
}

void regexVMFree(regex_vm_t *vm) {
    regexVMStringTableFree(vm);
    regexVMClassTableFree(vm);
    regexVMGroupTableFree(vm);
    _regexDealloc(vm, _regexMemContext);
}

void regexVMBuildDestroy(regex_vm_build_t *build) {
    regexVMFree(build->vm);
}

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
    // TODO

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
        REGEX_VM_INSTR_DECODE2(instr, vm, pc);
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
                if((name = regexVMGroupNameFromIndex(vm, (int)instr[1])) != NULL) {
                    fprintf(fp, " [%s]\n", name);
                } else {
                    fprintf(fp, "\n");
                }
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

/////////////////////////////////////////////////////////////////////////////
// Pattern compilation
/////////////////////////////////////////////////////////////////////////////

const char *regexGetCompileStatusStr(eRegexCompileStatus status) {
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
        default: return "unknown failure";
    }
}

// Return a compiled regex, or an error and position within the pattern
eRegexCompileStatus regexCompile(regex_compile_ctx_t *ctx, const char *pattern,
                                 unsigned int flags) {
    regex_vm_build_t build;
    regex_vm_pc_patch_t *patch_list = NULL;

    memset(ctx, 0, sizeof(regex_compile_ctx_t));
    ctx->pattern = pattern;
    ctx->status = eCompileOk;

    // VM allocated
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

    // Next, convert the infix form of the regular expression to postfix form,
    // and derive an NFA representation. We accomplish this using the shunting
    // yard algorithm. This is then converted into a VM bytecode sequence, for
    // runtime evaluation.

    if((ctx->status = regexShuntingYard(&(build.tokens))) != eCompileOk) {
        regexWalkAllTokens(build.tokens, VM_PC_UNVISITED);
        regexTokenDestroy(build.tokens, 0);
        regexVMBuildDestroy(&build);
        ctx->vm = NULL;
        return ctx->status;
    }

    regexWalkAllTokens(build.tokens, VM_PC_PENDING_ASSIGNMENT);
    regexWalkAllTokens(build.tokens, VM_PC_UNVISITED);

    if(!regexVMProgramGenerate(&build, &patch_list, build.tokens)) {
        regexWalkAllTokens(build.tokens, VM_PC_PENDING_ASSIGNMENT);
        regexWalkAllTokens(build.tokens, VM_PC_UNVISITED);
        regexTokenDestroy(build.tokens, 0);
        regexVMBuildDestroy(&build);
        regexVMPatchListFree(patch_list);
        ctx->vm = NULL;
        ctx->status = eCompileOutOfMem;
        return ctx->status;
    }

    regexVMPatchJumps(&build, &patch_list);
    regexVMPatchListFree(patch_list);

    // Fixup the lookup tables
    build.vm->string_tbl_size = build.string_tbl_count;
    build.vm->class_tbl_size = build.class_tbl_count;
    build.vm->group_tbl_size = build.groups;

    build.vm->size = build.pc;
    ctx->vm = build.vm;

    return eCompileOk;
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

typedef struct regex_thread_s regex_thread_t;
struct regex_thread_s {
    unsigned int pc;
    int pos;
    regex_thread_t *next;
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
    regex_thread_t *pool;
};

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
    regex_thread_t *thread, *walk;

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
    _regexDealloc(eval, _regexMemContext);
}

int regexVMNoTextAdvance(unsigned int instr) {
    switch(instr & 0xFU) {
        case eTokenMatch:
        case eTokenJmp:
        case eTokenSplit:
        case eTokenSave:
            return 1;
        default:
            return 0;
    }
}

typedef enum {
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
    unsigned int instr[3];

    while(regexVMNoTextAdvance(eval->vm->program[thread->pc])) {
        REGEX_VM_INSTR_DECODE2(instr, eval->vm, thread->pc);
        switch(instr[0]) {
            case eTokenSave:
                if(instr[1] % 2) {
                    // Group end
                    thread->subexprs[instr[1]] = eval->sp;
                } else {
                    // Group start
                    if(thread->subexprs[instr[1]] == NULL) {
                        thread->subexprs[instr[1]] = eval->sp;
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
                if(!complete || eval->pos == eval->len) {
                    return eEvalMatch;
                }
                return eEvalNoMatch;
            case eTokenJmp:
                thread->pc = instr[1];
                continue;
            default:
                return eEvalInternalError;
        }
    }
    REGEX_VM_INSTR_DECODE2(instr, eval->vm, thread->pc);
    switch(instr[0]) {
        case eTokenCharLiteral:
            if(instr[1] != *(eval->sp)) {
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
            if(str[thread->pos] == *(eval->sp)) {
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
            if(!charClassBitmapCheck(bitmap, *(eval->sp))) {
                return eEvalNoMatch;
            } else {
                thread->pc++;
            }
            return eEvalMatch;
        case eTokenCharAny:
            thread->pc++;
            return eEvalMatch;
        case eTokenByte:
            thread->pc++;
            return eEvalMatch;
        default:
            return eEvalInternalError;
    }
}

regex_match_t *regexMatch(regex_vm_t *vm, const char *text, int len, int complete) {
    regex_match_t *match;
    regex_eval_t *eval;
    regex_thread_t *thread;
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
        if(eval->queue == NULL) {
            break;
        }
        eval->thread = eval->queue;
        eval->queue = NULL;
        for(thread = eval->thread; thread != NULL; thread = eval->thread) {
            eval->thread = thread->next;
            switch(regexThreadProcess(eval, thread, complete)) {
                default:
                case eEvalInternalError:
                case eEvalOutOfMem:
                    regexEvalFree(eval);
                    return NULL;
                case eEvalMatch:
                case eEvalContinue:
                    if(eval->vm->program[thread->pc] == eTokenMatch) {
                        if((complete && (eval->pos == eval->len)) || (!complete)) {
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
    if((match = _regexAlloc(sizeof(regex_match_t) + (sizeof(char *) * 2 * vm->group_tbl_size), _regexMemContext)) == NULL) {
        return NULL;
    }
    memset(match, 0, sizeof(regex_match_t));
    match->vm = vm;
    match->text = text;
    match->len = eval->len;
    match->pos = eval->sp;
    for(k = 0; k < (vm->group_tbl_size * 2); k++) {
        match->subexprs[k] = thread->subexprs[k];
    }
    regexEvalFree(eval);

    return match;
}

void regexMatchFree(regex_match_t *match) {
    _regexDealloc(match, _regexMemContext);
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

#define REGEX_TEST_MAIN
#ifdef REGEX_TEST_MAIN

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
    int k, len;
    const char *ptr;

    if(match == NULL) {
        printf("No match\n");
        return;
    }
    printf("Evaluation of [");
    printEscapedStr(stdout, match->text, match->len);
    printf("]\n");
    printf("Match found\n");
    printf("    %d groups\n", regexGroupCountGet(match->vm));
    for(k = 0; k <= regexGroupCountGet(match->vm); k++) {
        if((ptr = regexGroupValueGet(match, k, &len)) == NULL) {
            printf("        %d [%s]: no match\n", k, regexGroupNameLookup(match->vm, k));
        } else {
            //printf("        %d [%s]: [%*.*s]\n", k, regexGroupNameLookup(match->vm, k), len, len, ptr);
            printf("        %d [%s]: [", k, regexGroupNameLookup(match->vm, k));
            printEscapedStr(stdout, ptr, len);
            printf("]\n");
        }
    }
}

int main(int argc, char **argv) {
    regex_compile_ctx_t result;
    regex_match_t *match;
    char test[] = "abcdefgj\0foo033195catghj";

    if(argc > 1) {
        if(regexCompile(&result, argv[1], 0) != eCompileOk) {
            printf("Compile failed: %s", regexGetCompileStatusStr(result.status));
            return 1;
        }
        if(argc > 2) {
            regexVMPrintProgram(stdout, result.vm);
            printf("-------------------------\n");

            if((match = regexMatch(result.vm, argv[2], REGEX_STR_NULL_TERMINATED, 0)) == NULL) {
                printf("No match\n");
                return 1;
            } else {
                regexDumpMatch(match);
                regexMatchFree(match);
            }

            //regexVMGenerateDeclaration(result.vm, "myparser", stdout);
            //regexVMGenerateDefinition(result.vm, "myparser", stdout);
        }
    }

    return 0;
}

#endif // REGEX_TEST_MAIN

#endif // MOJO_REGEX_IMPLEMENTATION
#endif // _MOJO_REGEX_HEADER_

