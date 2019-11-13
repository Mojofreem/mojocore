#ifndef _MOJO_REGEX_HEADER_
#define _MOJO_REGEX_HEADER_

#define MOJO_REGEX_COMPILE_IMPLEMENTATION
#define MOJO_REGEX_EVALUATE_IMPLEMENTATION
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
        \u#### unicode char point
        \d [0-9]
        \D [^0-9]
        \s [ \t\f\v\r\n]
        \S [^ \t\f\v\r\n]
        \w [a-zA-Z0-9_] - TODO adapt for unicode utf8 sequences
        \W [^a-zA-Z0-9_] - TODO adapt for unicode utf8 sequences

    TODO
        \B byte (differs from ., in that the latter MAY match multibyte (UTF8))
        \X full unicode glyph (may be multiple chars)
        ^  start of string (assertion, non consuming)
        $  end of string (assertion, non consuming)
        \< match start of word (assertion, non consuming)
        \> match end of word (assertion, non consuming)
        eTokenUnicodeLiteral
        eTokenAnyCharDotAll
        unicode db -> digits, whitespace, uppercase, lowercase, combining marks
            use PCRE notation?: \p{__}
            \M - combining mark (M_)
            \N - numeric digit (N_)
            \P - punctuation (P_)
            \Z - any unicode whitespace (Z_)
            \L - any unicode letter (L_)
            \U - uppercase unicode letter (Lu)
            \l - lowercase unicode letter (Ll)


    (?P<name>...)  named subexpressions
    (?:...) non capturing subexpressions - TODO
    (?*...) compound subexpressions - TODO
    (?i) case insensitive match - TODO

Todo:
    properly handle unicode chars (we decode the escape value, but the lexer
        does not properly deal with the multiple bytes)
    multipass char class parsing, to separate ascii and unicode handling
    unicode compilation toggle - do NOT generate unicode char classes in non
        unicode mode

Regex VM Bytecode (v2)

    Each operation is encoded into a single 32 bit int value:

         32    -     18 17     -     4 3- 0
        |--------------|--------------|----|
        |14 bits (op a)|14 bits (op b)|4bit|
        | Operand A    | Operand B    | Op |

    Operators:               opcode     Operand A               Operand B
        eTokenCharLiteral       1       char to match
        eTokenCharClass         2       class idx to match
        eTokenStringLiteral     3       str idx to match
        eTokenCharAny           4
        eTokenMatch             5
        eTokenSplit             6       program counter         program counter
        eTokenJmp               7       program counter
        eTokenSave              8       subexpression number

All VM programs are prefixed with: TODO

        0 split 1 3
        1 anychar
        2 jmp 1

    At runtime, if a partial match is requested, exceution begins at 0. If a
    full match is requested, execution begins at 3.

Test: a(?P<foolio>bcd(?iefg*[hijk]foo)?)[0-9]+cat abcdefgefgjfoo8167287catfoo
*/

/////////////////////////////////////////////////////////////////////////////
// Unicode import concept
/////////////////////////////////////////////////////////////////////////////

// For utf8 encoded characters, trailing bytes have 6 bits of character data,
// as the leading bits identify the byte as part of a multibyte codepoint.
// This simplifies the unicode char class table entries to only need 64 bits
// (8 bytes) to represent the byte.

// | 4 byte lead | 3 byte lead | 2 byte lead | low byte |
//                             |<---- 2 byte group ---->|
//               |<------------ 3 byte group ---------->|
// |<------------------ 4 byte group ------------------>|

typedef struct unicode_2_byte_group_s unicode_2_byte_group_t;
struct unicode_2_byte_group_s {
    unsigned char prefix;
    unsigned char *bitmap; // 8 byte bitfield
};

typedef struct unicode_3_byte_group_s unicode_3_byte_group_t;
struct unicode_3_byte_group_s {
    unsigned char *prefix;
    int subgroup_count;
    unicode_2_byte_group_t *subgroups;
};

typedef struct unicode_4_byte_group_s unicode_4_byte_group_t;
struct unicode_4_byte_group_s {
    unsigned char *prefix;
    int subgroup_count;
    unicode_3_byte_group_t *subgroups;
};

typedef struct unicode_charclass_tree_s unicode_charclass_tree_t;
struct unicode_charclass_tree_s {
    int four_byte_count;
    unicode_4_byte_group_t *four_byte_groups;
    int three_byte_count;
    unicode_3_byte_group_t *three_byte_groups;
    int two_byte_count;
    unicode_2_byte_group_t *two_byte_groups;
    unsigned char *one_byte_bitmap;
};

/////////////////////////////////////////////////////////////////////////////

typedef struct regex_vm_s regex_vm_t;
struct regex_vm_s {
    unsigned int *program;          // VM encoded regex pattern
    int size;                       // number of instructions in the program
    char **string_table;            // table of string literals used in the pattern
    int string_tbl_size;            // number of strings in the string table
    unsigned int **class_table;    // table of character class bitmaps (32 bytes each)
    int class_tbl_size;             // number of bitmaps in the class table
    char **group_table;             // table of subexpression group names
    int group_tbl_size;             // number of groups in the group table
};

typedef void *(*regexMemAllocator_t)(size_t size, void *ctx);
typedef void (*regexMemDeallocator_t)(void *ptr, void *ctx);

void regexSetMemoryAllocator(regexMemAllocator_t alloc, regexMemDeallocator_t dealloc, void *context);

#ifdef MOJO_REGEX_COMPILE_IMPLEMENTATION

typedef enum {
    eCompileOk,
    eCompileCharClassRangeIncomplete,
    eCompileCharClassIncomplete,
    eCompileEscapeCharIncomplete,
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

#endif // MOJO_REGEX_COMPILE_IMPLEMENTATION

#ifdef MOJO_REGEX_EVALUATE_IMPLEMENTATION

typedef struct regex_match_s regex_match_t;
struct regex_match_s {
    const char *text;
    const char *pos;
    regex_vm_t *vm;
    const char *subexprs[0];
};

regex_match_t *regexMatch(regex_vm_t *vm, const char *text, int complete);
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

regexMemAllocator_t _regexAlloc = regexDefMemAllocator;
regexMemDeallocator_t _regexDealloc = regexDefMemDeallocator;
void *_regexMemContext = NULL;

void regexSetMemoryAllocator(regexMemAllocator_t alloc, regexMemDeallocator_t dealloc, void *context) {
    _regexAlloc = alloc;
    _regexDealloc = dealloc;
    _regexMemContext = context;
}

// Data structures and function declarations ////////////////////////////////

#define DEF_VM_SIZE_INC 1024
#define DEF_VM_STRTBL_INC 32

#define VM_PC_UNVISITED     ((int)-1)
#define VM_PC_PENDING_ASSIGNMENT    ((int)-2)

#define META_DIGITS_PATTERN     "0-9"
#define META_WHITESPACE_PATTERN " \\t\\f\\v\\r\\n"
#define META_WORD_PATTERN       "a-zA-Z0-9_"

#define META_CLASS(pattern)     pattern "]"
#define META_CLASS_INV(pattern) "^" pattern "]"

const char *unicode_combining_marks;
const char *unicode_numeric;
const char *unicode_punctuation;
const char *unicode_whitespace;
const char *unicode_letter;
const char *unicode_uppercase;
const char *unicode_lowercase;


typedef enum {
    eTokenNone,
    eTokenCharLiteral,
    eTokenUnicodeLiteral,
    eTokenCharClass,
    eTokenUnicodeClass,
    eTokenStringLiteral,
    eTokenCharAny,
    eTokenCharAnyDotAll,
    eTokenMatch,
    eTokenSplit,
    eTokenJmp,
    eTokenSave,
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
    };
    int pc;
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
    int group_tbl_count;
    int groups;
    int pc;
};

int regexVMGroupTableEntryAdd(regex_vm_build_t *build, const char *group, int len, int index);

/////////////////////////////////////////////////////////////////////////////
// Token management functions
/////////////////////////////////////////////////////////////////////////////

int regexTokenIsTerminal(regex_token_t *token, int preceeding) {
    switch(token->tokenType) {
        case eTokenCharLiteral:
        case eTokenStringLiteral:
        case eTokenCharClass:
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

regex_token_t *regexAllocToken(eRegexToken tokenType, int c, char *str) {
    regex_token_t *token;

    if((token = _regexAlloc(sizeof(regex_token_t), _regexMemContext)) == NULL) {
        return NULL;
    }
    memset(token, 0, sizeof(regex_token_t));
    token->tokenType = tokenType;
    if(str != NULL) {
        token->str = str;
    } else {
        token->c = c;
    }
    return token;
}

int regexTokenCreate(regex_token_t **list, eRegexToken tokenType, int c, char *str) {
    regex_token_t *token, *walk;

    if((token = regexAllocToken(tokenType, c, str)) == NULL) {
        return 0;
    }

    if(*list == NULL) {
        *list = token;
    } else {
        for(walk = *list; walk->next != NULL; walk = walk->next);
        if(regexTokenIsTerminal(token, 0) && regexTokenIsTerminal(walk, 1)) {
            // Two adjacent terminals have an implicit concatenation
            if((walk->next = regexAllocToken(eTokenConcatenation, 0, NULL)) == NULL) {
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
    eRegexPatternEscapedChar
} eRegexPatternCharState;

typedef struct parseChar_s parseChar_t;
struct parseChar_s {
    eRegexPatternCharState state;
    int c;
};

parseChar_t parseGetNextPatternChar(const char **pattern) {
    parseChar_t result = {
        .c = 0,
        .state = eRegexPatternInvalid
    };
    int k;

    if(**pattern == '\0') {
        result.state = eRegexPatternEnd;
        return result;
    }

    if(**pattern == '\\') {
        switch(*(*pattern + 1)) {
            case '\0': // End of pattern string
                result.state = eRegexPatternInvalid;
                return result;

            case 'd': // digit
            case 'D': // non digit
            case 's': // whitespace
            case 'S': // non whitespace
            case 'w': // word character
            case 'W': // non word character
            case 'X': // full unicode glyph (base + markers)
                result.state = eRegexPatternMetaClass;
                result.c = *(*pattern + 1);
                return result;

            case '0': result.c = '\0'; // null
            case 'a': result.c = '\a'; // alarm (bell)
            case 'b': result.c = '\b'; // backspace
            case 'e': result.c = '\x1B'; // escape
            case 'f': result.c = '\f'; // formfeed
            case 'n': result.c = '\n'; // newline
            case 'r': result.c = '\r'; // carraige return
            case 't': result.c = '\t'; // tab
            case '-': result.c = '-';  // escaped dash for character class ranges
            case 'v': result.c = '\v'; // vertical tab
                result.state = eRegexPatternEscapedChar;
                return result;

            case 'u': // unicode codepoint
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
    if((pc.state == eRegexPatternEnd) || (pc.state == eRegexPatternInvalid)) {
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
        return 0xF08080u | ((unsigned int)c & 0x3Fu) | (((unsigned int)c & 0xFC0u) << 2u) | (((unsigned int)c & 0x3F000u) << 4u) | ((unsigned int)c & 0x7000000u);
    }
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

/////////////////////////////////////////////////////////////////////////////
// Character class parsing handlers
/////////////////////////////////////////////////////////////////////////////

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

// TODO - integrate proper utf8 handling
eRegexCompileStatus parseCharClass(const char **pattern, unsigned int *bitmap) {
    parseChar_t c;
    int invert = 0;
    int range = 0;
    int last = 0;
    int next;

    memset(bitmap, 0, 32);

    c = parseGetNextPatternChar(pattern);
    parsePatternCharAdvance(pattern);
    if(c.state == eRegexPatternEnd) {
        return eCompileCharClassIncomplete;
    }

    if(c.state == eRegexPatternMetaChar && c.c == '^') {
        invert = 1;
        c = parseGetNextPatternChar(pattern);
        parsePatternCharAdvance(pattern);
    }

    for(;;) {
        switch(c.state) {
            case eRegexPatternInvalid:
                return eCompileEscapeCharIncomplete;

            case eRegexPatternInvalidEscape:
                return eCompileInvalidEscapeChar;

            case eRegexPatternChar:
                if(c.c == ']') {
                    if(range == 2) {
                        return eCompileCharClassRangeIncomplete;
                    }
                    if(invert) {
                        charClassBitmapInvert(bitmap);
                    }
                    return eCompileOk;
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

            case eRegexPatternUnicode:
                // TODO - special case handling
                break;

            case eRegexPatternMetaClass:
                return eCompileInvalidEscapeChar;

            case eRegexPatternEnd:
                if(range == 2) {
                    return eCompileCharClassRangeIncomplete;
                }
                return eCompileCharClassIncomplete;
        }
        c = parseGetNextPatternChar(pattern);
        parsePatternCharAdvance(pattern);
    }
}

int parseCharClassAndCreateToken(eRegexCompileStatus *status, const char **pattern, regex_token_t **tokens) {
    unsigned int bitmap[8], *ptr;

    // Operand, character class
    if((*status = parseCharClass(pattern, bitmap)) != eCompileOk) {
        return 0;
    }
    if((ptr = charClassBitmapCopy(bitmap)) == NULL) {
        *status = eCompileOutOfMem;
        return 0;
    }
    if(!regexTokenCreate(tokens, eTokenCharClass, 0, (char *)ptr)) {
        *status = eCompileOutOfMem;
        return 0;
    }
    return 1;
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

void regexPrintCharClass(unsigned int *bitmap) {
    regexPrintCharClassToFP(stdout, bitmap);
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

    index--;
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

    // Parse the regex pattern into a sequence of tokens (operators and operands)
    // The output of this stage is a sequence of lexical tokens in infix form

    // Loop through the pattern until we've handled it all
    for(; *pattern != '\0';) {
        // Get the next character in the pattern. The helper function assists
        // in disambiguating escaped characters.
        c = parseGetNextPatternChar(&pattern);
        parsePatternCharAdvance(&pattern);

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
                        if(!regexTokenCreate(tokens, eTokenCharAny, 0, 0)) {
                            SET_RESULT(eCompileOutOfMem);
                        }
                        continue;

                    case '[':
                        if(!parseCharClassAndCreateToken(&status, &pattern, tokens)) {
                            SET_RESULT(status);
                        }
                        continue;

                    case '?':
                        if(!regexTokenCreate(tokens, eTokenZeroOrOne, 0, 0)) {
                            SET_RESULT(eCompileOutOfMem);
                        }
                        continue;

                    case '*':
                        if(!regexTokenCreate(tokens, eTokenZeroOrMany, 0, 0)) {
                            SET_RESULT(eCompileOutOfMem);
                        }
                        continue;

                    case '+':
                        if(!regexTokenCreate(tokens, eTokenOneOrMany, 0, 0)) {
                            SET_RESULT(eCompileOutOfMem);
                        }
                        continue;

                    case '|':
                        if(!regexTokenCreate(tokens, eTokenAlternative, 0, 0)) {
                            SET_RESULT(eCompileOutOfMem);
                        }
                        continue;

                    case '(':
                        // Grouped subexpression, complex operator, resolves to a compound operand
                        build->groups++;
                        subexpr = build->groups;
                        str = NULL;
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
                        if(!regexTokenCreate(tokens, eTokenSubExprStart, subexpr, NULL)) {
                            SET_RESULT(eCompileOutOfMem);
                        }
                        continue;

                    case ')':
                        // End of grouped subexpression
                        if(!regexTokenCreate(tokens, eTokenSubExprEnd, 0, 0)) {
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
                    case 'd': // digit
                        class = META_CLASS(META_DIGITS_PATTERN);
                        if(!parseCharClassAndCreateToken(&status, &class, tokens)) {
                            SET_RESULT(status);
                        }
                        continue;

                    case 'D': // non digit
                        class = META_CLASS_INV(META_DIGITS_PATTERN);
                        if(!parseCharClassAndCreateToken(&status, &class, tokens)) {
                            SET_RESULT(status);
                        }
                        continue;

                    case 's': // whitespace
                        class = META_CLASS(META_WHITESPACE_PATTERN);
                        if(!parseCharClassAndCreateToken(&status, &class, tokens)) {
                            SET_RESULT(status);
                        }
                        continue;

                    case 'S': // non whitespace
                        class = META_CLASS_INV(META_WHITESPACE_PATTERN);
                        if(!parseCharClassAndCreateToken(&status, &class, tokens)) {
                            SET_RESULT(status);
                        }
                        continue;

                    case 'w': // word character
                        // TODO: derive from the unicode DB
                        class = META_CLASS(META_WORD_PATTERN);
                        if(!parseCharClassAndCreateToken(&status, &class, tokens)) {
                            SET_RESULT(status);
                        }
                        continue;

                    case 'W': // non word character
                        // TODO: derive from the unicode DB
                        // Derived from the unicode DB
                        class = META_CLASS_INV(META_WORD_PATTERN);
                        if(!parseCharClassAndCreateToken(&status, &class, tokens)) {
                            SET_RESULT(status);
                        }
                        continue;

                    case 'X': // full unicode glyph (base + markers)
                        // Represented by a micro NFA
                        // TODO
                        break;
                }
                break;

            case eRegexPatternChar:
            case eRegexPatternUnicode:
            case eRegexPatternEscapedChar:
                // Operand, either character literal or string literal
                len = parseGetPatternStrLen(&pattern);
                switch(PARSE_DEC_CHAR_COUNT(len)) {
                    case -1:
                        SET_RESULT(eCompileEscapeCharIncomplete);

                    case 0:
                        if(!regexTokenCreate(tokens,eTokenCharLiteral, (char)(c.c), 0)) {
                            SET_RESULT(eCompileOutOfMem);
                        }
                        break;

                    default:
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
                        if(!regexTokenCreate(tokens, eTokenStringLiteral, 0, str)) {
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
    if(!regexTokenCreate(&jmp, eTokenJmp, 0, NULL)) {
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
    if(!regexTokenCreate(&token, eTokenMatch, 0, NULL)) {
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
        if(!regexTokenCreate(&token, eTokenConcatenation, 0, NULL)) {
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
    vm->string_table = NULL;
    vm->string_tbl_size = 0;
}

int regexVMStringTableEntryAdd(regex_vm_build_t *build, const char *str) {
    int index;

    for(index = 0; index < build->string_tbl_count; index++) {
        if(!strcmp(build->vm->string_table[index], str)) {
            return index;
        }
    }

    if(build->vm->string_tbl_size <= build->string_tbl_count) {
        if((build->vm->string_table = realloc(build->vm->string_table, (build->vm->string_tbl_size + DEF_VM_STRTBL_INC) * sizeof(char *))) == NULL) {
            return -1;
        }
        memset(build->vm->string_table + build->vm->string_tbl_size, 0, DEF_VM_STRTBL_INC * sizeof(char *));
        build->vm->string_tbl_size += DEF_VM_STRTBL_INC;
    }
    if((build->vm->string_table[build->string_tbl_count] = strdup(str)) == NULL) {
        return -1;
    }
    index = build->string_tbl_count;
    build->string_tbl_count++;
    return index;
}

const char *regexVMStringTableEntryGet(regex_vm_t *vm, int string_table_id) {
    if(string_table_id >= vm->string_tbl_size) {
        return NULL;
    }
    return vm->string_table[string_table_id];
}

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
        if((build->vm->class_table = realloc(build->vm->class_table, (build->vm->class_tbl_size + DEF_VM_STRTBL_INC) * sizeof(unsigned int *))) == NULL) {
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

int regexVMGroupTableEntryAdd(regex_vm_build_t *build, const char *group, int len, int index) {
    int add = DEF_VM_STRTBL_INC;

    if(build->vm->group_tbl_size <= index) {
        while((build->vm->group_tbl_size + add) <= index) {
            add += DEF_VM_STRTBL_INC;
        }
        if((build->vm->group_table = realloc(build->vm->group_table, (build->vm->group_tbl_size + add) * sizeof(char *))) == NULL) {
            return 0;
        }
        memset(build->vm->group_table + build->vm->group_tbl_size, 0, add * sizeof(char *));
        build->vm->group_tbl_size += add;
        build->group_tbl_count += add;
    }

    if((build->vm->group_table[index] = _regexAlloc(len + 1, _regexMemContext)) == NULL) {
        return 0;
    }
    strncpy(build->vm->group_table[index], group, len);
    build->vm->group_table[index][len] = '\0';
    build->vm->group_tbl_size += add;

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
        if((build->vm->program = realloc(build->vm->program, (build->vm->size + DEF_VM_SIZE_INC) * sizeof(int))) == NULL) {
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
            if((idx_a = regexVMStringTableEntryAdd(build, token->str)) == -1) {
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
    _regexDealloc(build, _regexMemContext);
}

void regexVMGenerateDeclaration(regex_vm_t *vm, const char *symbol, FILE *fp) {
    fprintf(fp, "extern regex_vm_t *%s;\n", symbol);
}

#define MAX_VM_INSTR_PER_ROW    8

// TODO - recode exploded utf8 values
void regexEmitEscapedString(FILE *fp, const char *str) {
    for(; *str != '\0'; str++) {
        if((*str < 32) || (*str > 127)) {
            switch(*str) {
                case '\n': fprintf(fp, "\\n"); break;
                case '\r': fprintf(fp, "\\r"); break;
                case '\t': fprintf(fp, "\\t"); break;
                case '\f': fprintf(fp, "\\f"); break;
                case '\v': fprintf(fp, "\\v"); break;
                case '\a': fprintf(fp, "\\a"); break;
                default:
                    fprintf(fp, "\\x%2.2X\"\"", *str);
                    break;
            }
        } else {
            fputc(*str, fp);
        }
    }
}

void regexVMGenerateDefinition(regex_vm_t *vm, const char *symbol, FILE *fp) {
    int k, j;

    fprintf(fp, "unsigned int _%s_program[] = {", symbol);
    for(k = 0; k < vm->size; k++) {
        if(k != 0) {
            fputc(',', fp);
        }
        if(!(k % MAX_VM_INSTR_PER_ROW)) {
            fputs("\n    ", fp);
        } else {
            fputc(' ', fp);
        }
        fprintf(fp, "0x%.2X", vm->program[k]);
    }
    fprintf(fp, "\n};\n\n");

    fprintf(fp, "char *_%s_string_table[] = {\n", symbol);
    for(k = 0; k < vm->string_tbl_size; k++) {
        if(vm->string_table[k] == NULL) {
            fprintf(fp, "    NULL%s\n", (((k + 1) != vm->string_tbl_size) ? "," : ""));
        } else {
            fprintf(fp, "    \"");
            regexEmitEscapedString(fp, vm->string_table[k]);
            fprintf(fp, "\"%s\n", (((k + 1) != vm->string_tbl_size) ? "," : ""));
        }
    }
    fprintf(fp, "};\n\n");

    for(k = 0; k < vm->class_tbl_size; k++) {
        if(vm->class_table[k] == NULL) {
            continue;
        }
        fprintf(fp, "unsigned int _%s_class_entry_%d[] = {", symbol, k);
        for(j = 0; j < 8; j++) {
            fprintf(fp, "%s0x%8.8X%s", (j % 4 ? " " : "\n    "),
                    vm->class_table[k][j], (j != 7 ? "," : ""));
        }
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

    fprintf(fp, "char *_%s_group_table[] = {\n", symbol);
    for(k = 0; k < vm->group_tbl_size; k++) {
        if(vm->group_table[k] == NULL) {
            fprintf(fp, "    NULL,\n");
        } else {
            fprintf(fp, "    \"");
            regexEmitEscapedString(fp, vm->group_table[k]);
            fprintf(fp, "\",\n");
        }
    }
    fprintf(fp, "};\n\n");

    fprintf(fp, "regex_vm_t _%s = {\n", symbol);
    fprintf(fp, "    .program = _%s_program,\n", symbol);
    fprintf(fp, "    .size = %d,\n", vm->size);
    fprintf(fp, "    .string_table = _%s_string_table,\n", symbol);
    fprintf(fp, "    .string_tbl_size = %d,\n", vm->string_tbl_size);
    fprintf(fp, "    .class_table = _%s_class_table,\n", symbol);
    fprintf(fp, "    .class_tbl_size = %d,\n", vm->class_tbl_size);
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
                fprintf(fp, "char (%c:%d)\n", (instr[1] < 32 || instr[1] > 127) ? '-' : instr[1], instr[1]);
                break;
            case eTokenStringLiteral:
                fprintf(fp, "string(\"%s\")\n", vm->string_table[instr[1]]);
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
            default:
                fprintf(fp, "UNKNOWN [%d]!\n", instr[0]);
                return;        }
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

regex_eval_t *regexEvalCreate(regex_vm_t *vm, const char *pattern) {
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
                if(!complete || *eval->sp == '\0') {
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
            if((str = regexVMStringTableEntryGet(eval->vm, instr[1])) == NULL) {
                return eEvalInternalError;
            }
            if(thread->pos == -1) {
                thread->pos = 0;
            }
            if(str[thread->pos] == *(eval->sp)) {
                thread->pos++;
                if(str[thread->pos] == '\0') {
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
        default:
            return eEvalInternalError;
    }
}

regex_match_t *regexMatch(regex_vm_t *vm, const char *text, int complete) {
    regex_match_t *match;
    regex_eval_t *eval;
    regex_thread_t *thread;
    int k;

    if((eval = regexEvalCreate(vm, text)) == NULL) {
        return NULL;
    }

    if(!regexThreadCreate(eval, NULL, 0, 1)) {
        regexEvalFree(eval);
        return NULL;
    }

    for(; *eval->sp != '\0'; eval->sp++) {
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
                        if((complete && (*eval->sp == '\0')) || (!complete)) {
                            goto FoundMatch;
                        }
                    }
                    thread->next = eval->queue;
                    eval->queue = thread;
                    break;
                case eEvalNoMatch:
                    regexThreadFree(eval, thread);
                    continue;
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

void regexDumpMatch(regex_match_t *match) {
    int k, len;
    const char *ptr;

    printf("Evaluation of [%s]\n", match->text);
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
            printf("        %d [%s]: [%*.*s]\n", k, regexGroupNameLookup(match->vm, k), len, len, ptr);
        }
    }
    if((ptr = regexGroupValueGetByName(match, "foolio", &len)) != NULL) {
        printf("Foolio group is [%*.*s]\n", len, len, ptr);
    }
}

int main(int argc, char **argv) {
    regex_compile_ctx_t result;
    regex_match_t *match;

    if(argc > 1) {
        if(regexCompile(&result, argv[1]) != eCompileOk) {
            printf("Compile failed: %s", regexGetCompileStatusStr(result.status));
            return 1;
        }
        if(argc > 2) {
            regexVMPrintProgram(stdout, result.vm);

            if((match = regexMatch(result.vm, argv[2], 0)) == NULL) {
                printf("No match\n");
                return 1;
            }

            regexDumpMatch(match);
            regexMatchFree(match);

            //regexVMGenerateDeclaration(result.vm, "myparser", stdout);
            //regexVMGenerateDefinition(result.vm, "myparser", stdout);
        }
    }

    return 0;
}

#endif // REGEX_TEST_MAIN

#endif // MOJO_REGEX_IMPLEMENTATION
#endif // _MOJO_REGEX_HEADER_

