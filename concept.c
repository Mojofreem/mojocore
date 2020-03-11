/*

Regex VM Bytecode (v7)

    Each operation is encoded into a single 32 bit int value:

         31    -     18 17     -     4 3- 0
        |--------------|--------------|----|
        |14 bits (op a)|14 bits (op b)|4bit|
        | Operand A    | Operand B    | Op |

    Operators:               opcode     Operand A               Operand B
        eTokenCharLiteral       1       char to match           program counter
                                        inverse flag
        eTokenCharClass         2       class idx to match
        eTokenStringLiteral     3       str idx to match
        eTokenCharAny           4       dotall flag
        eTokenMatch             5       end of input flag
        eTokenSplit             6       program counter         program counter
        eTokenJmp               7       program counter
        eTokenSave              8       subexpression number    compound flag
        eTokenUtf8Class         9       leading low bytes       utf8 idx to match
                                        low byte count          inverse flag
        eTokenCall              A       program counter
        eTokenReturn            B
        eTokenByte              C
        eTokenUtf8Literal       D       encoding, bits spread across operands
        <reserved>              E
        <reserved>              F

    Note about eTokenCharLiteral:
        Operand A:
            [   31       ]...[  25 ... 18   ]
            [inverse flag]...[character byte]
        Matches the character specified. If the inverse flag is set, the logic
        is inverted, and the instruction matches any character EXCEPT the
        character specified. Operand B is optional, and, if specified, is the
        program counter to jmp to in the case of not match. If the value is
        0x3FFF, then the instruction reverts to default no match behaviour.
        Note that this instruction matches a single BYTE. For utf8 literals,
        use the eTokenUtf8Literal.

    Note about eTokenCharAny:
        In ASCII mode, this instruction matches any character _except_
        newline. In Unicode mode, this instruction matches a single unicode
        codepoint, which MAY match multiple bytes, but again, does NOT match
        newline. If the REGEX_FLAG_DOT_ALL global flag is enabled, or the
        Operand A dotall flag is set, the instruction will ALSO match newline.

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
        Operand A:
            [31-30][ 29 ... 24  ][ 23 ... 18 ]
            [ num ][midhigh bits][midlow bits]

        Operand B:
            [   17  ][    16 ... 4    ]
            [inverse][utf8 class index]

        This instruction represents the low bytes of a utf8 encoding (for 2, 3,
        and 4 byte encodings). For single byte encodings, a standard char class
        is used. num is the number of low bytes to match, with the lowest
        being the class bitmap range specified in operand B, and the midlow,
        and midhigh bytes stored in operand A. The low byte encoding is 6 bits
        per byte, with the 2 bit 0x80 prefix removed. If the inverse bit is
        set, then the match logic is inverted for all bytes present, including
        the class bitmap (it is NOT pre-inverted).

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
        This instruction explicitly matches the any single byte. This differs
        from both eTokenCharAny and eTokenCharAnyDotAll in that it always
        matches newline, and always matches a single byte.

    Note about eTokenUtf8Literal:
        Operands A & B
        [31 ... 30][   29  ][28 .. 25][24 ... 4 ]
        [encoding ][inverse][unused  ][codepoint]
        This instruction explicitly matches the encoded utf8 codepoint. Two bits
        represent the byte encoding (0 - 1 byte, 1 - 2 byte, 2 - 3 byte, and
        3 - 4 byte). The inverse bit will match any character other than the
        one specified.

VM program preamble:
    A pattern that begins with a start of line assertion, '^', will begin with:

        0 jmp 3
        1 jmp 3
        2 jmp 3

    with the actual pattern instructions beginning at position 3. A pattern that
    does NOT begin with the start of line assertion will begin with:

        0 split 1 3
        1 anychar
        2 jmp 1

    effectively, ".*", allowing a match to begin anywhere within the evaluated
    string (unanchored). The match functions will begin execution at 0 by
    default, and at position 3 for an unanchored match. This ensures that the
    start of line assertion will still be enforce when evaluating as an
    unanchored match.





 jmp's are created for:

    alternation
    zero-or-more




 */

typedef struct registry_s registry_t;
struct registry_s {
    void *key_ptr;
    size_t key_len;
    void *data_ptr;
    registry_t *next;
};

int registryEntryAdd(registry_t **registry, void *key, size_t key_len, void *data);
void *registryEntryGet(registry_t **registry, void *key, size_t key_len);
int registryEntryRemove(registry_t **registry, void *key, size_t key_len);
void registryEntryRemoveAll(registry_t **registry);

/*

// compile

void regexCompileCtxInit(regex_compile_ctx_t *ctx,
                         regexMemAllocator_t alloc,
                         regexMemDeallocator_t dealloc,
                         regexMemReallocator_t re_alloc,
                         void *mem_context);

eRegexCompileStatus_t status;
regex_compile_ctx_t *context;

const char *regexGetCompileStatusStr(eRegexCompileStatus_t status);
eRegexCompileStatus_t regexCompile(regex_compile_ctx_t *ctx, const char *pattern,
                                   unsigned int flags);
eRegexCompileStatus_t regexCompileWithAlloc(regex_compile_ctx_t *ctx,
                                            const char *pattern,
                                            unsigned int flags,
                                            regexMemAllocator_t alloc,
                                            regexMemDeallocator_t dealloc,
                                            regexMemReallocator_t re_alloc,
                                            void *mem_context);

void regexGetCompileResult(FILE *fp, regex_compile_ctx_t *ctx);


regexBuildStart(flags)
regexBuildSubexprStart([name], [subroutine], no_capture, case-ins)
regexBuildSubexprEnd()
regexBuildAddCharLiteral
regexBuildAddStringLiteral
regexBuildAddCharClass
regexBuildAddByte
regexBuildAddAnyChar
regexBuildQuantifyZeroOrOne
regexBuildQuantifyZeroOrMany
regexBuildQuantifyOneOrMany
regexBuildAddAlternative
regexBuildAssertStartOfLine
 regexBuildAssertEndOfLine
 regexBuildAssertStartOfWord
 regexBuildAssertEndOfWord
 regexBuildCallSubroutine
 regexBuildAddUnicodeGlyph
 regexBuildNamedClass

// build


// generate VM

void regexVMPrintProgram(FILE *fp, regex_vm_t *vm);

// evaluate

regex_vm_t *vm;
regex_match_t *match;

regex_match_t *regexMatch(regex_vm_t *vm, const char *text, int len, int anchored);
void regexMatchFree(regex_match_t *match);
const char *regexGroupValueGet(regex_match_t *match, int group, int *len);
const char *regexGroupValueGetByName(regex_match_t *match, const char *name, int *len);
int regexGroupCountGet(regex_vm_t *vm);
const char *regexGroupNameLookup(regex_vm_t *vm, int group);
int regexGroupIndexLookup(regex_vm_t *vm, const char *name);
int regexGroupCompoundCountGet(regex_match_t *match, int group);
const char *regexGroupCompoundValueGet(regex_match_t *match, int group, int num, int *len);


regexCompile(...) ->

Diagnostic and debug output formats
==================================================================
Build time (pattern, token, and NFA)
VM (instructions, optional debug metadata)

Build time input derives from 2 primary sources:
    pattern translation
    direct token stream assembly

token stream can exist in postfix (initial pattern translation form) and infix
formats. Token streams should be directly translatable between the 2 forms.

a pattern should be reproducible from a token stream. even a manually assembled
token stream should be convertable to a pattern (if syntactically valid)

token streams can be converted to an NFA. NFA's are not inherently able to be
reverted back to a direct token stream trivially. However, metadata should be
able to provide a mapping back to the initial elements that generated the NFA.

Build time diagnostics may be optionally prefixed with the suffix of the token
address as an identifier: 0x####





pattern: valid for lifetime of compilation
    may include subpatterns
        unicode property classes: valid for compilation lifetime
    may include subroutines:
        external patterns: valid for compilation lifetime
tokens:
    manually assembled
    should provide stream validation (ie., no open subexpressions)

regex_build_ctx_t
    common data structure for regex compilation

regex_builder_t
    structure to manage an in-process compilation (from both patterns and API)

regex_vm_t
    compiled VM bytecode and supporting data for evaluation


high level API
    regexCompile(...);
    regexVMSource(...);
    regexVMPrint(...);
    regexMatch(...);

low level API
    regex_build_ctx_t *regexBuildCtxCreate(void);

    int regexSubroutineRegisterPattern(regex_build_ctx_t *build_ctx, const char *pattern);
    int regexSubroutineRegisterTokens(regex_build_ctx_t *build_ctx, const char *pattern);

    int regexCharClassRegister(regex_build_ctx_t *build_ctx);

    regex_build_t *regexBuilderCreate(regex_build_ctx_t *build_ctx);

    regexTokenizePattern(regex_build_t *build, const char *pattern);

    regexBuildConcatenate(...);
    regexBuildAlternative(...);
    regexBuildQuantify(...);
        zero or one
        zero or many
        one or many
        range
    regexBuildGroup(...)
    regexBuildCreate(...)
        char literal
        char class
        string literal
        char any
        utf8 class
        byte
    regexBuildOpenGroup
    regexBuildCloseGroup
    regexBuildFinalize

pattern -> tokens
    regexTokenizePattern
api -> tokens
    api calls
tokens -> pattern
    regexPatternizeTokens
tokens + tokens -> tokens
    if token streams are valid, they're simply concatenated
tokens -> nfa
    regexNFAGraph
nfa -> vm
    regexVMGenerate
    regexVMPrint
    regexVMSource



utf8 property class
subroutine
    pattern
    token stream
pattern
token stream


stream builder
    add
        char
        string
        class
        utf8
        utf8 class
        subroutine
    wrap
        group
    quantify
        zero or one
        zero or more
        one or more
        range
*/

// Common regex compile/build attributes, shared among multiple patterns
typedef struct regex_build_ctx_s regex_build_ctx_t;
struct regex_build_ctx_s {

};

typedef struct regex_build_ctx_s regex_build_ctx_t;
struct regex_build_ctx_s {

};

typedef struct regex_group_stack_s regex_group_stack_t;
struct regex_group_stack_s {
    int group;
    regex_group_stack_t *next;
};

typedef struct regex_builder_s regex_builder_t;
struct regex_builder_s {
    regex_build_ctx_t *build_ctx;
    regex_group_stack_t *group_stack;
    eReBuildStatus_t status;
    regex_token_t *operands;
    regex_token_t *operators;
};

regexCompile();
regexBuilderCreate();
regexBuilderFinalize();
regexBuilderDestroy();

regexCreateToken();
regexQuantifyToken();

regexBuilderConcat(...)

regexBuilderAlternate(regex_builder_t *builder, regex_builder_t *alternate);


typedef enum {
    eRePatSrcAPI,
    eRePatSrcInternal,
    eRePatSrcClass,
    eRePatSrcSub
} eRePatternSource_t;

typedef struct regex_sub_pattern_s regex_sub_pattern_t;

typedef struct regex_pattern_s regex_pattern_t;
struct regex_pattern_s {
    eRePatternSource_t source;
    const char *pattern;
    int len;
    regex_pattern_t *next;
};

typedef struct regex_pattern_pos_s regex_pattern_pos_t;
struct regex_pattern_pos_s {
    regex_pattern_t *pattern;
    int pos;
    int span;
};

typedef struct regex_pattern_instance_s regex_pattern_instance_t;
struct regex_pattern_instance_s {
    regex_pattern_pos_t *position;
    regex_pattern_pos_t *parent_pos;
};


pattern(api)  :  (foolio\p{test}mcsneed)
(@7)                  --------
pattern(class):  [A-Za-z0-9\U{123456}\u1234-\u2345]
(@10)                      ----------
error: invalid unicode codepoint






/*
token                   pat   token    VM
----------------------  ----
eTokenCharLiteral       ' '
eTokenCharClass         [...]
eTokenStringLiteral     abc
eTokenCharAny           .
eTokenMatch
eTokenSplit
eTokenJmp
eTokenSave
eTokenUtf8Class         [...]
eTokenCall              \R{...}
eTokenReturn
eTokenByte              \B
eTokenAssertion         ' '
eTokenConcatenation
eTokenAlternative       |
eTokenZeroOrOne         ?
eTokenZeroOrMany        *
eTokenOneOrMany         +
eTokenRange             {#,#}
eTokenSubExprStart      (
eTokenSubExprEnd        )

char literal
    B   0x#### eTokenCharLiteral(' ':000, Inv)
    V   ####  char ' '(000) invert
    D   ####  char 0x## ' ' 000 invert
char class
    B   0x#### eTokenCharClass([...])
    V   ####  class [...]
    D   ####  class ### [...]
string literal
    B   0x#### eTokenStringLiteral("...")
    V   ####  string "..."
    D   ####  string ### "..."
char any
match
split
jmp
save
utf8 class
call
return
byte
assertion
concatenation
alternative
    B   0x#### eTokenAlternative()
zeroorone
    B   0x#### eTokenZeroOrOne()
zeroormany
    B   0x#### eTokenZeroOrMany()
oneormany
    B   0x#### eTokenOneOrMany()
range
    B   0x#### eTokenRange(#, #)
subexpr start
    B   0x#### eTokenSubExprStart(#, Name, compound|no_capture|case_ins)
subexpr end
    B   0x#### eTokenSubExprEnd()





vm - self contained regex expression

"pattern" -> ... -> vm
    errors
        pattern
        position
        reason
tokens+ -> ... -> vm
utf8-properties -> utf8-index
"pattern" -> sub-index
tokens+ -> sub-index
tokens-postfix
tokens-infix
dfa

global utf8-index
global sub-index

build-ctx
    pools
    subroutine-index

builder
    expression
    sub-exprs

tokens
    char
    string
    class
    any
    byte
    assert

operators
    concat
    alternate

subexpression
    subroutine
    group

quantifiers
    zero or one
    zero or many
    one or many
    range








*/

typedef struct regex_group_s regex_group_t;
struct regex_group_s {
    int group;
    regex_token_t *preceeding;
    regex_group_t *next;
};

typedef struct regex_expr_s regex_expr_t;
struct regex_expr_s {
    regex_group_t *group;
    regex_token_t *token;
};

regex_expr_t *regexExprCreate(void);
regex_expr_t *regexExprInit(regex_expr_t *expr);
void regexExprDestroy(regex_expr_t *expr);

void regexExprConcatToken(regex_expr_t expr, )

typedef struct regex_global_s regex_global_t;
struct regex_global_s {
    // mem allocators
    // base unicode properties
};

typedef struct regex_build_ctx_s regex_build_ctx_t;
struct regex_build_ctx_s {
    regex_global_t *global;
    // pools
    // subroutine index
    // vm image
};

typedef struct regex_build_s regex_build_t;
struct regex_build_s {

};

int regexSubroutineRegisterPattern(regex_build_ctx_t *build_ctx, const char *name, const char *alias, const char *pattern);
int regexSubroutineRegisterInline(regex_build_ctx_t *build_ctx, const char *name, int len);
int regexSubroutineInlineFinalize(regex_build_ctx_t *build_ctx, const char *name, int len, regex_token_t *tokens);
int regexSubroutineLookup(regex_build_ctx_t *build_ctx, const char *prefix, const char *name, int name_len, const char *alias, int alias_len);
int regexSubroutineRegisterTokens(regex_build_ctx_t *build_ctx, const char *prefix, const char *name, int name_len, const char *alias, int alias_len, regex_token_t *tokens);

// ptr or NULL is OutOfMem
regex_build_t *regexBuildCreate(regex_build_ctx_t *context);
void regexBuildDestroy(regex_build_ctx_t *context, regex_build_t *build);

// true on success, false on error with details in pattern
int regexBuildTokenizePattern(regex_build_ctx_t *context, regex_build_t *build, regex_pattern_t *pattern);

// invalid token, out of mem, unknown sub
int regexBuildConcatToken(regex_build_ctx_t *context, regex_build_t *build, regex_token_t *token);
int regexBuildAlternative(regex_build_ctx_t *context, regex_build_t *opt_a, regex_build_t *opt_b);
int regexBuildGroupStart(regex_build_ctx_t *context, regex_build_t *build, int no_capture, int compound, const char *subroutine, int name_len);
int regexBuildGroupEnd(regex_build_ctx_t *context, regex_build_t *build);
int regexBuildGroupWrap(regex_build_ctx_t *context, regex_build_t *build, int no_capture, int compound, const char *subroutine, int name_len);
int regexBuildQuantify(regex_build_ctx_t *context, regex_build_t *build, eReBuildQuantifier_t quantifier, int min, int max);
int regexBuildFinalize(regex_build_ctx_t *context, regex_build_t *build);

#if 0

workflows:

    api -> dfa
    pattern -> dfa
    class -> [node ->] dfa
    sub -> dfa

    api
        immediate error
        unbalanced
    pattern
        positional
        subroutine (external)
            reference is invalid
        class (internal)
        class (external)

    pattern


    api - immediate
    pattern - context
            pattern
            position
            reason
            class
                class string
                position
                reason
    subroutine - context
            pattern
            position
            reason
            class
                class string
                position
                reason


    any expression MAY reference:
        subroutines


/*




*/

#endif // 0

// VM image, generated by compilation, utilized in evaluation
typedef struct regex_vm_s regex_vm_t;

// Compile

// Evaluate

typedef struct regex_match_s regex_match_t;

regex_match_t *regexMatch(regex_vm_t *vm, const char *pattern)


// Compound text buffer

typedef struct _ct_segment_s _ct_segment_t;
struct _ct_segment_s {
    int pos;
    int len;
    char *str;
    void *meta;
    _ct_segment_t *next;
};

typedef struct compound_text_s compound_text_t;
struct compound_text_s {
    int pos;
    int len;
    _ct_segment_t *segment;
};

#define COMPTEXT_STR_IS_NULL_TERM   -1
#define COMPTEXT_FULL_BUFFER        -1

void compoundTextDestroy(compound_text_t *text, void *meta);
int compoundTextGetC(compound_text_t *text);
int compoundTextSetPos(compound_text_t *text, int pos);
int compoundTextGetPos(compound_text_t *text);
void *compoundTextGetMeta(compound_text_t *text);
int compoundTextIsEOL(compound_text_t *text);
int compoundTextIsEOF(compound_text_t *text);
int compoundTextDelRange(compound_text_t *text, int start, int end);
int compoundTextInsTextRange(compound_text_t *text, int start, int end, const char *text, int len, void *meta);
int compoundTextInsCompoundRange(compound_text_t *text, int start, int end, compound_text_t *text, int len, void *meta);
compound_text_t *compoundTextExtract(compound_text_t *text, int start, int end);
char *compoundTextToStr(compound_text_t *text);


// Every fragment has text, and a source (meta)
// When added, retain the position relative to it's parent (future updates DO NOT
// affect this value)

int (*textConsumeCallback_t)(void *context);

#ifndef EOF
// Normally defined in stdio
#define EOF -1
#endif // EOF

typedef struct text_consume_buffer_s text_consume_buffer_t;
struct text_consume_buffer_s {
    void *context;
    textConsumeCallback_t callback;
};


/*
Builder interface concept

    create builder
    finalize builder
        no further actions permitted after finalization
        provides final construct: token DFA
    destory builder

    append:
        char literal
        string literal
        char class
        char any
        call
        byte
        assertion
        utf8 literal
    quantifier:
        [applies to the previous construct: bare token, compound, or subexpression]
        zero or one
        zero or many
        one or many
        range
    group:
        start/end
        named:compound





*/








