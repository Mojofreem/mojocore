# mojocore

## regex.c

A simple, non backreferencing, regex implementation, implemented in a single C file.

The design was influenced by the following references:
* https://swtch.com/~rsc/regexp/regexp1.html
* https://swtch.com/~rsc/regexp/regexp2.html
* https://brilliant.org/wiki/shunting-yard-algorithm/
* http://www.reedbeta.com/blog/the-shunting-yard-algorithm/

Supported operations:

| Operator | Description                          |
| -------- | ------------------------------------ |
| `(...)`  | grouping operators, must be balanced |
| `[ab]`   | character class                      |
| `a\|b`    | alternative                          |
| `a?`     | zero or one                          |
| `a*`     | zero or many                         |
| `a+`     | one or many                          |
| `.`      | any character except newline         |
| `^`      | start of line assertion (non-consuming) |
| `$`      | end of line assertion (non-consuming) |

Meta and control characters may be escaped (by prefixing with backslash) to include
 their literal values. Escaping a non standard control character or a non-meta
 character is an error. Supported escape values are:

| Escape   | Value                    | Notes            |
| -------- | ------------------------ | ---------------- |
| `\a`     | alarm (bell)             |  |
| `\b`     | backspace                |  |
| `\e`     | escape (0x1B)            |  |
| `\f`     | formfeed                 |  |
| `\n`     | newline                  |  |
| `\r`     | return                   |  |
| `\t`     | tab                      |  |
| `\v`     | vertical tab             |  |
| `\0`     | null char                |  |
| `\.`     | period                   |  |
| `\(`     | literal open parenthesis |  |
| `\[`     | literal open bracket     |  |
| `\\|`    | literal pipe             |  |
| `\?`     | literal question mark    |  |
| `\+`     | literal plus sign        |  |
| `\*`     | literal asterisk         |  |
| `\x##`   | hex byte                 |  |
| `\u####` | unicode char point       | For codepoints `0` - `0xFFFF`         |
| `\U######` | Unicode char point     | For codepoints `0x10000` - `0x10FFFF` |
| `\R{name}` | Subroutine call        | _Non-standard_. Calls a subroutine previously defined by `(?R<name>...)` | 
| `\B`     | match a single byte      | _Non-standard_. Always matches a single byte, including newline. |
| `\p{class}` | Match a unicode property set codepoint | Integrated classes are listed below. |
| `\P{class}` | Match a unicode codepoint _NOT_ in the property set. | _TODO: not yet implemented_. Integrated classes are listed below. |

Meta classes are shortcuts for common character classes. Currently implemented
meta classes are:

| Escape | Meta class value |
| ------ | ---------------- | 
| `\d`   | `[0-9]`          |
| `\D`   | `[^0-9]`         |
| `\s`   | `[ \t\r\n\f\v]`  |
| `\S`   | `[^ \t\r\n\f\v]` |
| `\w`   | `[a-zA-Z0-9_]`   |
| `\W`   | `[^a-zA-Z0-9_]`  |

Certain unicode property classes are incorporated directly into the library. These
class definitions were generated from the Unicode DB, using the
`extract_unicode_props.py` Python script. Unicode property classes use the following
metacharacters:

| Class  | Property set contents |
| ------ | --------------------- |
| `L`    | Letters (_Note: `L` != `Ll` + `Lu`_) |
| `Ll`   | Lowercase letters     |
| `Lu`   | Uppercase letters     |
| `M`    | Combining marks       |
| `P`    | Punctuation           |
| `Z`    | Whitespace            |
| `N`    | Numeric digit         |

_Note on utf8 support_:
> Clang follows sane expectations and converts \u#### into utf8 encoding
  within a char string. gcc illogically converts \u#### into utf16
  encoding, even within a char string, and does not work properly with the
  evaluator. For gcc, you will need to manually encode utf8 characters.

> There is no standard way to support unicode codepoints. Clang handles
  \u#### conversion to utf8, but values above 0xFFFF are questionable.
  To that end, this implementation uses the, decidedly non-standard, \U10####
  notation. Note the capital U, and 6 hex digits.


Subexpressions (groups) are supported. Group capture is available. By default,
the implementation captures that longest superset of an subexpression and it's
plurality modifier (`+`, `*`, `?`). For example, given the pattern:

* `abc(foo)+def`

there is a single subexpression (_number 1, as group 0 always refers to the entire
matched pattern_). Thus, given the evaluation string:

* `abcfoofoofoodef`

the captured value of group 1 is `foofoofoo`.

There are several prefix attributes that may be applied to a subexpression:

| Meta attr       | Description |
| --------------- | ---------------
| `(?P<name>...)` | Name the subexpression, for capture retrieval reference. |
| `(?:...)`       | Define the subexpression as non-capturing. |
| `(?*...)`       | Capture compound subexpressions (_see description below_) |
| `(?R<name>...)` | Create a named subroutine, a sub pattern that can be reused later with `\R{name}`. |
| `(?i...)`       | case insensitive match - _TODO: not yet implemented._ |

Multiple subexpression meta attribute prefixes may be defined, but must be at the
head of the subexpression. Additional meta's must include their own unique 
`?` prefix, ie.: `(?:?P<name>...)`. Not all permutations are valid:

* `(?P<name>...)` and `(?:...)` are mutually exclusive (why name something unused?).
* `(?:...)` and `(?*...)` are mutually exclusive (compound non-captures?).
* `(?P<name>...)` and `(?R<name>...)` are mutually exclusive. Subexpressions
        are not generated "in-line", but are called out of order using the call/
        return instructions. A subroutine may be wrapped in a named subexpression
        however.
* `(?R<name>...)` and `(?*...)` are mutually exclusive, as a subroutine is not
        a capturing expression.

If the subexpression is marked as compound, then, in addition to the normal
capture semantics mentioned above, each unique match of the base expression is also
capture. For example, given the pattern `abc(f.o)+def`, and the evaluation string
`abcfoofiofyodef`, the captured value of the group would be `foofiofyo`, and the
compound capture values would be `foo`, `fio`, and `fyo`. 

### API

#### Pattern compilation

There are two phases to using this library: compilation, and evaluation. Patterns
may be compiled to a VM program in advance, and the resulting VM compiled
directly into your program to avoid further runtime pattern compilation. To
compile a pattern, you will call:

`eRegexCompileStatus_t regexCompile(regex_compile_ctx_t *ctx, const char *pattern);`

where `pattern` is a pointer to the null terminated string containing your regex
pattern, and `regex_compile_ctx_t` is a pointer to the regex context structure:

```
typedef struct regex_compile_ctx_s regex_compile_ctx_t;
struct regex_compile_ctx_s {
    eRegexCompileStatus_t status;
    const char *pattern;
    int position;
    regex_vm_t *vm;
};
``` 

`regexCompile` will initialize this structure, attempt to compile your pattern
into a regex VM program, and return the resulting program, or specify an error
condition encountered. The return value of `regexCompile` will provide details
on the result of the compilation via the `eRegexCompileStatus_t` value:

| enum value                         | Meaning |
| ---------------------------------- | --------|
| eCompileOk                         | compiled successfully |
| eCompileCharClassRangeIncomplete   | char class range is incomplete |
| eCompileCharClassIncomplete        | char class definition is incomplete |
| eCompileEscapeCharIncomplete       | escape character is incomplete |
| eCompileInvalidEscapeChar          | invalid escaped metacharacter |
| eCompileMalformedSubExprName       | subexpression name is malformed |
| eCompileUnsupportedMeta            | expression uses an unsupported meta character |
| eCompileOutOfMem                   | out of memory |
| eCompileMissingOperand             | missing operand during postfix transform |
| eCompileMissingSubexprStart        | missing subexpr start \"(\" | 
| eCompileInternalError              | unknown internal error token (_a bug in the library_) |

If an error was encountered, the `position` field of the context structure will
indicate the pattern index at which the error occurred. For convenience, the
`const char *regexGetCompileStatusStr(eRegexCompileStatus_t status);` function
will provide a human readable string version of the enum value. The regex VM
program is stored in the `vm` field of the context.

When you are done with the VM program, you can free it with:

`void regexVMFree(regex_vm_t *vm);`

#### Pre-compiling patterns

If you want to pre-generate your regex VM program, and avoid runtime pattern
compilation, you can generate a source version of a compiled VM.

`void regexVMGenerateDeclaration(regex_vm_t *vm, const char *symbol, FILE *fp);`

This function will generate a `regex_vm_t` C source declaration, named with the
value of `symbol`, and emit it to the provided FILE pointer `fp`. 

`void regexVMGenerateDefinition(regex_vm_t *vm, const char *symbol, FILE *fp);`

This function will generate a `regex_vm_t` C source declaration, named with the
value of `symbol`, and emit it to the provided FILE pointer `fp`. The output of
these functions can be compiled directly into your program, and used with the
pattern evaluation functions.

A convenience function is provided to emit a human readable version of the
VM program:

`void regexVMPrintProgram(FILE *fp, regex_vm_t *vm);`

_Note that this is for reference only, and there is no inbuilt means by which
to convert the human readable form back into VM bytecode._

#### Evaluation

With a valid `regex_vm_t` structure, you can now evaluate text using the match
function:

`regex_match_t *regexMatch(regex_vm_t *vm, const char *text, int len, int complete);`

where `vm` is the VM program to use, `text` is the content to evaluate, and
`complete` indicates whether or not this is an unanchored match (_`complete` is
true for an anchored match_)

This function returns a `regex_match_t` structure on a successful match, or `null`
if the text did not match.

##### Captured subexpressions

* `int regexGroupCountGet(regex_vm_t *vm);` - returns the number of groups in the
pattern program. _Note: this is the number in the pattern, *NOT* the actual
number captured in an evaluation._

* `const char *regexGroupNameLookup(regex_vm_t *vm, int group);` - Look up the name
of a subexpression, if it was defined using `(?P<name>...)` syntax. If not
defined, returns `null`.

* `int regexGroupIndexLookup(regex_vm_t *vm, const char *name);` - Look up the
group number of a named subexpression, if it was defined using `(?P<name>...)`
syntax. If not defined, returns `-1`.

* `const char *regexGroupValueGet(regex_match_t *match, int group, int *len);` -
Returns a pointer to the start of the subexpression number `group` in `text` if
the subexpression was captured, or `null` if not. If `group` is not a valid
subexpression, returns `null`. The returned pointer is *NOT*
null terminated at the end of the capture group (_since it is a pointer to the
original, const, `text` string_), and the capture group length is instead stored
in `len`.

* `const char *regexGroupValueGetByName(regex_match_t *match, const char *name, int *len);`
Returns a pointer to the start of the subexpression identified by `name` in `text` if
the subexpression was captured, or `null` if not. If `name` is not a valid named
subexpression, returns `null`. The returned pointer is *NOT*
null terminated at the end of the capture group (_since it is a pointer to the
original, const, `text` string_), and the capture group length is instead stored
in `len`.

When you are done with the results, you can free the match data with:

`void regexMatchFree(regex_match_t *match);`

#### Memory allocators

If you wish to use your own memory allocation scheme, a function is provided to
override the default use of `malloc`/`free`:

`void regexSetMemoryAllocator(regexMemAllocator_t alloc, regexMemDeallocator_t dealloc, void *context);`

using an allocator/deallocator defined as follows:

* `typedef void *(*regexMemAllocator_t)(size_t size, void *ctx);`
* `typedef void (*regexMemDeallocator_t)(void *ptr, void *ctx);`

The context value is blindly passed to the allocators provided.

### Example run

The library has a main driver stub for testing. It accepts two runtime arguments:
a pattern, and text to evaluate. Using an example pattern of
`a(?P<foolio>bcd(?i\w*[hijk]foo)?)\d+cat` against a sample text of
`abcdefgefgjfoo8167287catfoo`, we find:

#### Evaluation results

```
Evaluation of [abcdefgefgjfoo8167287catfoo]
Match found
    2 groups
        0 [(null)]: [abcdefgefgjfoo8167287cat]
        1 [foolio]: [bcdefgefgjfoo]
        2 [(null)]: [efgefgjfoo]
Foolio group is [bcdefgefgjfoo]
```

#### Generate VM bytecode

```
000  char (a:97)
001  save 0
002  string("bcd")
003  split 4, 12
004  save 2
005  split 6, 8
006  class([0-9A-Z_a-z])
007  jmp 5
008  class([hijk])
009  string("foo")
010  save 3
011  save 1 [foolio]
012  class([0-9])
013  split 12, 14
014  string("cat")
015  match
```

#### Generated VM program C source

```C
extern regex_vm_t *myparser;

unsigned int _myparser_program[] = {
    0x611, 0x08, 0x03, 0x300046, 0x28, 0x200066, 0x02, 0x57,
    0x12, 0x13, 0x38, 0x18, 0x22, 0x3800C6, 0x23, 0x05
};

char *_myparser_string_table[] = {
    "bcd",
    "foo",
    "cat"
};

unsigned int _myparser_class_entry_0[] = {
    0x00000000, 0x03FF0000, 0x87FFFFFE, 0x07FFFFFE, 0x00000000, 0x00000000, 0x00000000, 0x00000000
};

unsigned int _myparser_class_entry_1[] = {
    0x00000000, 0x00000000, 0x00000000, 0x00000F00, 0x00000000, 0x00000000, 0x00000000, 0x00000000
};

unsigned int _myparser_class_entry_2[] = {
    0x00000000, 0x03FF0000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000
};

unsigned int *_myparser_class_table[] = {
    _myparser_class_entry_0,
    _myparser_class_entry_1,
    _myparser_class_entry_2,
};

char *_myparser_group_table[] = {
    "foolio",
    NULL,
};

regex_vm_t _myparser = {
    .program = _myparser_program,
    .size = 16,
    .string_table = _myparser_string_table,
    .string_tbl_size = 3,
    .class_table = _myparser_class_table,
    .class_tbl_size = 3,
    .group_table = _myparser_group_table,
    .group_tbl_size = 2
};

regex_vm_t *myparser = &_myparser;
```

### Future improvements

* Start of word assertion `\<`, that only matches when a word character follows
    a non-word character (_non consuming_)
* End of word assertion `\>`, that only matches when a word character preceeds a
    non-word character (_non consuming_)
* Inline case insensitive match modifier `(?i)`
* Global case insensitive matching flag
* Counter repetition (_range_) plurality operator `{n,m}`
* Convert to a single file header library (_ala Sean Barrett's example_)
* Pattern normalization:
    * singleton char classes reduced to a char literal (_ie., `[a]` -> `a`_)
    * sequence of char literals reduced to a string literal
    * alternative char literals reduced to a char class (_ie., `a|b|c` -> `[abc]`_)
    * reduce redundant metacharacters (_ie., `(a*)*` -> `(a)*`_)
* Flag: `caseinsensitive` - treat the entire pattern as case insensitive.
* Flag: `nocapture` - subexpressions are not captured. Simplifies compilation,
    parsing, and lowers runtime memory overhead.
* unicode:
    * Match a full unicode glyph `\X` (_may be multiple chars, and may include
        additional marker glyphs_)
    * Update unicode db parser to handle arbitrary character groups and
        property sets, prebuild unicode class trees
    * Handle inverted property sets (`\P`)

### Regex VM Bytecode (_v2_) \[Needs updating]

#### Pattern program

The VM bytecode is implemented as a sequence of 32 integer values, with each
instruction encoded as a single value:

```
+--------------+--------------+----+
|32    -     18|17     -     4|3- 0|
+--------------+--------------+----+
|14 bits (op a)|14 bits (op b)|4bit|
| Operand A    | Operand B    | Op |
+--------------+--------------+----+
```

Each operator is specified by the low 4 bits, with two 14 bit operands. This
functionally limits the implementation to `16384` effective instructions per
program (_pattern_).

#### Operators

| Operators           | OpCode  | Operand A            | Operand B       |
| ------------------- | ------- | -------------------- | --------------- |
| eTokenCharLiteral   |     1   | char to match        | _N/A_           |
| eTokenCharClass     |     2   | class idx to match   | _N/A_           |
| eTokenStringLiteral |     3   | str idx to match     | _N/A_           |
| eTokenCharAny       |     4   | _N/A_                | _N/A_           |
| eTokenMatch         |     5   | _N/A_                | _N/A_           |
| eTokenSplit         |     6   | program counter      | program counter |
| eTokenJmp           |     7   | program counter      | _N/A_           |
| eTokenSave          |     8   | subexpression number | _N/A_           |

#### TODO - Unanchored operators

As an inbuilt convenience, prefix unanchored patterns with a simple open
capture:

```
        0 split 1 3
        1 anychar
        2 jmp 1
```

At runtime, if a partial match is requested, execution begins at `0`. If a
full match is requested, execution begins at `3`.
