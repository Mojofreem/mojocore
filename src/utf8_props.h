/////////////////////////////////////////////////////////////////////////////
//
// This file was automatically generated from the UAX unicode database via the
// extract_unicode_props.py script
//
// Invocation: extract_unicode_props.py --uax ../unicode-db -i ../src/utf8_props.h -s ../src/utf8_props.c -p M -p N -p P -p Z -p L -p Lu -p Ll -t _regex_char_class_default_import_table -b _REGEX_DEFAULT_UAX_DB_DEFINITIONS_
//
/////////////////////////////////////////////////////////////////////////////

#ifndef _REGEX_DEFAULT_UAX_DB_DEFINITIONS_
#define _REGEX_DEFAULT_UAX_DB_DEFINITIONS_

#ifndef regex_unicode_class_t
typedef struct regex_unicode_class_s regex_unicode_class_t;
#endif // regex_unicode_class_t

#ifndef regex_unicode_class_s
struct regex_unicode_class_s {
    const char *abbreviation;
    const char *property;
    const char *class_string;
    regex_unicode_class_t *next;
};
#endif // regex_unicode_class_s

/////////////////////////////////////////////////////////////////////////////
//
// This generated table includes the UAX DB definitions for:
//
//     Mark .............. M
//     Number ............ N
//     Punctuation ....... P
//     Separator ......... Z
//     Letter ............ L
//     Uppercase_Letter .. Lu
//     Lowercase_Letter .. Ll
//
/////////////////////////////////////////////////////////////////////////////

extern regex_unicode_class_t *_regex_char_class_default_import_table;

#endif // _REGEX_DEFAULT_UAX_DB_DEFINITIONS_
