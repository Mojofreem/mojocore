#ifndef _RE_UTIL_HEADER_
#define _RE_UTIL_HEADER_

#include <stdio.h>


#if 0
typedef struct regex_string_entry_s regex_string_entry_t;
struct regex_string_entry_s {
    char *str;
    int index;
    regex_string_entry_t *next;
};

regex_string_entry_t *regexStringEntryCreate(const char *str, int index);
int regexStringListEntryAdd(const char *str, int index, regex_string_entry_t **list);
void regexStringListFree(regex_string_entry_t **list);

typedef struct regex_string_table_s regex_string_table_t;
struct regex_string_table_s {
    char **strings;
    int size;
};

regex_string_table_t *regexStringTableFromList(regex_string_entry_t *list);


typedef struct regex_charclass_entry_s regex_charclass_entry_t;
struct regex_charclass_entry_s {
    unsigned char *bitmap;
    int index;
    regex_charclass_entry_t *next;
};

regex_charclass_entry_t *regexCharClassEntryCreate(const unsigned char *bitmap, int index);
int regexCharClassListEntryAdd(const unsigned char *bitmap, int index, regex_charclass_entry_t **list);
void regexCharClassListFree(regex_charclass_entry_t **list);

typedef struct regex_charclass_table_s regex_charclass_table_t;
struct regex_charclass_table_s {
    unsigned char **classes;
    int size;
};

regex_charclass_table_t *regexCharClassTableFromList(regex_charclass_entry_t *list);
#endif

typedef struct regex_group_entry_s regex_group_entry_t;
struct regex_group_entry_s {
    int index;
    regex_group_entry_t *next;
    char group[0];
};

regex_group_entry_t *regexGroupEntryCreate(const char *name, int index);
int regexGroupEntryListAdd(const char *name, int index, regex_group_entry_t **list);
void regexGroupEntryListFree(regex_group_entry_t **list);

typedef struct regex_group_table_s regex_group_table_t;
struct regex_group_table_s {
    char **groups;
    int size;
};

regex_group_table_t *regexGroupTableFromList(regex_group_entry_t *list);
void regexGroupTableFree(regex_group_table_t *table);
void regexGroupTableToSource(regex_group_table_t *table, const char *symbol, FILE *fp);


#endif // _RE_UTIL_HEADER_
