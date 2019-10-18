#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "re_util.h"

#if 0
typedef struct regex_string_entry_s regex_string_entry_t;
struct regex_string_entry_s {
    char *str;
    int index;
    regex_string_entry_t *next;
};

regex_string_entry_t *regexStringEntryCreate(const char *str, int index) {
    regex_string_entry_t *entry;

    if((entry = malloc(sizeof(regex_group_entry_t))) == NULL) {
        return NULL;
    }
    memset(entry, 0, sizeof(regex_group_entry_t));
    if((entry->str = strdup(str)) == NULL) {
        free(entry);
        return NULL;
    }
    entry->index = index;
    return entry;
}

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

/*
typedef struct regex_group_entry_s regex_group_entry_t;
struct regex_group_entry_s {
    char *group;
    int index;
    regex_group_entry_t *next;
};
*/

regex_group_entry_t *regexGroupEntryCreate(const char *name, int index) {
    regex_group_entry_t *entry;
    int len = (int)strlen(name) + 1;

    if((entry = malloc(sizeof(regex_group_entry_t) + len)) == NULL) {
        return NULL;
    }
    memset(entry, 0, sizeof(regex_group_entry_t));
    strcpy(entry->group, name);
    entry->index = index;
    return entry;
}

int regexGroupEntryListAdd(const char *name, int index, regex_group_entry_t **list) {
    regex_group_entry_t *entry;

    if((entry = regexGroupEntryCreate(name, index)) == NULL) {
        return 0;
    }
    entry->next = *list;
    *list = entry;
    return 1;
}

void regexGroupEntryListFree(regex_group_entry_t **list) {
    regex_group_entry_t *entry;

    for(entry = *list; entry != NULL; entry = *list) {
        *list = (*list)->next;
        free(entry);
    }
}

/*
typedef struct regex_group_table_s regex_group_table_t;
struct regex_group_table_s {
    char **groups;
    int size;
};
*/

regex_group_table_t *regexGroupTableFromList(regex_group_entry_t *list) {
    size_t highest = 0;
    size_t offset = 0;
    size_t pos = 0;
    size_t size = 0;
    regex_group_entry_t *entry;
    regex_group_table_t *table;

    if((table = malloc(sizeof(regex_group_table_t))) == NULL) {
        return NULL;
    }
    memset(table, 0, sizeof(regex_group_table_t));

    // Determine number of entries, and size of strings
    for(entry = list; entry != NULL; entry = entry->next) {
        if(highest < entry->index) {
            highest = entry->index;
        }
    }
    if(highest == 0) {
        return table;
    }

    if((table->groups = malloc(sizeof(char *) * highest)) == NULL) {
        free(table);
        return NULL;
    }
    memset(table->groups, 0, sizeof(char *) * highest);
    table->size = highest;

    for(entry = list; entry != NULL; entry = entry->next) {
        if((table->groups[entry->index - 1] = strdup(entry->group)) == NULL) {
            for(; highest > 0; highest--) {
                if(table->groups[highest - 1] != NULL) {
                    free(table->groups[highest - 1]);
                }
            }
            free(table->groups);
            free(table);
            return NULL;
        }
    }

    return table;
}

void regexGroupTableFree(regex_group_table_t *table) {
    int k;

    if(table != NULL) {
        for(k = 0; k < table->size; k++) {
            if(table->groups[k] != NULL) {
                free(table->groups[k]);
            }
        }
        free(table->groups);
        free(table);
    }
}

void regexStringEscape(FILE *fp, const char *str) {
    while(*str != '\0') {
        if((*str < 32) || (*str > 127)) {
            switch(*str) {
                case '\r': fprintf(fp, "\\r"); break;
                case '\n': fprintf(fp, "\\n"); break;
                case '\f': fprintf(fp, "\\f"); break;
                case '\v': fprintf(fp, "\\v"); break;
                case '\t': fprintf(fp, "\\t"); break;
                case '\a': fprintf(fp, "\\a"); break;
                default: fprintf(fp, "\\x%2.2X\"\"", *str); break;
            }
        } else {
            fputc(*str, fp);
        }
        str++;
    }
}

void regexGroupTableToSource(regex_group_table_t *table, const char *symbol, FILE *fp) {
    int k;

    // Header declaration
    fprintf(fp, "extern regex_group_table_t *%s;\n", symbol);

    // Source definition
    fprintf(fp, "// Regex group name table definition\n\n");
    fprintf(fp, "char *_%s_group_index[] = {\n", symbol);
    for(k = 0; k < table->size; k++) {
        if(table->groups[k] == NULL) {
            fprintf(fp, "    NULL,\n");
        } else {
            fprintf(fp, "    \"");
            regexStringEscape(fp, table->groups[k]);
            fprintf(fp, "\",\n");
        }
    }
    fprintf(fp, "};\n\n");

    fprintf(fp, "regex_group_table_t _%s_table = {\n", symbol);
    fprintf(fp, "    .groups = _%s_group_index,\n", symbol);
    fprintf(fp, "    .size = %d\n", table->size);
    fprintf(fp, "};\n\n");

    fprintf(fp, "regex_group_table_t *%s = &_%s_table;\n\n", symbol, symbol);
}


