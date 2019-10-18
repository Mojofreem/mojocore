#ifndef _RE_VM_HEADER_
#define _RE_VM_HEADER_


#include <stdio.h>
#include "re.h"


typedef struct regex_vm_s regex_vm_t;
struct regex_vm_s {
    int *program;
    char **string_table;
    int string_tbl_size;
    int string_tbl_count;
    unsigned char **class_table;
    int class_tbl_size;
    int class_tbl_count;
    char **group_table;
    int group_tbl_size;
    int size;
    int pc;
};

regex_vm_t *regexVMCreate(regex_token_t *tokens);
void regexVMDestroy(regex_vm_t *vm);

void regexVMGenerateDeclaration(regex_vm_t *vm, const char *symbol, FILE *fp);
void regexVMGenerateSource(regex_vm_t *vm, const char *symbol, FILE *fp);
void regexVMPrintProgram(FILE *fp, regex_vm_t *vm);


#endif // _RE_VM_HEADER_
