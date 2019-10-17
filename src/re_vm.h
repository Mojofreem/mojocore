#ifndef _RE_VM_HEADER_
#define _RE_VM_HEADER_


#include "re.h"


typedef struct regex_vm_s regex_vm_t;
struct regex_vm_s {
    unsigned char *program;
    char *string_table;
    int string_tbl_size;
    unsigned char *class_table;
    int class_tbl_size;
    char *group_table;
    int group_tbl_size;
    int size;
};


eRegexCompileStatus regexGenerateVMProgram(regex_vm_t **vm, regex_token_t **tokens);


#endif // _RE_VM_HEADER_
