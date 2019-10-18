#include "re_vm.h"


#define DEF_VM_SIZE_INC 1024
#define DEF_VM_STRTBL_INC 32

#define VM_PC_UNVISITED     ((int)-1)
#define VM_PC_PENDING_ASSIGNMENT    ((int)-2)


void regexVMStringTableFree(regex_vm_t *vm) {
    int k;
    if(vm->string_table != NULL) {
        for(k = 0; k < vm->string_tbl_size; k++) {
            if(vm->string_table[k] != NULL) {
                free(vm->string_table[k]);
            }
        }
        free(vm->string_table);
    }
    vm->string_table = NULL;
    vm->string_tbl_size = 0;
    vm->string_tbl_count = 0;
}

int regexVMStringTableEntryAdd(regex_vm_t *vm, const char *str) {
    int index;

    for(index = 0; index < vm->string_tbl_count; index++) {
        if(!strcmp(vm->string_table[index], str)) {
            return index;
        }
    }

    if(vm->string_tbl_size <= vm->string_tbl_count) {
        if((vm->string_table = realloc(vm->string_table, (vm->string_tbl_size + DEF_VM_STRTBL_INC) * sizeof(char *))) == NULL) {
            return -1;
        }
        memset(vm->string_table + vm->string_tbl_size, 0, DEF_VM_STRTBL_INC * sizeof(char *));
        vm->string_tbl_size += DEF_VM_STRTBL_INC;
    }
    if((vm->string_table[vm->string_tbl_count] = strdup(str)) == NULL) {
        return -1;
    }
    index = vm->string_tbl_count;
    vm->string_tbl_count++;
    return index;
}

void regexVMClassTableFree(regex_vm_t *vm) {
    int k;
    if(vm->class_table != NULL) {
        for(k = 0; k < vm->class_tbl_count; k++) {
            if(vm->class_table[k] != NULL) {
                free(vm->class_table[k]);
            }
        }
        free(vm->class_table);
    }
    vm->class_table = NULL;
    vm->class_tbl_count = 0;
    vm->class_tbl_size = 0;
}

int regexVMClassTableEntryAdd(regex_vm_t *vm, const unsigned char *bitmap) {
    int index;

    for(index = 0; index < vm->class_tbl_count; index++) {
        if(!memcmp(vm->class_table[index], bitmap, 32)) {
            return index;
        }
    }

    if(vm->class_tbl_size <= vm->class_tbl_count ) {
        if((vm->class_table = realloc(vm->class_table, (vm->class_tbl_size + DEF_VM_STRTBL_INC) * sizeof(char *))) == NULL) {
            return -1;
        }
        memset(vm->class_table + vm->class_tbl_size, 0, DEF_VM_STRTBL_INC * sizeof(char *));
        vm->class_tbl_size += DEF_VM_STRTBL_INC;
    }

    if((vm->class_table[vm->class_tbl_count] = malloc(32)) == NULL) {
        return -1;
    }
    memcpy(vm->class_table[vm->class_tbl_count], bitmap, 32);
    index = vm->class_tbl_count;
    vm->class_tbl_count++;
    return index;
}

void regexVMGroupTableFree(regex_vm_t *vm) {
    int k;
    if(vm->group_table != NULL) {
        for(k = 0; k < vm->group_tbl_size; k++) {
            if(vm->group_table[k] != NULL) {
                free(vm->group_table[k]);
            }
        }
        free(vm->group_table);
    }
    vm->group_table = NULL;
    vm->group_tbl_size = 0;
}

int regexVMGroupTableEntryAdd(regex_vm_t *vm, const char *group, int index) {
    int add = DEF_VM_STRTBL_INC;

    if(vm->group_tbl_size <= index) {
        while((vm->group_tbl_size + add) <= index) {
            add += DEF_VM_STRTBL_INC;
        }
        if((vm->group_table = realloc(vm->group_table, (vm->group_tbl_size + add) * sizeof(char *))) == NULL) {
            return -1;
        }
        memset(vm->group_table + vm->group_tbl_size, 0, add * sizeof(char *));
        vm->group_tbl_size += add;
    }

    if((vm->group_table[index] = strdup(group)) == NULL) {
        return -1;
    }
    vm->group_tbl_size += add;
    return index;
}

/////////////////////////////////////////////////////////////////////////////

int regexVMProgramGrow(regex_vm_t *vm, int size) {
    if((vm->size - vm->pc) > size) {
        return 1;
    }
    if((vm->program = realloc(vm->program, (vm->size + DEF_VM_SIZE_INC) * sizeof(int))) == NULL) {
        return 0;
    }
    memset(vm->program + vm->size, 0, DEF_VM_SIZE_INC * sizeof(int));
    vm->size += DEF_VM_SIZE_INC;
    return 1;
}

int regexVMProgramAdd(regex_vm_t *vm, eRegexToken opcode, int arg1, int arg2) {
    int size;

    switch(opcode) {
        case eTokenCharLiteral: size = 2; break;
        case eTokenCharClass: size = 2; break;
        case eTokenStringLiteral: size = 2; break;
        case eTokenCharAny: size = 1; break;
        case eTokenSplit: size = 3; break;
        case eTokenJmp: size = 2; break;
        case eTokenSave: size = 2; break;
        case eTokenMatch: size = 1; break;
        default: return 0;
    }
    if(!regexVMProgramGrow(vm, size)) {
        return 0;
    }
    vm->program[vm->pc] = opcode;
    vm->pc++;
    if(size > 1) {
        vm->program[vm->pc] = arg1;
        vm->pc++;
        if(size > 2) {
            vm->program[vm->pc] = arg2;
            vm->pc++;
        }
    }
    return 1;
}

int regexVMInstrCharLiteralCreate(regex_vm_t *vm, int c) {
    return regexVMProgramAdd(vm, eTokenCharLiteral, c, 0);
}

int regexVMInstrStringLiteralCreate(regex_vm_t *vm, const char *str) {
    int index;

    if((index = regexVMStringTableEntryAdd(vm, str)) == -1) {
        return 0;
    }

    return regexVMProgramAdd(vm, eTokenStringLiteral, index, 0);
}

int regexVMInstrClassLiteralCreate(regex_vm_t *vm, const unsigned char *bitmap) {
    int index;

    if((index = regexVMClassTableEntryAdd(vm, bitmap)) == -1) {
        return 0;
    }

    return regexVMProgramAdd(vm, eTokenCharClass, index, 0);
}

int regexVMInstrCharAnyLiteralCreate(regex_vm_t *vm) {
    return regexVMProgramAdd(vm, eTokenCharAny, 0, 0);
}

int regexVMInstrSplitCreate(regex_vm_t *vm, int idx_a, int idx_b) {
    return regexVMProgramAdd(vm, eTokenSplit, idx_a, idx_b);
}

int regexVMInstrSaveCreate(regex_vm_t *vm, int group) {
    return regexVMProgramAdd(vm, eTokenSave, group, 0);
}

int regexVMInstrMatchCreate(regex_vm_t *vm) {
    return regexVMProgramAdd(vm, eTokenMatch, 0, 0);
}

int regexVMInstrJmpCreate(regex_vm_t *vm, int idx) {
    return regexVMProgramAdd(vm, eTokenJmp, idx, 0);
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

typedef struct regex_vm_pc_patch_s regex_vm_pc_patch_t;
struct regex_vm_pc_patch_s {
    regex_token_t *token;
    int pc;
    regex_vm_pc_patch_t *next;
};

int regexAddPCPatchEntry(regex_vm_pc_patch_t **patch_list, regex_token_t *token, int index) {
    regex_vm_pc_patch_t *entry;

    if((entry = malloc(sizeof(regex_vm_pc_patch_t))) == NULL) {
        return 0;
    }
    memset(entry, 0, sizeof(regex_vm_pc_patch_t));
    entry->pc = index;
    entry->token = token;
    entry->next = *patch_list;
    *patch_list = entry;
    return 1;
}

int regexVMProgramGenerate(regex_vm_t *vm, regex_vm_pc_patch_t **patch_list, regex_token_t *token) {
    int idx_a, idx_b;

    if(token == NULL) {
        return 1;
    }
    if(token->pc != VM_PC_UNVISITED) {
        return 1;
    }
    switch(token->tokenType) {
        case eTokenCharLiteral:
            token->pc = vm->pc;
            if(!regexVMInstrCharLiteralCreate(vm, token->c)) {
                return 0;
            }
            return regexVMProgramGenerate(vm, patch_list, token->out_a);
        case eTokenStringLiteral:
            token->pc = vm->pc;
            if(!regexVMInstrStringLiteralCreate(vm, token->str)) {
                return 0;
            }
            return regexVMProgramGenerate(vm, patch_list, token->out_a);
        case eTokenCharClass:
            token->pc = vm->pc;
            if(!regexVMInstrClassLiteralCreate(vm, token->bitmap)) {
                return 0;
            }
            return regexVMProgramGenerate(vm, patch_list, token->out_a);
        case eTokenCharAny:
            token->pc = vm->pc;
            if(!regexVMInstrCharAnyLiteralCreate(vm)) {
                return 0;
            }
            return regexVMProgramGenerate(vm, patch_list, token->out_a);
        case eTokenSave:
            token->pc = vm->pc;
            if(!regexVMInstrSaveCreate(vm, token->group)) {
                return 0;
            }
            return regexVMProgramGenerate(vm, patch_list, token->out_a);
        case eTokenSplit:
            token->pc = vm->pc;
            if((idx_a = token->out_a->pc) == VM_PC_UNVISITED) {
                if(!regexAddPCPatchEntry(patch_list, token->out_a, vm->pc + 1)) {
                    return 0;
                }
            }
            if((idx_b = token->out_b->pc) == VM_PC_UNVISITED) {
                if(!regexAddPCPatchEntry(patch_list, token->out_b, vm->pc + 2)) {
                    return 0;
                }
            }
            if(!regexVMInstrSplitCreate(vm, idx_a, idx_b)) {
                return 0;
            }
            if(!regexVMProgramGenerate(vm, patch_list, token->out_a)) {
                return 0;
            }
            return regexVMProgramGenerate(vm, patch_list, token->out_b);
        case eTokenMatch:
            token->pc = vm->pc;
            if(!regexVMInstrMatchCreate(vm)) {
                return 0;
            }
            return 1;
        case eTokenJmp:
            token->pc = vm->pc;
            if((idx_a = token->out_a->pc) == VM_PC_UNVISITED) {
                if(!regexAddPCPatchEntry(patch_list, token->out_a, vm->pc + 1)) {
                    return 0;
                }
            }
            if(!regexVMInstrJmpCreate(vm, idx_a)) {
                return 0;
            }
            return regexVMProgramGenerate(vm, patch_list, token->out_b);
        default:
            printf("UNKNOWN [%d]!\n", token->tokenType);
            return 0;
    }
}

void regexVMPatchJumps(regex_vm_t *vm, regex_vm_pc_patch_t **patch_list) {
    regex_vm_pc_patch_t *patch, *next;

    for(patch = *patch_list; patch != NULL; patch = next) {
        next = patch->next;
        printf("Patch %d (of %d) to %d\n", patch->pc, vm->pc, patch->token->pc);
        vm->program[patch->pc] = patch->token->pc;
        free(patch);
    }
    *patch_list = NULL;
}

regex_vm_t *regexVMCreate(regex_token_t *tokens) {
    regex_vm_t *vm;
    regex_vm_pc_patch_t *patch_list = NULL;

    if((vm = malloc(sizeof(regex_vm_t))) == NULL) {
        return NULL;
    }
    memset(vm, 0, sizeof(regex_vm_t));

    regexWalkAllTokens(tokens, VM_PC_PENDING_ASSIGNMENT);
    regexWalkAllTokens(tokens, VM_PC_UNVISITED);

    printf("Generating bytecode...\n");

    if(!regexVMProgramGenerate(vm, &patch_list, tokens)) {
        // TODO - cleanup
        return NULL;
    }

    printf("Patching jumps...\n");

    regexVMPatchJumps(vm, &patch_list);

    return vm;
}

void regexVMDestroy(regex_vm_t *vm) {
    regexVMStringTableFree(vm);
    regexVMClassTableFree(vm);
    regexVMGroupTableFree(vm);
    free(vm);
}

void regexVMGenerateDeclaration(regex_vm_t *vm, const char *symbol, FILE *fp) {

}

void regexVMGenerateSource(regex_vm_t *vm, const char *symbol, FILE *fp) {

}

void regexVMPrintProgram(FILE *fp, regex_vm_t *vm) {
    int pc;
    int value;

    for(pc = 0; pc < vm->pc; pc++) {
        fprintf(fp, "%3.3d  ", pc);
        switch(vm->program[pc]) {
            case eTokenCharLiteral:
                pc++;
                value = vm->program[pc];
                fprintf(fp, "char (%c:%d)\n", (value < 32 || value > 127) ? '-' : value, value);
                break;
            case eTokenStringLiteral:
                pc++;
                fprintf(fp, "string(\"%s\")\n", vm->string_table[vm->program[pc]]);
                break;
            case eTokenCharClass:
                pc++;
                fprintf(fp, "class([");
                regexPrintCharClassToFP(fp, vm->class_table[vm->program[pc]]);
                fprintf(fp, "])\n");
                break;
            case eTokenCharAny:
                fprintf(fp, "anychar\n");
                break;
            case eTokenSave:
                pc++;
                fprintf(fp, "save %d\n", vm->program[pc]);
                break;
            case eTokenSplit:
                pc++;
                fprintf(fp, "split %d, %d\n", vm->program[pc], vm->program[pc + 1]);
                pc++;
                break;
            case eTokenMatch:
                fprintf(fp, "match\n");
                break;
            case eTokenJmp:
                pc++;
                fprintf(fp, "jmp %d\n", vm->program[pc]);
                break;
            default:
                printf("UNKNOWN [%d]!\n", vm->program[pc]);
                return;        }
    }
}
