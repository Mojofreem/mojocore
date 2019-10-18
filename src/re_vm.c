#include "re_vm.h"


typedef enum {
    eVMCharLiteral = 0,
    eVMClassLiteral,
    eVMStringLiteral,
    eVMAnyChar,
    eVMSplit,
    eVMJmp,
    eVMSave,
    eVMMatch
} eRegexVMOpcode;

#define DEF_VM_SIZE_INC 1024
#define DEF_VM_STRTBL_INC 32

#define VM_PC_UNVISITED     -1
#define VM_PC_PENDING_ASSIGNMENT    -2

/*
typedef struct regex_vm_split_ptr

typedef struct regex_vm_build_s regex_vm_build_t;
struct regex_vm_build_s {
    regex_vm_t *vm;
    int pc;

};

typedef enum {
    OP_GREATER_OR_EQUAL,
    OP_ALL
} eRegexOpApply;



char *regexVMGetStringTableEntry(regex_vm_build_t *vm, int index) {
    if(index >= vm->str_tbl_index || index < 0) {
        return "ERROR";
    }
    return vm->str_table[index];
}

unsigned char *regexVMGetClassTableEntry(regex_vm_build_t *vm, int index) {
    if(index >= vm->class_tbl_index || index < 0) {
        return (unsigned char *)"                                ";
    }
    return vm->class_table[index];
}

regex_vm_build_t *regexVMBuildCreate(void) {
    regex_vm_build_t *vm;

    if((vm = malloc(sizeof(regex_vm_build_t))) == NULL) {
        return NULL;
    }
    memset(vm, 0, sizeof(regex_vm_build_t));
    return vm;
}

int regexVMPtrTableGrow(char ***table, int count, int *size) {
    if((count + 1) >= *size) {
        if((*table = realloc(*table, (*size + DEF_VM_STRTBL_INC) * sizeof(char *))) == NULL) {
            return 0;
        }
        memset(*table + *size, 0, DEF_VM_STRTBL_INC * sizeof(char *));
        *size += DEF_VM_SIZE_INC;
    }
    return 1;
}

int regexVMStringTableEntryCreate(regex_vm_build_t *vm, char *str) {
    char *entry;
    int index;

    if(!regexVMPtrTableGrow(&(vm->str_table), vm->str_tbl_index, &(vm->str_tbl_size))) {
        return -1;
    }

    if((entry = strdup(str)) == NULL) {
        return -1;
    }
    vm->str_table[vm->str_tbl_index] = entry;
    index = vm->str_tbl_index;
    vm->str_tbl_index++;
    return index;
}

int regexVMClassTableEntryCreate(regex_vm_build_t *vm, unsigned char *bitmap) {
    unsigned char *entry;
    int index;

    if(!regexVMPtrTableGrow((char ***)(&(vm->class_table)), vm->class_tbl_index, &(vm->class_tbl_size))) {
        return -1;
    }

    if((entry = mapCopy(bitmap)) == NULL) {
        return -1;
    }
    vm->class_table[vm->class_tbl_index] = entry;
    index = vm->class_tbl_index;
    vm->class_tbl_index++;
    return index;
}

int regexVMProgramGrow(regex_vm_build_t *vm, int count) {
    if((vm->pc + count) >= vm->prog_size) {
        if((vm->program = realloc(vm->program, (vm->prog_size + DEF_VM_SIZE_INC) * sizeof(int))) == NULL) {
            return 0;
        }
        memset(vm->program + vm->prog_size, 0, DEF_VM_SIZE_INC * sizeof(int));
        vm->prog_size += DEF_VM_SIZE_INC;
    }
    return 1;
}

int regexVMInstrTerminal(regex_vm_build_t *vm, regex_state_t *token) {
    int idx;
    int size = (token->opcode == eCharAny) ? 1 : 2;

    if(!regexVMProgramGrow(vm, size)) {
        return 0;
    }
    vm->program[vm->pc] = token->opcode;
    token->pc = vm->pc;
    vm->pc++;
    switch(token->opcode) {
        case eCharLiteral:
            vm->program[vm->pc] = token->c;
            vm->pc++;
            break;
        case eCharClass:
            if((idx = regexVMClassTableEntryCreate(vm, token->bitmap)) == -1) {
                return 0;
            }
            vm->program[vm->pc] = idx;
            vm->pc++;
            break;
        case eStringLiteral:
            if((idx = regexVMStringTableEntryCreate(vm, token->str)) == -1) {
                return 0;
            }
            vm->program[vm->pc] = idx;
            vm->pc++;
            break;
        default:
            break;
    }
    return 1;
}

int regexVMInstrMatch(regex_vm_build_t *vm) {
    // TODO
    return 1;
}

int regexVMInstrSplit(regex_vm_build_t *vm, regex_state_t *state_a, regex_state_t *state_b) {
    // TODO
    return 1;
}

int regexVMInstrSave(regex_vm_build_t *vm, int subexpr) {
    // TODO
    return 1;
}

int regexVMInstrCreate(regex_vm_build_t *vm, regex_state_t *token) {
    printf("instr create [pc=%d][%d %p %p]...\n", vm->pc, token->opcode, token->out_a, token->out_b);

    if(token == NULL) {
        return 1;
    }

    switch(token->opcode) {
        case eCharLiteral:
        case eCharClass:
        case eStringLiteral:
        case eCharAny:
            if(!regexVMInstrTerminal(vm, token)) {
                return 0;
            }
            break;
        case eMatch:
            if(!regexVMInstrMatch(vm)) {
                return 0;
            }
            return 1;
        case eSplit:
            if(!regexVMInstrSplit(vm, token->out_a, token->out_b)) {
                return 0;
            }
            break;
        case eSave:
            if(!regexVMInstrSave(vm, token->c)) {
                return 0;
            }
            break;
        default:
            printf("ERROR: Unknown token opcode [%d]", token->opcode);
            return 0;
    }
    if(!regexVMInstrCreate(vm, token->out_a)) {
        return 0;
    }
    if(token->opcode == eSplit) {
        if(!regexVMInstrCreate(vm, token->out_b)) {
            return 0;
        }
    }
    return 1;
}

void regexVMInstrPrint(regex_vm_build_t *vm, int *pc, int *program) {
    char c;

    printf("%4.4d  ", *pc);
    switch(program[*pc]) {
        case eCharLiteral:
            c = (char)program[*pc + 1];
            printf("char %c\n", (c < 32 || c > 127) ? '-' : c);
            pc += 1;
            break;
        case eMatch:
            printf("match\n");
            pc += 1;
            break;
        case eJmp:
            printf("jmp %d\n", program[*pc + 1]);
            pc += 2;
            break;
        case eSplit:
            printf("split %d, %d\n", program[*pc + 1], program[*pc + 2]);
            pc += 3;
            break;
        case eSave:
            printf("save %d\n", program[*pc + 1]);
            pc += 2;
            break;
        case eCharClass:
            printf("class [");
            regexPrintCharClass(regexVMGetClassTableEntry(vm, program[*pc + 1]));
            printf("]\n");
            pc += 2;
            break;
        case eStringLiteral:
            printf("string \"%s\"\n", regexVMGetStringTableEntry(vm, program[*pc + 1]));
            pc += 2;
            break;
        case eCharAny:
            printf("anychar\n");
            pc += 1;
            break;
    }
}

void regexVMPrintProgram(regex_vm_build_t *vm) {
    int pc;

    for(pc = 0; pc < vm->pc;) {
        regexVMInstrPrint(vm, &pc, vm->program);
    }
}

eRegexCompileStatus regexNFAToVMBytecode(regex_compile_ctx_t *ctx) {
    eRegexCompileStatus status;
    regex_vm_build_t *vm;

    printf("Generating bytecode...\n");

    if((vm = regexVMBuildCreate()) == NULL) {
        return eCompileOutOfMem;
    }

    if(!regexVMInstrCreate(vm, ctx->state_tree)) {
        return eCompileOutOfMem;
    }

    // TODO - patch split fixups

    printf("Bytecode complete:\n\n");

    //regexVMPrintProgram(vm);

    printf("---------------------\n");

    return eCompileOk;
}
*/

typedef struct regex_vm_str_table_s regex_vm_str_table_t;
struct regex_vm_str_table_s {
    char **strings;
    int size;
    int count;
};

regex_vm_str_table_t *regexVMStringTableCreate(void) {
    regex_vm_str_table_t *table;

    if((table = malloc(sizeof(regex_vm_str_table_t))) == NULL) {
        return NULL;
    }
    memset(table, 0, sizeof(regex_vm_str_table_t));
    return table;
}

void regexVMStringTableFree(regex_vm_str_table_t *table) {
    int k;
    if(table->strings != NULL) {
        for(k = 0; k < table->count; k++) {
            if(table->strings[k] != NULL) {
                free(table->strings[k]);
            }
        }
    }
    free(table->strings);
    free(table);
}

int regexVMStringTableEntryAdd(regex_vm_str_table_t *table, const char *str) {
    int index;

    if(table->size > table->count) {
        return 1;
    }

    for(index = 0; index < table->count; index++) {
        if(!strcmp(table->strings[index], str)) {
            return index;
        }
    }

    if((table->strings = realloc(table->strings, (table->size + DEF_VM_STRTBL_INC) * sizeof(char *))) == NULL) {
        return -1;
    }
    memset(table->strings + table->size, 0, DEF_VM_STRTBL_INC * sizeof(char *));

    if((table->strings[table->count] = strdup(str)) == NULL) {
        return -1;
    }
    index = table->count;
    table->count++;
    return index;
}

typedef struct regex_vm_class_table_s regex_vm_class_table_t;
struct regex_vm_class_table_s {
    unsigned char **bitmaps;
    int size;
    int count;
};

regex_vm_class_table_t *regexVMClassTableCreate(void) {
    regex_vm_class_table_t *table;

    if((table = malloc(sizeof(regex_vm_class_table_t))) == NULL) {
        return NULL;
    }
    memset(table, 0, sizeof(regex_vm_class_table_t));
    return table;
}

void regexVMClassTableFree(regex_vm_class_table_t *table) {
    int k;
    if(table->bitmaps != NULL) {
        for(k = 0; k < table->count; k++) {
            if(table->bitmaps[k] != NULL) {
                free(table->bitmaps[k]);
            }
        }
    }
    free(table->bitmaps);
    free(table);
}

int regexVMClassTableEntryAdd(regex_vm_class_table_t *table, const unsigned char *bitmap) {
    int index;

    if(table->size > table->count) {
        return 1;
    }

    for(index = 0; index < table->count; index++) {
        if(!memcmp(table->bitmaps[index], bitmap, 32)) {
            return index;
        }
    }

    if((table->bitmaps = realloc(table->bitmaps, (table->size + DEF_VM_STRTBL_INC) * sizeof(char *))) == NULL) {
        return -1;
    }
    memset(table->bitmaps + table->size, 0, DEF_VM_STRTBL_INC * sizeof(char *));

    if((table->bitmaps[table->count] = malloc(32)) == NULL) {
        return -1;
    }
    memcpy(table->bitmaps[table->count], bitmap, 32);
    index = table->count;
    table->count++;
    return index;
}

typedef struct regex_vm_pc_patch_s regex_vm_pc_patch_t;
struct regex_vm_pc_patch_s {
    int index;
    regex_vm_pc_patch_t *next;
};

regex_vm_pc_patch_t *regexVMPcPatchCreate(int index) {
    regex_vm_pc_patch_t *patch;

    if((patch = malloc(sizeof(regex_vm_pc_patch_t))) == NULL) {
        return NULL;
    }
    memset(patch, 0, sizeof(regex_vm_pc_patch_t));
    patch->index = index;

    return patch;
}

regex_vm_pc_patch_t *regexVmPcPatchAppend(regex_vm_pc_patch_t *patch_a, regex_vm_pc_patch_t *patch_b) {
    regex_vm_pc_patch_t *patch;

    if(patch_a == NULL) {
        return patch_b;
    }
    if(patch_b == NULL) {
        return NULL;
    }
    for(patch = patch_a; patch->next != NULL; patch = patch->next);
    patch->next = patch_b;

    return patch_a;
}

void regexVMPcPatchFree(regex_vm_pc_patch_t *patch) {
    regex_vm_pc_patch_t *next;

    for(; patch != NULL; patch = next) {
        next = patch->next;
        free(patch);
    }
}

typedef struct regex_vm_build_s regex_vm_build_t;
struct regex_vm_build_s {
    int *program;
    size_t size;
    size_t pc;
    regex_vm_str_table_t *str_table;
    regex_vm_class_table_t *class_table;
};

int regexVMProgramGrow(regex_vm_build_t *vm, int size) {
    if((vm->size - vm->pc) > size) {
        return 1;
    }
    if((vm->program = realloc(vm->program, (vm->size + DEF_VM_SIZE_INC) * sizeof(int))) == NULL) {
        return 0;
    }
    memset(vm->program + vm->size, 0, DEF_VM_SIZE_INC * sizeof(int));
    return 1;
}

int regexVMProgramAdd(regex_vm_build_t *vm, eRegexVMOpcode opcode, int arg1, int arg2) {
    int size;

    switch(opcode) {
        case eVMCharLiteral: size = 2;
        case eVMClassLiteral: size = 2;
        case eVMStringLiteral: size = 2;
        case eVMAnyChar: size = 1;
        case eVMSplit: size = 3;
        case eVMJmp: size = 2;
        case eVMSave: size = 2;
        case eVMMatch: size = 1;
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

int regexVMProgramCharLiteralCreate(regex_vm_build_t *vm, int c) {
    return regexVMProgramAdd(vm, eVMCharLiteral, c, 0);
}

int regexVMProgramStringLiteralCreate(regex_vm_build_t *vm, char *str) {
    int index;

    if((index = regexVMStringTableEntryAdd(vm->str_table, str)) == -1) {
        return 0;
    }

    return regexVMProgramAdd(vm, eVMStringLiteral, index, 0);
}

int regexVMProgramClassLiteralCreate(regex_vm_build_t *vm, unsigned char *bitmap) {
    int index;

    if((index = regexVMClassTableEntryAdd(vm->class_table, bitmap)) == -1) {
        return 0;
    }

    return regexVMProgramAdd(vm, eVMClassLiteral, index, 0);
}

int regexVMProgramAnyCharLiteralCreate(regex_vm_build_t *vm) {
    return regexVMProgramAdd(vm, eVMAnyChar, 0, 0);
}

int regexVMProgramSplitCreate(regex_vm_build_t *vm, int pc_a, int pc_b) {
    return regexVMProgramAdd(vm, eVMSplit, pc_a, pc_b);
}

int regexVMProgramJumpCreate(regex_vm_build_t *vm, int pc) {
    return regexVMProgramAdd(vm, eVMJmp, pc, 0);
}

int regexVMProgramSaveCreate(regex_vm_build_t *vm, int group) {
    return regexVMProgramAdd(vm, eVMSave, group, 0);
}

int regexVMProgramMatchCreate(regex_vm_build_t *vm) {
    return regexVMProgramAdd(vm, eVMMatch, 0, 0);
}

void regexVMProgramPrint(int *program, int size) {

}

void regexVMBuildFree(regex_vm_build_t *build) {
    free(build);
}

regex_vm_build_t *regexVMBuildCreate(void) {
    regex_vm_build_t *build;

    if((build = malloc(sizeof(regex_vm_build_t))) == NULL) {
        return NULL;
    }
    memset(build, 0, sizeof(regex_vm_build_t));

    if((build->str_table = regexVMStringTableCreate()) == NULL) {
        free(build);
        return NULL;
    }

    if((build->class_table = regexVMClassTableCreate()) == NULL) {
        regexVMStringTableFree(build->str_table);
        free(build);
        return NULL;
    }

    return build;
}

int regexVMInstrCreateFromState(regex_vm_build_t *vm /*, regex_state_t *state*/) {
    regex_token_t *state;
    if(state == NULL) {
        return 1;
    }
#if 0
    switch(state->tokenType) {
        case eCharLiteral:
            state->pc = vm->pc;
            regexVMProgramCharLiteralCreate(vm, state->c);
            return regexVMInstrCreateFromState(vm, state->out_a);
        case eMatch:
            state->pc = vm->pc;
            regexVMProgramMatchCreate(vm);
            return 1;
        case eSplit:
            state->pc = vm->pc;
            regexVMProgramSplitCreate(vm, 0 ,0);
            return 1;
        case eSave:
            state->pc = vm->pc;
            regexVMProgramSaveCreate(vm, state->group);
            return regexVMInstrCreateFromState(vm, state->out_a);
        case eCharClass:
            state->pc = vm->pc;
            regexVMProgramClassLiteralCreate(vm, state->bitmap);
            return regexVMInstrCreateFromState(vm, state->out_a);
        case eStringLiteral:
            state->pc = vm->pc;
            regexVMProgramStringLiteralCreate(vm, state->str);
            return regexVMInstrCreateFromState(vm, state->out_a);
        case eCharAny:
            state->pc = vm->pc;
            regexVMProgramAnyCharLiteralCreate(vm);
            return regexVMInstrCreateFromState(vm, state->out_a);
        default:
            return 0;
    }
#endif
    return 0;
}

void regexStatePCClear(regex_state_t *state, int value) {
    if(state != NULL) {
        if(state->pc == value) {
            return;
        }
        state->pc = value;
        if(state->out_a != NULL) {
            regexStatePCClear(state->out_a, value);
        }
        if(state->out_b != NULL) {
            regexStatePCClear(state->out_b, value);
        }
    }
}

eRegexCompileStatus regexGenerateVMProgram(regex_vm_t **vm, regex_state_t *state) {
    regex_vm_build_t *build;

    if(state == NULL) {
        return eCompileInternalError;
    }

    if((build = regexVMBuildCreate()) == NULL) {
        return eCompileOutOfMem;
    }

    regexStatePCClear(state, 0);
    regexStatePCClear(state, VM_PC_UNVISITED);

    if(!regexVMInstrCreateFromState(build, state)) {
        return eCompileOutOfMem;
    }

    return eCompileOk;
}
