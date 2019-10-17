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

#define DEF_VM_SIZE_INC 1024
#define DEF_VM_STRTBL_INC 32


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

int regexVMInstrTerminal(regex_vm_build_t *vm, regex_state_t *state) {
    int idx;
    int size = (state->opcode == eCharAny) ? 1 : 2;

    if(!regexVMProgramGrow(vm, size)) {
        return 0;
    }
    vm->program[vm->pc] = state->opcode;
    state->pc = vm->pc;
    vm->pc++;
    switch(state->opcode) {
        case eCharLiteral:
            vm->program[vm->pc] = state->c;
            vm->pc++;
            break;
        case eCharClass:
            if((idx = regexVMClassTableEntryCreate(vm, state->bitmap)) == -1) {
                return 0;
            }
            vm->program[vm->pc] = idx;
            vm->pc++;
            break;
        case eStringLiteral:
            if((idx = regexVMStringTableEntryCreate(vm, state->str)) == -1) {
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

int regexVMInstrCreate(regex_vm_build_t *vm, regex_state_t *state) {
    printf("instr create [pc=%d][%d %p %p]...\n", vm->pc, state->opcode, state->out_a, state->out_b);

    if(state == NULL) {
        return 1;
    }

    switch(state->opcode) {
        case eCharLiteral:
        case eCharClass:
        case eStringLiteral:
        case eCharAny:
            if(!regexVMInstrTerminal(vm, state)) {
                return 0;
            }
            break;
        case eMatch:
            if(!regexVMInstrMatch(vm)) {
                return 0;
            }
            return 1;
        case eSplit:
            if(!regexVMInstrSplit(vm, state->out_a, state->out_b)) {
                return 0;
            }
            break;
        case eSave:
            if(!regexVMInstrSave(vm, state->c)) {
                return 0;
            }
            break;
        default:
            printf("ERROR: Unknown state opcode [%d]", state->opcode);
            return 0;
    }
    if(!regexVMInstrCreate(vm, state->out_a)) {
        return 0;
    }
    if(state->opcode == eSplit) {
        if(!regexVMInstrCreate(vm, state->out_b)) {
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


eRegexCompileStatus regexGenerateVMProgram(regex_vm_t *vm, regex_token_t **tokens);
