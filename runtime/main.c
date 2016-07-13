#include "main.h"
#include "interpreter.h"
#include "compiler.h"

int VM_Init(vm_t *vm, vmMachine_t machine, const char *file) {
    FILE *f; size_t fSize; byte *tmp;
    size_t ic = 0; byte *currentInstruction;
    int i;

    memset(vm, 0, sizeof(*vm));

    // ----
    // read data from file
    // ----
    printf("reading file %s...", file);

    f = fopen(file, "rb");
    if (!f) {
        printf(" failed!\n");
        return 1;
    }

    fseek(f, 0, SEEK_END);
    fSize = ftell(f);
    fseek(f, 0, SEEK_SET);

    tmp = malloc(fSize);
    if (!tmp) {
        fclose(f);
        printf(" failed!\n");
        return 1;
    }

    fread(tmp, fSize, 1, f);
    fclose(f);
    printf("\n");

    // ----
    // verify qvm
    // ----
    printf("verifying VM...");
    if (fSize < sizeof(vmHeader_t)) {
        free(tmp); VM_Free(vm);
        printf(" failed!\n");
        return 1;
    }

    vm->header = *(vmHeader_t *)tmp;
    if (vm->header.magic != VM_MAGIC) {
        free(tmp); VM_Free(vm);
        printf(" failed!\n");
        return 1;
    }
    printf("\n");

    // ----
    // setup memory
    // ----
    printf("setting up memory for code segment...");
    vm->codeSize = vm->header.codeSegmentLength;
    vm->code = malloc(vm->codeSize);
    if (!vm->code) {
        free(tmp); VM_Free(vm);
        printf(" failed!\n");
        return 1;
    }
    printf("\n");
    memcpy(vm->code, tmp + vm->header.codeSegmentOffset, vm->header.codeSegmentLength);

    printf("setting up memory for data segment...");
    vm->dataSize = vm->header.dataSegmentLength + vm->header.litSegmentLength + vm->header.bssSegmentLength;
    for (i = 0; vm->dataSize > ( 1 << i ); i++) {}
    vm->dataSize = 1 << i;
    vm->dataMask = vm->dataSize - 1;
    vm->data = calloc(vm->dataSize, sizeof(byte));
    if (!vm->data) {
        free(tmp); VM_Free(vm);
        printf(" failed!\n");
        return 1;
    }
    printf("\n");
    memcpy(vm->data, tmp + vm->header.dataSegmentOffset, vm->header.dataSegmentLength + vm->header.litSegmentLength);

    free(tmp);

    // ----
    // instruction pointers
    // ----
    printf("setting up instruction pointers...");
    vm->instructionPointers = malloc(vm->header.instructionCount * sizeof(byte *));
    if (!vm->instructionPointers) {
        VM_Free(vm);
        printf(" failed!\n");
        return 1;
    }

    currentInstruction = vm->code;
    while(ic != vm->header.instructionCount) {
        byte *nextInstruction;

        switch(*currentInstruction) {
			// 0 Byte Parameter OpCodes
			case OP_UNDEF:
			case OP_IGNORE:
			case OP_BREAK:
			case OP_CALL:
			case OP_PUSH:
			case OP_POP:
			case OP_JUMP:
			case OP_LOAD1:
			case OP_LOAD2:
			case OP_LOAD4:
			case OP_STORE1:
			case OP_STORE2:
			case OP_STORE4:
			case OP_SEX8:
			case OP_SEX16:
			case OP_NEGI:
			case OP_ADD:
			case OP_SUB:
			case OP_DIVI:
			case OP_DIVU:
			case OP_MODI:
			case OP_MODU:
			case OP_MULI:
			case OP_MULU:
			case OP_BAND:
			case OP_BOR:
			case OP_BXOR:
			case OP_BCOM:
			case OP_LSH:
			case OP_RSHI:
			case OP_RSHU:
			case OP_NEGF:
			case OP_ADDF:
			case OP_SUBF:
			case OP_DIVF:
			case OP_MULF:
			case OP_CVIF:
			case OP_CVFI:
                nextInstruction = currentInstruction + 1;
                break;

			// 1 Byte Parameter OpCodes
			case OP_ARG:
                nextInstruction = currentInstruction + 2;
				break;

			// 4 Byte Parameter OpCodes
            case OP_BLOCK_COPY:
            case OP_ENTER:
            case OP_LEAVE:
			case OP_CONST:
			case OP_LOCAL:
			case OP_EQ:
			case OP_NE:
			case OP_LTI:
			case OP_LEI:
			case OP_GTI:
			case OP_GEI:
			case OP_LTU:
			case OP_LEU:
			case OP_GTU:
			case OP_GEU:
			case OP_EQF:
			case OP_NEF:
			case OP_LTF:
			case OP_LEF:
			case OP_GTF:
			case OP_GEF:
                nextInstruction = currentInstruction + 5;
                break;

            default:
                VM_Free(vm);
                printf("\nunknown instruction %#02x!\n", *currentInstruction);
                printf("failed!\n");
                return 1;
        }

        vm->instructionPointers[ic++] = currentInstruction;
        currentInstruction = nextInstruction;
    }
    printf("\n");

    printf("------------- VM Info -------------\n");
    printf("Magic:               %#x\n", vm->header.magic);
    printf("Instruction Count:   %#010x\n", vm->header.instructionCount);
    printf("CODE Segment:        %#010x (Length: %#x)\n", vm->header.codeSegmentOffset, vm->header.codeSegmentLength);
    printf("DATA Segment:        %#010x (Length: %#x)\n", vm->header.dataSegmentOffset, vm->header.dataSegmentLength);
    printf("LIT  Segment Length: %#010x\n", vm->header.litSegmentLength);
    printf("BSS  Segment Length: %#010x\n", vm->header.bssSegmentLength);
    printf("-----------------------------------\n");

    vm->sp = vm->data + vm->dataSize;

    if (machine == VM_INTERPRETED) {
        vm->machine = VM_INTERPRETED;
        printf("interpreting virtual machine...\n");
    } else {
        vm->machine = VM_COMPILED;
        printf("compiling VM into native machine code...\n");
        VM_Compile(vm);
    }

    return 0;
}

void VM_Free(vm_t *vm) {
    //if (vm->code) free(vm->code); vm->code = NULL;
    if (vm->data) free(vm->data); vm->data = NULL;
    if (vm->instructionPointers) free(vm->instructionPointers); vm->instructionPointers = NULL;
}

void VM_BlockCopy(vm_t *vm, uint32_t dest, uint32_t src, uint32_t n) {
    if ((dest & vm->dataMask) != dest
    || (src & vm->dataMask) != src
    || ((dest + n) & vm->dataMask) != dest + n
    || ((src + n) & vm->dataMask) != src + n) {
        printf("OP_BLOCK_COPY out of range!\n");
        exit(1);
    }

    memcpy(vm->data + dest, vm->data + src, n);
}

#define VM_PTR(x) ((uint32_t)(vm->data + (x & vm->dataMask & ~3)))

int32_t VM_Syscall(vm_t *vm, syscall_t syscall, int32_t *args) {
    switch(syscall) {
        case SYSCALL_PRINTLN:
            printf("SYSCALL PRINT: %s\n", (const char *)VM_PTR(args[0]));
            return 0;
        case SYSCALL_MULTIPLY:
            return args[0] * args[1];
        case SYSCALL_PIDIGIT:
            printf("%04d", args[0]);
            return 0;
        case SYSCALL_RECURSIVE: {
            int32_t res = VM_Call(vm, args[0], args[0]);
            return res;
        } default:
            printf("unknown syscall: %i\n", syscall);
            return 1;
    }
}

int32_t VM_Call(vm_t *vm, int arg1, int arg2) {
    if (vm->machine == VM_INTERPRETED) {
        return VM_CallInterpreted(vm, arg1, arg2);
    } else {
        return VM_CallCompiled(vm, arg1, arg2);
    }
}

void VM_ErrorJump(vm_t *vm) {
    printf("jump violation detected.\n");

    VM_Free(vm);
    exit(1);
}

int main(int argc, char **argv) {
    char qvmfile[512];
    vm_t vm;
    vmMachine_t machine;

    if (argc < 3) return 1;

    sprintf(qvmfile, "%s/%s.qvm", argv[1], argv[1]);

    machine = VM_INTERPRETED;
    if (!strcmp(argv[2], "compile")) {
        machine = VM_COMPILED;
    }

    if ( VM_Init(&vm, machine, qvmfile) ) {
        return EXIT_FAILURE;
    }

    printf("calling VM entry...\n");
    int32_t res = VM_Call(&vm, 20, 20);

    printf("\nRESULT: %i\n", res);

    VM_Free(&vm);
    return EXIT_SUCCESS;
}
