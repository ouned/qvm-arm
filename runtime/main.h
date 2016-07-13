#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#ifndef MAIN_H
#define MAIN_H

#define VM_MAGIC 0x12721444

#define OPSTACK_SIZE 256
#define OPSTACK_MASK (OPSTACK_SIZE-1)

#define PROGRAMSTACK_SIZE 0x10000
#define PROGRAMSTACK_MASK (PROGRAMSTACK_SIZE-1)

#define MAX_SYSCALL_ARGS 15

typedef uint8_t byte;
typedef enum {qfalse, qtrue} qboolean;

typedef struct {
    uint32_t magic;
    uint32_t instructionCount;
    uint32_t codeSegmentOffset; uint32_t codeSegmentLength;
    uint32_t dataSegmentOffset; uint32_t dataSegmentLength;
    uint32_t litSegmentLength;
    uint32_t bssSegmentLength;
} vmHeader_t;

typedef enum {
    VM_INTERPRETED,
    VM_COMPILED
} vmMachine_t;

typedef struct {
    vmMachine_t machine;

    byte *code;
    uint32_t codeSize;

    byte *data;
    uint32_t dataSize;
    uint32_t dataMask;

    vmHeader_t header;

    byte programStack[PROGRAMSTACK_SIZE];
    byte *sp;

    byte **instructionPointers;
    byte **jumpTable;
} vm_t;

typedef enum {
	OP_UNDEF,

	OP_IGNORE,

	OP_BREAK,

	OP_ENTER,
	OP_LEAVE,
	OP_CALL,
	OP_PUSH,
	OP_POP,

	OP_CONST,
	OP_LOCAL,

	OP_JUMP,

	//-------------------

	OP_EQ,
	OP_NE,

	OP_LTI,
	OP_LEI,
	OP_GTI,
	OP_GEI,

	OP_LTU,
	OP_LEU,
	OP_GTU,
	OP_GEU,

	OP_EQF,
	OP_NEF,

	OP_LTF,
	OP_LEF,
	OP_GTF,
	OP_GEF,

	//-------------------

	OP_LOAD1,
	OP_LOAD2,
	OP_LOAD4,
	OP_STORE1,
	OP_STORE2,
	OP_STORE4,
	OP_ARG,

	OP_BLOCK_COPY,

	//-------------------

	OP_SEX8,
	OP_SEX16,

	OP_NEGI,
	OP_ADD,
	OP_SUB,
	OP_DIVI,
	OP_DIVU,
	OP_MODI,
	OP_MODU,
	OP_MULI,
	OP_MULU,

	OP_BAND,
	OP_BOR,
	OP_BXOR,
	OP_BCOM,

	OP_LSH,
	OP_RSHI,
	OP_RSHU,

	OP_NEGF,
	OP_ADDF,
	OP_SUBF,
	OP_DIVF,
	OP_MULF,

	OP_CVIF,
	OP_CVFI
} opcode_t;

int VM_Init(vm_t *vm, vmMachine_t machine, const char *file);
void VM_Free(vm_t *vm);
void VM_BlockCopy(vm_t *vm, uint32_t dest, uint32_t src, uint32_t n);
void VM_ErrorJump(vm_t *vm);
int32_t VM_Call(vm_t *vm, int arg1, int arg2);

typedef enum {
    SYSCALL_PRINTLN,
    SYSCALL_MULTIPLY,
    SYSCALL_PIDIGIT,
    SYSCALL_RECURSIVE
} syscall_t;

int32_t VM_Syscall(vm_t *vm, syscall_t syscall, int32_t *args);

#endif // MAIN_H

