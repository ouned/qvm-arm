#include <windows.h>
#include "compiler.h"

// ==============================================
// edi = programStack pointer
// esi = opStack pointer
// ==============================================

vm_t *currvm;
byte *nativeCode;
size_t nativeCodeSize, nativeCodeLength;

void EmitString(const char *str) {
    for (;;) {
        char bstr[3];
        bstr[0] = *str++; bstr[1] = *str++; bstr[2] = 0;

        byte b = strtol(bstr, NULL, 16);
        nativeCode[nativeCodeLength++] = b;

        if (!*str++)
            break;
    }
}

void EmitByte(byte b) {
    nativeCode[nativeCodeLength++] = b;
}

void EmitInteger(uint32_t num) {
    *(uint32_t *)(&nativeCode[nativeCodeLength]) = num;
    nativeCodeLength += sizeof(uint32_t);
}

void EmitConstantJumpInstruction(opcode_t opCode, uint32_t destinationOpCodeNum) {
    byte *sourceAddr = &nativeCode[nativeCodeLength];

    if (destinationOpCodeNum >= currvm->header.instructionCount) {
        printf("jump violation detected\n");
        exit(1);
        return;
    }

    byte *destAddr = currvm->jumpTable[destinationOpCodeNum];

    intptr_t jmpdiff = destAddr - (sourceAddr + 6);
    uintptr_t jmplen = (uintptr_t)jmpdiff;
    if (jmpdiff <= 0) {
        jmplen = 0xFFFFFFFF + jmpdiff - 5;
    }

    switch (opCode) {
        case OP_EQ:  EmitString("0F 84"); break; // JE:  Check equality (integer) (compares NIS vs TOS, jump to $PARM if true).
        case OP_NE:  EmitString("0F 85"); break; // JNE: Check inequality (integer) (NIS vs TOS, jump to $PARM if true).
        case OP_LTI: EmitString("0F 8C"); break; // JL:  Check less-than (signed integer) (NIS vs TOS, jump to $PARM if true).
        case OP_LEI: EmitString("0F 8E"); break; // JLE: Check less-than or equal-to (signed integer) (NIS vs TOS, jump to $PARM if true).
        case OP_GTI: EmitString("0F 8F"); break; // JG:  Check greater-than (signed integer) (NIS vs TOS), jump to $PARM if true.
        case OP_GEI: EmitString("0F 8D"); break; // JGE: Check greater-than or equal-to (signed integer) (NIS vs TOS), jump to $PARM if true.
        case OP_LTU: EmitString("0F 82"); break; // JB:  Check less-than (unsigned integer) (NIS vs TOS), jump to $PARM if true.
        case OP_LEU: EmitString("0F 86"); break; // JBE: Check less-than or equal-to (unsigned integer) (NIS vs TOS), jump to $PARM if true.
        case OP_GTU: EmitString("0F 87"); break; // JA:  Check greater-than (unsigned integer) (NIS vs TOS), jump to $PARM if true.
        case OP_GEU: EmitString("0F 83"); break; // JAE: Check greater-than or equal-to (unsigned integer) (NIS vs TOS), jump to $PARM if true.
        default: printf("OpCode implementation missing for %u\n", opCode); exit(1);
    }

    EmitInteger(jmplen);
}

void VM_CompiledSyscall() {
    byte *sp, *opsp;
    int32_t nic;

    __asm__ volatile(
        "mov %%edi, %[programStackOut]\n"
        "mov %%esi, %[opStackOut]\n"
        "mov %%eax, %[nicOut]"
        : [programStackOut] "=m" (sp),
          [opStackOut] "=m" (opsp),
          [nicOut] "=m" (nic)
    );

    syscall_t syscall = -nic - 1;
    int32_t *args = (int32_t *)(sp + sizeof(int32_t) * 2);

    opsp -= sizeof(int32_t);
    *(int32_t *)opsp = VM_Syscall(currvm, syscall, args);

    __asm__ volatile(
        "mov %[programStack], %%edi\n"
        "mov %[opStack], %%esi\n"
        :: [programStack] "m" (sp),
           [opStack] "m" (opsp)
    );
}

int VM_Compile(vm_t *vm) {
    size_t i, pass;

    currvm = vm;
    nativeCodeSize = vm->codeSize * 8;
    nativeCode = malloc(nativeCodeSize);
    vm->jumpTable = calloc(vm->header.instructionCount, sizeof(byte *));

    for (pass = 0; pass < 2; pass++) {
        nativeCodeLength = 0;

        for (i = 0; i < vm->header.instructionCount; i++) {
            byte *pcbp = vm->instructionPointers[i];
            byte *arg = pcbp + 1;

            vm->jumpTable[i] = &nativeCode[nativeCodeLength];

            switch (*pcbp) {
            /* ---------------------------------------------------------------------------- */
            /* ---------------------------------- BASICS ---------------------------------- */
            /* ---------------------------------------------------------------------------- */
                case OP_IGNORE:
                    // No-Operation (nop) instruction.

                    EmitString("90");                                   // nop
                    break;
                case OP_ENTER: {
                    // Begin procedure body, adjust stack $PARM octets for frame (always at least 8 (i.e. 2 words)).
                    // Frame contains all local storage/variables and arguments space for any calls within this procedure.

                    EmitString("81 EF");                                // sub edi, DWORD
                    EmitInteger(*(uint32_t *)arg);
                    break;
                } case OP_LEAVE: {
                    // End procedure body, $PARM is same as that of the matching ENTER.

                    EmitString("81 C7");                                // add edi, DWORD
                    EmitInteger(*(uint32_t *)arg);
                    EmitString("C3");                                   // ret
                    break;
                } case OP_LOCAL: {
                    // Get address of local storage (local variable or argument) (TOS <- (frame + $PARM)).

                    EmitString("89 F8");                                // mov eax, edi
                    EmitString("05");                                   // add eax, DWORD
                    EmitInteger(*(uint32_t *)arg);
                    EmitString("2D");                                   // sub eax, DWORD
                    EmitInteger((uint32_t)vm->data);
                    EmitString("83 ee 04");                             // sub esi, 4
                    EmitString("89 06");                                // mov [esi], eax
                    break;
                } case OP_CONST: {
                    // Push literal value onto stack (TOS <- $PARM).

                    EmitString("83 ee 04");                             // sub esi, 4
                    EmitString("C7 06");                                // mov [esi], DWORD
                    EmitInteger(*(uint32_t *)arg);
                    break;
                } case OP_LOAD1: {
                    // Load 1-octet value from address in TOS (TOS <- [TOS]).

                    EmitString("8B 06");                                // mov eax, [esi]
                    EmitString("0F B6 80");                             // movzx eax, BYTE [DWORD + eax]
                    EmitInteger((uint32_t)vm->data);
                    EmitString("89 06");                                // mov [esi], eax
                    break;
                } case OP_LOAD2: {
                    // Load 2-octet value from address in TOS (TOS <- [TOS]).

                    EmitString("8B 06");                                // mov eax, [esi]
                    EmitString("0F B7 80");                             // movzx eax, WORD [DWORD + eax]
                    EmitInteger((uint32_t)vm->data);
                    EmitString("89 06");                                // mov [esi], eax
                    break;
                } case OP_LOAD4: {
                    // Load 4-octet value from address in TOS (TOS <- [TOS]).

                    EmitString("8B 06");                                // mov eax, [esi]
                    EmitString("8B 80");                                // mov eax, [DWORD + eax]
                    EmitInteger((uint32_t)vm->data);
                    EmitString("89 06");                                // mov [esi], eax
                    break;
                } case OP_STORE1: {
                    // TOS is 1-octet value to store, destination address in next-in-stack ([NIS] <- TOS).

                    EmitString("8B 46 04");                             // mov eax, [esi + 4]
                    EmitString("8B 1E");                                // mov ebx, [esi]
                    EmitString("83 C6 08");                             // add esi, 8

                    EmitString("88 98");                                // mov [DWORD + eax], bl
                    EmitInteger((uint32_t)vm->data);
                    break;
                } case OP_STORE2: {
                    // TOS is 2-octet value to store, destination address in next-in-stack ([NIS] <- TOS).

                    EmitString("8B 46 04");                             // mov eax, [esi + 4]
                    EmitString("8B 1E");                                // mov ebx, [esi]
                    EmitString("83 C6 08");                             // add esi, 8

                    EmitString("66 89 98");                             // mov [DWORD + eax], bx
                    EmitInteger((uint32_t)vm->data);
                    break;
                } case OP_STORE4: {
                    // TOS is 4-octet value to store, destination address in next-in-stack ([NIS] <- TOS).

                    EmitString("8B 46 04");                             // mov eax, [esi + 4]
                    EmitString("8B 1E");                                // mov ebx, [esi]
                    EmitString("83 C6 08");                             // add esi, 8

                    EmitString("89 98");                                // mov [DWORD + eax], ebx
                    EmitInteger((uint32_t)vm->data);
                    break;
                } case OP_PUSH: {
                    // Push nonsense (void) value to opstack (TOS <- 0).

                    EmitString("83 ee 04");                             // sub esi, 4
                    break;
                } case OP_POP: {
                    // Pop a value from stack (remove TOS, decrease stack by 1).

                    EmitString("83 C6 04");                             // add esi, 4
                    break;
                } case OP_ARG: {
                    // TOS is 4-octet value to store into arguments-marshalling space of the indicated octet offset (ARGS[offset] <- TOS).

                    EmitString("8B 1E");                                // mov ebx, [esi]
                    EmitString("83 C6 04");                             // add esi, 4
                    EmitString("89 5F");                                // mov [edi + BYTE], ebx
                    EmitByte(*(byte *)arg);
                    break;
                } case OP_CALL: {
                    // Make call to procedure (code address <- TOS).

                    EmitString("8B 06");                                // mov eax, [esi]
                    EmitString("83 C6 04");                             // add esi, 4

                    EmitString("83 F8 00");                             // cmp eax, 0
                    EmitString("7D 09");                                // jge internal
                    EmitString("BB");                                   // mov ebx, DWORD
                    EmitInteger((uint32_t)(&VM_CompiledSyscall));
                    EmitString("FF D3");                                // call [ebx]
                    EmitString("EB 07");                                // jmp next
                    EmitString("FF 14 85");                             // internal: call [DWORD + eax * 4]
                    EmitInteger((uint32_t)vm->jumpTable);
                    break;                                              // next:
            /* ---------------------------------------------------------------------------- */
            /* --------------------------------- BRANCHES --------------------------------- */
            /* ---------------------------------------------------------------------------- */
                } case OP_JUMP: {
                    // Branch (code address <- TOS)

                    EmitString("8B 06");                                // mov eax, [esi]
                    EmitString("83 C6 04");                             // add esi, 4

                    EmitString("FF 24 85");                             // jmp [DWORD + eax * 4]
                    EmitInteger((uint32_t)vm->jumpTable);
                    break;
                } case OP_EQ:
                  case OP_NE:
                  case OP_LTI:
                  case OP_LEI:
                  case OP_GTI:
                  case OP_GEI:
                  case OP_LTU:
                  case OP_LEU:
                  case OP_GTU:
                  case OP_GEU: {
                    EmitString("8B 46 04");                             // mov eax, [esi + 4]
                    EmitString("8B 1E");                                // mov ebx, [esi]
                    EmitString("83 C6 08");                             // add esi, 8

                    EmitString("39 D8");                                // cmp eax, ebx
                    EmitConstantJumpInstruction(*pcbp, *(uint32_t *)arg);
                    break;
            /* ---------------------------------------------------------------------------- */
            /* -------------------------------- OPERATIONS -------------------------------- */
            /* ---------------------------------------------------------------------------- */
                } case OP_SEX8: {
                    // Sign-extend 8-bit (TOS <- TOS).

                    EmitString("0F BE 06");                             // movsx eax, BYTE [esi]
                    EmitString("89 06");                                // mov [esi], eax
                    break;
                } case OP_SEX16: {
                    // Sign-extend 16-bit (TOS <- TOS).

                    EmitString("0F BF 06");                             // movsx eax, WORD [esi]
                    EmitString("89 06");                                // mov [esi], eax
                    break;
                } case OP_NEGI: {
                    // Negate signed integer (TOS <- -TOS).

                    EmitString("F7 1E");                                // neg DWORD [esi]
                    break;
                } case OP_ADD: {
                    // Add integer-wise (TOS <- NIS + TOS).

                    EmitString("8B 46 04");                             // mov eax, [esi + 4]
                    EmitString("03 06");                                // add eax, [esi]
                    EmitString("83 C6 04");                             // add esi, 4
                    EmitString("89 06");                                // mov [esi], eax
                    break;
                } case OP_SUB: {
                    // Subtract integer-wise (TOS <- NIS - TOS).

                    EmitString("8B 46 04");                             // mov eax, [esi + 4]
                    EmitString("2B 06");                                // sub eax, [esi]
                    EmitString("83 C6 04");                             // add esi, 4
                    EmitString("89 06");                                // mov [esi], eax
                    break;
                } case OP_DIVI: {
                    // Divide (signed integer) (TOS <- NIS / TOS).

                    EmitString("8B 46 04");                             // mov eax, [esi + 4]
                    EmitString("31 D2");                                // xor edx, edx
                    EmitString("F7 3E");                                // idiv DWORD [esi]
                    EmitString("83 C6 04");                             // add esi, 4
                    EmitString("89 06");                                // mov [esi], eax
                    break;
                } case OP_DIVU: {
                    // Divide (unsigned integer) (TOS <- NIS / TOS).

                    EmitString("8B 46 04");                             // mov eax, [esi + 4]
                    EmitString("31 D2");                                // xor edx, edx
                    EmitString("F7 36");                                // div DWORD [esi]
                    EmitString("83 C6 04");                             // add esi, 4
                    EmitString("89 06");                                // mov [esi], eax
                    break;
                } case OP_MODI: {
                    // Modulo (signed integer) (TOS <- NIS mod TOS).

                    EmitString("8B 46 04");                             // mov eax, [esi + 4]
                    EmitString("31 D2");                                // xor edx, edx
                    EmitString("F7 3E");                                // idiv DWORD [esi]
                    EmitString("83 C6 04");                             // add esi, 4
                    EmitString("89 16");                                // mov [esi], edx
                    break;
                } case OP_MODU: {
                    // Modulo (unsigned integer) (TOS <- NIS mod TOS).

                    EmitString("8B 46 04");                             // mov eax, [esi + 4]
                    EmitString("31 D2");                                // xor edx, edx
                    EmitString("F7 36");                                // div DWORD [esi]
                    EmitString("83 C6 04");                             // add esi, 4
                    EmitString("89 16");                                // mov [esi], edx
                    break;
                } case OP_MULI: {
                    // Multiply (signed integer) (TOS <- NIS * TOS).

                    EmitString("8B 46 04");                             // mov eax, [esi + 4]
                    EmitString("0F AF 06");                             // imul eax, [esi]
                    EmitString("83 C6 04");                             // add esi, 4
                    EmitString("89 06");                                // mov [esi], eax
                    break;
                } case OP_MULU: {
                    // Multiply (unsigned integer) (TOS <- NIS * TOS).

                    EmitString("8B 46 04");                             // mov eax, [esi + 4]
                    EmitString("F7 26");                                // mul DWORD [esi]
                    EmitString("83 C6 04");                             // add esi, 4
                    EmitString("89 06");                                // mov [esi], eax
                    break;
                } case OP_BAND: {
                    // Bitwise AND (TOS <- NIS & TOS).

                    EmitString("8B 46 04");                             // mov eax, [esi + 4]
                    EmitString("23 06");                                // and eax, [esi]
                    EmitString("83 C6 04");                             // add esi, 4
                    EmitString("89 06");                                // mov [esi], eax
                    break;
                } case OP_BOR: {
                    // Bitwise OR (TOS <- NIS | TOS).

                    EmitString("8B 46 04");                             // mov eax, [esi + 4]
                    EmitString("0B 06");                                // or eax, [esi]
                    EmitString("83 C6 04");                             // add esi, 4
                    EmitString("89 06");                                // mov [esi], eax
                    break;
                } case OP_BXOR: {
                    // Bitwise XOR (TOS <- NIS ^ TOS).

                    EmitString("8B 46 04");                             // mov eax, [esi + 4]
                    EmitString("33 06");                                // xor eax, [esi]
                    EmitString("83 C6 04");                             // add esi, 4
                    EmitString("89 06");                                // mov [esi], eax
                    break;
                } case OP_BCOM: {
                    // Bitwise complement (TOS <- ~TOS).

                    EmitString("F7 16");                                // not DWORD [esi]
                    break;
                } case OP_LSH: {
                    // Bitwise left-shift (TOS <- NIS << TOS).

                    EmitString("8B 46 04");                             // mov eax, [esi + 4]
                    EmitString("8B 0E");                                // mov ecx, [esi]
                    EmitString("D3 E0");                                // shl eax, cl
                    EmitString("83 C6 04");                             // add esi, 4
                    EmitString("89 06");                                // mov [esi], eax
                    break;
                } case OP_RSHI: {
                    // Algebraic (signed) right-shift (TOS <- NIS >> TOS).

                    EmitString("8B 46 04");                             // mov eax, [esi + 4]
                    EmitString("8B 0E");                                // mov ecx, [esi]
                    EmitString("D3 F8");                                // sar eax, cl
                    EmitString("83 C6 04");                             // add esi, 4
                    EmitString("89 06");                                // mov [esi], eax
                    break;
                } case OP_RSHU: {
                    // Bitwise (unsigned) right-shift (TOS <- NIS >> TOS).

                    EmitString("8B 46 04");                             // mov eax, [esi + 4]
                    EmitString("8B 0E");                                // mov ecx, [esi]
                    EmitString("D3 E8");                                // shr eax, cl
                    EmitString("83 C6 04");                             // add esi, 4
                    EmitString("89 06");                                // mov [esi], eax
                    break;
                } default: {
                    printf("unknown instruction %#02x!\n", *pcbp);
                    exit(EXIT_FAILURE);
                }
            }
        }
    }

    printf("VM compiled into %u bytes of code\n", (unsigned int)nativeCodeLength);

    byte *oldCodePos = nativeCode;
    free(vm->code);
    vm->code = VirtualAlloc(NULL, nativeCodeLength, MEM_COMMIT, PAGE_EXECUTE_READWRITE);
    memcpy(vm->code, nativeCode, nativeCodeLength);
    vm->codeSize = nativeCodeLength;
    free(nativeCode);

    // fixup jumpTable for new locations
    intptr_t jmpdiff = vm->code - oldCodePos;
    for (i = 0; i < vm->header.instructionCount; i++) {
        vm->jumpTable[i] += jmpdiff;
    }

    return 0;
}

int32_t VM_CallCompiled(vm_t *vm, int32_t arg1, int32_t arg2) {
    byte *opsp, opStack[OPSTACK_SIZE];
    byte *sp;

    opsp = opStack + sizeof(opStack);
    sp = vm->data + vm->dataSize;

    // arguments
    sp -= sizeof(int32_t);
    *(int32_t *)sp = arg2;
    sp -= sizeof(int32_t);
    *(int32_t *)sp = arg1;

    // return stack
    sp -= sizeof(int32_t);
    *(int32_t *)sp = 0;

    // return address
    sp -= sizeof(int32_t);
    *(int32_t *)sp = -1;

    __asm__ volatile(
        "push %%eax\n"
        "push %%ebx\n"
        "push %%ecx\n"
        "push %%edx\n"
        "push %%edi\n"
        "push %%esi\n"

        "mov %[programStack], %%edi\n"
        "mov %[opStack], %%esi\n"

        "mov %[codeAddress], %%eax\n"
        "call *%%eax\n"

        "mov %%edi, %[programStackOut]\n"
        "mov %%esi, %[opStackOut]\n"

        "pop %%esi\n"
        "pop %%edi\n"
        "pop %%edx\n"
        "pop %%ecx\n"
        "pop %%ebx\n"
        "pop %%eax"
        : [programStackOut] "=m" (sp),
          [opStackOut] "=m" (opsp)
        : [codeAddress] "m" (vm->code),
          [programStack] "m" (sp),
          [opStack] "m" (opsp)
    );

    return *(int32_t *)(opsp);
}
