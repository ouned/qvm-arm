#include "interpreter.h"

int32_t VM_CallInterpreted(vm_t *vm, int32_t arg1, int32_t arg2) {
    uint32_t pc = 0;

    // arguments
    vm->sp -= sizeof(int32_t);
    *(int32_t *)vm->sp = arg2;
    vm->sp -= sizeof(int32_t);
    *(int32_t *)vm->sp = arg1;

    // return stack
    vm->sp -= sizeof(int32_t);
    *(int32_t *)vm->sp = 0;

    // return address
    vm->sp -= sizeof(int32_t);
    *(int32_t *)vm->sp = -1;

    for(;;) {
        if (pc >= vm->header.instructionCount) {
            VM_ErrorJump(vm);
        }

        byte *pcbp = vm->instructionPointers[pc];
        byte *arg = pcbp + 1;

        switch (*pcbp) {
        /* ---------------------------------------------------------------------------- */
        /* ---------------------------------- BASICS ---------------------------------- */
        /* ---------------------------------------------------------------------------- */
            case OP_IGNORE: {
                // No-Operation (nop) instruction.
                pc++;
                break;
            } case OP_ENTER: {
                // Begin procedure body, adjust stack $PARM octets for frame (always at least 8 (i.e. 2 words)).
                // Frame contains all local storage/variables and arguments space for any calls within this procedure.
                vm->sp -= *(uint32_t *)arg;

#ifdef TRACE_VM
                printf("OP_ENTER: $PARM=%u\n", *(uint32_t *)arg);
#endif

                pc++;
                break;
            } case OP_LEAVE: {
                // End procedure body, $PARM is same as that of the matching ENTER.
                vm->sp += *(uint32_t *)arg;

#ifdef TRACE_VM
                printf("OP_LEAVE: $PARM=%u\n", *(uint32_t *)arg);
#endif

                if (*(int32_t *)vm->sp == -1) {
                    // leaving VM

                    int32_t retval = *(int32_t *)(vm->opsp);
                    vm->opsp += sizeof(uint32_t);
                    vm->sp += sizeof(int32_t) * 4;
                    return retval;
                } else {
                    pc = *(int32_t *)vm->sp;
                }

                break;
            } case OP_LOCAL: {
                // Get address of local storage (local variable or argument) (TOS <- (frame + $PARM)).
                byte *addr = (byte *)((uintptr_t)vm->sp + *(uint32_t *)arg);
                uint32_t vmaddr = (uint32_t)(addr - vm->data);

#ifdef TRACE_VM
                printf("OP_LOCAL: VMADDR=%#x OFFSET=%u\n", vmaddr, *(uint32_t *)arg);
#endif

                vm->opsp -= sizeof(uint32_t);
                *(uint32_t *)vm->opsp = vmaddr;

                pc++;
                break;
            } case OP_CONST: {
                // Push literal value onto stack (TOS <- $PARM).
                vm->opsp -= sizeof(int32_t);
                *(int32_t *)vm->opsp = *(int32_t *)arg;

#ifdef TRACE_VM
                printf("OP_CONST: VAL=%i\n", *(int32_t *)arg);
#endif

                pc++;
                break;
            } case OP_LOAD1: {
                // Load 1-octet value from address in TOS (TOS <- [TOS]).
                uint32_t vmaddr = *(uint32_t *)vm->opsp;

                vmaddr &= vm->dataMask;
                uint8_t *addr = (uint8_t *)((uintptr_t)vm->data + vmaddr);

#ifdef TRACE_VM
                printf("OP_LOAD1: VMADDR=%#x VAL=%i\n", vmaddr, (int)*addr);
#endif

                *(uint32_t *)vm->opsp = *addr;

                pc++;
                break;
            } case OP_LOAD2: {
                // Load 2-octet value from address in TOS (TOS <- [TOS]).
                uint32_t vmaddr = *(uint32_t *)vm->opsp;

                vmaddr &= vm->dataMask & ~1;
                uint16_t *addr = (uint16_t *)((uintptr_t)vm->data + vmaddr);

#ifdef TRACE_VM
                printf("OP_LOAD2: VMADDR=%#x VAL=%i\n", *(uint32_t *)vm->opsp, (int)*addr);
#endif

                *(uint32_t *)vm->opsp = *addr;

                pc++;
                break;
            } case OP_LOAD4: {
                // Load 4-octet value from address in TOS (TOS <- [TOS]).
                uint32_t vmaddr = *(uint32_t *)vm->opsp;

                vmaddr &= vm->dataMask & ~3;
                uint32_t *addr = (uint32_t *)((uintptr_t)vm->data + vmaddr);

#ifdef TRACE_VM
                printf("OP_LOAD4: VMADDR=%#x VAL=%i\n", *(uint32_t *)vm->opsp, (int)*addr);
#endif

                *(uint32_t *)vm->opsp = *addr;

                pc++;
                break;
            } case OP_STORE1: {
                // Lowest octet of TOS is 1-octet value to store, destination address in next-in-stack ([NIS] <- TOS).
                uint8_t val = *(uint8_t *)vm->opsp;
                uint32_t vmaddr = *(uint32_t *)(vm->opsp + sizeof(uint32_t));
                vmaddr &= vm->dataMask;
                uint8_t *addr = (uint8_t *)((uintptr_t)vm->data + vmaddr);

#ifdef TRACE_VM
                printf("OP_STORE1: VMADDR=%#x VAL=%i\n", vmaddr, (int)val);
#endif

                *addr = val;
                vm->opsp += sizeof(int32_t); vm->opsp += sizeof(int32_t);

                pc++;
                break;
            } case OP_STORE2: {
                // Lowest two octets of TOS is 2-octet value to store, destination address in next-in-stack ([NIS] <- TOS).
                uint16_t val = *(uint16_t *)vm->opsp;
                uint32_t vmaddr = *(uint32_t *)(vm->opsp + sizeof(uint32_t));
                vmaddr &= vm->dataMask & ~1;
                uint16_t *addr = (uint16_t *)((uintptr_t)vm->data + vmaddr);

#ifdef TRACE_VM
                printf("OP_STORE2: VMADDR=%#x VAL=%i\n", vmaddr, (int)val);
#endif

                *addr = val;
                vm->opsp += sizeof(int32_t); vm->opsp += sizeof(int32_t);

                pc++;
                break;
            } case OP_STORE4: {
                // TOS is 4-octet value to store, destination address in next-in-stack ([NIS] <- TOS).
                uint32_t val = *(uint32_t *)vm->opsp;
                uint32_t vmaddr = *(uint32_t *)(vm->opsp + sizeof(uint32_t));
                vmaddr &= vm->dataMask & ~3;
                uint32_t *addr = (uint32_t *)((uintptr_t)vm->data + vmaddr);

#ifdef TRACE_VM
                printf("OP_STORE4: VMADDR=%#x VAL=%i\n", vmaddr, (int)val);
#endif

                *addr = val;
                vm->opsp += sizeof(int32_t); vm->opsp += sizeof(int32_t);

                pc++;
                break;
            } case OP_PUSH: {
                // Push nonsense (void) value to opstack (TOS <- 0).
                vm->opsp -= sizeof(int32_t);

#ifdef TRACE_VM
                printf("OP_PUSH\n");
#endif

                pc++;
                break;
            } case OP_POP: {
                // Pop a value from stack (remove TOS, decrease stack by 1).
                vm->opsp += sizeof(int32_t);

#ifdef TRACE_VM
                printf("OP_POP\n");
#endif

                pc++;
                break;
            } case OP_ARG: {
                // TOS is 4-octet value to store into arguments-marshalling space of the indicated octet offset (ARGS[offset] <- TOS).
                uint32_t vmaddr = vm->sp - vm->data + *(uint8_t *)arg;
                vmaddr &= vm->dataMask & ~3;
                int32_t *addr = (int32_t *)(vm->data + vmaddr);

#ifdef TRACE_VM
                printf("OP_ARG: VMADDR=%#x OFFSET=%u VAL=%i\n", vmaddr, (unsigned)(*(uint8_t *)arg), *(int32_t *)vm->opsp);
#endif

                *addr = *(int32_t *)vm->opsp;
                vm->opsp += sizeof(int32_t);

                pc++;
                break;
            } case OP_BLOCK_COPY: {
                // Copy $PARM bytes from [TOS] to [NIS]
                uint32_t from = *(uint32_t *)vm->opsp;
                uint32_t to = *(uint32_t *)(vm->opsp + sizeof(uint32_t));
                uint32_t length = *(uint32_t *)arg;

#ifdef TRACE_VM
                printf("OP_BLOCK_COPY: TO=%#x FROM=%#x LENGTH=%#x\n", to, from, length);
#endif

                vm->opsp += sizeof(int32_t); vm->opsp += sizeof(int32_t);
                VM_BlockCopy(vm, to, from, length);

                pc++;
                break;
        /* ---------------------------------------------------------------------------- */
        /* --------------------------------- BRANCHES --------------------------------- */
        /* ---------------------------------------------------------------------------- */
            } case OP_CALL: {
                // Make call to procedure (code address <- TOS).
                int32_t nic = *(int32_t *)vm->opsp;
                vm->opsp += sizeof(int32_t);

#ifdef TRACE_VM
                printf("OP_CALL: NIC=%#x\n", nic);
#endif

                if (nic >= 0) {
                    // in-vm call
                    *(int32_t *)vm->sp = pc + 1; // return address
                    pc = nic;
                } else {
                    // syscall
                    syscall_t syscall = -nic - 1;
                    int32_t *args = (int32_t *)(vm->sp + sizeof(int32_t) * 2);

                    int32_t res = VM_Syscall(vm, syscall, args);
                    vm->opsp -= sizeof(int32_t);
                    *(int32_t *)vm->opsp = res;

                    pc++;
                }

                break;
            } case OP_JUMP: {
                // Branch (code address <- TOS)
                uint32_t nic = *(uint32_t *)vm->opsp;

#ifdef TRACE_VM
                printf("OP_JUMP: NIC=%u\n", nic);
#endif

                vm->opsp += sizeof(int32_t);
                pc = nic;
                break;
            } case OP_EQ: {
                // Check equality (integer) (compares NIS vs TOS, jump to $PARM if true).
                int32_t tos = *(int32_t *)vm->opsp;
                int32_t nis = *(int32_t *)(vm->opsp + sizeof(int32_t));

#ifdef TRACE_VM
                printf("OP_EQ: %i==%i=%s NIC_ON_TRUE=%u\n", nis, tos, nis == tos ? "true" : "false", *(uint32_t *)arg);
#endif
                vm->opsp += sizeof(int32_t) * 2;
                if (nis == tos) {
                    pc = *(uint32_t *)arg;
                } else {
                    pc++;
                }

                break;
            }  case OP_NE: {
                // Check inequality (integer) (NIS vs TOS, jump to $PARM if true).
                int32_t tos = *(int32_t *)vm->opsp;
                int32_t nis = *(int32_t *)(vm->opsp + sizeof(int32_t));

#ifdef TRACE_VM
                printf("OP_NE: %i!=%i=%s NIC_ON_TRUE=%u\n", nis, tos, nis != tos ? "true" : "false", *(uint32_t *)arg);
#endif
                vm->opsp += sizeof(int32_t) * 2;
                if (nis != tos) {
                    pc = *(uint32_t *)arg;
                } else {
                    pc++;
                }

                break;
            } case OP_LTI: {
                // Check less-than (signed integer) (NIS vs TOS, jump to $PARM if true).
                int32_t tos = *(int32_t *)vm->opsp;
                int32_t nis = *(int32_t *)(vm->opsp + sizeof(int32_t));

#ifdef TRACE_VM
                printf("OP_LTI: %i<%i=%s NIC_ON_TRUE=%u\n", nis, tos, nis < tos ? "true" : "false", *(uint32_t *)arg);
#endif
                vm->opsp += sizeof(int32_t) * 2;
                if (nis < tos) {
                    pc = *(uint32_t *)arg;
                } else {
                    pc++;
                }

                break;
            } case OP_LEI: {
                // Check less-than or equal-to (signed integer) (NIS vs TOS, jump to $PARM if true).
                int32_t tos = *(int32_t *)vm->opsp;
                int32_t nis = *(int32_t *)(vm->opsp + sizeof(int32_t));

#ifdef TRACE_VM
                printf("OP_LEI: %i<=%i=%s NIC_ON_TRUE=%u\n", nis, tos, nis <= tos ? "true" : "false", *(uint32_t *)arg);
#endif
                vm->opsp += sizeof(int32_t) * 2;
                if (nis <= tos) {
                    pc = *(uint32_t *)arg;
                } else {
                    pc++;
                }

                break;
            } case OP_GTI: {
                // Check greater-than (signed integer) (NIS vs TOS), jump to $PARM if true.
                int32_t tos = *(int32_t *)vm->opsp;
                int32_t nis = *(int32_t *)(vm->opsp + sizeof(int32_t));

#ifdef TRACE_VM
                printf("OP_GTI: %i>%i=%s NIC_ON_TRUE=%u\n", nis, tos, nis > tos ? "true" : "false", *(uint32_t *)arg);
#endif
                vm->opsp += sizeof(int32_t) * 2;
                if (nis > tos) {
                    pc = *(uint32_t *)arg;
                } else {
                    pc++;
                }

                break;
            } case OP_GEI: {
                // Check greater-than or equal-to (signed integer) (NIS vs TOS), jump to $PARM if true.
                int32_t tos = *(int32_t *)vm->opsp;
                int32_t nis = *(int32_t *)(vm->opsp + sizeof(int32_t));

#ifdef TRACE_VM
                printf("OP_GEI: %i>=%i=%s NIC_ON_TRUE=%u\n", nis, tos, nis >= tos ? "true" : "false", *(uint32_t *)arg);
#endif
                vm->opsp += sizeof(int32_t) * 2;
                if (nis >= tos) {
                    pc = *(uint32_t *)arg;
                } else {
                    pc++;
                }

                break;
            } case OP_LTU: {
                // Check less-than (unsigned integer) (NIS vs TOS), jump to $PARM if true.
                uint32_t tos = *(uint32_t *)vm->opsp;
                uint32_t nis = *(uint32_t *)(vm->opsp + sizeof(uint32_t));

#ifdef TRACE_VM
                printf("OP_LTU: %i<%i=%s NIC_ON_TRUE=%u\n", nis, tos, nis < tos ? "true" : "false", *(uint32_t *)arg);
#endif
                vm->opsp += sizeof(uint32_t) * 2;
                if (nis < tos) {
                    pc = *(uint32_t *)arg;
                } else {
                    pc++;
                }

                break;
            } case OP_LEU: {
                // Check less-than or equal-to (unsigned integer) (NIS vs TOS), jump to $PARM if true.
                uint32_t tos = *(uint32_t *)vm->opsp;
                uint32_t nis = *(uint32_t *)(vm->opsp + sizeof(uint32_t));

#ifdef TRACE_VM
                printf("OP_LEU: %i<=%i=%s NIC_ON_TRUE=%u\n", nis, tos, nis <= tos ? "true" : "false", *(uint32_t *)arg);
#endif
                vm->opsp += sizeof(uint32_t) * 2;
                if (nis <= tos) {
                    pc = *(uint32_t *)arg;
                } else {
                    pc++;
                }

                break;
            } case OP_GTU: {
                // Check greater-than (unsigned integer) (NIS vs TOS), jump to $PARM if true.
                uint32_t tos = *(uint32_t *)vm->opsp;
                uint32_t nis = *(uint32_t *)(vm->opsp + sizeof(uint32_t));

#ifdef TRACE_VM
                printf("OP_GTU: %i>%i=%s NIC_ON_TRUE=%u\n", nis, tos, nis > tos ? "true" : "false", *(uint32_t *)arg);
#endif
                vm->opsp += sizeof(uint32_t) * 2;
                if (nis > tos) {
                    pc = *(uint32_t *)arg;
                } else {
                    pc++;
                }

                break;
            } case OP_GEU: {
                // Check greater-than or equal-to (unsigned integer) (NIS vs TOS), jump to $PARM if true.
                uint32_t tos = *(uint32_t *)vm->opsp;
                uint32_t nis = *(uint32_t *)(vm->opsp + sizeof(uint32_t));

#ifdef TRACE_VM
                printf("OP_GEU: %i>=%i=%s NIC_ON_TRUE=%u\n", nis, tos, nis >= tos ? "true" : "false", *(uint32_t *)arg);
#endif
                vm->opsp += sizeof(uint32_t) * 2;
                if (nis >= tos) {
                    pc = *(uint32_t *)arg;
                } else {
                    pc++;
                }

                break;
            } case OP_EQF: {
                // Check equality (float) (NIS vs TOS, jump to $PARM if true).
                float tos = *(float *)vm->opsp;
                float nis = *(float *)(vm->opsp + sizeof(float));

#ifdef TRACE_VM
                printf("OP_EQF: %f==%f=%s NIC_ON_TRUE=%u\n", nis, tos, nis == tos ? "true" : "false", *(uint32_t *)arg);
#endif
                vm->opsp += sizeof(float) * 2;
                if (nis == tos) {
                    pc = *(uint32_t *)arg;
                } else {
                    pc++;
                }

                break;
            } case OP_NEF: {
                // Check inequality (float) (NIS vs TOS, jump to $PARM if true).
                float tos = *(float *)vm->opsp;
                float nis = *(float *)(vm->opsp + sizeof(float));

#ifdef TRACE_VM
                printf("OP_NEF: %f!=%f=%s NIC_ON_TRUE=%u\n", nis, tos, nis != tos ? "true" : "false", *(uint32_t *)arg);
#endif
                vm->opsp += sizeof(float) * 2;
                if (nis != tos) {
                    pc = *(uint32_t *)arg;
                } else {
                    pc++;
                }

                break;
            } case OP_LTF: {
                // Check less-than (float) (NIS vs TOS, jump to $PARM if true).
                float tos = *(float *)vm->opsp;
                float nis = *(float *)(vm->opsp + sizeof(float));

#ifdef TRACE_VM
                printf("OP_LTF: %f<%f=%s NIC_ON_TRUE=%u\n", nis, tos, nis < tos ? "true" : "false", *(uint32_t *)arg);
#endif
                vm->opsp += sizeof(float) * 2;
                if (nis < tos) {
                    pc = *(uint32_t *)arg;
                } else {
                    pc++;
                }

                break;
            } case OP_LEF: {
                // Check less-than or equal-to (float) (NIS vs TOS, jump to $PARM if true).
                float tos = *(float *)vm->opsp;
                float nis = *(float *)(vm->opsp + sizeof(float));

#ifdef TRACE_VM
                printf("OP_LEF: %f<=%f=%s NIC_ON_TRUE=%u\n", nis, tos, nis <= tos ? "true" : "false", *(uint32_t *)arg);
#endif
                vm->opsp += sizeof(float) * 2;
                if (nis <= tos) {
                    pc = *(uint32_t *)arg;
                } else {
                    pc++;
                }

                break;
            } case OP_GTF: {
                // Check greater-than (float) (NIS vs TOS, jump to $PARM if true).
                float tos = *(float *)vm->opsp;
                float nis = *(float *)(vm->opsp + sizeof(float));

#ifdef TRACE_VM
                printf("OP_GTF: %f>%f=%s NIC_ON_TRUE=%u\n", nis, tos, nis > tos ? "true" : "false", *(uint32_t *)arg);
#endif
                vm->opsp += sizeof(float) * 2;
                if (nis > tos) {
                    pc = *(uint32_t *)arg;
                } else {
                    pc++;
                }

                break;
            } case OP_GEF: {
                // Check greater-than or equal-to (float) (NIS vs TOS, jump to $PARM if true).
                float tos = *(float *)vm->opsp;
                float nis = *(float *)(vm->opsp + sizeof(float));

#ifdef TRACE_VM
                printf("OP_GEF: %f>=%f=%s NIC_ON_TRUE=%u\n", nis, tos, nis >= tos ? "true" : "false", *(uint32_t *)arg);
#endif
                vm->opsp += sizeof(float) * 2;
                if (nis >= tos) {
                    pc = *(uint32_t *)arg;
                } else {
                    pc++;
                }

                break;
        /* ---------------------------------------------------------------------------- */
        /* -------------------------------- OPERATIONS -------------------------------- */
        /* ---------------------------------------------------------------------------- */
            } case OP_SEX8: {
                // Sign-extend 8-bit (TOS <- TOS).
#ifdef TRACE_VM
                printf("OP_SEX8: UNSIGNED=%u SIGNED=%i\n", *(uint8_t *)vm->opsp, (int8_t)(*(uint8_t *)vm->opsp));
#endif

                *(int32_t *)vm->opsp = (int8_t)(*(uint8_t *)vm->opsp);
                pc++;
                break;
            } case OP_SEX16: {
                // Sign-extend 16-bit (TOS <- TOS).
#ifdef TRACE_VM
                printf("OP_SEX16: UNSIGNED=%u SIGNED=%i\n", *(uint16_t *)vm->opsp, (int16_t)(*(uint16_t *)vm->opsp));
#endif

                *(int32_t *)vm->opsp = (int16_t)(*(uint16_t *)vm->opsp);
                pc++;
                break;
            } case OP_NEGI: {
                // Negate signed integer (TOS <- -TOS).
#ifdef TRACE_VM
                printf("OP_NEGI: POSI=%i NEGI=%i\n", (int)(*(int32_t *)vm->opsp), (int)(-(*(int32_t *)vm->opsp)));
#endif

                *(int32_t *)vm->opsp = -(*(int32_t *)vm->opsp);
                pc++;
                break;
            } case OP_ADD: {
                // Add integer-wise (TOS <- NIS + TOS).
                int32_t op2 = *(int32_t *)vm->opsp;
                int32_t op1 = *(int32_t *)(vm->opsp + sizeof(int32_t));

#ifdef TRACE_VM
                printf("OP_ADD: %i+%i=%i\n", op1, op2, op1 + op2);
#endif

                vm->opsp += sizeof(int32_t);
                *(int32_t *)vm->opsp = op1 + op2;

                pc++;
                break;
            } case OP_SUB: {
                // Subtract integer-wise (TOS <- NIS - TOS).
                int32_t op2 = *(int32_t *)vm->opsp;
                int32_t op1 = *(int32_t *)(vm->opsp + sizeof(int32_t));

#ifdef TRACE_VM
                printf("OP_SUB: %i-%i=%i\n", op1, op2, op1 - op2);
#endif

                vm->opsp += sizeof(int32_t);
                *(int32_t *)vm->opsp = op1 - op2;

                pc++;
                break;
            } case OP_DIVI: {
                // Divide (signed integer) (TOS <- NIS / TOS).
                int32_t op2 = *(int32_t *)vm->opsp;
                int32_t op1 = *(int32_t *)(vm->opsp + sizeof(int32_t));

#ifdef TRACE_VM
                printf("OP_DIVI: %i/%i=%i\n", op1, op2, op1 / op2);
#endif

                vm->opsp += sizeof(int32_t);
                *(int32_t *)vm->opsp = op1 / op2;

                pc++;
                break;
            } case OP_DIVU: {
                // Divide (unsigned integer) (TOS <- NIS / TOS).
                uint32_t op2 = *(uint32_t *)vm->opsp;
                uint32_t op1 = *(uint32_t *)(vm->opsp + sizeof(uint32_t));

#ifdef TRACE_VM
                printf("OP_DIVU: %u/%u=%u\n", op1, op2, op1 / op2);
#endif

                vm->opsp += sizeof(uint32_t);
                *(uint32_t *)vm->opsp = op1 / op2;

                pc++;
                break;
            } case OP_MODI: {
                // Modulo (signed integer) (TOS <- NIS mod TOS).
                int32_t op2 = *(int32_t *)vm->opsp;
                int32_t op1 = *(int32_t *)(vm->opsp + sizeof(int32_t));

#ifdef TRACE_VM
                printf("OP_MODI: %i%%%i=%i\n", op1, op2, op1 % op2);
#endif

                vm->opsp += sizeof(int32_t);
                *(int32_t *)vm->opsp = op1 % op2;

                pc++;
                break;
            } case OP_MODU: {
                // Modulo (unsigned integer) (TOS <- NIS mod TOS).
                uint32_t op2 = *(uint32_t *)vm->opsp;
                uint32_t op1 = *(uint32_t *)(vm->opsp + sizeof(uint32_t));

#ifdef TRACE_VM
                printf("OP_MODU: %u%%%u=%u\n", op1, op2, op1 % op2);
#endif

                vm->opsp += sizeof(uint32_t);
                *(uint32_t *)vm->opsp = op1 % op2;

                pc++;
                break;
            } case OP_MULI: {
                // Multiply (signed integer) (TOS <- NIS * TOS).
                int32_t op2 = *(int32_t *)vm->opsp;
                int32_t op1 = *(int32_t *)(vm->opsp + sizeof(int32_t));

#ifdef TRACE_VM
                printf("OP_MULI: %i*%i=%i\n", op1, op2, op1 * op2);
#endif

                vm->opsp += sizeof(int32_t);
                *(int32_t *)vm->opsp = op1 * op2;

                pc++;
                break;
            } case OP_MULU: {
                // Multiply (unsigned integer) (TOS <- NIS * TOS).
                uint32_t op2 = *(uint32_t *)vm->opsp;
                uint32_t op1 = *(uint32_t *)(vm->opsp + sizeof(uint32_t));

#ifdef TRACE_VM
                printf("OP_MULU: %u*%u=%u\n", op1, op2, op1 * op2);
#endif

                vm->opsp += sizeof(uint32_t);
                *(uint32_t *)vm->opsp = op1 * op2;

                pc++;
                break;
            } case OP_BAND: {
                // Bitwise AND (TOS <- NIS & TOS).
                uint32_t op2 = *(uint32_t *)vm->opsp;
                uint32_t op1 = *(uint32_t *)(vm->opsp + sizeof(uint32_t));

#ifdef TRACE_VM
                printf("OP_BAND: %u&%u=%u\n", op1, op2, op1 & op2);
#endif

                vm->opsp += sizeof(uint32_t);
                *(uint32_t *)vm->opsp = op1 & op2;

                pc++;
                break;
            } case OP_BOR: {
                // Bitwise OR (TOS <- NIS | TOS).
                uint32_t op2 = *(uint32_t *)vm->opsp;
                uint32_t op1 = *(uint32_t *)(vm->opsp + sizeof(uint32_t));

#ifdef TRACE_VM
                printf("OP_BOR: %u|%u=%u\n", op1, op2, op1 | op2);
#endif

                vm->opsp += sizeof(uint32_t);
                *(uint32_t *)vm->opsp = op1 | op2;

                pc++;
                break;
            } case OP_BXOR: {
                // Bitwise XOR (TOS <- NIS ^ TOS).
                uint32_t op2 = *(uint32_t *)vm->opsp;
                uint32_t op1 = *(uint32_t *)(vm->opsp + sizeof(uint32_t));

#ifdef TRACE_VM
                printf("OP_BXOR: %u^%u=%u\n", op1, op2, op1 ^ op2);
#endif

                vm->opsp += sizeof(uint32_t);
                *(uint32_t *)vm->opsp = op1 ^ op2;

                pc++;
                break;
            } case OP_BCOM: {
                // Bitwise complement (TOS <- ~TOS).
                uint32_t op = *(uint32_t *)vm->opsp;

#ifdef TRACE_VM
                printf("OP_BCOM: ~%u=%u\n", op, ~op);
#endif

                *(uint32_t *)vm->opsp = ~op;

                pc++;
                break;
            } case OP_LSH: {
                // Bitwise left-shift (TOS <- NIS << TOS).
                uint32_t op2 = *(uint32_t *)vm->opsp;
                uint32_t op1 = *(uint32_t *)(vm->opsp + sizeof(uint32_t));

#ifdef TRACE_VM
                printf("OP_LSH: %i<<%i=%i\n", op1, op2, op1 << op2);
#endif

                vm->opsp += sizeof(uint32_t);
                *(uint32_t *)vm->opsp = op1 << op2;

                pc++;
                break;
            } case OP_RSHI: {
                // Algebraic (signed) right-shift (TOS <- NIS >> TOS).
                int32_t op2 = *(int32_t *)vm->opsp;
                int32_t op1 = *(int32_t *)(vm->opsp + sizeof(int32_t));

#ifdef TRACE_VM
                printf("OP_RSHI: %i>>%i=%i\n", op1, op2, op1 >> op2);
#endif

                vm->opsp += sizeof(int32_t);
                *(int32_t *)vm->opsp = op1 >> op2;

                pc++;
                break;
            } case OP_RSHU: {
                // Bitwise (unsigned) right-shift (TOS <- NIS >> TOS).
                uint32_t op2 = *(uint32_t *)vm->opsp;
                uint32_t op1 = *(uint32_t *)(vm->opsp + sizeof(uint32_t));

#ifdef TRACE_VM
                printf("OP_RSHU: %u>>%u=%u\n", op1, op2, op1 >> op2);
#endif

                vm->opsp += sizeof(uint32_t);
                *(uint32_t *)vm->opsp = op1 >> op2;

                pc++;
                break;
            } case OP_NEGF: {
                // Negate float value (TOS <- -TOS).
                float op = *(float *)vm->opsp;

#ifdef TRACE_VM
                printf("OP_NEGF: %f=>%f\n", op, -op);
#endif

                *(float *)vm->opsp = -op;

                pc++;
                break;
            } case OP_ADDF: {
                // Add integer-wise (TOS <- NIS + TOS).
                float op2 = *(float *)vm->opsp;
                float op1 = *(float *)(vm->opsp + sizeof(float));

#ifdef TRACE_VM
                printf("OP_ADDF: %f+%f=%f\n", op1, op2, op1 + op2);
#endif

                vm->opsp += sizeof(float);
                *(float *)vm->opsp = op1 + op2;

                pc++;
                break;
            } case OP_SUBF: {
                // Subtract floats (TOS <- NIS - TOS).
                float op2 = *(float *)vm->opsp;
                float op1 = *(float *)(vm->opsp + sizeof(float));

#ifdef TRACE_VM
                printf("OP_SUBF: %f-%f=%f\n", op1, op2, op1 - op2);
#endif

                vm->opsp += sizeof(float);
                *(float *)vm->opsp = op1 - op2;

                pc++;
                break;
            } case OP_DIVF: {
                // Divide floats (TOS <- NIS / TOS).
                float op2 = *(float *)vm->opsp;
                float op1 = *(float *)(vm->opsp + sizeof(float));

#ifdef TRACE_VM
                printf("OP_DIVF: %f/%f=%f\n", op1, op2, op1 / op2);
#endif

                vm->opsp += sizeof(float);
                *(float *)vm->opsp = op1 / op2;

                pc++;
                break;
            } case OP_MULF: {
                // Multiply floats (TOS <- NIS x TOS).
                float op2 = *(float *)vm->opsp;
                float op1 = *(float *)(vm->opsp + sizeof(float));

#ifdef TRACE_VM
                printf("OP_MULF: %f*%f=%f\n", op1, op2, op1 * op2);
#endif

                vm->opsp += sizeof(float);
                *(float *)vm->opsp = op1 * op2;

                pc++;
                break;
            } case OP_CVIF: {
                // Convert signed integer to float (TOS <- TOS).
                int32_t op = *(int32_t *)vm->opsp;

#ifdef TRACE_VM
                printf("OP_CVIF: %i=>%f\n", op, (float)op);
#endif

                *(float *)vm->opsp = (float)op;

                pc++;
                break;
            } case OP_CVFI: {
                // Convert float to signed integer (TOS <- TOS).
                float op = *(float *)vm->opsp;

#ifdef TRACE_VM
                printf("OP_CVFI: %f=>%i\n", op, (int32_t)op);
#endif

                *(int32_t *)vm->opsp = (int32_t)op;

                pc++;
                break;
            } default: {
                printf("unknown instruction %#02x!\n", *pcbp);
                exit(EXIT_FAILURE);
			}
        }
    }
}
