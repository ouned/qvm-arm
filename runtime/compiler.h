#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include "main.h"
#ifndef COMPILER_H
#define COMPILER_H

int VM_Compile(vm_t *vm);
int32_t VM_CallCompiled(vm_t *vm, int32_t arg1, int32_t arg2);

#endif // COMPILER_H

