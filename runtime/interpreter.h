#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include "main.h"
#ifndef INTERPRETER_H
#define INTERPRETER_H

#define TRACE_VM

int32_t VM_CallInterpreted(vm_t *vm, int32_t arg1, int32_t arg2);

#endif // INTERPRETER_H

