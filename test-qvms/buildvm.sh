#!/bin/bash

cd $1

../../compiler/q3lcc -A -S -Wf-target=bytecode -Wf-g $1.c

if [ -f syscalls.asm ]
then
	../../compiler/q3asm -vq3 -o $1 $1.asm syscalls.asm
else
	../../compiler/q3asm -vq3 -o $1 $1.asm
fi

