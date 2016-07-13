@ECHO OFF
cd %1

if exist %1.c (
	..\..\compiler\q3lcc.exe -A -S -Wf-target=bytecode -Wf-g %1.c

	if exist syscalls.asm (
		..\..\compiler\q3asm.exe -vq3 -o %1 %1.asm syscalls.asm
	) else (
		..\..\compiler\q3asm.exe -vq3 -o %1 %1.asm
	)
) else (
	..\..\compiler\q3asm.exe -vq3 -o %1 %1.asm
)

cd ..