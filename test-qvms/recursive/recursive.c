int trap_Recursive(int arg);

int entry(int arg1, int arg2) {
	if (arg1 == 1337) {
		return trap_Recursive(-10);
	} else if (arg1 == -10) {
		return 1001;
	}
	
	return trap_Recursive(1337);
}
