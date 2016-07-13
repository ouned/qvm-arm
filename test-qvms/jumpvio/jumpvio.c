int entry(int arg1, int arg2) {
	void (*test)(void) = (void(*)(void))(0x12345678);

	test();
	return 0;
}
