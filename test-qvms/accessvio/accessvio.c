int entry(int arg1, int arg2) {
	int dummy;
	char *hehe = (char*)(0x12345678);
	short *hehe2 = (short*)(0x12345678);
	int *hehe3 = (int*)(0x12345678);

	dummy = *hehe;
	*hehe = 0;
	dummy += *hehe2;
	*hehe2 = 0;
	dummy += *hehe3;
	*hehe3 = 0;

	return dummy;
}
