int entry(int arg1, int arg2) {
	int res;
	unsigned int res2;
	
	res = arg1 << arg2;
	res = arg1 >> arg2;
	res2 = (unsigned int)arg1 >> (unsigned int)arg2;
	
	return res - res2;
}
