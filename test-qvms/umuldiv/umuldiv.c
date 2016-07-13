static unsigned int res;

int entry(int arg1, int arg2) {
	unsigned int num1 = (unsigned int)arg1;
	unsigned int num2 = (unsigned int)arg2;
	
	res = (num1 * num2) / 5;
	return (int)res;
}
