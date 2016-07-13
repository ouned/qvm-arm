int entry(int arg1, int arg2) {
	short num1 = (short)arg1;
	short num2 = (short)arg2;
	
	return (int)((unsigned int)num1 % (unsigned int)num2);
}
