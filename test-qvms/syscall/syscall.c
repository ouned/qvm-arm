int trap_Println(const char *line);
int trap_Multiply(int num1, int num2);

void printf(const char *txt);

int entry(int arg1, int arg2) {
	int res;
	
	res = trap_Multiply(arg1, arg2);
	res += 1000;
	
	printf("Result is calculated.");
	return res;
}

void printf(const char *txt) {
	trap_Println(txt);
}
