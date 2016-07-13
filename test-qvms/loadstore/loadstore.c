static int global1;
static short global2;
static char global3;

int entry(int arg1, int arg2) {
	global1 = arg1 + arg2;
	global2 = arg1 + arg2;
	global3 = arg1 + arg2;
	
	return global1 + global2 + global3;
}
