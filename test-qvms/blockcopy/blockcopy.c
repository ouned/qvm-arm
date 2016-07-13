typedef struct {
	char name[64];
	int arr[16];
} lelstruct;

int entry(int arg1, int arg2) {
	lelstruct a, b;
	
	a.name[0] = 'z';
	a.arr[15] = 0xFFFFFFF;
	
	b = a;
	return b.arr[15];
}
