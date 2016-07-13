int afunc(int arg2);

int entry(int arg1, int arg2) {
	return arg1 + afunc(arg2);
}

int afunc(int arg2) {
	return arg2 + 10;
}
