int getnumber(int num);

int entry(int arg1, int arg2) {
	float a = (float)arg1 + 2.5f * arg2;

	if (a >= 100) {
		return getnumber(10);
	} else {
		return getnumber(20);
	}
}

int getnumber(int num) {
	return num;
}
