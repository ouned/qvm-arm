int entry(int arg1, int arg2) {
	int res = 0;

	goto wat;

lel:
	res += 100;
	return res;

wat:
	res = arg1 + arg2;

	goto lel;
}
