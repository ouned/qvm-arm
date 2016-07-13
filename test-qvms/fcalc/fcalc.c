int entry(int arg1, int arg2) {
	int ret;
	float a = (float)arg1;
	float b = (float)arg2;
	
	ret = ((a + -b) * 2.0f - b) / 5.0f;
	return ret;
}
