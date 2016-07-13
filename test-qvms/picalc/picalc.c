#define SCALE 10000
#define ARRINIT 2000

#define DIGITS 8000

int digitsarr[DIGITS + 1];

int trap_PIDigit(int digit);

void pi_digits(int digits);

int entry(int arg1, int arg2) {
	pi_digits(DIGITS);
	return 0;
}

void pi_digits(int digits) {
	int i;
	int carry = 0;
	int *arr = digitsarr;

	for (i = 0; i <= digits; ++i)
		arr[i] = ARRINIT;

	for (i = digits; i > 0; i -= 14) {
		int sum = 0;
		int j;

		for (j = i; j > 0; --j) {
			sum = sum * j + SCALE * arr[j];
			arr[j] = sum % (j * 2 - 1);
			sum /= j * 2 - 1;
		}

		trap_PIDigit(carry + sum / SCALE);
		carry = sum % SCALE;
	}
}
