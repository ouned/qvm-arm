int entry(int arg1, int arg2) {
	int sint = arg1;
	unsigned int uint = (unsigned int)arg2;
	
	sint &= 0xFF0000FF; uint &= 0xFF0000FF;
	sint |= 0x000000FF; uint |= 0x000000FF;
	sint ^= 0x00FF0000; uint ^= 0x00FF0000;
	sint = ~sint; uint = ~uint;
	
	return sint + (int)uint;
}
