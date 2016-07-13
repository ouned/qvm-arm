# QVM ARM JIT
This is my q3vm arm jit implementation.  
I made this purely because I was interested in how the VM works.  
It is fully functional and was [successfully tested on a raspberry pi](https://www.youtube.com/watch?v=LsCRgXWleMw) running [jk2mv](https://github.com/mvdevs/jk2mv). 

Interpreter / x86 jit are half-implemented and shouldn't be used without further investigation.

# Features
- opStack, dataSegment protection
- generates code based on available processor features
- works on >= armv5
- gnueabi only

![Status](http://abload.de/img/progressh2uix.png)

# LICENSE
GPLv2
