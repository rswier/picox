gcc -m32 -I. -o example example.c XLib.c
gcc -o xexample example.c -L/usr/X11R6/lib -lX11
