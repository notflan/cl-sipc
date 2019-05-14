TRANS:=src/sipc.c
INCLUDE:=include/
STD:=gnu11
CFLAGS:= -g --std=$(STD) -Wall -pedantic -I$(INCLUDE)
LCFLAGS:= -c $(CFLAGS) -fPIC

all: clean libsipc sipcli

all-ffi: all libsipc-ffi

clean:
	rm -f *.o
	rm -f *.so
	rm -f sipcli
	rm -f *.socket

libsipc:
	gcc $(LCFLAGS) $(TRANS)
	gcc -shared -o $@.so *.o

libsipc-ffi:
	gcc $(LCFLAGS) $(TRANS) src/ffi.c
	gcc -shared -o $@.so *.o

sipcli: libsipc
	gcc $(CFLAGS) -Wl,-rpath=./ src/cli.c -o $@ -L./ -lsipc
	./$@
