TRANS:=src/sipc.c
INCLUDE:=include/
CFLAGS:= -c -Wall -fPIC -I$(INCLUDE)
STD:=--std=gnu11

all: clean libsipc sipcli

all-ffi: all libsipc-ffi

clean:
	rm -f *.o
	rm -f *.so
	rm -f sipcli
	rm -f *.socket

libsipc:
	gcc $(STD) $(CFLAGS) $(TRANS)
	gcc -shared -o $@.so *.o

libsipc-ffi:
	gcc $(STD) $(CFLAGS) $(TRANS) src/ffi.c
	gcc -shared -o $@.so *.o

sipcli: libsipc
	gcc $(STD) -Wall -pedantic -I$(INCLUDE) -Wl,-rpath=./ src/test.c -o $@ -L./ -lsipc
	./$@
