TRANS:=src/sipc.c
INCLUDE:=include/
CFLAGS:= -c -Wall -pedantic -fPIC -I$(INCLUDE)

all: clean libsipc test

clean:
	rm -f *.o
	rm -f *.so
	rm -f test
	rm -f *.socket

libsipc:
	gcc $(CFLAGS) $(TRANS)
	gcc -shared -o $@.so *.o

test: libsipc
	gcc -Wall -pedantic -I$(INCLUDE) -Wl,-rpath=./ src/test.c -o $@ -L./ -lsipc
	./$@
