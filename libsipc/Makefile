TRANS:=src/sipc.c
INCLUDE:=include/
CFLAGS:= -c -Wall -pedantic -fPIC -I$(INCLUDE)

all: clean libsipc sipcli

clean:
	rm -f *.o
	rm -f *.so
	rm -f sipcli
	rm -f *.socket

libsipc:
	gcc $(CFLAGS) $(TRANS)
	gcc -shared -o $@.so *.o

sipcli: libsipc
	gcc -Wall -pedantic -I$(INCLUDE) -Wl,-rpath=./ src/test.c -o $@ -L./ -lsipc
	./$@
