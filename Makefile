

ffi:
	cd libsipc && make libsipc-ffi

clean:
	cd libsipc && make clean
	rm -f *.socket

install: ffi
	cp -f libsipc.so /usr/lib/libsipc.so

uninstall:
	rm -f /usr/lib/libsipc.so
