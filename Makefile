HOME:=$(shell getent passwd `logname` | cut -d: -f6)
LOCAL_PROJECTS:=$(HOME)/quicklisp/local-projects

ffi:
	cd libsipc && make libsipc-ffi

utils:
	cd libsipc && make all-ffi
	ln -sf libsipc/sipcli sipcli

clean:
	cd libsipc && make clean
	rm -f *.socket
	rm -f sipcli

install-ffi:
	if [[ $(shell id -u) == 0 ]]; then cp -f libsipc.so /usr/lib/libsipc.so; fi

install: ffi install-ffi
	if [ -d "$(LOCAL_PROJECTS)" ]; then sudo -u `logname` ln -nsi "`pwd`" "$(LOCAL_PROJECTS)/cl-sipc"; fi

uninstall-ffi:
	if [[ $(shell id -u) == 0 ]]; then rm -f /usr/lib/libsipc.so; fi

uninstall: uninstall-ffi
	if [ -L "$(LOCAL_PROJECTS)/cl-sipc" ] && [ "`readlink -f $(LOCAL_PROJECTS)/cl-sipc`" == "`pwd`" ]; then sudo -u `logname` rm -i $(LOCAL_PROJECTS)/cl-sipc; fi
