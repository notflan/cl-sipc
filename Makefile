HOME:=$(shell getent passwd `logname` | cut -d: -f6)
LOCAL_INSTALL:=$(HOME)/quicklisp/local-projects
SYSTEM_INSTALL:=/usr/local/lib

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
	if [[ $(shell id -u) == 0 ]]; then cp -f libsipc.so $(SYSTEM_INSTALL)/libsipc.so; fi

install-ffi-symbolic:
	ln -sf "`pwd`/libsipc.so" $(SYSTEM_INSTALL)/libsipc.so

install: ffi install-ffi
	if [ -d "$(LOCAL_INSTALL)" ]; then sudo -u `logname` ln -nsi "`pwd`" "$(LOCAL_INSTALL)/cl-sipc"; fi

uninstall-ffi:
	if [[ $(shell id -u) == 0 ]]; then rm -f $(SYSTEM_INSTALL)/libsipc.so; fi

uninstall: uninstall-ffi
	if [ -L "$(LOCAL_INSTALL)/cl-sipc" ] && [ "`readlink -f $(LOCAL_INSTALL)/cl-sipc`" == "`pwd`" ]; then sudo -u `logname` rm -i $(LOCAL_INSTALL)/cl-sipc; fi
