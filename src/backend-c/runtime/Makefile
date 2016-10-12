BOEHM_GC_INSTALL=boehm-gc-install
boehm-gc-install :
	PWD=`pwd`; cd boehm-gc; ./configure --prefix=${PWD}/${BOEHM_GC_INSTALL}
	cd boehm-gc; make
	cd boehm-gc; make check
	cd boehm-gc; make install

all : boehm-gc-install
	cc runtime.c -c -o runtime.o
	cc nonegc.c  -c -o nonegc.o

clean :
	cd boehm-gc; make distclean
	rm -rf $BOEHM_GC_INSTALL
	rm -f *.o
