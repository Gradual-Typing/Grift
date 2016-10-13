all : boehm-gc-install runtime.o nonegc.o

boehm-gc-install :
	PWD=`pwd`; cd boehm-gc; ./configure --prefix=${PWD}/boehm-gc-install
	cd boehm-gc; make
	cd boehm-gc; make check
	cd boehm-gc; make install

%.c :

%.o : %.c
	cc $< -c -o $@

clean :
	cd boehm-gc; make distclean
	rm -rf boehm-gc-install
	rm -f *.o
