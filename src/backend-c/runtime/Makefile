all : boehm-gc-install runtime.o nonegc.o

boehm-gc-install :
	PWD=`pwd`; cd boehm-gc; ./configure --prefix=${PWD}/boehm-gc-install
	$(MAKE) -C boehm-gc
	$(MAKE) -C boehm-gc check
	$(MAKE) -C boehm-gc install

%.c :

%.o : %.c
	cc $< -c -o $@

clean :
	rm -rf boehm-gc-install
	-$(MAKE) -C boehm-gc/libatomic_ops -k distclean
	-$(MAKE) -C boehm-gc -k distclean
	rm -f *.o
