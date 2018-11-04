NDEBUG ?= -D NDEBUG

all : boehm-gc-install nonegc.o runtime.o castprofiler.o

debug: tidy ndebug all

boehm-gc-install :
	PWD=`pwd`; cd boehm-gc; ./configure --prefix=${PWD}/boehm-gc-install
	$(MAKE) -C boehm-gc
	$(MAKE) -C boehm-gc check
	$(MAKE) -C boehm-gc install
	-$(MAKE) -C boehm-gc/libatomic_ops -k distclean
	-$(MAKE) -C boehm-gc -k distclean

%.c :

%.o : %.c
	cc $(NDEBUG) $< -c -o $@

runtime.o : io.o assoc_stack.o hashcons.o
	ld -r $^ -o $@

tidy :
	rm -f *.o	

clean : tidy
	rm -rf boehm-gc-install
	-$(MAKE) -C boehm-gc/libatomic_ops -k distclean
	-$(MAKE) -C boehm-gc -k distclean


ndebug :
	$(eval NDEBUG := )
