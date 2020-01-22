NDEBUG ?= -D NDEBUG

all : libgc nonegc.o runtime.o castprofiler.o

debug: tidy ndebug all

libgc : boehm-gc-install/lib/libgc.a

boehm-gc-install/lib/libgc.a :
	PWD=`pwd`; cd boehm-gc; ./configure --prefix=${PWD}/boehm-gc-install
	$(MAKE) -C boehm-gc
	$(MAKE) -C boehm-gc check
	$(MAKE) -C boehm-gc install
	-$(MAKE) -C boehm-gc/libatomic_ops -k distclean
	-$(MAKE) -C boehm-gc -k distclean

%.c :

%.o : %.c
	cc $(NDEBUG) -Iboehm-gc-install/include -std=c99 $< -c -o $@

runtime.o : boehm-gc-install/lib/libgc.a io.o assoc_stack.o hashcons.o suspended_cast.o cast_queue.o
	ld -r $^ -o $@

test : runtime.o
	cd tests && make all && ./tests

tidy :
	rm -f *.o

clean : tidy
	rm -rf boehm-gc-install
	-$(MAKE) -C boehm-gc/libatomic_ops -k distclean
	-$(MAKE) -C boehm-gc -k distclean
	cd tests && make clean

ndebug :
	$(eval NDEBUG := )
