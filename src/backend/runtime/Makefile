NDEBUG ?= -D NDEBUG

all : libgc nonegc.o runtime.o castprofiler.o

debug: tidy ndebug all

libgc : boehm-gc-install/lib/libgc.a

boehm-gc-install/lib/libgc.a :
	PWD=`pwd`; mkdir boehm-gc/build; \
		   cd boehm-gc/build; \
		   ../configure --prefix=${PWD}/boehm-gc-install
	$(MAKE) -C boehm-gc/build
	$(MAKE) -C boehm-gc/build check
	$(MAKE) -C boehm-gc/build install
	rm -r boehm-gc/build

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
	-cd tests && make clean

ndebug :
	$(eval NDEBUG := )
