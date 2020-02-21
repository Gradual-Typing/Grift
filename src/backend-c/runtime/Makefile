NDEBUG ?= -D NDEBUG

all : libgc nonegc.o runtime.o castprofiler.o

debug: tidy ndebug libgc nonegc.o debug_object/runtime.o castprofiler.o

profile: tidy ndebug libgc nonegc.o profile_object/runtime.o castprofiler.o

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

debug_object/%.o : %.c debug_object
	cc -Iboehm-gc-install/include -std=c99 -g $< -c -o $@

debug_object:
	mkdir $@

debug_object/runtime.o : boehm-gc-install/lib/libgc.a debug_object/io.o debug_object/assoc_stack.o debug_object/hashcons.o debug_object/suspended_cast.o debug_object/cast_queue.o
	ld -r $^ -o $@

profile_object/%.o : %.c profile_object
	cc -Iboehm-gc-install/include -std=c99 -pg $< -c -o $@

profile_object:
	mkdir $@

profile_object/runtime.o : boehm-gc-install/lib/libgc.a profile_object/io.o profile_object/assoc_stack.o profile_object/hashcons.o profile_object/suspended_cast.o profile_object/cast_queue.o
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
