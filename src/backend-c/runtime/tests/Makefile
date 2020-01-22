all : tests

%.c :

%.o : %.c
	cc -I../boehm-gc-install/include -g -std=c99 $< -c -o $@

tests/%.c :

tests.o : ../boehm-gc-install/lib/libgc.a ../cast_queue.o ../suspended_cast.o cast_queue.o main.o
	ld -r $^ -o $@

tests : tests.o

clean :
	rm -f *.o
	rm tests
