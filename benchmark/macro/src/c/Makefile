SCHMLDIR=../../../..
LIBGC=$(SCHMLDIR)/src/backend-c/runtime/boehm-gc-install/lib/libgc.a
LIBS=$(LIBGC) -pthread -lm 
CC=clang
OPTS=-O3

all: bin/n-body

bin:
	mkdir bin

logs:
	mkdir logs
	$(CC) --version >> logs/compile.log
	echo options >> logs/compile.log
	echo $(OPTS) >> logs/compile.log
	echo libraries >> logs/compile.log
	echo $(LIBS) >> logs/compile.log

bin/%: bin logs
	$(CC) $*.c $(OPTS) $(LIBS) -o $@

clean:
	rm -rf *.o *~ bin logs
