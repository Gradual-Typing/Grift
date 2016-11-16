CC=gsc
OPTS=-exe -cc-options "-O3"

all: bin/n-body

bin:
	mkdir bin

logs:
	mkdir logs
	$(CC) -v >> logs/compile.log
	echo options >> logs/compile.log
	echo $(OPTS) >> logs/compile.log

bin/%: bin logs
	$(CC) $(OPTS) -o $@ $*.scm

clean:
	rm -rf bin logs *~ *.o
