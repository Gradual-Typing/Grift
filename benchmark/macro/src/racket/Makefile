CC=raco exe
PROGS=n-body
TGTS=$(addprefix bin/, $(PROGS))

all: $(TGTS)

bin:
	mkdir bin

logs:
	mkdir logs
	racket --version >> logs/compile.log

bin/% : bin logs
	$(CC) -o $@ $*.rkt

clean:
	rm -rf logs compiled bin *~
