SCHML=racket ../../../../main.rkt
OPTS=-O 3
COERCEOPTS=${OPTS} --coercions
CASTOPTS=${OPTS} --type-based-casts
PROGS=n-body
TGTS=$(addprefix coercions/bin/, $(PROGS)) $(addprefix casts/bin/, $(PROGS))

all: $(TGTS)

coercions   :
	mkdir coercions

coercions/bin : coercions
	mkdir coercions/bin

coercions/logs :
	mkdir coercions/logs
	racket --version >> coercions/logs/compile.log
	$(SCHML) --version >> coercions/logs/compile.log
	echo 'options:' >> coercions/logs/compile.log
	echo $(COERCEOPTS) >> coercions/logs/compile.log

coercions/bin/% : coercions/bin coercions/logs
	${SCHML} $(COERCEOPTS) -o $@ $*.schml 

casts:
	mkdir casts

casts/bin : casts
	mkdir casts/bin

casts/logs : casts
	mkdir casts/logs
	racket --version >> casts/logs/compile.log
	$(SCHML) --version >> casts/logs/compile.log
	echo 'options:' >> casts/logs/compile.log
	echo $(CASTOPTS) >> casts/logs/compile.log

casts/bin/% : casts/bin casts/logs
	${SCHML} $(CASTOPTS) -o $@ $*.schml

clean:
	rm -rf casts coercions *~
