LIBGC=../../../src/backend-c/runtime/boehm-gc-install/lib/libgc.a
SCHML=racket ../../../main.rkt -O 3
GAMBIT=gsc -exe -cc-options "-O3"
GCC=gcc -O3

all:
	./lattice.sh 300 5 fresh 1 2 1 3 3 4 2 4
clean:
	rm -rf lattice/

c:
	gcc -O3 src/c/n-body.c -o bin/c/n-body ${LIBGC}
	racket ../../../main.rkt -O 3 \
		-o bin/static/n-body.coercions src/static/n-body.schml
	racket ../../../main.rkt -O 3 --type-based-casts \
		-o bin/static/n-body.type-based-casts src/static/n-body.schml
	racket ../../../main.rkt -O 3 \
		-o bin/dyn/n-body.coercions src/dyn/n-body.schml
	racket ../../../main.rkt -O 3 --type-based-casts \
		-o bin/dyn/n-body.type-based-casts src/dyn/n-body.schml
	gsc -exe -cc-options "-O3" -o bin/gambit/n-body src/gambit/n-body.scm

d:
	gcc -Wall -Wextra src/c/n-body.c -o bin/c/n-body ${LIBGC}
	racket ../../../main.rkt -g --assert-statically-typed \
		-o bin/static/n-body.coercions src/static/n-body.schml
	racket ../../../main.rkt -g --assert-statically-typed --type-based-casts \
		-o bin/static/n-body.type-based-casts src/static/n-body.schml
	racket ../../../main.rkt -g \
		-o bin/dyn/n-body.coercions src/dyn/n-body.schml
	racket ../../../main.rkt -g --type-based-casts \
		-o bin/dyn/n-body.type-based-casts src/dyn/n-body.schml
	gsc -exe -o bin/gambit/n-body src/gambit/n-body.scm

t: d
	echo 50 | ./bin/static/n-body.coercions
	echo 50 | ./bin/static/n-body.type-based-casts
	echo 50 | ./bin/dyn/n-body.coercions
	echo 50 | ./bin/dyn/n-body.type-based-casts
	./bin/c/n-body 50
	./bin/gambit/n-body 50

r: c
	time echo 5000000 | ./bin/static/n-body.coercions
	time echo 5000000 | ./bin/static/n-body.type-based-casts
	time echo 5000000 | ./bin/dyn/n-body.coercions
	time echo 5000000 | ./bin/dyn/n-body.type-based-casts
	time ./bin/c/n-body 5000000
	time ./bin/gambit/n-body 5000000

b: 
	$(GCC) src/c/blackscholes.c -o bin/c/blackscholes ${LIBGC}	
	$(SCHML) --coercions \
		 --assert-statically-typed \
		 -o bin/static/blackscholes.coercions\
		 src/static/blackscholes.schml
	$(SCHML) --type-based-casts \
		 --assert-statically-typed \
		 -o bin/static/blackscholes.type-based-casts \
		 src/static/blackscholes.schml
	$(SCHML) --coercions \
		 -o bin/dyn/blackscholes.coercions \
		 src/dyn/blackscholes.schml
	$(SCHML) --type-based-casts \
		 -o bin/dyn/blackscholes.type-based-casts \
		 src/dyn/blackscholes.schml
	$(GAMBIT) -o bin/gambit/blackscholes src/gambit/blackscholes.scm

BLACKSCHOLES_INPUT=time cat inputs/blackscholes/in_64K.txt

br:
	$(BLACKSCHOLES_INPUT) | bin/c/blackscholes > /dev/null
	$(BLACKSCHOLES_INPUT) | bin/static/blackscholes.coercions > /dev/null
	$(BLACKSCHOLES_INPUT) | bin/static/blackscholes.type-based-casts > /dev/null
	$(BLACKSCHOLES_INPUT) | bin/dyn/blackscholes.coercions > /dev/null
	$(BLACKSCHOLES_INPUT) | bin/dyn/blackscholes.type-based-casts > /dev/null
	$(BLACKSCHOLES_INPUT) | bin/gambit/blackscholes > /dev/null



