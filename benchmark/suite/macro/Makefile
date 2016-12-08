LIBGC=../../../src/backend-c/runtime/boehm-gc-install/lib/libgc.a

all:
	./lattice.sh
clean:
	rm -rf lattice/

test:
	./lattice.sh 5 5 16

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
