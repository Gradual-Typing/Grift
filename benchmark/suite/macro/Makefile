LIBGC=../../../src/backend-c/runtime/boehm-gc-install/lib/libgc.a
SCHML=racket ../../../main.rkt -O 3
GAMBIT=gsc -exe -cc-options "-O3"
GCC=gcc -O3

all:
	./lattice.sh 300 5 fresh 1 2 1 3 3 4 2 4

clean:
	rm -rf lattice/




