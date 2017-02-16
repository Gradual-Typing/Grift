all:
	racket run_bogus.rkt --generate
	emacs changes.c tmp/src/*x1*byHand.c


test:
	racket run.rkt --generate
	sleep 1
	touch tmp/src/*.byHand.c
	racket run.rkt -r 1 --run

clean:
	rm -rf tmp
