all:
	racket run.rkt --generate
	emacs changes.c src/*x1*byHand.c
	racket run.rkt --run

test:
	racket run.rkt --generate
	sleep 1
	touch src/*.byHand.c
	racket run.rkt -r 1 --run

