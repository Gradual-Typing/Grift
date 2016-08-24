all:
	raco make -v run.rkt
	racket run.rkt

test:
	raco make -v run.rkt
	racket run.rkt --test

save:
	git rev-parse --verify HEAD > out/commit-hash.txt
	tar -cf archive/`date -I`-snapshot.tar run.rkt plot.py src tmp out

clean:
	rm -rf src tmp compiled out
