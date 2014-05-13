PATH := ${PATH}:${HOME}/.local/bin

.PHONY: test clean
test:
	racket testing/dev-tests.rkt

clean:
	rm -r */compiled
	rm -r doc/
