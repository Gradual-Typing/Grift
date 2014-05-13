PATH := ${PATH}:${HOME}/.local/bin

.PHONY: test clean
test:
	racket testing/tests.rkt
install:
	raco pkg install
clean:
	rm -rf */compiled
	rm -rf doc/
	rm -rf compiled
