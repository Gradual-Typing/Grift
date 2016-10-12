all:
	cd src/backend-c/runtime/; make
	raco make -v -j 3 main.rkt

timed:
	time raco make -v -j 3 main.rkt

test: all
	raco make -v -j 4 tests/main.rkt
	racket tests/main.rkt	

clean:
	find . -path '*/compiled/*' -delete
	find . -type d -name "compiled" -delete
	cd src/backend-c/runtime/; make clean
