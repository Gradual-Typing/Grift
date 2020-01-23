all:
	racket -v
	raco make -v -j 4 main.rkt
	cd src/backend-c/runtime/; make

install:
	raco pkg install --batch --auto --fail-fast --name grift

remove:
	raco pkg remove --batch --auto grift

update:
	raco pkg update --batch --auto grift

timed:
	time raco make main.rkt

test: all
# Investigate why travis fails with undefined reference errors when
# running the tests for the runtime.
# make -C src/backend-c/runtime test
	raco test .

clean:
	rm -f *~ *#* *.c *.out *.o *.s
	find . -path '*/compiled/*' -delete
	find . -type d -name "compiled" -delete
	cd src/backend-c/runtime/; make clean

