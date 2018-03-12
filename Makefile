all:
	racket -v
	raco make -v -j 4 main.rkt
	cd src/backend-c/runtime/; make

timed:
	time raco make main.rkt

test: all
	raco make -v tests/main.rkt
	racket tests/main.rkt	

clean:
	rm -f *~ *#* *.c *.out *.o *.s
	find . -path '*/compiled/*' -delete
	find . -type d -name "compiled" -delete
	cd src/backend-c/runtime/; make clean

