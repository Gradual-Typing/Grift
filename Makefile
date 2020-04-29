all:
	racket -v
	raco make -v -j 4 main.rkt
	cd src/backend/runtime/; make

install:
	raco pkg install --batch --auto --fail-fast --name grift

remove:
	raco pkg remove --batch --auto grift

update:
	raco pkg update --batch --auto grift

timed:
	time raco make main.rkt

test: all
	racket tests/main.rkt --llvm
	raco test .


clean:
	rm -fr *.c *.out *.o *.s *.ll *.dSYM *.log
	find . -name '*~' -delete
	find . -name '*#*' -delete
	find . -path '*/compiled/*' -delete
	find . -type d -name "compiled" -delete
	cd src/backend/runtime/; make clean

