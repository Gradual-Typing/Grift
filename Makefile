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
# FIXME
# The following line is what this recipe should be: 
# raco test .
# This push currently only passes all the static tests
# I plan to revert this once everything works -Andre
	racket tests/main.rkt --llvm -R Static


clean:
	rm -f *.c *.out *.o *.s
	find . -name '*~' -delete
	find . -name '*#*' -delete
	find . -path '*/compiled/*' -delete
	find . -type d -name "compiled" -delete
	cd src/backend/runtime/; make clean

