all:
	raco make -v run.rkt
	racket run.rkt

test:
	raco make -v run.rkt
	racket run.rkt --test

linux_save:
	git rev-parse --verify HEAD > out/commit-hash.txt
	cat /proc/cpuinfo > out/cpuinfo.txt
	cat /proc/meminfo > out/meminfo.txt
	tar -cf `date -I`-micro-partial.tar run.rkt plot.py src tmp out

plot:
	python plot.py


clean:
	rm -rf src tmp compiled out
