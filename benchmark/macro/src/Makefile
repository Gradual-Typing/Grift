all: setup run analyze

setup.lock: clean
	(cd c       && make)
	(cd racket  && make)
	(cd gambit  && make)
	(cd static  && make)
	(cd dynamic && make)
	touch setup.lock

setup: setup.lock

run.lock:
	sh run.sh
	touch run.lock

run: run.lock

analyze.txt: 
	racket analyze.rkt >> analyze.txt

analyze: analyze.txt
	cat analyze.txt

clean:
	(cd c       && make clean)
	(cd racket  && make clean)
	(cd gambit  && make clean)
	(cd static  && make clean)
	(cd dynamic && make clean)
	rm -rf compiled
	rm -rf *.lock *.txt *.tex *.log *.dat *~

