all:
	raco make -v -j 3 main.rkt

clean:
	rm -rf compiled
	rm -rf src/compiled
	rm -rf src/schml/compiled
	rm -rf src/casts/compiled
	rm -rf src/data/compiled
	rm -rf src/backend-c/compiled
	rm -rf test/compiled
