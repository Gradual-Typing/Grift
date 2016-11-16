tmp=$(shell date +%Y_%m_%d_%H_%M_%S)

run:
	(cd src && make setup && make run && make analyze)
	cp -r src $(tmp)
	(cd src && make clean)
	tar -acf $(tmp).tbz $(tmp)

clean:
	(cd src && make clean)
	rm -rf *~

test:
	(cd src && make setup && make test && make clean)



