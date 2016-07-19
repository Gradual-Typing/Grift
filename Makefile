all:
	raco make -v -j 3 main.rkt

clean:
	find src/ -path '*/compiled/*' -delete
	find src/ -type d -name "compiled" -delete
