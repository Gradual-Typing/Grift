.PHONY: clean all

targets=array fft n_body ray blackscholes matmult quicksort tak

all: $(targets)

clean:
	find . -not \( -name "*.rkt" -or -name "Makefile" \) -delete

%:
	raco make $@.rkt
