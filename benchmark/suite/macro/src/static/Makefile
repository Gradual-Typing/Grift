.PHONY: clean all

targets=array fft n_body ray blackscholes matmult quicksort tak
grift=../../../../../main.rkt
all: $(targets)

clean:
	find . -not \( -name "*.grift" -or -name "Makefile" \) -delete

%:
	$(grift) -O 3 -o $@ $@.grift

