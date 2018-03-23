targets=array fft n_body ray blackscholes matmult quicksort tak
grift=../../../../../main.rkt

.PHONY: clean all

all: $(targets)

clean:
	find . -not \( -name "*.grift" -or -name "Makefile" \) -delete

%: %.grift
	$(grift) -o $@ -O 3 $@.grift

