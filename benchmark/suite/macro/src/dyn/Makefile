targets=array fft n_body ray blackscholes matmult quicksort tak
grift=../../../../../main.rkt

.PHONY: clean all

all: $(targets)

clean:
	find . -not \( -name "*.schml" -or -name "Makefile" \) -delete

%: %.schml
	$(grift) -o $@ -O 3 $@.schml

