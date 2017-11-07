.PHONY: all clean outs

targets=array fft n_body ray blackscholes matmult quicksort tak

all: $(targets)

clean:
	find . -not \( -name "*.scm" -or -name "Makefile" \) -delete

%:
	gsc -exe -o $@ -cc-options "-O3" $@.scm
