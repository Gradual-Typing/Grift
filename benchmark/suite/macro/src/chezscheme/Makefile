.PHONY: clean all

BENCHS=array fft n_body ray blackscholes matmult quicksort tak
targets=$(foreach bench,$(BENCHS),$(addsuffix .so, $(bench)))

all: $(targets)

clean:
	find . -not \( -name "*.ss" -or -name "Makefile" \) -delete

%.so: %.ss
	echo '(compile-program "$*.ss")' | scheme --optimize-level 2 -q
