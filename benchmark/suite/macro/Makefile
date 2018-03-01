LIBGC=../../../src/backend-c/runtime/boehm-gc-install/lib/libgc.a
GRIFT=racket ../../../main.rkt -O 3
GAMBIT=gsc -exe -cc-options "-O3"
GCC=gcc -O3

BIN_SAMPLES ?= 3
NUM_BINS ?= 30
TIMING_SAMPLES ?= 30
TEST_DIR=test
BENCHMARK_DIR ?=fresh

.PHONY: external internal all clean test-clean outputs

all: external internal

external:
	./extremes.sh $(TIMING_SAMPLES) $(BENCHMARK_DIR) 17 19

internal: 
	./lattice_bins.sh $(BIN_SAMPLES) $(NUM_BINS) $(TIMING_SAMPLES) \
		$(BENCHMARK_DIR) 17 13 19 17 19 15 17 7 17 8 19 11

test:
	TIMING_SAMPLES=1 BIN_SAMPLES=1 NUM_BINS=10 BENCHMARK_DIR=$(TEST_DIR) \
		$(MAKE) all

clean:
	find extremes lattice_bins -executable -and \( -not -type d \) -delete
	find extremes lattice_bins -type d -and -name "compiled" -exec rm -r {} \;
	$(MAKE) -C src clean

test-clean:
	rm -rf extremes/$(TEST_DIR) lattice_bins/$(TEST_DIR)

outputs:
	$(MAKE) -C outputs
