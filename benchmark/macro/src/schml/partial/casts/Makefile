ROOT_DIR=../../..
COMPILE=$(SCHML)
COMPILE_OPTIONS=--type-based-casts -O 3 
SRC_DIR=../src
SRC_EXT=schml

RUNS := 1

include $(ROOT_DIR)/shared.mk

# Has to come after the include I think
COMPILE_INVOKE=if [ ! -e $(shell dirname $@) ];\
	         then mkdir -p $(shell dirname $@);\
	       fi;\
	       $(COMPILE) $(COMPILE_OPTIONS) -o $@ \
		$(SRC_DIR)/$*.$(SRC_EXT) $(COMPILE_LIBRARIES) 


.PHONY: all
all: $(BINS) $(ERRS)
