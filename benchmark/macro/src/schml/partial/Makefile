ROOT_DIR:=../..
AUTO_DIRS:=src .trash counts
MADE_CLEAN:=casts coercions
RUNS ?=1
include $(ROOT_DIR)/shared.mk 



SRCS:=$(addprefix src/, $(PROGS))

# BINS has to be the delayed assignment so that
# it is calculated after generate has run.
BINS=$(foreach IMPL, $(GRIFT_IMPLS), \
       $(addprefix $(IMPL)/, \
	 $(subst $(SRC_DIR)/, $(BIN_DIR)/, \
          $(subst .grift,, \
	   $(wildcard $(SRC_DIR)/*/*)))))

ERRS=$(subst $(BIN_DIR),$(OUT_DIR), $(addsuffix .err, $(BINS)))



.PHONY: all generate setup run
all: | generate setup run
generate : $(SRCS)
setup : 
	$(MAKE) $(BINS)

run:
	if [ -n "$(ERRS)" ]; \
	 then $(MAKE) $(ERRS);\
	fi

coercions/%:
	$(MAKE) -C coercions $*

casts/%:	
	$(MAKE) -C casts $*

src/%: src counts .trash
	cp ../static/src/$*.grift src/$*.grift
	dynamizer src/$* $(LATTICE_SIZE) >> counts/$*
	mv src/$*.grift .trash/
