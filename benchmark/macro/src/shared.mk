

ifeq ($(origin ROOT_DIR), undefined)
  $(error ROOT_DIR must be set for this include to work)
endif

SCHML_DIR=$(ROOT_DIR)/../../../
SCHML=racket $(SCHML_DIR)/main.rkt

# the basic structure of a benchmark configuration
# each "bechmark configuration" has log, bin, and out directories
# each configurations is built from a src directory that
# is at the configuration directory level or higher.
SRC_DIR ?= src
BIN_DIR ?= bin
OUT_DIR ?= out
LOG_DIR ?= log

AUTO_DIRS ?=$(BIN_DIR) $(OUT_DIR)

# Adding an input file is the driver for whether the make
# files pay attention to src files.
INPUTS_DIR ?= $(ROOT_DIR)/inputs
INPUTS=$(wildcard $(INPUTS_DIR)/*)
PROGS=$(foreach INPUT, $(INPUTS), $(shell basename $(INPUT)))

BINS=$(addprefix $(BIN_DIR)/, $(PROGS))
OUTS=$(addprefix $(OUT_DIR)/, $(addsuffix .out, $(PROGS)))
ERRS=$(addprefix $(OUT_DIR)/, $(addsuffix .err, $(PROGS)))

# files cleaned up by clean command
RUBISH:=\\\#*\\\# *~
CLEANED ?= $(LOG_DIR) $(AUTO_DIRS) *.txt *.log

# setup for running benchmarks
export RUNS ?= 20
export LATTICE_SIZE ?= 1000

RUNTIMER ?= sh $(ROOT_DIR)/run.sh

COMPILE_VERSION ?=$(COMPILE) --version
COMPILE_INVOKE=$(COMPILE) $(COMPILE_OPTIONS) -o $@ \
		$(SRC_DIR)/$*.$(SRC_EXT) $(COMPILE_LIBRARIES) 


SCHML_TYPES:=static dynamic
SCHML_IMPLS:=coercions casts
SCHML_CONFIGS:= $(foreach TYPE, $(SCHML_TYPES), \
		  $(foreach IMPL, $(SCHML_IMPLS), \
		    $(TYPE)/$(IMPL)))

# Make keeps all intermediate files
.SECONDARY:

# Must come before other rules so that it is the default
.PHONY: default
default: all

clean: $(addsuffix .made_clean, $(MADE_CLEAN))
	$(RM) -r $(RUBISH) $(CLEANED)

.PHONY: %.made_clean
%.made_clean:
	make -C $* clean

# Anything in variable AUTO_DIRS is a directory that can
# be made automatically
$(AUTO_DIRS):
	mkdir $@

$(LOG_DIR):
	mkdir $(LOG_DIR)
	$(COMPILE_VERSION) >>  $(LOG_DIR)/compile.log
	echo options    >>  $(LOG_DIR)/compile.log
	echo $(COMPILE_OPTIONS) >>  $(LOG_DIR)/compile.log
	echo libraries  >>  $(LOG_DIR)/compile.log
	echo $(COMPILE_LIBRARIES) >>  $(LOG_DIR)/compile.log

$(BIN_DIR)/% : $(BIN_DIR) $(LOG_DIR)
	echo >> $(LOG_DIR)/compile.log # print a new line
	echo `date -Isecond` $@ start >> $(LOG_DIR)/compile.log
	$(COMPILE_INVOKE)
	echo `date -Isecond` $@ finished >> $(LOG_DIR)/compile.log

$(OUT_DIR)/%.out: $(BIN_DIR)/% $(INPUTS_DIR) $(LOG_DIR) $(OUT_DIR)
	echo >> $(LOG_DIR)/run.log # print a new line
	echo `date -Isecond` $@ $(RUNS) runs start >> $(LOG_DIR)/run.log
	if [ ! -e $(OUT_DIR)/$(shell dirname $*) ];\
          then mkdir -p $(OUT_DIR)/$(shell dirname $*);\
	fi
	$(RUNTIMER) $(RUNS) $(BIN_DIR)/$* \
                $(OUT_DIR)/$*.out $(OUT_DIR)/$*.err \
		$(INPUTS_DIR)/$(firstword $(subst /, , $*))
	echo `date -Isecond` $@ $(RUNS) runs finished >> $(LOG_DIR)/run.log

$(OUT_DIR)/%.err: $(OUT_DIR)/%.out ;
