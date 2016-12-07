ROOT_DIR:=../..
AUTO_DIRS:=
CLEANED:=
MADE_CLEAN:=casts coercions
include $(ROOT_DIR)/shared.mk 

BINS:= $(foreach CONFIG, $(SCHML_IMPLS), $(addprefix $(CONFIG)/, $(BINS)))

OUTS:= $(foreach CONFIG, $(SCHML_IMPLS), $(addprefix $(CONFIG)/, $(OUTS)))

.PHONEY: all
all : $(BINS) $(OUTS)

coercions/%:
	$(MAKE) -C coercions $*

casts/%:
	$(MAKE) -C casts $*

# COMPILER=racket ../../../../../main.rkt
# COMPILER_VERSION=$(COMPILER) --version
# COMPILER_OPTIONS=--assert-statically-typed -O 3 --coercions

# CASTOPTS=$(OPTS) --type-based-casts
# COERCEOPTS=$(OPTS) --coercions
# INPUTS=$(wildcard ../../inputs/*)
# PROGS=$(foreach INPUT, $(INPUTS), $(shell basename $(INPUT)))
# SCHMLS:=coercions casts
# BIN_DIRS:=$(addsuffix /bin, $(SCHMLS))
# OUT_DIRS:=$(addsuffix /out, $(SCHMLS))
# LOG_DIRS:=$(addsuffix /log, $(SCHMLS))
# BINS:=$(foreach DIR, $(BIN_DIRS), $(addprefix $(DIR)/, $(PROG)))
# OUTS:=$(foreach DIR, $(OUT_DIRS), $(addprefix $(DIR)/, \
# 				    $(addsuffix .out, $(PROGS))))
# DIRS:=$(SCHMLS) $(BIN_DIRS) $(OUT_DIRS) $(LOG_DIRS)
# RUNTIMER=sh ../../run.sh
# RUNS=10

# .DEFAULT:
# .PHONY: all clean
# .SECONDARY:


# all: $(BINS) $(OUTS) ;


# $(DIRS):
# 	mkdir -p $@

# coercions/log/compile.log : coercions coercions/log
# 	racket --version >> coercions/log/compile.log
# 	$(SCHML) --version >> coercions/log/compile.log
# 	echo 'options:' >> coercions/log/compile.log
# 	echo $(COERCEOPTS) >> coercions/log/compile.log

# coercions/bin/% : coercions/bin src/%.schml
# 	${SCHML} ${OPTS} -o $@ src/$*.schml 

# coercions/out/%.out : coercions/log coercions/out \
# 		      coercions/bin/% ../../inputs/%
# 	echo >> coercions/log/run.log # print a new line
# 	echo `date -Isecond` $@ $(RUNS) start >> coercions/log/run.log
# 	$(RUNTIMER) $(RUNS) coercions/bin/$* coercions/out ../../inputs
# 	echo `date -Isecond` $@ $(RUNS) finish >> coercions/log/run.log

# coercions/out/%.err : coercions/out/%.out ;

# casts/log/compile.log : casts casts/log
# 	racket --version > casts/log/compile.log
# 	$(SCHML) --version >> casts/log/compile.log
# 	echo 'options:' >> casts/log/compile.log
# 	echo $(CASTOPTS) >> casts/log/compile.log

# casts/bin/% : casts/bin src/%.schml
# 	${SCHML} $(CASTOPTS) -o $@ src/$*.schml

# casts/out/%.out : casts/log casts/out casts/bin/% ../../inputs/%
# 	echo >> casts/log/run.log # print a new line
# 	echo `date -Isecond` $@ $(RUNS) start >> casts/log/run.log
# 	$(RUNTIMER) $(RUNS) casts/bin/$* casts/out ../../inputs
# 	echo `date -Isecond` $@ $(RUNS) finish >> casts/log/run.log

# casts/out/%.err : casts/out/%.out ;

# clean:
# 	rm -rf casts coercions *~ *.o src/*~
