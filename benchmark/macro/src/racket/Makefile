ROOT_DIR=..
COMPILE_VERSION:=racket --version
COMPILE=raco exe
SRC_DIR=src
SRC_EXT=rkt
CLEANED=src/*~ log bin out compiled
include $(ROOT_DIR)/shared.mk

.PHONY: all
all: $(BINS) $(ERRS)


# bin/% : bin log
# 	echo >> log/compile.log # print a new line
# 	echo `date -Isecond` $@ start >> log/compile.log
# 	$(COMPILE) -o bin/$* src/$*.rkt
# 	echo `date -Isecond` $@ finish >> log/compile.log

# out/%.out: bin/% ../inputs/% log out
# 	echo >> log/run.log # print a new line
# 	echo `date -Isecond` $@ $(RUNS) start >> log/run.log
# 	$(RUNTIMER) $(RUNS) bin/$* out ../inputs/
# 	echo `date -Isecond` $@ $(RUNS) finish >> log/run.log

# out/%.err: out/%.out ;

# clean :
# 	${RM} -r *.o \#*# *~ src/*~ log bin out compiled
