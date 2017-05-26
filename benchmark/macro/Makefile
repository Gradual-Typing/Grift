SRC:=src
WORK:=$(shell date +%Y_%m_%d_%H_%M_%S)
TEST:=test

.PHONY: all
all: $(WORK).tgz

.PHONY: test
test:
	$(MAKE) clean
	$(MAKE) -C $(SRC) clean
	cp -r $(SRC) $(TEST)
	(export RUNS=1; export LATTICE_SIZE=2; $(MAKE) -C $(TEST) | tee $(TEST)/test.log)

.PHONY: clean
clean:
	rm -rf $(TEST) Makefile~

$(WORK):
	$(MAKE) -C $(SRC) clean
	cp -r $(SRC) $(WORK)

$(WORK)/make.log: $(WORK)
	$(MAKE) -C $* | tee $*/make.log

$(WORK).tgz: $(WORK)/make.log
	echo archived on `date -Iseconds` >> $*/archive.log
	tar -acf $*.tbz $*
