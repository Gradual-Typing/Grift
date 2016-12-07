SRC:=src
WORK:=$(shell date +%Y_%m_%d_%H_%M_%S)
TEST:=test

.PHONY: all
all: $(WORK) $(WORK)/make.log $(WORK).tgz

.PHONY: test
test:
	$(MAKE) clean
	$(MAKE) -C $(SRC) clean
	cp -r $(SRC) $(TEST)
	(export RUNS=1; export LATTICE_SIZE=2; $(MAKE) -C $(TEST) | tee $(TEST)/test.log)


.PHONY: quick
quick:
	(export RUNS=3; export LATTICE_SIZE=10; $(MAKE) all)

.PHONY: clean
clean:
	rm -rf $(TEST) Makefile~

$(WORK):
	$(MAKE) -C $(SRC) clean
	cp -r $(SRC) $(WORK)

%/make.log:
	$(MAKE) -C $* | tee $*/make.log

%.tgz: 
	echo archived on `date -Iseconds` >> $*/archive.log
	tar -acf $*.tbz $*
