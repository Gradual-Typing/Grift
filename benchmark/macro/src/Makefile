# must be defined before shared.mk include
ROOT_DIR:=.
OTHER_LANGS:=c racket gambit
LANGS:=$(OTHER_LANGS) grift
MADE_CLEAN:=$(LANGS)
AUTO_DIRS:=
CLEANED:=lattice stats *.log
include shared.mk

CONFIGS:=$(OTHER_LANGS) $(addprefix grift/, $(SCHML_CONFIGS))
ALL_BINS:=$(foreach CONFIG, $(CONFIGS), $(addprefix $(CONFIG)/, $(BINS)))
ALL_ERRS:=$(foreach CONFIG, $(CONFIGS), $(addprefix $(CONFIG)/, $(ERRS)))

#

.PHONY: all
all: setup run analyze

.PHONY: setup
setup: $(ALL_BINS)
	$(MAKE) -C grift/partial generate setup

.PHONY: run
run: $(ALL_ERRS)
	$(MAKE) -C grift/partial run

c/%:
	$(MAKE) -C c $*

grift/%:
	$(MAKE) -C grift $*

racket/%:
	$(MAKE) -C racket $*

gambit/%:
	$(MAKE) -C gambit $*

.PHONY: analyze
analyze: stats lattice

stats:
	racket analyze.rkt --assert-runs $(RUNS)

lattice:
	racket lattice-analysis.rkt --assert-runs $(RUNS)







