# must be defined before shared.mk include
ROOT_DIR:=.
OTHER_LANGS:=c racket gambit
LANGS:=$(OTHER_LANGS) schml
MADE_CLEAN:=$(LANGS)
AUTO_DIRS:=
CLEANED:=lattice stats *.log
include shared.mk

CONFIGS:=$(OTHER_LANGS) $(addprefix schml/, $(SCHML_CONFIGS))
ALL_BINS:=$(foreach CONFIG, $(CONFIGS), $(addprefix $(CONFIG)/, $(BINS)))
ALL_ERRS:=$(foreach CONFIG, $(CONFIGS), $(addprefix $(CONFIG)/, $(ERRS)))

#

.PHONY: all
all: setup run analyze

.PHONY: setup
setup: $(ALL_BINS)
	$(MAKE) -C schml/partial generate setup

.PHONY: run
run: $(ALL_ERRS)
	$(MAKE) -C schml/partial run

c/%:
	$(MAKE) -C c $*

schml/%:
	$(MAKE) -C schml $*

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







