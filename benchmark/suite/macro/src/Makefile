LANGS=dyn static racket typed_racket chezscheme gambit ocaml
LANGS_CLEAN=$(addsuffix .clean, $(LANGS))

.PHONY: all clean $(LANGS) $(LANG_CLEAN)

all: $(LANGS)

clean: $(LANGS_CLEAN)

$(LANGS):
	$(MAKE) -C $@

%.clean:
	$(MAKE) -C $* clean
