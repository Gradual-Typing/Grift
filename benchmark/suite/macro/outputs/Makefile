LANGS=dyn static racket typed_racket chezscheme gambit ocaml

.PHONY: all clean $(LANGS) $(LANGS_CLEAN)

all: $(LANGS)

clean:
	find . -name "*.txt" -delete
	find . -name "*~" -delete
	find . -name "#*#" -delete

$(LANGS):
	$(MAKE) -C $@
