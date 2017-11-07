BENCHS=array blackscholes fft matmult n_body quicksort ray tak
INPUT=../../inputs
INPUTS=$(foreach bench,$(BENCHS),$(wildcard $(INPUT)/$(bench)/*))
TARGETS=$(foreach input,$(INPUTS),$(subst $(INPUT)/,,$(input)))
LANG_SRC=../../src/$(LANG)

.PHONY: all clean make-lang

all: $(TARGETS)

clean:
	rm -f $(TARGETS)

# the subst here is hacky but it removes the trailing / from the
# benchmark directory name
.SECONDEXPANSION:

# The shell comand function is expected to fail silently
# when the delta.arg file doesn't exist
$(TARGETS): $(LANG_SRC)/$$(subst /,,$$(dir $$@))$(MAYBE_SUFFIX)
	$(MAYBE_INTERP) $(LANG_SRC)/$(subst /,,$(dir $@))$(MAYBE_SUFFIX) \
		> $@ < $(INPUT)/$@
	racket ../../lib/parse-time.rkt --strip --in $@ --out $@ --lang $(LANG)
	racket ../../lib/compare-results.rkt \
		$(shell cat $(dir $@)delta.arg 2>/dev/null) $@ ../Dyn/$@

$(foreach bench,$(BENCHS),$(LANG_SRC)/$(bench)$(MAYBE_SUFFIX)):
	$(MAKE) -C $(LANG_SRC)

