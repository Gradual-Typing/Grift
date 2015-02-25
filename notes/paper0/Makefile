PAPER=paper
TEX=pdflatex
TEMPFILES=*~
DERIVITIVES=$(PAPER).log $(PAPER).pdf $(PAPER).aux
REMOVE=rm -f

All:
	$(TEX) $(PAPER).tex

clean:
	$(REMOVE) $(TEMPFILES) $(DERIVITIVES)

