include ../../../Makefile.config

B2B=$(TOOLSBIN)/b2b
BC=$(TOOLSBIN)/bchart

EXAMPLES= example1.ps example2.ps example3.ps example4.ps example5.ps  \
          example6.ps example7.ps example8.ps

BARCHART= barchart

all: example

example: example.ps

example.ps: example.dvi
	dvips example.dvi -o example.ps

example.dvi: $(EXAMPLES) example.tex
	latex example.tex

clean:
	/bin/rm -f $(EXAMPLES) example.dvi example.ps *.log *.aux 2> /dev/null
	/bin/rm -f *~ 2> /dev/null

example1.ps: example1.bc
	$(B2B) example1.bc | $(BC) > example1.ps

example2.ps: example2.bc
	$(B2B) example2.bc | $(BC) > example2.ps

example3.ps: example3.bc
	$(B2B) example3.bc | $(BC) > example3.ps

example4.ps: example3.bc
	$(B2B) --rotate 45 example3.bc | $(BC) > example4.ps

example5.ps: example3.bc
	$(B2B) --rotate 45 example3.bc | $(BC) > example5.ps

example6.ps: example6.bc
	$(B2B) --rotate 45 --xscale 2 example6.bc | $(BC) > example6.ps

example7.ps: example7.bc
	$(B2B) --rotate 45 --xscale 2 --color example7.bc | $(BC) > example7.ps

example8.ps: example2.bc
	$(B2B) --color example2.bc | $(BC) > example8.ps
