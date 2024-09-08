include ../../../Makefile.config

BC=$(TOOLSBIN)/barchart

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
	$(BC) example1.bc > example1.ps

example2.ps: example2.bc
	$(BC) example2.bc > example2.ps

example3.ps: example3.bc
	$(BC) example3.bc > example3.ps

example4.ps: example3.bc
	$(BC) --rotate 45 example3.bc > example4.ps

example5.ps: example3.bc
	$(BC) --rotate 45 example3.bc > example5.ps

example6.ps: example6.bc
	$(BC) --rotate 45 --xscale 2 example6.bc > example6.ps

example7.ps: example7.bc
	$(BC) --rotate 45 --xscale 2 --color example7.bc > example7.ps

example8.ps: example2.bc
	$(BC) --color example2.bc > example8.ps
