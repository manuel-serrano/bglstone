include ../../../Makefile.config

RUNIT=$(TOOLSBIN)/runit
R2B=$(TOOLSBIN)/r2b
BC=$(TOOLSBIN)/barchart

all: fibfx fibfl fib.ps example.tex
	latex example.tex

fib.stat: fib.runit
	$(RUNIT) fib.runit -o fib.stat

fib.bc: fib.stat
	$(R2B) -t "Fib on Unix" fib.stat > fib.bc

fib.ps: fib.bc
	$(BC) -s "s" fib.bc > fib.ps

fibfx:
	$(BGLTOOLS) -unsafe -O2 fibfx.scm -o fibfx

fibfl:
	$(BGLTOOLS) -unsafe -O2 fibfl.scm -o fibfl

clean:
	/bin/rm -f fib.ps
	/bin/rm -f fib.bc
	/bin/rm -f fib.stat
	/bin/rm -f fibfl fibfx fibfl.o fibfx.o
	/bin/rm -f *.dvi *.ps *.log *.aux
	/bin/rm -f *~
