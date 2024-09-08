include ../../../Makefile.config

R2B=$(TOOLSBIN)/r2b
BC=$(TOOLSBIN)/bchart

all: fib.ps fib2.ps fib3.ps
	latex example.tex

fib.bc:
	$(R2B) -t "Fib on Unix" owens.fib redrock.fib > fib.bc

fib.ps: fib.bc
	$(BC) -r 45 -s "s" fib.bc > fib.ps

fib2.bc:
	$(R2B) -r -t "Fib on Unix (/ Owens)" owens.fib redrock.fib > fib2.bc
fib2.ps: fib2.bc
	$(BC) fib2.bc > fib2.ps

fib3.bc:
	$(R2B) -p -t "Fib on Unix (\% Owens)" owens.fib redrock.fib > fib3.bc
fib3.ps: fib3.bc
	$(BC) -s "%" fib3.bc > fib3.ps

clean:
	/bin/rm -f fib.ps fib2.ps fib3.ps
	/bin/rm -f fib.bc fib2.bc fib3.bc
	/bin/rm -f *.dvi *.ps *.log *.aux
	/bin/rm -f *~
