include ../../../Makefile.config

R2TEX=$(TOOLSBIN)/r2tex

all: fib.tex fib2.tex fib3.tex fib4.tex fib5.tex
	latex example.tex

fib.tex:
	$(R2TEX) -valsuffix " s" -t "Fib on Unix" -st "Fib" owens.fib redrock.fib > fib.tex

fib2.tex:
	$(R2TEX) -valsuffix " $$\\delta$$" -r -t "Fib on Unix (/ Owens)" -st "Fib" owens.fib redrock.fib > fib2.tex

fib3.tex:
	$(R2TEX) -valsuffix " \%" -p -t "Fib on Unix (\% Owens)" -st "Fib" owens.fib redrock.fib > fib3.tex

fib4.tex:
	$(R2TEX) -valsuffix " $$\\delta$$" -vr -t "Fib on Unix (+ / Owens)" -st "Fib" owens.fib redrock.fib > fib4.tex

fib5.tex:
	$(R2TEX) -valsuffix " \%" -vp -t "Fib on Unix (+ \% Owens)" -st "Fib" owens.fib redrock.fib > fib5.tex


clean:
	/bin/rm -f fib.tex fib2.tex fib3.tex fib4.tex fib5.tex
	/bin/rm -f *.dvi *.ps *.log *.aux
	/bin/rm -f *~
