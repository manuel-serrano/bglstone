BENCHITDIR=/users/serrano/prgm/utils/benchit
RUNNUMBER=3
RUNIT=$(BENCHITDIR)/runit/bin/runit -v -r $(RUNNUMBER)
R2B=$(BENCHITDIR)/r2b/bin/r2b -tv -vf 12 -rl 45
R2T=$(BENCHITDIR)/r2tex/bin/r2tex

all: all.tex
	latex bench.tex
	dvips bench.dvi -o bench.ps 

clean:
	/bin/rm -f bench.dvi bench.ps bench.aux bench.log
