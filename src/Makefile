#*=====================================================================*/
#*    serrano/prgm/project/bglstone/src/Makefile                       */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Sun Aug  5 14:54:21 2001                          */
#*    Last change :  Tue Jun 24 17:31:05 2025 (serrano)                */
#*    Copyright   :  2001-25 Manuel Serrano                            */
#*    -------------------------------------------------------------    */
#*    The Makefile that handles bglstone benchmarks                    */
#*=====================================================================*/
include ../Makefile.config

#*---------------------------------------------------------------------*/
#*    BENCHMARKS                                                       */
#*---------------------------------------------------------------------*/
# Default benchmarks
BENCH=scm

TBENCHMARKS=hello

# The Scheme benchmarks
SBENCHMARKS=bague beval boyer conform earley fib fft leval maze \
            mbrot nucleic peval puzzle queens qsort sieve traverse almabench \
            slatex scheme compiler

# The R7RS benchmarks
RBENCHMARKS=r7rs-compiler r7rs-dynamic r7rs-earley r7rs-fibfp r7rs-fft \
            r7rs-graphs r7rs-matrix r7rs-maze r7rs-mbrot r7rs-nboyer \
            r7rs-nucleic r7rs-parsing r7rs-peval r7rs-pnpoly \
            r7rs-ray r7rs-sboyer r7rs-scheme r7rs-simplex r7rs-slatex \
            r7rs-sum1 r7rs-sumfp

# The Bigloo additional benchmarks
BBENCHMARKS=beval bigloo boyer cgc conform earley leval \
            maze nucleic peval rgc traverse

# A subset of Scheme benchmakrs
OBENCHMARKS=fib mbrot qsort sieve

# Bglstone subset
STONEBENCHMARKS=beval bigloo nucleic

# Full Bigloo benchmarks
FBENCHMARKS=$(SBENCHMARKS) bigloo cgc rgc

# The C benchmarks
CBENCHMARKS=bague fft qsort almabench fib mbrot

#*---------------------------------------------------------------------*/
#*    SUFFIXES                                                         */
#*---------------------------------------------------------------------*/
.SUFFIXES:
.SUFFIXES: .runit .stat

#*---------------------------------------------------------------------*/
#*    show                                                             */
#*---------------------------------------------------------------------*/
show:
	@ $(MAKE) show.$(BENCH)

show.test:
	@ echo "$(TBENCHMARKS)"

show.scm:
	@ echo "$(SBENCHMARKS)"

show.r7rs:
	@ echo "$(RBENCHMARKS)"

show.bgl:
	@ echo "$(BBENCHMARKS)"

show.bglstone:
	@ echo "$(STONEBENCHMARKS)"

show.other:
	@ echo "$(OBENCHMARKS)"

show.fullbgl:
	@ echo "$(FBENCHMARKS)"

#*---------------------------------------------------------------------*/
#*    compile                                                          */
#*---------------------------------------------------------------------*/
compile: 
	$(MAKE) compile.$(BENCH)

compile.scm:
	$(MAKE) docompile BENCHMARKS="$(SBENCHMARKS)"

compile.r7rs:
	$(MAKE) docompile BENCHMARKS="$(RBENCHMARKS)"

compile.test:
	$(MAKE) docompile BENCHMARKS="$(TBENCHMARKS)"

compile.bgl:
	$(MAKE) docompile BENCHMARKS="$(BBENCHMARKS)"

compile.bglstone:
	$(MAKE) docompile BENCHMARKS="$(STONEBENCHMARKS)"

compile.other:
	$(MAKE) docompile BENCHMARKS="$(OBENCHMARKS)"

compile.c:
	$(MAKE) docompile BENCHMARKS="$(CBENCHMARKS)"

compile.fullbgl:
	$(MAKE) docompile BENCHMARKS="$(FBENCHMARKS)"

compile.:
	$(MAKE) docompile

docompile: mkln
	@ for p in $(BENCHMARKS); do \
	    echo "[0m[1;29m$$p:[0m"; \
            for e in $(TARGETS); do \
              if [ -d $$p/$$e ]; then \
                 echo "[0m[1;33m- $$e ($$p)[0m"; \
                 (cd $$p/$$e; $(MAKE) -f ../../makefiles/Makefile.$$e compile) || exit -1; \
              fi; \
            done; \
          done
               
#*---------------------------------------------------------------------*/
#*    run                                                              */
#*---------------------------------------------------------------------*/
run:
	$(MAKE) run.$(BENCH)

run.test:
	$(MAKE) dorun BENCHMARKS="$(TBENCHMARKS)"

run.scm:
	$(MAKE) dorun BENCHMARKS="$(SBENCHMARKS)"

run.r7rs:
	$(MAKE) dorun BENCHMARKS="$(RBENCHMARKS)"

run.bgl:
	$(MAKE) dorun BENCHMARKS="$(BBENCHMARKS)"

run.bglstone:
	$(MAKE) dorun BENCHMARKS="$(STONEBENCHMARKS)"

run.other:
	$(MAKE) dorun BENCHMARKS="$(OBENCHMARKS)"

run.fullbgl:
	$(MAKE) dorun BENCHMARKS="$(FBENCHMARKS)"

run.c:
	$(MAKE) dorun BENCHMARKS="$(CBENCHMARKS)"

dorun:
	@ for e in $(TARGETS); do \
	    echo "[0m[1;29m- $$e[0m"; \
            $(MAKE) $$e.stat || exit -2; \
	    cp $$e.stat ../results/$(NAME)/$$e.stat; \
          done

#*---------------------------------------------------------------------*/
#*    clean                                                            */
#*---------------------------------------------------------------------*/
clean:
	-@ for p in $(SBENCHMARKS) $(BBENCHMARKS) $(OBENCHMARKS) $(TBENCHMARKS) $(FBENCHMARKS) $(RBENCHMARKS); do \
            for e in $(TARGETS); do \
              if [ -d $$p/$$e ]; then \
                (cd $$p/$$e; $(MAKE) -f ../../makefiles/Makefile.$$e clean); \
              fi; \
            done; \
          done
	@ for e in $(TARGETS); do \
            /bin/rm -f $$e.runit; \
          done   
	@ /bin/rm -f _base.stat _base.runit

#*---------------------------------------------------------------------*/
#*    cleanall                                                         */
#*---------------------------------------------------------------------*/
cleanall:
	for p in $(SBENCHMARKS) $(BBENCHMARKS) $(OBBENCHMARKS) $(TBENCHMARKS) $(FBENCHMARKS) $(RBENCHMARKS); do \
            for e in $(TARGETS); do \
              if [ -d $$p/$$e ]; then \
                (cd $$p/$$e; $(MAKE) -f ../../makefiles/Makefile.$$e clean); \
              fi; \
            done; \
          done
	@ for e in $(TARGETS); do \
            /bin/rm -f $$e.stat; \
          done

#*---------------------------------------------------------------------*/
#*    .runit.stat                                                      */
#*---------------------------------------------------------------------*/
.runit.stat:
	$(TOOLSBIN)/runit $*.runit $(VERBOSE) $(REPETITION) $(RUNFLAGS) > $*.stat

#*---------------------------------------------------------------------*/
#*    runit                                                            */
#*---------------------------------------------------------------------*/
.PHONY: _base.runit bigloo.runit bigloo-jvm.runit bigloo-wasm.runit bigloo-saw.runit c.runit gambit.runit

_base.runit:
	$(MAKE) one-runit SYSTEM=_base SUFFIX=exe DIR=bigloo TITLE=Base COMPILER="`$(BIGLOO) -version`"
bigloo.runit:
	$(MAKE) one-runit SYSTEM=bigloo SUFFIX=exe DIR=bigloo TITLE=Bigloo COMPILER="`$(BIGLOO) -version`"
bigloo-jvm.runit:
	$(MAKE) one-runit SYSTEM=bigloo-jvm SUFFIX=bat DIR=bigloo-jvm TITLE=BiglooJVM COMPILER="`$(BIGLOO) -version` `$(BJVM) -version 2>&1 | head -n 1`"
bigloo-wasm.runit:
	$(MAKE) one-runit SYSTEM=bigloo-wasm SUFFIX=sh DIR=bigloo-wasm TITLE=BiglooWASM COMPILER="`$(BIGLOO) -version`"
bigloo-saw.runit:
	$(MAKE) one-runit SYSTEM=bigloo-saw SUFFIX=exe DIR=bigloo-saw TITLE=BiglooSAW COMPILER="`$(BIGLOO) -version`"
c.runit:
	$(MAKE) one-runit SYSTEM=c SUFFIX=exe DIR=c TITLE=C COMPILER="$(CC)"
gambit.runit:
	$(MAKE) one-runit SYSTEM=gambit SUFFIX=exe DIR=gambit TITLE=gambit COMPILER="`$(GAMBIT) -v | awk '{print $$1}'`"

#*---------------------------------------------------------------------*/
#*    one-runit                                                        */
#*---------------------------------------------------------------------*/
one-runit:
	@ echo "(title \"$(TITLE)\"" > $(SYSTEM).runit
	@ echo "   (compiler \"$(COMPILER)\")" >> $(SYSTEM).runit
	@ echo "   (configuration)" >> $(SYSTEM).runit
	@ echo "   (benchmarks" >> $(SYSTEM).runit
	@for p in $(BENCHMARKS); do \
           if [ -d $$p/$(DIR) ]; then \
             if [ -f $$p/RUN-OPTIONS ]; then \
	        echo "     ($$p \"(cd ./$$p/$(DIR); ./$(DIR).$(SUFFIX) `cat $$p/RUN-OPTIONS`)\")" >> $(SYSTEM).runit; \
             else \
                if [ -f $$p/RUN-OPTIONS.$(DIR) ]; then \
	          echo "     ($$p \"(cd ./$$p/$(DIR); ./$(DIR).$(SUFFIX) `cat $$p/RUN-OPTIONS.$(DIR)`)\")" >> $(SYSTEM).runit; \
                else \
	          echo "     ($$p \"(cd ./$$p/$(DIR); ./$(DIR).$(SUFFIX))\")" >> $(SYSTEM).runit; \
                fi; \
             fi; \
          fi; \
        done
	@ echo "     ))" >> $(SYSTEM).runit;

#*---------------------------------------------------------------------*/
#*    links                                                            */
#*---------------------------------------------------------------------*/
mkln:
	@ for p in $(BENCHMARKS); do \
            if [ -d $$p/bigloo ]; then \
	       (cd $$p; \
               if [ ! -e bigloo-jvm ]; then \
                  ln -s bigloo bigloo-jvm; \
               fi; \
               if [ ! -e bigloo-wasm ]; then \
                  ln -s bigloo bigloo-wasm; \
               fi || exit -1; \
               if [ ! -e bigloo-saw ]; then \
                  ln -s bigloo bigloo-saw; \
               fi || exit -1; \
               if [ ! -e gambit ]; then \
                  ln -s bigloo gambit; \
               fi) || exit -1; \
            fi \
	  done

cleanln:
	@ for p in $(BENCHMARKS); do \
	    (cd $$p; $(RM) -f bigloo-jvm) || exit -1; \
	    (cd $$p; $(RM) -f bigloo-saw) || exit -1; \
	    (cd $$p; $(RM) -f bigloo-wasm) || exit -1; \
	  done
