#*=====================================================================*/
#*    serrano/prgm/project/bglstone/Makefile                           */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Sun Aug  5 11:50:34 2001                          */
#*    Last change :  Sat Sep  7 13:37:38 2024 (serrano)                */
#*    Copyright   :  2001-24 Manuel Serrano                            */
#*    -------------------------------------------------------------    */
#*    The general Bglstone Makefile                                    */
#*=====================================================================*/

#*---------------------------------------------------------------------*/
#*    help                                                             */
#*---------------------------------------------------------------------*/
help:
	@ echo "report [CONFIGS=]......... compile, run and produce a report"
	@ echo "report.ps [CONFIGS=]...... only produce a benchmark report"
	@ echo 
	@ echo "show [BENCH=]............. show the benchmarks"
	@ echo "compile [BENCH=].......... compile the benchmarks"
	@ echo "run [VERBOSE=] [RUNFLAGS=] run the compiled benchmarks"
	@ echo "rerun [BENCH=] [VERBOSE=]. rerun the compiled benchmarks"
	@ echo "test [BENCH=] [VERBOSE=].. test the compiled benchmarks"
	@ echo "bench [BENCH=] [VERBOSE=]. compile and run the benchmarks"
	@ echo "base...................... compile and run the base benchmarks"
	@ echo 
	@ echo "tools..................... compile the bench tools" 
	@ echo 
	@ echo "clean..................... remove the compilation objects"
	@ echo "distclean................. clean + unconfigure"
	@ echo "cleantools................ remove bench tools"
	@ echo "cleanbench................ remove bench results"
	@ echo "cleanreport............... remove bench reports"
	@ echo "cleanall.................. remove everything"
	@ echo "cleanbase................. remove the base results"
	@ echo "cleanresults.............. remove all the previous results"
	@ echo 
	@ echo "distrib................... create a Bglstone distrib"
	@ echo 
	@ echo "$$""TARGETS=<the benched targets> (default: bigloo, bigloo-jvm, bigloo-wasm, java, c, csharp, ...)"
	@ echo "$$""BENCH=<test,bglstone,bgl,fullbgl,scm,other> (default: fullbgl)"
	@ echo "$$""CONFIGS=the configurations to be included in the report"
	@ echo "  don't include \"base\" in the CONFIG variable."
	@ echo "$$""VERBOSE=-v[2]"
	@ echo "$$""RUNFLAGS=runtit options (such as \"--user+sys\" or \"--wall-clock\")"

#*---------------------------------------------------------------------*/
#*    Default settings                                                 */
#*---------------------------------------------------------------------*/
TARGETS	= bigloo
BENCH	= fullbgl

#*---------------------------------------------------------------------*/
#*    report                                                           */
#*---------------------------------------------------------------------*/
report: compile run report.ps

report.ps: tools
	@ (cd report; $(MAKE) report.ps BENCH=$(BENCH) TARGETS="$(TARGETS)"; cp report.ps ../report.ps)

#*---------------------------------------------------------------------*/
#*    show                                                             */
#*---------------------------------------------------------------------*/
show:
	@ echo "[0m[1;35m>>> show[0m"
	@ (cd src; $(MAKE) show BENCH=$(BENCH) TARGETS="$(TARGETS)")

#*---------------------------------------------------------------------*/
#*    compile                                                          */
#*---------------------------------------------------------------------*/
compile: tools
	@ echo "[0m[1;35m>>> compile[0m"
	@ (cd src; $(MAKE) compile BENCH=$(BENCH) TARGETS="$(TARGETS)")

#*---------------------------------------------------------------------*/
#*    run                                                              */
#*---------------------------------------------------------------------*/
run: tools
	@ echo "[0m[1;35m>>> run[0m"
	@ (cd src; $(MAKE) run BENCH=$(BENCH) TARGETS="$(TARGETS)")

#*---------------------------------------------------------------------*/
#*    bench                                                            */
#*---------------------------------------------------------------------*/
bench: compile run

#*---------------------------------------------------------------------*/
#*    base                                                             */
#*---------------------------------------------------------------------*/
base: results/base/_base.stat

results/base/_base.stat: src/_base.stat
	cp src/_base.stat results/base/_base.stat

src/_base.stat:
	$(MAKE) compile TARGETS=bigloo
	$(MAKE) run TARGETS=_base

#*---------------------------------------------------------------------*/
#*    rerun                                                            */
#*---------------------------------------------------------------------*/
rerun: 
	-/bin/rm -f src/*.stat 2> /dev/null
	$(MAKE) run BENCH=$(BENCH) TARGETS="$(TARGETS)"

#*---------------------------------------------------------------------*/
#*    test                                                             */
#*---------------------------------------------------------------------*/
test: tools
	@ echo "[0m[1;35m>>> test[0m"
	@ (cd src; $(MAKE) run REPETITION="-r 1" BENCH=$(BENCH) TARGETS="$(TARGETS)")

#*---------------------------------------------------------------------*/
#*    tools                                                            */
#*---------------------------------------------------------------------*/
.PHONY: tools
tools:
	@ echo "[0m[1;35m>>> tools[0m"
	@ (cd tools; $(MAKE) BENCH=$(BENCH) TARGETS="$(TARGETS)")

#*---------------------------------------------------------------------*/
#*    distrib                                                          */
#*---------------------------------------------------------------------*/
distrib:
	$(MAKE) cleanall BENCH=$(BENCH) TARGETS="$(TARGETS)"
	cleanup
	(cd ..; tar cvfz $$HOME/prgm/distrib/bglstone.tar.gz bglstone)

#*---------------------------------------------------------------------*/
#*    cleanning                                                        */
#*---------------------------------------------------------------------*/
clean:
	@ (cd src; $(MAKE) clean BENCH=$(BENCH) TARGETS="$(TARGETS)")
	@ (cd src; $(MAKE) cleanln BENCH=$(BENCH) TARGETS="$(TARGETS)")

distclean: clean
	/bin/rm -f Makefile.config

cleantools:
	-@ (cd tools; $(MAKE) clean BENCH=$(BENCH) TARGETS="$(TARGETS)")

cleanbench:
	@ (cd src; $(MAKE) cleanall BENCH=$(BENCH) TARGETS="$(TARGETS)")
	@ (cd src/bigloo; touch bigloo/foo.class; /bin/rm -f bigloo/*.class)

cleanbase:
	@ (cd src; /bin/rm -f base.runit _base.stat)
	@ (cd results; /bin/rm -f base/_base.stat)

cleanresults:
	@ (cd src; touch foo.runit foo.stat; /bin/rm -f *.runit *.stat)
	@ (cd results; /bin/rm -rf [^b]*)

cleanreport:
	/bin/rm -f report.ps
	(cd report; $(MAKE) clean BENCH=$(BENCH) TARGETS="$(TARGETS)")

cleantex:
	@ (cd tex; $(MAKE) clean BENCH=$(BENCH) TARGETS="$(TARGETS)")

cleanall: cleanbench cleantools cleanreport distclean cleantex

