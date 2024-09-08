#*=====================================================================*/
#*    serrano/prgm/project/bglstone/report/Makefile                    */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Thu Aug  9 09:34:18 2001                          */
#*    Last change :  Wed Jan  8 16:59:51 2003 (serrano)                */
#*    Copyright   :  2001-03 Manuel Serrano                            */
#*    -------------------------------------------------------------    */
#*    The Makefile that handles bglstone reports                       */
#*=====================================================================*/
include ../Makefile.config

#*---------------------------------------------------------------------*/
#*    Configuration                                                    */
#*---------------------------------------------------------------------*/
R2TEX=$(TOOLSBIN)/r2tex
R2B=$(TOOLSBIN)/r2b
BC=$(TOOLSBIN)/bchart
STATINFO=$(TOOLSBIN)/statinfo

RESDIR=../results
CONFIGS=

#*---------------------------------------------------------------------*/
#*    Configuration                                                    */
#*---------------------------------------------------------------------*/
MAX_SCALE_VALUE=
BCHAR_HEIGHT=25
BCHAR_WIDTH=18

BNAME="(lambda (t c) (let ((lbl (assq 'label c))) (if (pair? lbl) (cadr lbl) (string-append (cond ((string-ci=? t \"bigloo\") \"B\") ((string-ci=? t \"bigloojvm\") \"BJ\") (else t)) \":\" (cadr (assq 'arch c)) \"-\" (cadr (assq 'mhz c))))))"

#*---------------------------------------------------------------------*/
#*    SUFFIXES:                                                        */
#*---------------------------------------------------------------------*/
.SUFFIXES: 
.SUFFIXES: .tex .bc .ps .eps .dvi .stat

#*---------------------------------------------------------------------*/
#*    report.ps                                                        */
#*---------------------------------------------------------------------*/
report.ps: report.tex
	latex report.tex; dvips -o report.ps report.dvi

#*---------------------------------------------------------------------*/
#*    report.tex                                                       */
#*---------------------------------------------------------------------*/
report.tex: header.tex $(RESDIR)
	cp header.tex $*.tex
	$(MAKE) base-report
	$(MAKE) all-report
	if [ "$(CONFIGS) " = " " ]; then \
	  for p in $(RESDIR)/*; do \
            if [ $$p != "$(RESDIR)/base" ]; then \
              $(MAKE) one-report ARCHI=`basename $$p`; \
            fi \
          done \
        else \
	  for p in $(RESDIR)/$(CONFIGS); do \
            $(MAKE) one-report ARCHI=`basename $$p`; \
          done \
        fi
	echo "\\end{document}" >> $*.tex

#*---------------------------------------------------------------------*/
#*    base-report ...                                                  */
#*---------------------------------------------------------------------*/
base-report: $(RESDIR)/base/_base.stat
	@ echo >> report.tex
	@ echo "%% Base =====================================" >> report.tex
	@ echo "\\section{Base}" >> report.tex
	@ $(MAKE) config-info DEST=report.tex SRC=$(RESDIR)/base/_base.stat
	@ echo >> report.tex
	@ echo "\\begin{center}" >> report.tex
	@ echo "\\begin{small}" >> report.tex
	$(R2TEX) -t "CPU+SYS seconds" -valsuffix s -st "Benchmarks" $(RESDIR)/base/_base.stat >> report.tex
	@ echo "\\end{small}" >> report.tex
	@ echo "\\end{center}" >> report.tex

#*---------------------------------------------------------------------*/
#*    all-report ...                                                   */
#*---------------------------------------------------------------------*/
all-report: $(RESDIR)/base/_base.stat
	@ echo >> report.tex
	@ echo "%% All =====================================" >> report.tex
	@ echo "\\section{All}" >> report.tex
	@ echo "\\begin{center}" >> report.tex
	@ echo "\\begin{small}" >> report.tex
	@ echo "[0m[1;35m>>> TeX table[0m"
	@ if [ "$(CONFIGS) " = " " ]; then \
            if [ "$(TARGETS) " = " " ]; then \
	      $(R2TEX) -vR -t "CPU+SYS seconds" -valsuffix "$$\\times$$" -st "Benchmarks" $(RESDIR)/base/_base.stat $(RESDIR)/[^_]*/*.stat -bname $(BNAME) >> report.tex; \
            else \
	      $(R2TEX) -vR -t "CPU+SYS seconds" -valsuffix "$$\\times$$" -st "Benchmarks" $(RESDIR)/base/_base.stat $(TARGETS:%=$(RESDIR)/[^_]*/%.stat) -bname $(BNAME) >> report.tex; \
            fi; \
         else \
            if [ "$(TARGETS) " = " " ]; then \
	      $(R2TEX) -vR -t "CPU+SYS seconds" -valsuffix "$$\\times$$" -st "Benchmarks" $(RESDIR)/base/_base.stat $(CONFIGS:%=$(RESDIR)/%/*.stat) -bname $(BNAME) >> report.tex; \
            else \
              f=""; \
              for c in dummy $(CONFIGS); do \
                if [ ! $$c = "dummy" ]; then \
                  for s in $(TARGETS); do \
                    f="$(RESDIR)/$$c/$$s.stat $$f"; \
                  done; \
                fi; \
              done; \
	      $(R2TEX) -vR -t "CPU+SYS seconds" -valsuffix "$$\\times$$" -st "Benchmarks" $(RESDIR)/base/_base.stat $$f -bname $(BNAME) >> report.tex; \
            fi; \
          fi
	@ echo "\\end{small}" >> report.tex
	@ echo "\\end{center}" >> report.tex
	@ echo "\\begin{center}" >> report.tex
	@ echo "\\begin{small}" >> report.tex
	@ echo "\\epsfig{file=all.eps,width=$(BCHAR_WIDTH)cm}" >> report.tex
	@ echo "\\end{small}" >> report.tex
	@ echo "\\end{center}" >> report.tex
	@ echo "[0m[1;35m>>> bchart[0m"
	@ if [ "$(CONFIGS) " = " " ]; then \
            if [ "$(TARGETS) " = " " ]; then \
	      $(R2B) -R $(RESDIR)/base/_base.stat -t "Everyone vs `$(STATINFO) $(RESDIR)/base/_base.stat arch` (`$(STATINFO) $(RESDIR)/base/_base.stat mhz`MHz)" $(RESDIR)/[^_]*/*.stat -o all.bc -bname $(BNAME); \
            else \
	      $(R2B) -R $(RESDIR)/base/_base.stat -t "Everyone vs `$(STATINFO) $(RESDIR)/base/_base.stat arch` (`$(STATINFO) $(RESDIR)/base/_base.stat mhz`MHz)" $(TARGETS:%=$(RESDIR)/[^_]*/%.stat) -o all.bc -bname $(BNAME); \
            fi; \
          else \
            if [ "$(TARGETS) " = " " ]; then \
	      $(R2B) -R $(RESDIR)/base/_base.stat -t "Everyone vs `$(STATINFO) $(RESDIR)/base/_base.stat arch` (`$(STATINFO) $(RESDIR)/base/_base.stat mhz`MHz)" $(CONFIGS:%=$(RESDIR)/%/*.stat) -o all.bc -bname $(BNAME); \
            else \
              f=; \
              for c in dummy $(CONFIGS); do \
                if [ ! $$c = "dummy" ]; then \
                  for s in $(TARGETS); do \
                    f="$(RESDIR)/$$c/$$s.stat $$f"; \
                  done; \
                fi; \
              done; \
              $(R2B) -R $(RESDIR)/base/_base.stat -t "Everyone vs `$(STATINFO) $(RESDIR)/base/_base.stat arch` (`$(STATINFO) $(RESDIR)/base/_base.stat mhz`MHz)" $$f -o all.bc -bname $(BNAME); \
            fi \
          fi
	@ if [ "$(MAX_SCALE_VALUE) " = " " ]; then \
            $(BC) -h $(BCHAR_HEIGHT) all.bc -lv -o all.eps; \
          else \
            $(BC) -h $(BCHAR_HEIGHT) all.bc -lv -max $(MAX_SCALE_VALUE) -o all.eps; \
          fi

#*---------------------------------------------------------------------*/
#*    one-report                                                       */
#*---------------------------------------------------------------------*/
one-report:
	@ echo "[0m[1;33mone-report: $(ARCHI)[0m"
	@ echo >> report.tex
	@ echo "%% $(ARCHI) =====================================" >> report.tex
	@ echo "\\section{$(ARCHI)}" >> report.tex
	@ echo "config-info DEST=report.tex SRC=$(word 1, $(TARGETS:%=$(RESDIR)/$(ARCHI)/%)).stat"
	@ $(MAKE) config-info DEST=report.tex SRC=$(word 1, $(TARGETS:%=$(RESDIR)/$(ARCHI)/%)).stat
	@ echo "\\begin{itemize}" >> report.tex
	 for p in $(TARGETS); do \
	     echo "\\item{\\underline{$$p}}: `$(STATINFO) $(RESDIR)/$(ARCHI)/$$p.stat comment`" >> report.tex; \
          done
	@ echo "\\end{itemize}" >> report.tex
	@ echo >> report.tex
	@ echo "\\begin{center}" >> report.tex
	@ echo "\\begin{small}" >> report.tex
	$(R2TEX) -vR -t "CPU+SYS seconds" -valsuffix "$$\\times$$" -st "Benchmarks" $(RESDIR)/base/_base.stat $(TARGETS:%=$(RESDIR)/$(ARCHI)/%.stat) >> report.tex
	@ echo "\\end{small}" >> report.tex
	@ echo "\\end{center}" >> report.tex
	@ echo "\\begin{center}" >> report.tex
	@ echo "\\begin{small}" >> report.tex
	@ echo "\\epsfig{file=$(ARCHI).eps,width=$(BCHAR_WIDTH)cm}" >> report.tex
	@ echo "\\end{small}" >> report.tex
	@ echo "\\end{center}" >> report.tex
	(src=$(word 1, $(TARGETS)); \
	  $(R2B) -R -t "`$(STATINFO) $(RESDIR)/$(ARCHI)/$$src.stat arch` (`$(STATINFO) $(RESDIR)/$(ARCHI)/$$src.stat mhz`MHz) vs `$(STATINFO) $(RESDIR)/base/_base.stat arch` (`$(STATINFO) $(RESDIR)/base/_base.stat mhz`MHz)" $(RESDIR)/base/_base.stat $(TARGETS:%=$(RESDIR)/$(ARCHI)/%.stat) -o $(ARCHI).bc)
	$(BC) -h 24 $(ARCHI).bc -o $(ARCHI).eps

#*---------------------------------------------------------------------*/
#*    config-info                                                      */
#*---------------------------------------------------------------------*/
.PHONY: config-info

config-info:
	@ echo "\\begin{small}" >> $(DEST)
	@ echo "\\begin{itemize}" >> $(DEST)
	@ echo " \\item Processor: `$(STATINFO) $(SRC) arch`, `$(STATINFO) $(SRC) mhz`MHz" >> $(DEST)
	@ echo " \\item Memory: `$(STATINFO) $(SRC) memory`MB" >> $(DEST)
	@ echo " \\item System: `$(STATINFO) $(SRC) os`" >> $(DEST)
	@ echo " \\item Compiler: `$(STATINFO) $(SRC) compiler`" >> $(DEST)
	@ echo " \\item Hardware: `$(STATINFO) $(SRC) hardware`" >> $(DEST)
	@ echo "\\end{itemize}" >> $(DEST)
	@ echo "\\end{small}" >> $(DEST)

#*---------------------------------------------------------------------*/
#*    cleaning                                                         */
#*---------------------------------------------------------------------*/
clean:
	/bin/rm -f report.tex report.dvi report.ps report.pdf
	/bin/rm -f base.tex
	/bin/rm -f *.log *.aux
	/bin/rm -f *.eps *.bc

clenall: clean

