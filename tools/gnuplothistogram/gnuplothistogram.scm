;*=====================================================================*/
;*    .../bglstone/tools/gnuplothistogram/gnuplothistogram.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Sep 11 08:01:47 2024                          */
;*    Last change :  Wed Jul  2 15:38:41 2025 (serrano)                */
;*    Copyright   :  2024-25 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Generates a .csv and .plot files for gnuplot.                    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module gnuplothistogram
   (main main))

;*---------------------------------------------------------------------*/
;*    include-template ...                                             */
;*---------------------------------------------------------------------*/
(define-macro (include-template file)
   (call-with-input-file file read-string))

;*---------------------------------------------------------------------*/
;*    global parameters                                                */
;*---------------------------------------------------------------------*/
(define *fout* "fig")
(define *inputs* '())
(define *aliases* '())
(define *absolute-title* "Wall clock (@PROCESSOR@)")
(define *relative-title* "Relative time (@PROCESSOR@)")
(define *user-title* #f)
(define *absolute-ylabel* "execution time (in @TIME-UNIT@)")
(define *relative-ylabel* "relative time")
(define *user-ylabel* #f)
(define *time-unit* 'sec)
(define *relative* #f)
(define *relative-position* 'left)
(define *errorbars* "errorbars lw 1")
(define *force-errorbars* #unspecified)
(define *logscale* #f)
(define *xfontsize* "8")
(define *yfontsize* "10")
(define *vfontsize* "6")
(define *format* "pdf")
(define *benchmarks* '())
(define *separator* -1)
(define *size* "")
(define *base-color* "red")
(define *values* #f)
(define *lmargin* "6")
(define *rmargin* "1")
(define *bmargin* "0")
(define *tmargin* "") ;; activate automatic tmargin
(define *key* "under nobox")
(define *min-threshold* 10)
(define *range* "[0:*]")

(define *offset-tables*
   `#(- #(0)
	#(,(- (/ 1 6)) ,(/ 1 6))
	#(,(- (/ 1 6)) 0 ,(/ 1 6))
	#(,(- (/ 2 6)) ,(- (/ 1 6)) ,(/ 1 6) ,(/ 2 6))
	#(,(- (/ 2 6)) ,(- (/ 1 6)) 0 ,(/ 1 6) ,(/ 2 6))
	#(,(- (/ 3 6)) ,(- (/ 2 6)) ,(- (/ 1 6)) 0 ,(/ 1 6) ,(/ 2 6) ,(/ 3 6))))

(define *colors*
   '#("#3264c8" "#fa9600" "#d83812" "#109318"
      "#93ade2" "#edd20b" "#00a0bf" "#72bf00"
      "#969996" "#4b30ed"))
   
;*---------------------------------------------------------------------*/
;*    *template* ...                                                   */
;*---------------------------------------------------------------------*/
(define *template* (include-template "./gnuplothistogram-template.plot"))

;*---------------------------------------------------------------------*/
;*    main ...                                                         */
;*---------------------------------------------------------------------*/
(define (main args)
   ;; command line parsing
   (parse-args args)
   ;; sanityf check
   (unless (pair? *inputs*)
      (print "*** ERROR: input file missing")
      (usage)
      (exit 1))
   ;; read all the input files
   (let ((stats (map read-stat *inputs*)))
      ;; output the csv file
      (with-output-to-file (string-append *fout* ".csv")
	 (lambda ()
	    (output-csv stats)))
      ;; output the plot file
      (with-output-to-file (string-append *fout* ".plot")
	 (lambda ()
	    (output-plot stats)))))

;*---------------------------------------------------------------------*/
;*    errorbars? ...                                                   */
;*---------------------------------------------------------------------*/
(define (errorbars?)
   (or (eq? *force-errorbars* #t)
       (and (not *relative*) (not (eq? *force-errorbars* #f)))))

;*---------------------------------------------------------------------*/
;*    parse-args ...                                                   */
;*---------------------------------------------------------------------*/
(define (parse-args args)
   (args-parse (cdr args)
      (("-help" (help "This message"))
       (usage)
       (newline)
       (args-parse-usage #f)
       (exit 0))
      (("-o" ?fout (help "Output file basename"))
       (set! *fout* (pregexp-replace ".pdf$" fout "")))
      (("--unit" ?unit (help "Set time unit (default \"s\""))
       (set! *time-unit* (string->symbol unit)))
      (("--rename" ?name ?alias (help "System renaming"))
       (set! *aliases* (cons (cons name alias) *aliases*)))
      (("--template" ?file (help "Template file name"))
       (set! *template* (call-with-input-file file read-string)))
      ((("-r" "--relative") (help "Display relative values"))
       (set! *relative* 'avec))
      (("--relative-sans" (help "Display relative values (sans base)"))
       (set! *relative* 'sans)
       (set! *relative-position* 'left))
      (("--relative-sans-right" (help "Display relative values, flushed right"))
       (set! *relative* 'sans)
       (set! *relative-position* 'right))
      (("--relative-sans-left" (help "Display relative values, flushed left"))
       (set! *relative* 'sans)
       (set! *relative-position* 'left))
      ((("-t" "--title") ?title (help "Figure title"))
       (set! *user-title* title))
      ((("-l" "--ylabel") ?label (help "Figure ylabel"))
       (set! *user-ylabel* label))
      ((("-g" "--logscale") (help "Log scale"))
       (set! *logscale* #t))
      (("--x-fontsize" ?size (help "x font-size"))
       (set! *xfontsize* size))
      (("--y-fontsize" ?size (help "y font-size"))
       (set! *yfontsize* size))
      (("--v-fontsize" ?size (help "value font-size"))
       (set! *vfontsize* size))
      ((("-f" "--format") ?format (help "output format"))
       (set! *format* format))
      (("--benchmarks" ?benchs (help "List of benchmarks to use"))
       (set! *benchmarks* (call-with-input-string benchs port->sexp-list)))
      (("--separator" ?index (help "Vertical separation line"))
       (set! *separator* (string->integer index)))
      (("--size" ?size (help "output size"))
       (set! *size* (format "size ~a" size)))
      (("--basecolor" ?color (help "Set base color (for --relative-sans)"))
       (set! *base-color* color))
      (("--colors" ?colors (help "Bar colors (space or comma separated)"))
       (vector-copy! *colors* 0 (list->vector (string-split colors " ,"))))
      (("--values" (help "Add values label to the bars"))
       (set! *values* #t))
      (("--lmargin" ?lmargin (help "left margin"))
       (set! *lmargin* lmargin))
      (("--rmargin" ?rmargin (help "right margin"))
       (set! *rmargin* rmargin))
      (("--bmargin" ?bmargin (help "bottom margin"))
       (set! *bmargin* bmargin))
      (("--tmargin" ?tmargin (help "top margin"))
       (set! *tmargin* tmargin))
      (("--key" ?key (help "gnuplot key configuration"))
       (set! *key* key))
      (("--min-threshold" ?threshold (help "min significant value"))
       (set! *min-threshold* (string->number threshold)))
      (("--errorbars" (help "force error bars even for relative histographs"))
       (set! *force-errorbars* #t))
      (("--no-errorbars" (help "disable error bars"))
       (set! *force-errorbars* #f))
      (("--range" ?range (help "set gnuplot range"))
       (set! *range* range))
      (else
       (set! *inputs* (append *inputs* (list else))))))

;*---------------------------------------------------------------------*/
;*    usage ...                                                        */
;*---------------------------------------------------------------------*/
(define (usage)
   (print "usage: gnuplothistogram [options] file ..."))

;*---------------------------------------------------------------------*/
;*    output-csv ...                                                   */
;*---------------------------------------------------------------------*/
(define (output-csv stats)

   (define (absolute-data stats benchmarks)
      (for-each (lambda (benchmark)
		   (display* benchmark ",  ")
		   (printf "~(,  )\n"
		      (map (lambda (stat)
			      (let ((val (assq benchmark (cdddr stat))))
				 (if (null? val)
				     (error (cadr stat)
					"Cannot find benchmark value"
					benchmark)
				     (format "~(,)"
					(times-in-unit
					   (median (threshold (caddr val))))))))
			 stats)))
	 benchmarks))

   (define (relative-data stats benchmarks)
      (for-each (lambda (benchmark)
		   (with-handler
		      (lambda (e)
			 (fprint (current-error-port)
			    "*** ERROR: wrong benchmark entry " benchmark)
			 (raise e))
		      (let* ((val (assq benchmark (cdddr (car stats))))
			     (base (median (threshold (caddr val)))))
			 (display* benchmark ",  ")
			 (printf "~(,  )\n"
			    (map (lambda (stat)
				    (let* ((val (assq benchmark (cdddr stat)))
					   (med (median (threshold (caddr val)))))
				       (cond
					  ((null? val)
					   (error (cadr stat)
					      "Cannot find benchmark value"
					      benchmark))
					  ((errorbars?)
					  	(let* ((bases (threshold (caddr (assq benchmark (cdddr (car stats))))))
							        (times (threshold (caddr val)))
							        (ratios (map (lambda (x y) (/ x y)) times bases))
							        (mean-ratio (geomean ratios))
							        (stddev-ratio (geostddev ratios))
							        (lo-bar (/ mean-ratio stddev-ratio))
							        (hi-bar (* mean-ratio stddev-ratio)))
							(format "~a, ~a, ~a" mean-ratio lo-bar hi-bar)))
					  (else
					   (format "~a"
					      (/ (car (median (threshold (caddr val)))) (car base)))))))
			       (if (eq? *relative* 'avec)
				   stats
				   (cdr stats)))))))
	 benchmarks))
   
   (let ((benchmarks (if (pair? *benchmarks*)
			 *benchmarks*
			 (sort-benchmarks
			    (delete-duplicates
			       (map car
				  (apply append
				     (map cdddr stats))))))))
      ;; following lines with benchmark values
      (if *relative*
	  (begin
	     (printf "# ~a / ~( )\n" (system-name (car stats)) (map system-name (cdr stats)))
	     (relative-data stats benchmarks))
	  (begin
	     (printf "# ~( )\n" (map system-name stats))
	     (absolute-data stats benchmarks)))))

;*---------------------------------------------------------------------*/
;*    output-plot ...                                                  */
;*---------------------------------------------------------------------*/
(define (output-plot stats)
   
   (define (absolute-plot stats)
      (let loop ((stats stats)
		 (i 0))
	 (printf "   '~a.csv' u ~(:):xtic(1) title '~a' ls ~d"
	    (basename *fout*)
	    (iota 3 (+fx (*fx i 3) 2))
	    (system-name (car stats))
	    (+fx i 1))
	 (when (pair? (cdr stats))
	    (print ",\\")
	    (loop (cdr stats) (+fx i 1)))))
   
   (define (absolute-values stats)
      (let ((table (vector-ref *offset-tables* (length stats))))
	 (let loop ((stats stats)
		    (i 0))
	    (printf "   '~a.csv' u ($0+~a):($~a+.15):(sprintf(\"%3.2f\",$~a)) with labels font 'Verdana,~a' rotate by 90 notitle"
	       (basename *fout*)
	       (vector-ref table i)
	       (+fx i 2)
	       (+fx i 2)
	       *vfontsize*)
	    (when (pair? (cdr stats))
	       (print ",\\")
	       (loop (cdr stats) (+fx i 1))))))
   
   (define (relative-plot stats)
      (let loop ((stats (if (eq? *relative* 'avec) stats (cdr stats)))
		 (i 0))
	 (printf "   '~a.csv' u ~a:xtic(1) title '~a' ls ~d"
	    (basename *fout*) (+fx i 2)
	    (system-name (car stats))
	    (+fx i 1))
	 (when (pair? (cdr stats))
	    (print ",\\")
	    (loop (cdr stats) (+fx i 1)))))
   
   (define (relative-plot-errorbars stats)
      (let loop ((stats (if (eq? *relative* 'avec) stats (cdr stats)))
		 (i 0))
	 (printf "   '~a.csv' u ~a:~a:~a:xtic(1) title '~a' ls ~d"
	    (basename *fout*) (+fx i 2) (+fx i 3) (+fx i 4)
	    (system-name (car stats))
	    (+fx (/fx i 3) 1))
	 (when (pair? (cdr stats))
	    (print ",\\")
	    (loop (cdr stats) (+fx i 3)))))
   
   (define (relative-values stats)
      (let* ((stats (if (eq? *relative* 'avec) stats (cdr stats)))
	     (table (vector-ref *offset-tables* (length stats))))
	 (let loop ((stats stats)
		    (i 0))
	    (printf "   '~a.csv' u ($0+~a):($~a+.15):(sprintf(\"%3.2f\",$~a)) with labels font 'Verdana,~a' rotate by 90 notitle"
	       (basename *fout*)
	       (vector-ref table i)
	       (+fx i 2)
	       (+fx i 2)
	       *vfontsize*)
	    (when (pair? (cdr stats))
	       (print ",\\")
	       (loop (cdr stats) (+fx i 1))))))
   
   (define (relative-values-errorbars stats)
      (let* ((stats (if (eq? *relative* 'avec) stats (cdr stats)))
	     (table (vector-ref *offset-tables* (length stats))))
	 (let loop ((stats stats)
		    (i 0))
	    (printf "   '~a.csv' u ($0+~a):($~a+.15):(sprintf(\"%3.2f\",$~a)) with labels font 'Verdana,~a' rotate by 90 notitle"
	       (basename *fout*)
	       (vector-ref table (/fx i 3))
	       (+fx i 2)
	       (+fx i 2)
	       *vfontsize*)
	    (when (pair? (cdr stats))
	       (print ",\\")
	       (loop (cdr stats) (+fx i 3))))))
   
   (let ((times-length (length (caddr (car (stat-times (car stats))))))
	 (proc (assq-get 'processor (stat-configuration (car stats)) "")))
      ;; color patching
      (let loop ((i (-fx (vector-length *colors*) 1)))
	 (when (>= i 0)
	    (set! *template*
	       (pregexp-replace* (format "@COLOR~a@" i)
		  *template* (vector-ref *colors* i)))
	    (loop (-fx i 1))))
      ;; margins
      (set! *template* (pregexp-replace "@LMARGIN@" *template* *lmargin*))
      (set! *template* (pregexp-replace "@RMARGIN@" *template* *rmargin*))
      (set! *template* (pregexp-replace "@BMARGIN@" *template* *bmargin*))
      (let ((s (pregexp-replace*
		  "@RANGE@"
		  (pregexp-replace*
		     "@KEY@"
		     (pregexp-replace*
			"@SIZE@"
			(pregexp-replace*
			   "@FORMAT@"
			   (pregexp-replace
			      "@ERRORBARS@"
			      (pregexp-replace
				 "@XTICS@"
				 (pregexp-replace
				    "@YTICS@"
				    (pregexp-replace*
				       "@TIME-UNIT@"
				       (pregexp-replace*
					  "@PROCESSOR@"
					  (pregexp-replace*
					     "@YLABEL@"
						(pregexp-replace*
						 "@TMARGIN"
						  (pregexp-replace*
						  	"@TITLE@"
						  	(pregexp-replace* "@BASENAME@" *template* (basename *fout*))
						  	(cond
						  	  (*user-title* *user-title*)
						  	  (*relative* *relative-title*)
						  	  (else *absolute-title*)))
						  *tmargin*)
					     (cond
						(*user-ylabel* *user-ylabel*)
						(*relative* *relative-ylabel*)
						(else *absolute-ylabel*)))
					  proc)
				       (symbol->string! *time-unit*))
				    *yfontsize*)
				 *xfontsize*)
			      (if (errorbars?)
				  *errorbars*
				  ""))
			   *format*)
			*size*)
		     *key*)
		  *range*)))
	 
	 ;; dummy print for grabbing GPVAL_Y_MAX
	 (when (>fx *separator* 0)
	    (print "set output '/dev/null'\nset terminal dumb\n")
	    (print "plot \\")
	    (cond
	       ((not *relative*) (absolute-plot stats))
	       ((errorbars?) (relative-plot-errorbars stats))
	       (else (relative-plot stats)))
	    
	    (when *values*
	       (print ", \\")
	       (cond
		  ((not *relative*) (absolute-plot stats))
		  ((errorbars?) (relative-values-errorbars stats))
		  (else (relative-values stats))))
	    (print "\nreset\n"))
	 
	 (print s)
	 
	 (when (eq? *relative* 'sans)
	    (printf "set arrow 1 from graph 0, first 1 to graph 1, first 1 nohead lc '~a' lw 2 dt '---' front\n" *base-color*)
	    (printf "set label 1 '~a' font 'Verdana,10' at ~a,1 offset 0.1,0.4 left tc '~a' front\n\n"
	       (system-name (car stats))
	       (case *relative-position*
		  ((left) -1)
		  ((right) (-fx (length (cddr (car stats))) 2)))
	       *base-color*))
	 
	 (when *logscale*
	    (print "set logscale y\n"))
	 
	 (when (>fx *separator* 0)
	    (printf "set arrow from ~a,GPVAL_Y_MIN to ~a,GPVAL_Y_MAX nohead ls 1000 dashtype 2\n\n"
	       (- *separator* 0.5)
	       (- *separator* 0.5)))
	 
	 (print "plot \\")
	 (cond
	    ((not *relative*) (absolute-plot stats))
	    ((errorbars?) (relative-plot-errorbars stats))
	    (else (relative-plot stats)))
	 
	 (when *values*
	    (print ", \\")
	    (cond
	       ((not *relative*) (absolute-plot stats))
	       ((errorbars?) (relative-values-errorbars stats))
	       (else (relative-values stats)))))))
	 
;*---------------------------------------------------------------------*/
;*    read-stat ...                                                    */
;*---------------------------------------------------------------------*/
(define (read-stat file)
   
   (define (correct-benchmark? b)
      (match-case b
	 (((? symbol?) (? string?) ?times) (every number? times))
	 (else #f)))
   
   (call-with-input-file file
      (lambda (p)
	 (let ((s (read p)))
	    (match-case s
	       ((?title (? string?) (configuration . ?-) . ?benchmarks)
		(if (every correct-benchmark? benchmarks)
		    s
		    (error file "wrong benchmark"
		       (find (lambda (b) (not (correct-benchmark? b)))
			  benchmarks))))
	       (else
		(error file "wrong stat file" s)))))))

;*---------------------------------------------------------------------*/
;*    system-name ...                                                  */
;*---------------------------------------------------------------------*/
(define (system-name stat)
   (let* ((name (cadr stat))
	  (c (assoc name *aliases*)))
      (if (pair? c)
	  (cdr c)
	  name)))

;*---------------------------------------------------------------------*/
;*    assq-get ...                                                     */
;*---------------------------------------------------------------------*/
(define (assq-get key lst::pair-nil def)
   (let ((c (assoc key lst)))
      (if (pair? c)
	  (cadr c)
	  def)))

;*---------------------------------------------------------------------*/
;*    sort-benchmarks ...                                              */
;*---------------------------------------------------------------------*/
(define (sort-benchmarks benchmarks)
   (sort (lambda (x y) (string<=? (symbol->string! x) (symbol->string! y)))
      benchmarks))

;*---------------------------------------------------------------------*/
;*    stat-configuration ...                                           */
;*---------------------------------------------------------------------*/
(define (stat-configuration stat)
   (cdr (caddr stat)))

;*---------------------------------------------------------------------*/
;*    stat-times ...                                                   */
;*---------------------------------------------------------------------*/
(define (stat-times stat)
   (cdddr stat))

;*---------------------------------------------------------------------*/
;*    times-in-unit ...                                                */
;*---------------------------------------------------------------------*/
(define (times-in-unit times)
   (if (eq? *time-unit* 'sec)
       (map (lambda (v) (/ v 1000)) times)
       times))

;*---------------------------------------------------------------------*/
;*    mean ...                                                         */
;*---------------------------------------------------------------------*/
(define (mean times)
   (/ (apply + times) (length times)))

;*---------------------------------------------------------------------*/
;*    median ...                                                       */
;*---------------------------------------------------------------------*/
(define (median times)
   (let* ((vec (list->vector times))
	  (times (sort (lambda (a b) (<= a b)) vec))
	  (tm (vector-ref times (/fx (vector-length vec) 2))))
      (list tm (vector-ref times 0) (vector-ref times (-fx (vector-length vec) 1)))))

;*---------------------------------------------------------------------*/
;*    variance ...                                                     */
;*---------------------------------------------------------------------*/
(define (variance times)
  (let* ((m (mean times))
         (c (apply + (map (lambda (v) (* (- v m) (- v m))) times))))
    (/ c (length times))))

;*---------------------------------------------------------------------*/
;*    deviation ...                                                    */
;*---------------------------------------------------------------------*/
(define (deviation times)
   (sqrt (variance times)))

;*---------------------------------------------------------------------*/
;*    threshold ...                                                    */
;*---------------------------------------------------------------------*/
(define (threshold times)
   (map (lambda (t) (if (> t *min-threshold*) t 0)) times))

;*---------------------------------------------------------------------*/
;*    inverse-distribution ...                                         */
;*---------------------------------------------------------------------*/
(define (inverse-distribution times)
   (map (lambda (y) (/ 1 y)) times))

;*---------------------------------------------------------------------*/
;*    geomean ...                                                     */
;*---------------------------------------------------------------------*/
(define (geomean ratios)
   (exp (/ (apply + (map log ratios)) (length ratios))))

;*---------------------------------------------------------------------*/
;*    geostddev ...                                                   */
;*---------------------------------------------------------------------*/
(define (geostddev ratios)
   (let* ((lgm (log (geomean ratios)))
          (log-diffs
			(map
				(lambda (v) (let ((lv (log v))) (* (- lv lgm) (- lv lgm))))
				ratios))
          (variance (/ (apply + log-diffs) (length ratios))))
     (exp (sqrt variance))))