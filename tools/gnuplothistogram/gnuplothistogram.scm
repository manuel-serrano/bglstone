;*=====================================================================*/
;*    .../bglstone/tools/gnuplothistogram/gnuplothistogram.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Sep 11 08:01:47 2024                          */
;*    Last change :  Sun Nov 10 14:17:38 2024 (serrano)                */
;*    Copyright   :  2024 Manuel Serrano                               */
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
(define *errorbars* "errorbars lw 1")
(define *logscale* #f)
(define *xfontsize* "8")
(define *yfontsize* "10")
(define *format* "pdf")
(define *size* "")
   
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
       (set! *fout* fout))
      (("--unit" ?unit (help "Set time unit (default \"s\""))
       (set! *time-unit* (string->symbol unit)))
      (("--rename" ?name ?alias (help "System renaming"))
       (set! *aliases* (cons (cons name alias) *aliases*)))
      (("--template" ?file (help "Template file name"))
       (set! *template* (call-with-input-file file read-string)))
      ((("-r" "--relative") (help "Display relative values"))
       (set! *relative* #t))
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
      ((("-f" "--format") ?format (help "output format"))
       (set! *format* format))
      ((("-s" "--size") ?size (help "output size"))
       (set! *size* size))
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
					   (median (caddr val)))))))
			 stats)))
	 benchmarks))

   (define (relative-data stats benchmarks)
      (for-each (lambda (benchmark)
		   (let* ((val (assq benchmark (cdddr (car stats))))
			  (base (car (median (caddr val)))))
		      (display* benchmark ",  ")
		      (printf "~(,  )\n"
			 (map (lambda (stat)
				 (let ((val (assq benchmark (cdddr stat))))
				    (if (null? val)
					(error (cadr stat)
					   "Cannot find benchmark value"
					   benchmark)
					(format "~(,)"
					   (/ (car (median (caddr val))) base)))))
			    stats))))
	 benchmarks))
   
   (let ((benchmarks (sort-benchmarks
			(delete-duplicates
			   (map car
			      (apply append
				 (map cdddr stats)))))))
      ;; first line with system names
      (printf "#  ~( )\n" (map system-name stats))
      ;; following lines with benchmark values
      (if *relative*
	  (relative-data stats benchmarks)
	  (absolute-data stats benchmarks))))

;*---------------------------------------------------------------------*/
;*    output-plot ...                                                  */
;*---------------------------------------------------------------------*/
(define (output-plot stats)

   (define (absolute-plot stats)
      (let loop ((stats stats)
		 (i 0))
	 (printf "   '~a.csv' u ~(:):xtic(1) title '~a' ls ~d"
	    (basename *fout*) (iota 3 (+fx (*fx i 3) 2))
	    (system-name (car stats))
	    (+fx i 1))
	 (when (pair? (cdr stats))
	    (print ",\\")
	    (loop (cdr stats) (+fx i 1)))))

   (define (relative-plot stats)
      (let loop ((stats stats)
		 (i 0))
	 (printf "   '~a.csv' u ~a:xtic(1) title '~a' ls ~d"
	    (basename *fout*) (+fx i 2)
	    (system-name (car stats))
	    (+fx i 1))
	 (when (pair? (cdr stats))
	    (print ",\\")
	    (loop (cdr stats) (+fx i 1)))))
   
   (let ((times-length (length (caddr (car (stat-times (car stats))))))
	 (proc (assq-get 'processor (stat-configuration (car stats)) "")))
      (let ((s (pregexp-replace*
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
					  "@TITLE@"
					  (pregexp-replace* "@BASENAME@" *template* (basename *fout*))
					  (cond
					     (*user-title* *user-title*)
					     (*relative* *relative-title*)
					     (else *absolute-title*)))
				       (cond
					  (*user-ylabel* *user-ylabel*)
					  (*relative* *relative-ylabel*)
					  (else *absolute-ylabel*)))
				    proc)
				 (symbol->string! *time-unit*))
			      *yfontsize*)
			   *xfontsize*)
			(if *relative* "" *errorbars*))
		     *format*)
		  *size*)))
	 (print s)
	 (when *logscale*
	    (print "set logscale y\n"))
	 (print "plot \\")
	 (if *relative*
	     (relative-plot stats)
	     (absolute-plot stats)))))
	 
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
;*    deviation ...                                                    */
;*---------------------------------------------------------------------*/
(define (deviation times)
   (let* ((m (mean times))
	  (c (apply + (map (lambda (v) (* (- v m) (- v m))) times))))
      (sqrt (/ c (length times)))))

