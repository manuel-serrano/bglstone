;*=====================================================================*/
;*    serrano/prgm/project/bglstone/tools/runit/runit.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Jan  2 14:40:05 2001                          */
;*    Last change :  Fri Oct  4 16:00:20 2024 (serrano)                */
;*    Copyright   :  2001-24 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    RUNIT: run (several times if needed) user command and measure    */
;*    how long they last.                                              */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module runit
   (main main))

;*---------------------------------------------------------------------*/
;*    Global control                                                   */
;*---------------------------------------------------------------------*/
(define *repetition* 5)
(define *fin* #f)
(define *fout* #f)
(define *verbose* 0)

(define *date* #f)
(define *hostname* #f)
(define *uptime* #f)
(define *machine* #f)
(define *proc* #f)
(define *mhz* #f)
(define *mem* #f)
(define *os* #f)
(define *comments* '())
(define *compiler* #f)

(define *noerror* #t)

(define *timer* 'wall-clock)

;*---------------------------------------------------------------------*/
;*    read-proc-mhz ...                                                */
;*---------------------------------------------------------------------*/
(define (read-proc-mhz)
   (when (file-exists? "/proc/cpuinfo")
      (with-input-from-file "/proc/cpuinfo"
	 (lambda ()
	    (let loop ((l (read-line)))
	       (if (eof-object? l)
		   "---"
		   (let ((m (pregexp-match "cpu MHz[ \\t]+: ([0-9]+)" l)))
		      (if (pair? m) 
			  (cadr m)
			  (loop (read-line))))))))))

;*---------------------------------------------------------------------*/
;*    read-proc-model ...                                              */
;*---------------------------------------------------------------------*/
(define (read-proc-model)
   (when (file-exists? "/proc/cpuinfo")
      (with-input-from-file "/proc/cpuinfo"
	 (lambda ()
	    (let loop ((l (read-line)))
	       (if (eof-object? l)
		   "---"
		   (let ((m (pregexp-match "model name[ \\t]+: (.+)" l)))
		      (if (pair? m) 
			  (cadr m)
			  (loop (read-line))))))))))

;*---------------------------------------------------------------------*/
;*    setup-platform-configuration! ...                                */
;*---------------------------------------------------------------------*/
(define (setup-platform-configuration!)
   (set! *date* (date))
   (unless (string? *hostname*)
      (set! *hostname* (or (getenv "HOSTNAME") (getenv "HOST"))))
   (unless (string? *uptime*)
      (with-input-from-file "| uptime"
	 (lambda ()
	    (set! *uptime* (read-line)))))
   (unless (string? *machine*)
      (with-input-from-file "| uname -m"
	 (lambda ()
	    (set! *machine* (read-of-strings)))))
   (unless (string? *proc*)
      (with-input-from-file "| uname -p"
	 (lambda ()
	    (set! *proc* (read-of-strings))
	    (when (string=? *proc* "unknown")
	       (set! *proc* (read-proc-model))))))
   (unless (string? *mhz*)
      (set! *mhz* (read-proc-mhz)))
   (unless (string? *mem*)
      (when (file-exists? "/proc/meminfo")
	 (with-input-from-file "/proc/meminfo"
	    (lambda ()
	       (read-line)
	       (read)
	       (set! *mem*
		  (integer->string
		     (inexact->exact
			(/ (string->real (read-of-strings)) (* 1024 1024)))))))
	 (set! *mem* "---")))
   (unless (string? *os*)
      (set! *os* (string-append
		    (with-input-from-file "| uname -s"
		       (lambda ()
			  (read-of-strings)))
		    " "
		    (with-input-from-file "| uname -r"
		       (lambda ()
			  (read-of-strings)))))))

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
	  (times (sort (lambda (a b) (>= a b)) vec))
	  (tm (vector-ref times (/fx (vector-length vec) 2))))
      (list tm (vector-ref vec 0) (vector-ref vec (-fx (vector-length vec) 1)))))

;*---------------------------------------------------------------------*/
;*    deviation ...                                                    */
;*---------------------------------------------------------------------*/
(define (deviation times)
   (let* ((m (mean times))
	  (c (apply + (map (lambda (v) (* (- v m) (- v m))) times))))
      (sqrt (/ c (length times)))))
      
;*---------------------------------------------------------------------*/
;*    repeat ...                                                       */
;*---------------------------------------------------------------------*/
(define (repeat repetition proc)
   (let loop ((repetition repetition)
	      (times '()))
      (if (>fx repetition 0)
	  (multiple-value-bind (value real sys user)
	     (time (if *noerror*
		       (lambda ()
			  (with-handler
			     (lambda (e)
				0)
			     (proc)))
		       proc))
	     (when (>=fx *verbose* 1)
		(display "." (current-error-port))
		(display repetition (current-error-port))
		(flush-output-port (current-error-port)))
	     (loop (-fx repetition 1)
		(if (eq? *timer* 'user+sys)
		    (cons (+ sys user) times)
		    (cons real times))))
	  times)))

;*---------------------------------------------------------------------*/
;*    prec2 ...                                                        */
;*---------------------------------------------------------------------*/
(define (prec2 n)
   (let* ((s (number->string n))
	  (i (string-index s #\.)))
      (if i
	  (if (<fx i (-fx (string-length s) 2))
	      (substring s 0 (+fx i 3))
	      s)
	  (string-append s ".00"))))
      
;*---------------------------------------------------------------------*/
;*    timeit ...                                                       */
;*---------------------------------------------------------------------*/
(define (timeit what repetition)
   (when (> *verbose* 0)
      (with-output-to-port (current-error-port)
	 (lambda ()
	    (display* "   Executing [" what "]...")
	    (if (> *verbose* 1)
		(display* #\Newline
		   "   repetition   : " repetition
		   #\Newline
		   "   starting date: " (date)
		   #\Newline)))))
   (flush-output-port (current-error-port))
   (let ((times (cond
		   ((string? what)
		    (repeat repetition
		       (lambda ()
			  (unless (=fx (system (string-append what ">/dev/null 2>&1")) 0)
			     (error "timeit" "command failed" what)))))
		   ((and (procedure? what) (correct-arity? what 0))
		    (repeat repetition what))
		   (else
		    (error "runit" "Illegal command or thunk" what)))))
      (when (null? times)
	 (error "runit" "Execution failed" what)
	 (exit -1))
      (let ((tm (mean times))
	    (dev (deviation times)))
	 (when (>fx *verbose* 0)
	    (fprint (current-error-port) " "
	       (prec2 (/ tm 1000)) "s [" (flonum->fixnum (/ (* 100 dev) tm)) "%] "
	       (if (eq? *timer* 'user+sys) "(cpu+sys)" "(wall-clock)")))
	 times)))

;*---------------------------------------------------------------------*/
;*    main ...                                                         */
;*---------------------------------------------------------------------*/
(define (main argv)
   
   (define (parse-and-run)
      
      (define (loop descrs)
	 (when (pair? descrs)
	    (let ((descr (car descrs)))
	       (match-case descr
		  ((configuration)
		   (setup-platform-configuration!)
		   (pp `(configuration
			   (date ,*date*)
			   (host ,*hostname*)
			   (uptime ,*uptime*)
			   (time ',*timer*)
			   (arch ,*machine*)
			   (processor ,*proc*)
			   (mhz ,*mhz*)
			   (memory ,*mem*)
			   (comment ,@*comments*)
			   ,@(if *compiler*
				 `((compiler ,*compiler*))
				 '())
			   (os ,*os*)
			   (iterations ,*repetition*)))
		   (loop (cdr descrs)))
		  ((comment ?cmt)
		   (set! *comments* (cons cmt *comments*))
		   (loop (cdr descrs)))
		  ((compiler ?cmp)
		   (set! *compiler* cmp)
		   (loop (cdr descrs)))
		  ((benchmarks . ?benchmarks)
		   (for-each (match-lambda 
				((?lbl ?command)
				 (let ((times (timeit command *repetition*)))
				    (display " ")
				    (write (list lbl command times))
				    (newline)
				    (loop (cdr descrs))))
				(?bench
				 (error "runit" "Illegal benchmark description" bench)))
		      benchmarks))
		  (else
		   (error "runit" "Illegal command description" descr))))))
      
      (let ((descr (read)))
	 (match-case descr
	    ((title ?string . ?rest)
	     (print "(title \"" string "\"")
	     (loop rest)
	     (print ")"))
	    (else
	     (error "runit" "Illegal description" descr)))))
   
   (args-parse (cdr argv)
      ((("-h" "--help") (help "This message"))
       (print "usage: runit [options] [filename]")
       (newline)
       (args-parse-usage #f)
       (exit 0))
      (("-o" ?fout (help "Output file"))
       (set! *fout* fout))
      (("-v" (help "Verbose mode"))
       (set! *verbose* (+fx 1 *verbose*)))
      (("-v2" (help "Very Verbose mode"))
       (set! *verbose* (+fx 2 *verbose*)))
      (("-r" ?num (help "Set the time each program must be ran"))
       (set! *repetition* (string->integer num)))
      (("--mhz" ?value (help "Processor frequency"))
       (set! *mhz* value))
      (("--mem" ?value (help "Memory size"))
       (set! *mem* value))
      (("--host" ?name (help "Host name"))
       (set! *hostname* name))
      (("--uptime" ?value (help "Uptime"))
       (set! *uptime* value))
      (("--os" ?name (help "Os name"))
       (set! *os* name))
      (("--comment" ?cmt (help "Configuration comment"))
       (set! *comments* (cons cmt *comments*)))
      (("--compiler" ?cmp (help "Configuration compiler"))
       (set! *compiler* cmp))
      (("--wall-clock" (help "Use wall clock instead of user+cpu time"))
       (set! *timer* 'wall-clock))
      (("--user+sys" (help "Use user+cpu time instead of wall clock"))
       (set! *timer* 'user+sys))
      (else
       (set! *fin* else)))
   
   (define (doit)
      (if (string? *fin*)
	  (with-input-from-file *fin* parse-and-run)
	  (parse-and-run)))
   
   (if (string? *fout*)
       (with-output-to-file *fout* doit)
       (doit)))
