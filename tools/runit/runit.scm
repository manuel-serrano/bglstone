;*=====================================================================*/
;*    serrano/prgm/project/bglstone/tools/runit/runit.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Jan  2 14:40:05 2001                          */
;*    Last change :  Sat Sep  7 14:08:27 2024 (serrano)                */
;*    Copyright   :  2001-24 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    RUNIT: run (several times if needed) user command and measure    */
;*    how long they last.                                              */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module runit
   (extern (%timeit-cmd::double (::string ::int ::int) "timeit_cmd")
	   (%clockit-cmd::double (::string ::int ::int) "clockit_cmd")
	   (%timeit-thunk::double (::obj ::int ::int) "timeit_thunk")
	   (%clockit-thunk::double (::obj ::int ::int) "clockit_thunk"))
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

(define *timer* 'wall-clock)

;*---------------------------------------------------------------------*/
;*    read-proc-mhz ...                                                */
;*---------------------------------------------------------------------*/
(define (read-proc-mhz)
   (if (file-exists? "/proc/cpuinfo")
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
;*    setup-platform-configuration! ...                                */
;*---------------------------------------------------------------------*/
(define (setup-platform-configuration!)
   (set! *date* (date))
   (if (not (string? *hostname*))
       (set! *hostname* (getenv "HOSTNAME")))
   (if (not (string? *uptime*))
       (with-input-from-file "| uptime"
	  (lambda ()
	     (set! *uptime* (read-line)))))
   (if (not (string? *machine*))
       (with-input-from-file "| uname -m"
	  (lambda ()
	     (set! *machine* (read-of-strings)))))
   (if (not (string? *proc*))
       (with-input-from-file "| uname -p"
	  (lambda ()
	     (set! *proc* (read-of-strings)))))
   (if (not (string? *mhz*))
       (set! *mhz* (read-proc-mhz)))
   (if (not (string? *mem*))
       (if (file-exists? "/proc/meminfo")
	   (with-input-from-file "/proc/meminfo"
	      (lambda ()
		 (read-line)
		 (read)
		 (set! *mem* (integer->string
			      (inexact->exact (/ (string->real (read-of-strings)) (* 1024 1024)))))))
	   (set! *mem* "---")))
   (if (not (string? *os*))
       (set! *os* (string-append
		   (with-input-from-file "| uname -s"
		      (lambda ()
			 (read-of-strings)))
		   " "
		   (with-input-from-file "| uname -r"
		      (lambda ()
			 (read-of-strings)))))))

;*---------------------------------------------------------------------*/
;*    timeit ...                                                       */
;*---------------------------------------------------------------------*/
(define (timeit what repetition)
   (if (> *verbose* 0)
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
   (let ((res (cond
		 ((string? what)
		  (if (eq? *timer* 'user+sys)
		      (%timeit-cmd (string-append what ">/dev/null 2>&1")
				   repetition
				   *verbose*)
		      (%clockit-cmd (string-append what ">/dev/null 2>&1")
				    repetition
				    *verbose*)))
		 ((and (procedure? what) (correct-arity? what 0))
		  (if (eq? *timer* 'user+sys)
		      (%timeit-thunk what repetition *verbose*)
		      (%clockit-thunk what repetition *verbose*)))
		 (else
		  (error "runit" "Illegal command or thunk" what)))))
      (if (< res 0.0)
	  (begin
	     (error 'runit "Execution failed" what)
	     (exit -1)))
      (if (> *verbose* 1)
	  (display "     *** min run: " (current-error-port)))
      (if (> *verbose* 0)
	  (fprint (current-error-port) " " res "s "
		  (if (eq? *timer* 'user+sys) "(cpu+sys)" "(wall-clock)")))
      res))

;*---------------------------------------------------------------------*/
;*    main ...                                                         */
;*---------------------------------------------------------------------*/
(define (main argv)
   (define (parse-and-run)
      (define (loop descrs)
	 (if (pair? descrs)
	     (let ((descr (car descrs)))
		(match-case descr
		   ((configuration)
		    (setup-platform-configuration!)
		    (pp `(configuration
			  (date ,*date*)
			  (host ,*hostname*)
			  (uptime ,*uptime*)
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
	 	   ((?lbl ?command)
		    (let ((time (timeit command *repetition*)))
		       (write (list lbl command time))
		       (newline)
		       (loop (cdr descrs))))
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
