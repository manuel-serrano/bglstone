;*=====================================================================*/
;*    serrano/prgm/project/bglstone/tools/r2tex/r2tex.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Jan  3 09:35:01 2001                          */
;*    Last change :  Wed Jan  8 16:55:50 2003 (serrano)                */
;*    Copyright   :  2001-03 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The converter from RUNIT output files to BARCHART input files.   */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module runit->latex
   (extern (macro sprintf%g::int (::string ::string ::double) "sprintf"))
   (main main))

;*---------------------------------------------------------------------*/
;*    Global control                                                   */
;*---------------------------------------------------------------------*/
(define *inputs* '())
(define *fout* #unspecified)
(define *absolute* #t)
(define *relative* #f)
(define *title* "")
(define *subtitle* "")
(define *cw* "1")
(define *column-header-prefix* "\\footnotesize\\textsf")
(define *entry-prefix* "\\textbf")
(define *min-prefix* "\\textbf")
(define *max-prefix* "\\textit")
(define *val-suffix* #f)
(define *accept-missing?* #t)
(define *bar-name* (lambda (t c) t))
(define *configs* '())
   
;*---------------------------------------------------------------------*/
;*    parse-args ...                                                   */
;*---------------------------------------------------------------------*/
(define (parse-args argv)
   (args-parse (cdr argv)
      (("-help" (help "This message"))
       (print "usage: runit [options] [filename]")
       (newline) 
       (args-parse-usage #f)
       (exit 0))
      (("-o" ?fout (help "Output file"))
       (set! *fout* fout))
      (("-r" (help "Relative values only (X/first file)"))
       (set! *absolute* #f)
       (set! *relative* 'ratio))
      (("-R" (help "Relative values only (first file/X)"))
       (set! *absolute* #f)
       (set! *relative* 'oitar))
      (("-vr" (help "Relative and absolute values (X/first file)"))
       (set! *absolute* #t)
       (set! *relative* 'ratio))
      (("-vR" (help "Relative and absolute values (first file/X)"))
       (set! *absolute* #t)
       (set! *relative* 'oitar))
      (("-p" (help "Percentage values only (with respect to the first file)"))
       (set! *absolute* #f)
       (set! *relative* 'percent))
      (("-vp" (help "Percentage and absolute values (with respect to the first file)"))
       (set! *absolute* #t)
       (set! *relative* 'percent))
      (("-t" ?title (help "Set the title"))
       (set! *title* title))
      (("-st" ?title (help "Set the left column title"))
       (set! *subtitle* title))
      (("-cw" ?size (help "Column width"))
       (set! *cw* (string->number size)))
      (("-cprefix" ?prefix (help "Column header prefix"))
       (set! *column-header-prefix* prefix))
      (("-eprefix" ?prefix (help "Row header prefix"))
       (set! *entry-prefix* prefix))
      (("-minprefix" ?prefix (help "Min row value prefix"))
       (set! *min-prefix* prefix))
      (("-maxprefix" ?prefix (help "Max row value prefix"))
       (set! *max-prefix* prefix))
      (("-valsuffix" ?suffix (help "Row value suffix"))
       (set! *val-suffix* suffix))
      (("-mi" (help "Accept missing statistics"))
       (set! *accept-missing?* #t))
      (("-nmi" (help "Reject missing statistics"))
       (set! *accept-missing?* #f))
      (("-bname" ?str (help "Function that compute the bar-names"))
       (set! *bar-name* (with-input-from-string str
			   (lambda ()
			      (eval (read))))))
      (else
       (set! *inputs* (cons else *inputs*)))))

;*---------------------------------------------------------------------*/
;*    main ...                                                         */
;*---------------------------------------------------------------------*/
(define (main argv)
   (define (doit)
      (cond
	 ((and *relative* *absolute*)
	  (runit->latex-absolute+relative))
	 (*relative*
          (runit->latex-relative))
	 (else
          (runit->latex))))
   (parse-args argv)
   (set! *inputs* (reverse! *inputs*))
   (if (string? *fout*)
       (with-output-to-file *fout* doit)
       (doit)))

;*---------------------------------------------------------------------*/
;*    read-runit-file ...                                              */
;*---------------------------------------------------------------------*/
(define (read-runit-file file)
   (with-input-from-file file
      (lambda ()
         (let ((descr (read)))
            (match-case descr
               ((title ?title (configuration . ?conf) . ?rest)
		(set! *configs* (cons conf *configs*))
                (cons title rest))
               ((title ?title . ?rest)
		(set! *configs* (cons #f *configs*))
                (cons title rest))
               (else
                (error "r2tex" "Illegal runit file" file)))))))

;*---------------------------------------------------------------------*/
;*    make-tabular-header ...                                          */
;*---------------------------------------------------------------------*/
(define (make-tabular-header len)
   (define (make-column-descr len)
      (let loop ((i 0)
		 (res ""))
	 (if (=fx i len)
	     res
	     (loop (+fx i 1) (string-append "r|" res)))))
   (let ((slen (integer->string len))
	 (slen+1 (integer->string (+fx 1 len))))
      (string-append "\\begin{tabular}{|l|"
		     (make-column-descr len)
		     #"}\n\\cline{2-" slen+1 #"}\n"
		     "\\multicolumn{1}{l|}{} & \\multicolumn{" slen 
		     "}{c|}{\\em " *title* #"} \\\\\\hline\n"
		     "{\\em{" *subtitle* "}} & ")))

;*---------------------------------------------------------------------*/
;*    for-each-but-last ...                                            */
;*---------------------------------------------------------------------*/
(define (for-each-but-last fun lst)
   (let loop ((lst lst))
      (if (not (or (null? lst) (null? (cdr lst))))
	  (begin
	     (fun (car lst))
	     (loop (cdr lst))))))

;*---------------------------------------------------------------------*/
;*    for-each-but-last2 ...                                           */
;*---------------------------------------------------------------------*/
(define (for-each-but-last2 fun lst1 lst2)
   (let loop ((lst1 lst1)
	      (lst2 lst2))
      (if (not (or (null? lst1) (null? (cdr lst1))))
	  (begin
	     (fun (car lst1) (car lst2))
	     (loop (cdr lst1) (cdr lst2))))))

;*---------------------------------------------------------------------*/
;*    print-column-header ...                                          */
;*---------------------------------------------------------------------*/
(define (print-column-header descrs)
   (define (make-col-header descr config stop)
      (string-append "\\multicolumn{1}{c|}{\\makebox[" *cw* "cm]{"
		     *column-header-prefix*
		     "{"
		     (*bar-name* (car descr) config)
		     "}}}"
		     stop))
   (if (null? descrs)
       (error "r2tex" "Can't produce relative statistics" "Only one value provided"))
   (for-each-but-last2 (lambda (d k)
			  (display (make-col-header d k " & ")))
		       descrs
		       (reverse *configs*))
   (print (make-col-header (car (last-pair descrs)) (car *configs*) " \\\\")))

;*---------------------------------------------------------------------*/
;*    find-id-value ...                                                */
;*---------------------------------------------------------------------*/
(define (find-id-value id descr)
   (let ((cell (assoc id (cdr descr))))
      (if (pair? cell)
	  (caddr cell)
	  (if *accept-missing?*
	      "\\_"
	      (error "r2tex"
		     (string-append "Can't find entry `"
				    (symbol->string id)
				    "' for description")
		     (car descr))))))

;*---------------------------------------------------------------------*/
;*    format-number ...                                                */
;*---------------------------------------------------------------------*/
(define (format-number number)
   (if (fixnum? number)
       number
       (let ((res (make-string 20)))
	  (cond
	     ((> number 100.)
	      (sprintf%g res #"%#06.2f" number)
	      (substring res 0 6))
	     ((> number 10.)
	      (sprintf%g res #"%#05.2f" number)
	      (substring res 0 5))
	     (else
	      (sprintf%g res #"%#04.3f" number)
	      (substring res 0 4))))))

;*---------------------------------------------------------------------*/
;*    print-descr-value ...                                            */
;*---------------------------------------------------------------------*/
(define (print-descr-value val minv maxv stop val-suffix)
   (cond
      ((not (number? val))
       (display val))
      ((= val minv)
       (display* "{" *min-prefix* "{" (format-number val) "}} ")
       (if (string? val-suffix)
	   (display val-suffix)))
      ((= val maxv)
       (display* "{" *max-prefix* "{" (format-number val) "}} ")
       (if (string? val-suffix)
	   (display val-suffix)))
      (else
       (display* (format-number val) " ")
       (if (string? val-suffix)
	   (display val-suffix))))
   (display stop))

;*---------------------------------------------------------------------*/
;*    safe-min ...                                                     */
;*---------------------------------------------------------------------*/
(define (safe-min . args)
   (let loop ((args args)
	      (min #f))
      (cond
	 ((null? args)
	  min)
	 ((not (number? (car args)))
	  (loop (cdr args) min))
	 ((or (not (number? min)) (< (car args) min))
	  (loop (cdr args) (car args)))
	 (else
	  (loop (cdr args) min)))))
	  
;*---------------------------------------------------------------------*/
;*    safe-max ...                                                     */
;*---------------------------------------------------------------------*/
(define (safe-max . args)
   (let loop ((args args)
	      (max #f))
      (cond
	 ((null? args)
	  max)
	 ((not (number? (car args)))
	  (loop (cdr args) max))
	 ((or (not (number? max)) (> (car args) max))
	  (loop (cdr args) (car args)))
	 (else
	  (loop (cdr args) max)))))
	  
;*---------------------------------------------------------------------*/
;*    print-descr-line ...                                             */
;*---------------------------------------------------------------------*/
(define (print-descr-line descrs)
   (map (lambda (val)
	   (let* ((id (car val))
		  (sid (string-capitalize (symbol->string id)))
		  (vals (map (lambda (d) (find-id-value id d)) descrs))
		  (minv (apply safe-min vals))
		  (maxv (apply safe-max vals)))
	      (display* "{" *entry-prefix* "{" sid "}} & ")
	      (for-each-but-last (lambda (val)
				    (print-descr-value val minv maxv
						       " & " *val-suffix*))
				 vals)
	      (print-descr-value (car (last-pair vals)) minv maxv
				 #" \\\\\n" *val-suffix*)))
	(cdr (car descrs))))
   
;*---------------------------------------------------------------------*/
;*    runit->latex ...                                                 */
;*---------------------------------------------------------------------*/
(define (runit->latex)
   (let* ((descrs (map read-runit-file (filter file-exists? *inputs*)))
	  (tabular (make-tabular-header (length descrs))))
      (print tabular)
      (print-column-header descrs)
      (print #"\\hline")
      (print-descr-line descrs)
      (print #"\\hline\n\\end{tabular}\n")))

;*---------------------------------------------------------------------*/
;*    print-relative-descr-line ...                                    */
;*---------------------------------------------------------------------*/
(define (print-relative-descr-line base descrs)
   (map (lambda (val)
	   (let* ((id (car val))
		  (bval (caddr val))
		  (sid (string-capitalize (symbol->string id)))
		  (vals (map (lambda (d)
				(let ((fid (find-id-value id d)))
				   (if (not (number? fid))
				       fid
				       (let* ((ratio (if (eq? *relative* 'oitar)
							 (/ bval fid)
							 (/ fid bval)))
					      (val (if (memq *relative*
							     '(ratio oitar))
						       ratio
						       (inexact->exact
							(* ratio 100)))))
					  val))))
			     descrs))
		  (minv (apply safe-min vals))
		  (maxv (apply safe-max vals)))
	      (display* "{" *entry-prefix* "{" sid "}} & ")
	      (for-each-but-last (lambda (val)
				    (print-descr-value val minv maxv
						       " & " *val-suffix*))
				 vals)
	      (print-descr-value (car (last-pair vals)) minv maxv
				 #" \\\\\n" *val-suffix*)))
	(cdr base)))
   
;*---------------------------------------------------------------------*/
;*    runit->latex-relative ...                                        */
;*---------------------------------------------------------------------*/
(define (runit->latex-relative)
   (let* ((descrs (map read-runit-file (filter file-exists? *inputs*)))
	  (base (car descrs))
	  (descrs (cdr descrs))
	  (tabular (make-tabular-header (length descrs))))
      (print tabular)
      (print-column-header descrs)
      (print #"\\hline")
      (print-relative-descr-line base descrs)
      (print #"\\hline\n\\end{tabular}\n")))

;*---------------------------------------------------------------------*/
;*    print-absolute+relative-descr-line ...                           */
;*---------------------------------------------------------------------*/
(define (print-absolute+relative-descr-line base descrs)
   (map (lambda (val)
	   (let* ((id (car val))
		  (bval (caddr val))
		  (sid (string-capitalize (symbol->string id)))
		  (valas (map (lambda (d) (find-id-value id d)) descrs))
		  (valrs (map (lambda (d)
				 (let ((fid (find-id-value id d)))
				    (if (not (number? fid))
					fid
					(let* ((ratio (if (eq? *relative*
							       'oitar)
							  (/ bval fid)
							  (/ fid bval)))
					       (val (if (memq *relative*
							      '(ratio oitar))
							ratio
							(inexact->exact
							 (* ratio 100)))))
					   val))))
			      descrs))
		  (minv (apply safe-min valas))
		  (maxv (apply safe-max valas))
		  (stop (if (string? *val-suffix*)
			    (string-append *val-suffix* ") & ")
			    ") & "))
		  (stop2 (if (string? *val-suffix*)
			     (string-append *val-suffix* #") \\\\\n")
			     #") \\\\\n")))
	      (display* "{" *entry-prefix* "{" sid "}} & ")
	      (for-each-but-last2 (lambda (vala valr)
				     (if (not (number? vala))
					 (print-descr-value vala minv maxv
							    " & " "")
					 (begin
					    (print-descr-value vala minv maxv
							       " (" "")
					    (print-descr-value valr minv maxv
							       stop ""))))
				  valas
				  valrs)
	      (if (not (number? (car (last-pair valas))))
		  (print-descr-value (car (last-pair valas)) minv maxv
				     #"\\\\\n" "")
		  (begin
		     (print-descr-value (car (last-pair valas)) minv maxv
					" (" "")
		     (print-descr-value (car (last-pair valrs)) minv maxv
					stop2 "")))))
	(cdr base)))
   
;*---------------------------------------------------------------------*/
;*    runit->latex-absolute+relative ...                               */
;*---------------------------------------------------------------------*/
(define (runit->latex-absolute+relative)
   (let* ((descrs (map read-runit-file (filter file-exists? *inputs*)))
	  (base (car descrs))
	  (tabular (make-tabular-header (length descrs))))
      (print tabular)
      (print-column-header descrs)
      (print #"\\hline")
      (print-absolute+relative-descr-line base descrs)
      (print #"\\hline\n\\end{tabular}\n")))
