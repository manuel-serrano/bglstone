;*=====================================================================*/
;*    serrano/prgm/project/bglstone/tools/r2b/r2b.scm                  */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Jan  3 09:35:01 2001                          */
;*    Last change :  Wed Sep 11 08:05:03 2024 (serrano)                */
;*    Copyright   :  2001-24 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The converter from RUNIT output files to BARCHART input files.   */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module runit->barchart
   (main main))

;*---------------------------------------------------------------------*/
;*    Global control                                                   */
;*---------------------------------------------------------------------*/
(define *inputs* '())
(define *fout* #unspecified)
(define *relative* #f)
(define *title* "")
(define *limit* #f)
(define *accept-missing?* #t)
(define *accept-missing-values* '())
(define *bar-name* (lambda (t c)
		      (if (pair? *bar-names*)
			  (let ((name (car *bar-names*)))
			     (set! *bar-names* (cdr *bar-names*))
			     name)
			  t)))
(define *bar-names* '())
(define *configs* '())
(define *only* #f)
(define *excludes* '())

;*---------------------------------------------------------------------*/
;*    parse-args ...                                                   */
;*---------------------------------------------------------------------*/
(define (parse-args argv)
   (args-parse (cdr argv)
      (("-help" (help "This message"))
       (print "usage: r2b [options] [filename]")
       (newline)
       (args-parse-usage #f)
       (exit 0))
      (("-o" ?fout (help "Output file"))
       (set! *fout* fout))
      (("-r" (help "Relative bars (X/first file)"))
       (set! *relative* 'ratio))
      (("-R" (help "Relative bars (first file/X)"))
       (set! *relative* 'oitar))
      (("-limit" ?val (help "Maximum value to be scaled"))
       (set! *limit* (string->number val)))
      (("-p" (help "Percentage bars (with respect to the first file)"))
       (set! *relative* 'percentage))
      (("-t" ?title (help "Title"))
       (set! *title* title))
      (("-mi" (help "Accept missing statistics"))
       (set! *accept-missing?* #t))
      (("-nmi" (help "Reject missing statistics"))
       (set! *accept-missing?* #f))
      (("-only" ?id (help "Only include statistics about id (can't be repeated)"))
       (set! *accept-missing?* #t)
       (if (not (pair? *only*))
	   (set! *only* (list (string->symbol id)))
	   (set! *only* (cons (string->symbol id) *only*))))
      (("-omit" ?num (help "Set a omitted missing values (starting 0), requires -mi"))
       (set! *accept-missing-values* (cons (string->number num)
					   *accept-missing-values*)))
      (("-exclude" ?exc (help "Exclude values"))
       (set! *excludes* (cons (string->symbol exc) *excludes*)))
      (("-bname" ?str (help "Function that compute the bar-names"))
       (set! *bar-name* (with-input-from-string str
			   (lambda ()
			      (eval (read))))))
      (("-name" ?str (help "Function that compute the bar-names"))
       (set! *bar-names* (cons str *bar-names*)))
      (else
       (set! *inputs* (cons else *inputs*)))))

;*---------------------------------------------------------------------*/
;*    main ...                                                         */
;*---------------------------------------------------------------------*/
(define (main argv)
   (define (doit)
      (if *relative*
 	  (runit->barchart-relative)
	  (runit->barchart)))
   (parse-args argv)
   (set! *bar-names* (reverse! *bar-names*))
   (set! *accept-missing-values* (sort *accept-missing-values* <))
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
		(error "r2b" "Illegal runit file" file)))))))

;*---------------------------------------------------------------------*/
;*    find-max-value ...                                               */
;*---------------------------------------------------------------------*/
(define (find-max-value descrs)
   (define (find-max-descr-value descr maxv)
      (if (not (pair? descr))
	  maxv
	  (let loop ((maxv maxv)
		     (vals (cdr descr)))
	     (cond
		((null? vals)
		 maxv)
		((not (number? (cadr (car vals))))
		 (loop maxv (cdr vals)))
		((> (cadr (car vals)) maxv)
		 (loop (cadr (car vals)) (cdr vals)))
		(else
		 (loop maxv (cdr vals)))))))
   (let loop ((maxv 0)
	      (descrs descrs))
      (if (null? descrs)
	  maxv
	  (loop (find-max-descr-value (car descrs) maxv) (cdr descrs)))))

;*---------------------------------------------------------------------*/
;*    make-entries ...                                                 */
;*---------------------------------------------------------------------*/
(define (make-entries keys descrs)
   (define (find-id-value id descr)
      (let ((cell (assoc id (cdr descr))))
	 (if (pair? cell)
	     (if (pair? *only*)
		 (if (memq id *only*)
		     (caddr cell)
		     0)
		 (caddr cell))
	     (if *accept-missing?*
		 0
		 (error "r2b"
			(string-append "Can't find entry `"
				       (symbol->string id)
				       "' for description")
			(car descr))))))
   (map (lambda (vals)
	   (let* ((id (car vals))
		  (sid (string-capitalize (symbol->string id))))
	      `(,sid ,@(map (lambda (k d) (list k (find-id-value id d)))
			    keys
			    descrs))))
	(cdr (car descrs))))

;*---------------------------------------------------------------------*/
;*    get-scale ...                                                    */
;*---------------------------------------------------------------------*/
(define (get-scale max-value)
   (let ((ev (inexact->exact (/ max-value 10.))))
      (if (= ev 0)
	  1
	  ev)))

;*---------------------------------------------------------------------*/
;*    runit->barchart ...                                              */
;*---------------------------------------------------------------------*/
(define (runit->barchart)
   (let* ((descrs (map read-runit-file (filter file-exists? *inputs*)))
	  (keys (map (lambda (x) (gensym 'K)) descrs))
	  (entries (make-entries keys descrs))
	  (max-descr-value (find-max-value entries))
	  (max-value (+ max-descr-value (* max-descr-value 0.05)))
	  (scale (get-scale max-value))
	  (chart `(barchart ,*title*
			    ,@(cond
				 (*limit*
				  (list `(max-scale-value ,*limit*)))
				 ((eq? *relative* 'percentage)
				  (list `(max-scale-value 100)))
				 (else
				  '()))
			    (bar-names ,@(map (lambda (d k c)
						 (list k
						       (*bar-name* (car d) c)))
					      descrs keys (reverse *configs*)))
			    (bar-values ,@entries))))
      (if (eq? *relative* 'percentage)
	  (begin
	     (print '(set! *scale-step* 10))
	     (print '(set! *scale-init* 0))))
      (pp chart)
      (newline)))

;*---------------------------------------------------------------------*/
;*    make-ref-entries ...                                             */
;*---------------------------------------------------------------------*/
(define (make-ref-entries ref keys descrs)
   (define (find-id-value id descr)
      (let ((cell (assoc id (cdr descr))))
	 (if (pair? cell)
	     (if (pair? *only*)
		 (if (memq id *only*)
		     (caddr cell)
		     #f)
		 (caddr cell))
	     #f)))
   (define (make-ref-entry id bval k d)
      (let ((fid (find-id-value id d)))
	 (if (not fid)
	     (if *accept-missing?*
		 (list k #unspecified)
		 (error "r2b"
			(string-append "Can't find entry `"
				       (symbol->string id)
				       "' for description")
			(car d)))
	     (let* ((ratio (case *relative*
			      ((oitar)
			       (/ bval fid))
			      (else
			       (/ fid bval))))
		    (val (if (memq *relative* '(ratio oitar))
			     ratio
			     (inexact->exact (* ratio 100)))))
		(list k val)))))
   (filter (lambda (x) (not (eq? x #unspecified)))
	   (map (lambda (val)
		   (let* ((id (car val))
			  (bval (caddr val))
			  (sid (string-capitalize (symbol->string id)))
			  (entries (map (lambda (k d)
					   (make-ref-entry id bval k d))
					keys descrs)))
		      (if (memq id *excludes*)
			  #unspecified
			  ;; we check the entries if *accept-missing?* is #t
			  (if *accept-missing?*
			      (if (or (every? (lambda (x)
						 (eq? (cadr x) #unspecified))
					      entries)
				      (all-missing-values? entries))
				  #unspecified
				  `(,sid ,@(map (lambda (x)
						   (if (eq? (cdr x) #unspecified)
						       (list (car x) 0)
						       x))
						entries)))
			      `(,sid ,@entries)))))
		(cdr ref))))
   
;*---------------------------------------------------------------------*/
;*    runit->barchart-relative ...                                     */
;*---------------------------------------------------------------------*/
(define (runit->barchart-relative)
   (if (file-exists? (car *inputs*))
       (let* ((descrs (map read-runit-file (filter file-exists? (cdr *inputs*))))
	      (ref (read-runit-file (car *inputs*)))
	      (keys (map (lambda (x) (gensym 'K)) descrs))
	      (entries (make-ref-entries ref keys descrs))
	      (max-descr-value (find-max-value entries))
	      (max-value (cond
			    ((and (eq? *relative* 'ratio)
				  (< max-descr-value 1.))
			     1.)
			    ((and (eq? *relative* 'percentage)
				  (< max-descr-value 100))
			     100)
			    ((> max-descr-value 1.)
			     (inexact->exact
			      (ceiling
			       (+ max-descr-value (* max-descr-value 0.05)))))
			    (else
			     (+ max-descr-value (* max-descr-value 0.05)))))
	      (chart `(barchart ,*title*
				,@(cond
				     (*limit*
				      (list `(max-scale-value ,*limit*)))
				     ((eq? *relative* 'percentage)
				      (list `(max-scale-value 100)))
				     (else
				      '()))
				(bar-names ,@(map (lambda (d k c)
						     (list k
							   (*bar-name* (car d) c)))
						  descrs
						  keys
						  (reverse (cdr *configs*))))
				(bar-values ,@entries))))
	  (if (eq? *relative* 'percentage)
	      (begin
		 (print '(set! *scale-step* 10))
		 (print '(set! *scale-init* 0))))
	  (pp chart)
	  (newline))))
   
;*---------------------------------------------------------------------*/
;*    every? ...                                                       */
;*---------------------------------------------------------------------*/
(define (every? pred lst)
   (cond
      ((null? lst)
       #t)
      ((pred (car lst))
       (every? pred (cdr lst)))
      (else
       #f)))

;*---------------------------------------------------------------------*/
;*    all-missing-values? ...                                          */
;*---------------------------------------------------------------------*/
(define (all-missing-values?::bool vals::pair-nil)
   (if (null? *accept-missing-values*)
       #f
       (let loop ((vals vals)
		  (num (if *relative* 1 0))
		  (missing *accept-missing-values*))
	  (cond
	     ((null? vals)
	      (null? missing))
	     ((null? missing)
	      #t)
	     ((<fx num (car missing))
	      (loop (cdr vals) (+fx num 1) missing))
	     ((eq? (cadr (car vals)) #unspecified)
	      (loop (cdr vals)(+fx num 1) (cdr missing)))
	     (else
	      #f)))))

	  
