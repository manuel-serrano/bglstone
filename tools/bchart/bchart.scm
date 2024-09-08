;*=====================================================================*/
;*    serrano/tmp/BGL/bglstone/tools/bchart/bchart.scm                 */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Aug 14 13:45:31 2001                          */
;*    Last change :  Wed Mar 14 17:25:28 2012 (serrano)                */
;*    Copyright   :  2001-12 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Produce a Postscript barchart from a .bc description.            */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module bchart
   (include "bchart.sch")
   (extern (macro sprintf::int (::string ::string ::double) "sprintf")
	   (macro malloc::string (::int) "(char *)GC_MALLOC"))
   (static (class chart
	      title::bstring
	      (max-scale-value (default #f))
	      (max-value (default -1))
	      (bar-names (default '()))
	      (bar-names-length (default -1))
	      (bar-values (default '()))))
   (export *font-size*
	   *colors*
	   *width*
	   *height*
	   *orientation*
	   *name-rotation*
	   *font-width-ratio*
	   *text-font*
	   *text-bold-font*
	   *number-font*
	   *scale*
	   *scale-lines*
	   *scale-numbering*
	   *scale-rulers*
	   *scale-init*
	   *scale-step*
	   *legend*
	   *bar-value*
	   *bar-suffix*
	   *bar-width*
	   *bar-padding*
	   *margin-color*
	   *margin-width*
	   *line-width*)
   (eval   (export-all))
   (main   main))

;*---------------------------------------------------------------------*/
;*    Global parameters                                                */
;*---------------------------------------------------------------------*/
(define *version* 1.0)
(define *bchartrc* ".bchart")

(define *dest* 'stdout)
(define *src* 'stdin)
(define *inp* (current-input-port))
(define *outp* (current-output-port))
(define *tplp* #unspecified)
(define *home* (let ((str (getenv "HOME")))
		  (if (string? str) str ".")))
(define-macro (compiletime-pwd)
   (make-file-name (pwd) (dirname (car *src-files*))))
(define *template* (make-file-name (compiletime-pwd) "bchart.ps"))

;*---------------------------------------------------------------------*/
;*    Graphical configuration                                          */
;*---------------------------------------------------------------------*/
(define *font-size* 12)
(define *font-width-ratio* 0.75)
(define *text-font* "Helvetica-Roman")
(define *text-bold-font* "Helvetica-Bold")
(define *number-font* "Courier-Bold")
(define *width* 20)
(define *height* 10)
(define *orientation* 'portrait)
(define *name-rotation* 0)

(define *scale* #t)
(define *scale-lines* #t)
(define *scale-numbering* #t)
(define *scale-rulers* #t)
(define *scale-init* 1)
(define *scale-step* 1)

(define *legend* 'horizontal)

(define *bar-value* #t)
(define *bar-suffix* #f)

(define *bar-width* 'auto)
(define *bar-padding* 0.2)
(define *margin-color* 0.72)
(define *margin-width* 0.2)
(define *line-width* 0.2)

(define *color* #t)

(define *color-table*
   '("1 0 0"       
     "0.99 0.9 0.0" 
     "0.03 1.0 0.04"
     "0.57 0.22 0.54"
     "0.09 0.07 0.57"
     "0.91 0.36 0.16"
     "0.91 0.04 0.56"))

(define *gray-table*
   '(0 0.7 0.3 0.8 0.4 0.6 0.2 0.5 0.1 0.7))

;*---------------------------------------------------------------------*/
;*    Private global variables                                         */
;*---------------------------------------------------------------------*/
(define *scale-x0* 0)
(define *scale-y0* 0)
(define *scale-x1* *width*)
(define *scale-y1* *height*)
(define *scale-x2* *width*)
(define *scale-y2* *height*)
(define *scale-font-size* (inexact->exact (* *font-size* 0.8)))
(define *value-font-size* (inexact->exact (* *font-size* 0.8)))

(define *max-scale-value* -1)
(define *max-actual-value* -1)

(define *separator-width* 0)
(define *group-width* 0)

(define *colors* '())

(define *legend-sample-width* 0.5)

;*---------------------------------------------------------------------*/
;*    main ...                                                         */
;*---------------------------------------------------------------------*/
(define (main argv)
   ;; load the bchart file
   (if (not (member "-q" argv))
       (load-bchartrc (member "-v" argv)))
   ;; argument parsing
   (parse-args (cdr argv))
   (unwind-protect
      (begin
         (start-io!)
	 (let ((chart (src->chart)))
	    (set-font-sizes!)
	    (set-scale-dimensions! chart)
	    (set-bar-widths! chart)
	    (set-colors!)
	    (set! *max-scale-value* (chart-max-scale-value chart))
	    (set! *max-actual-value* (chart-max-value chart))
	    (ps-prologue)
	    (ps-chart chart)
	    (ps-epilogue)))
      (stop-io!)))

;*---------------------------------------------------------------------*/
;*    error ...                                                        */
;*---------------------------------------------------------------------*/
(define (bc-error mes obj)
   (let ((proc "bchart"))
      (if (epair? obj)
          (let ((cer (cer obj)))
             (match-case cer
                ((at ?fname ?pos ?ln)
                 (error/location proc mes obj fname pos))
                (else
                 ((@ error __error) proc mes obj))))
          ((@ error __error) proc mes obj))))

;*---------------------------------------------------------------------*/
;*    font-width ...                                                   */
;*---------------------------------------------------------------------*/
(define (font-width fsize)
   (* fsize *font-width-ratio*))

;*---------------------------------------------------------------------*/
;*    load-bchartrc ...                                                */
;*---------------------------------------------------------------------*/
(define (load-bchartrc verb?)
   (let ((file (if (file-exists? *bchartrc*)
                   *bchartrc*
                   (let ((fname (make-file-name (getenv "HOME") *bchartrc*)))
                      (if (file-exists? fname)
                          fname
                          #f)))))
      (if (string? file)
	  (load file))))

;*---------------------------------------------------------------------*/
;*    print-var ...                                                    */
;*---------------------------------------------------------------------*/
(define-macro (print-var name help values)
   `(do-print-var ',name ,help ,values ,name))

;*---------------------------------------------------------------------*/
;*    parse-args ...                                                   */
;*---------------------------------------------------------------------*/
(define (parse-args argvs)
   (define (do-print-var name::symbol help values default)
      (let ((str (make-string 15))
            (ssym (symbol->string name)))
         (blit-string! ssym 0 str 0 (min 15 (string-length ssym)))
         (display* "   " str " -- " help " " values " [default: ")
         (write default)
         (print "]")))
   (define (help-variables)
      (with-output-to-port (current-error-port)
	 (lambda ()
	    (newline)
	    (print "Runtime command file variables:")
	    (print-var *height* "Barchart height" "")
	    (print-var *width* "Barchart width" "")
	    (print-var *font-size* "Title font size" "")
	    (print-var *font-width-ratio* "Title font width ratio (width=size*ratio)" "")
	    (print-var *orientation* "Orientation" '(portrait landscape))
	    (print-var *name-rotation* "Name rotation" "")
	    (print-var *scale* "Scale" '(#t #f))
	    (print-var *scale-lines* "Scale lines" '(#t #f))
	    (print-var *scale-numbering* "Scale numbers" '(#t #f))
	    (print-var *scale-rulers* "Scale rulers" '(#t #f))
	    (print-var *scale-init* "Scale init value" "")
	    (print-var *scale-step* "Scale step value" "")
	    (print-var *legend* "Legend" '(horizontal vertical #f))
	    (print-var *bar-value* "Bar value" '(#t #f))
	    (print-var *bar-suffix* "Bar value suffix" "")
	    (print-var *bar-width* "Bar width" '(<integer> auto))
	    (print-var *bar-padding* "Bar padding ratio" "]0..1[")
	    (print-var *color* "Color/Gray barchart" '(#t #f))
	    (print-var *margin-color* "Color to display the barchart margin" '(<color> #f))
	    (print-var *margin-width* "The margin width" "")
	    (print-var *line-width* "The line width" "")
	    (print-var *color-table* "Color table" "")
	    (print-var *gray-table* "Gray table" ""))))
   (args-parse argvs
      (("-help" (help "This error message"))
       (print "bchart " *version*)
       (print "usage: [options] [src]")
       (args-parse-usage #f)
       (help-variables)
       (newline)
       (print "Runtime command file: " *bchartrc*)
       (exit -1))
      (("-o" ?file (help "Output file"))
       (set! *dest* file))
      (("-w" ?width (help "Bchart width"))
       (set! *width* (string->integer width))
       (set! *scale-x1* *width*)
       (set! *scale-x2* *width*))
      (("-h" ?height (help "Bchart height"))
       (set! *height* (string->integer height))
       (set! *scale-y1* *height*)
       (set! *scale-y2* *height*))
      (("-f" ?size (help "Bchart font size"))
       (set! *font-size* (string->integer size)))
      (("-rP" (help "Portrait mode (default)"))
       (set! *orientation* 'portrait))
      (("-rL" (help "Landscape mode"))
       (set! *orientation* 'landscape))
      (("-r" ?angle (help "Name rotation (an angle in degree)"))
       (set! *name-rotation* (string->number angle)))
      (("-ss" ?step (help "Scale step"))
       (set! *scale-step* (string->number step)))
      (("-si" ?init (help "Scale init value"))
       (set! *scale-init* (string->number init)))
      (("-s1" (help "Enable scale display"))
       (set! *scale* #t))
      (("-s0" (help "Disable scale display"))
       (set! *scale* #f))
      (("-sl1" (help "Enable scale line display"))
       (set! *scale-lines* #t))
      (("-sl0" (help "Disable scale display"))
       (set! *scale-lines* #f))
      (("-sn1" (help "Enable scale numbering"))
       (set! *scale-numbering* #t))
      (("-sn0" (help "Disable scale numbering"))
       (set! *scale-numbering* #f))
      (("-sr1" (help "Enable scale rulers"))
       (set! *scale-rulers* #t))
      (("-sr0" (help "Disable scale rulers"))
       (set! *scale-rulers* #f))
      (("-c" (help "Color barchart"))
       (set! *color* #t))
      (("-g" (help "Gray barchart"))
       (set! *color* #f))
      (("-cm" ?color (help "Margin Color [#f for no margin]"))
       (cond
	  ((string-ci=? color "#f")
	   (set! *margin-color* #f))
	  ((number? (string->number color))
	   (set! *margin-color* (string->number color)))
	  (else
	   (set! *margin-color* color))))
      (("-m" ?width (help "Margin width"))
       (set! *margin-width* (string->integer width)))
      (("-lw" ?width (help "Line width"))
       (set! *line-width* (string->integer width)))
      (("-bv1" (help "Enable bar value writing"))
       (set! *bar-value* #t))
      (("-bv0" (help "Disable bar value writing"))
       (set! *bar-value* #f))
      (("-s" ?suffix (help "Bar value suffix"))
       (set! *bar-suffix* suffix))
      (("-bw" ?width (help "Bar width"))
       (set! *bar-width* (string->integer width)))
      (("-bp" ?padding (help "Bar padding"))
       (set! *bar-padding* (string->real padding))
       (if (or (< *bar-padding* 0) (> *bar-padding* 1))
	   (bc-error "Bar padding should be bound by ]0..1[" *bar-padding*)))
      (("-lh" (help "-l[h|v|0]" "Legend layout (horizontal, vertical, #f)"))
       (set! *legend* 'horizontal))
      (("-lv")
       (set! *legend* 'vertical))
      (("-l0")
       (set! *legend* #f))
      (("-max" ?val (help "Max scale value"))
       (set! *max-scale-value* (string->number val)))
      ;; -eval
      (("-eval" ?string (help "Evaluate <string>"))
       (let ((port (open-input-string string)))
	  (let laap ((exp (read port)))
	     (if (eof-object? exp)
		 'done
		 (begin
		    (eval exp)
		    (laap (read port)))))))
      (else
       (if (string? *src*)
	   (bc-error "duplicated input source file argument" *src*)
	   (set! *src* else)))))
       
;*---------------------------------------------------------------------*/
;*    start-io! ...                                                    */
;*---------------------------------------------------------------------*/
(define (start-io!) 
   (if (string? *src*)
       (begin
	  (set! *inp* (open-input-file *src*))
	  (if (not (input-port? *inp*))
	      (bc-error "can't open file for input" *src*))))
   (set! *tplp* (open-input-file *template*))
   (if (not (input-port? *tplp*))
       (bc-error "can't open file for input" *template*))
   (if (string? *dest*)
       (begin
	  (set! *outp* (open-output-file *dest*))
	  (if (not (output-port? *outp*))
	      (bc-error "can't open file for output" *dest*)))))

;*---------------------------------------------------------------------*/
;*    stop-io! ...                                                     */
;*---------------------------------------------------------------------*/
(define (stop-io!)
   (if (and (input-port? *inp*)
	    (not (eq? *inp* (current-input-port))))
       (close-input-port *inp*))
   (if (and (output-port? *outp*)
	    (not (eq? *outp* (current-output-port))))
       (close-output-port *outp*))
   (if (input-port? *tplp*)
       (close-input-port *tplp*)))

;*---------------------------------------------------------------------*/
;*    set-font-sizes! ...                                              */
;*---------------------------------------------------------------------*/
(define (set-font-sizes!)
   (set! *scale-font-size* (inexact->exact (* *font-size* 0.8)))
   (set! *value-font-size* (inexact->exact (* *font-size* 0.8))))

;*---------------------------------------------------------------------*/
;*    number->length ...                                               */
;*    -------------------------------------------------------------    */
;*    The length of the string representing the number NUM.            */
;*---------------------------------------------------------------------*/
(define (number->length num)
   (cond
      ((< num 10)
       1)
      ((< num 100)
       2)
      ((< num 1000)
       3)
      (else
       4)))

;*---------------------------------------------------------------------*/
;*    degree->radian ...                                               */
;*---------------------------------------------------------------------*/
(define (degree->radian degree)
   (/ (* -4 (atan -1) degree) 180))

;*---------------------------------------------------------------------*/
;*    set-scale-dimensions! ...                                        */
;*    -------------------------------------------------------------    */
;*    Set the dimension of the scale in cm.                            */
;*---------------------------------------------------------------------*/
(define (set-scale-dimensions! chart)
   (with-access::chart chart (max-scale-value max-value bar-names bar-values)
      (define (longest-name)
	 ;; return the length of the longest bar-names
	 (let loop ((names bar-values)
		    (len 1))
	    (if (null? names)
		len
		(loop (cdr names)
		      (max (string-length (car (car names))) len)))))
      (define (set-dimensions! ratio)
	 (case *orientation*
	    ((portrait)
	     (set! *scale-x1* (- (* ratio *scale-x1*) 0.3))
	     (set! *scale-y1* *height*)
	     (set! *scale-x2* (- *width* 0.3))
	     (set! *scale-y2* *scale-y1*)
	     (multiple-value-bind (_ y0 _ _)
		(legend-dimensions chart)
		(set! *scale-x0* (cond
				    ((= *name-rotation* 0)
				     (+ (inch->cm 2)
					(* (inch->cm (font-width *font-size*))
					   (longest-name))))
				    (else
				     (+ (inch->cm *font-size*)
					(* (inch->cm (font-width *font-size*))
					   (longest-name)
					   (cos (degree->radian *name-rotation*)))))))
		(set! *scale-y1* (- y0 (inch->cm (+ *font-size* 2))))
		(set! *scale-y2* *scale-y1*)
		(if *margin-color*
		    (begin
		       (set! *scale-y0* (+ *scale-y0* 0.2 *margin-width*))
		       (set! *scale-x1* (- *scale-x1* 0.2 *margin-width*))
		       (set! *scale-x2* (- *scale-x2* 0.2 *margin-width*))))))
	    ((landscape)
	     (set! *scale-x0* (+ (inch->cm
				  (* *scale-font-size*
				     (number->length max-scale-value)))
				 0.1))
	     (set! *scale-y0* (cond
				 ((= *name-rotation* 0)
				  (inch->cm (+ *font-size* 2)))
				 (else
				  (+ (inch->cm *font-size*)
				     (* (inch->cm (font-width *font-size*))
					(longest-name)
					(sin (degree->radian *name-rotation*)))))))
	     (if *margin-color*
		 (set! *scale-x1* (- *scale-x1* 0.2)))
	     (multiple-value-bind (_ y0 _ _)
		(legend-dimensions chart)
		(set! *scale-y2* y0)
		(set! *scale-y1* (* ratio y0)))
	     (if *margin-color*
		 (begin
		    (set! *scale-x0* (+ *scale-x0* *margin-width*))
		    (set! *scale-x1* (- *scale-x1* *margin-width*))))
	     (set! *scale-x2* *scale-x1*))))
      (cond
	 ((>= max-scale-value max-value)
	  ;; the max-value is lesser than the max-scale-value, nothing to do
	  (set-dimensions! 1))
	 ((>= (+ max-scale-value (* 0.3 max-scale-value)) max-value)
	  ;; the extra value is no more that 33% greater than max-scale-value
	  (set-dimensions! (/ max-scale-value max-value)))
	 (else
	  ;; the extra value is more that 33% greater than max-scale-value
	  (set-dimensions! 0.75))))
   ;; in any case, we have to allocate room for the title
   (set! *scale-y1* (- *scale-y1* (inch->cm (* *font-size* 2))))
   (set! *scale-y2* (- *scale-y2* (inch->cm (* *font-size* 2)))))

;*---------------------------------------------------------------------*/
;*    set-bar-widths! ...                                              */
;*    -------------------------------------------------------------    */
;*    We compute the width (in cm) of each bars and the separator      */
;*    width (the whitespace between to group of bars).                 */
;*---------------------------------------------------------------------*/
(define (set-bar-widths! chart)
   (with-access::chart chart (bar-values bar-names-length)
      (define (set-dimensions! width)
	 (let* ((groups (length bar-values))
		(nb-bars (* groups bar-names-length))
		(nb-separators groups))
	    (cond
	       ((not (number? *bar-width*))
		(let ((width (/ width (+ nb-separators nb-bars))))
		   (set! *bar-width* width)
		   (set! *group-width* (* bar-names-length *bar-width*))
		   (set! *separator-width* width)))
	       ((> (* nb-bars *bar-width*) width)
		(bc-error "bar width too large" *bar-width*))
	       (else
		(set! *group-width* (* nb-bars *bar-width*))
		(set! *separator-width*
		      (/ (- width (* nb-bars *bar-width*))
			 nb-separators))))))
      (case *orientation*
	 ((landscape)
	  (set-dimensions! (- *scale-x1* *scale-x0*)))
	 ((portrait)
	  (set-dimensions! (- *scale-y1* *scale-y0*))))))

;*---------------------------------------------------------------------*/
;*    set-colors! ...                                                  */
;*---------------------------------------------------------------------*/
(define (set-colors!)
   (let ((l (if *color* *color-table* *gray-table*)))
      (set-cdr! (last-pair l) l)
      (set! *colors* l)))
		     
;*---------------------------------------------------------------------*/
;*    src->chart ...                                                   */
;*---------------------------------------------------------------------*/
(define (src->chart)
   (let loop ((exp (read *inp* #t))
	      (c #f))
      (if (eof-object? exp)
	  (if (chart? c)
	      c
	      (bc-error "Can't find chart description" c))
	  (match-case exp
	     (((or barchart bchart) . ?def)
	      (if (chart? c)
		  (bc-error "Duplication chart definition" exp)
		  (loop (read *inp* #t)
			(parse-chart exp))))
	     (else
	      (eval exp)
	      (loop (read *inp* #t)
		    c))))))

;*---------------------------------------------------------------------*/
;*    parse-chart ...                                                  */
;*---------------------------------------------------------------------*/
(define (parse-chart chart)
   (match-case chart
      ((?- (and ?title (? string?)) . ?opts)
       (let ((chart (instantiate::chart
		       (title title))))
	  ;; max value
	  (define (parse-max-value opt)
	     (match-case opt
		((?- (and ?num (? integer?)))
		 (if (not (integer? (chart-max-scale-value chart)))
		     (chart-max-scale-value-set! chart num)
		     (bc-error "Illegal duplicate max-scale-value" opt)))
		(else
		 (bc-error "Illegal max-scale-value syntax" opt))))
	  ;; bar names
	  (define (parse-bar-names opt)
	     (for-each (lambda (name)
			  (match-case name
			     (((? symbol?) (? string?))
			      #unspecified)
			     (else
			      (bc-error "Illegal bar name syntax" name))))
		       opt)
	     (chart-bar-names-set! chart opt)
	     (chart-bar-names-length-set! chart (length opt)))
	  ;; bar values
	  (define (parse-bar-values opt)
	     (for-each (lambda (name)
			  (match-case name
			     (((? string?) . ?rest)
			      (for-each (lambda (val)
					   (match-case val
					      (((? symbol?) (? number?))
					       #unspecified)
					      (((? symbol?) #unspecified)
					       #unspecified)
					      (else
					       (bc-error "Illegal bar value"
							 name))))
					rest)
			      #unspecified)
			     (else
			      (bc-error "Illegal bar name syntax" name))))
		       opt)
	     (chart-bar-values-set! chart opt))
	  ;; general options parsing
	  (for-each (lambda (opt)
		       (if (not (and (pair? opt) (symbol? (car opt))))
			   (bc-error "Illegal chart opt syntax" opt)
			   (case (car opt)
			      ((max-scale-value)
			       (parse-max-value opt))
			      ((bar-names)
			       (parse-bar-names (cdr opt)))
			      ((bar-values)
			       (parse-bar-values (cdr opt)))
			      (else
			       (bc-error "Illegal chart opt syntax" opt)))))
		    opts)
	  ;; check the consistency of the chart and adjust some chart
	  ;; global variables
	  (check-chart chart)))
      (else
       (bc-error "Illegal chart syntax" chart))))

;*---------------------------------------------------------------------*/
;*    check-chart ...                                                  */
;*---------------------------------------------------------------------*/
(define (check-chart chart)
   (with-access::chart chart (bar-names bar-names-length bar-values max-value)
      ;; we check that each value id is defined in the name list
      (for-each (lambda (val)
		   (let ((vals (cdr val)))
		      (if (<=fx (length vals) bar-names-length)
			  (for-each (lambda (v)
				       (if (not (assq (car v) bar-names))
					   (bc-error "Can't find name value"
						     v)))
				    vals)
			  (bc-error "Too many values provided" vals))))
		bar-values)
      ;; we compute the max effective value
      (for-each (lambda (val)
		   (let ((vals (cdr val)))
		      (if (<=fx (length vals) bar-names-length)
			  (for-each (lambda (v)
				       (if (and (number? (cadr v))
						(> (cadr v) max-value))
					   (set! max-value (cadr v))))
				    vals))))
		bar-values)
      ;; if max-scale-value is not specified, we fill it
      (with-access::chart chart (max-scale-value)
	 (if (not (number? max-scale-value))
	     (if (and (number? *max-scale-value*)
		      (> *max-scale-value* 0))
		 (set! max-scale-value *max-scale-value*)
		 (set! max-scale-value max-value))))
      ;; we are done
      chart))

;*---------------------------------------------------------------------*/
;*    ps-print ...                                                     */
;*---------------------------------------------------------------------*/
(define (ps-print . l)
   (apply fprint *outp* l))

;*---------------------------------------------------------------------*/
;*    ps-prin ...                                                      */
;*---------------------------------------------------------------------*/
(define (ps-prin . l)
   (for-each (lambda (v) (display v *outp*)) l))

;*---------------------------------------------------------------------*/
;*    ps-newline ...                                                   */
;*---------------------------------------------------------------------*/
(define (ps-newline)
   (newline *outp*))

;*---------------------------------------------------------------------*/
;*    ps-prologue ...                                                  */
;*---------------------------------------------------------------------*/
(define (ps-prologue)
   (ps-print "%!PS")
   (ps-print "%%Title: " *dest*)
   (ps-print "%%Creator: bchart (v " *version* "), M. Serrano")
   (ps-prin "%%CreationDate: ")
   (ps-print (date))
   (ps-print "%%For: " (getenv "USER"))
   (ps-print "%%BoundingBox: 0 0 "
	     (cm->inch *width*) " " (cm->inch *height*))
   (ps-print "%%Pages: 1")
   (ps-print "%%EndComments")
   (ps-newline)
   (ps-print "% Some global variables")
   (ps-print "/TextFont /" *text-font* " def")
   (ps-print "/TextBoldFont /" *text-bold-font* " def")
   (ps-print "/NumberFont /" *number-font* " def")
   (ps-newline)
   ;; template
   (let loop ((line (read-line *tplp*)))
      (if (eof-object? line)
	  #unspecified
	  (begin
	     (ps-print line)
	     (loop (read-line *tplp*))))))

;*---------------------------------------------------------------------*/
;*    ps-epilogue ...                                                  */
;*---------------------------------------------------------------------*/
(define (ps-epilogue)
   (ps-print #"showpage\n"))

;*---------------------------------------------------------------------*/
;*    cm->inch ...                                                     */
;*---------------------------------------------------------------------*/
(define (cm->inch cm)
   (round (+ 1 (/ (* cm 72) 2.54))))

;*---------------------------------------------------------------------*/
;*    inch->cm ...                                                     */
;*---------------------------------------------------------------------*/
(define (inch->cm inch)
   (/ (* 2.54 (- inch 1)) 72))

;*---------------------------------------------------------------------*/
;*    ps-newpath ...                                                   */
;*---------------------------------------------------------------------*/
(define (ps-newpath)
   (ps-print "newpath"))

;*---------------------------------------------------------------------*/
;*    ps-stroke ...                                                    */
;*---------------------------------------------------------------------*/
(define (ps-stroke)
   (ps-print "stroke"))

;*---------------------------------------------------------------------*/
;*    ps-closepath ...                                                 */
;*---------------------------------------------------------------------*/
(define (ps-closepath)
   (ps-print "closepath"))

;*---------------------------------------------------------------------*/
;*    ps-moveto ...                                                    */
;*---------------------------------------------------------------------*/
(define (ps-moveto x y)
   (ps-print x " cm " y " cm moveto"))
   
;*---------------------------------------------------------------------*/
;*    ps-drawline ...                                                  */
;*---------------------------------------------------------------------*/
(define (ps-drawline x0 y0 x1 y1)
   (ps-print x0 " cm " y0 " cm " x1 " cm " y1 " cm drawline"))

;*---------------------------------------------------------------------*/
;*    ps-drawbox ...                                                   */
;*---------------------------------------------------------------------*/
(define (ps-drawbox x0 y0 x1 y1)
   (ps-print x0 " cm " y0 " cm " x1 " cm " y1 " cm drawbox"))

;*---------------------------------------------------------------------*/
;*    ps-drawhuge-box ...                                              */
;*---------------------------------------------------------------------*/
(define (ps-drawhuge-box x0 y0 x1 y1 margin color)
   (define (drawhugebox-landscape kont)
      (let* ((y (- y1 (* 0.1 y1)))
	     (d (if (< (+ y margin) y1)
		    margin
		    (- y1 y))))
	 (ps-print x0 " cm " y0 " cm " x0 " cm " y " cm "
		   x1 " cm " (- y (- x1 x0)) " cm "
		   x1 " cm " y0 " cm drawpolygonbox")
	 (kont)
	 (ps-print x0 " cm " (+ y d) " cm " x0 " cm " y1 " cm "
		   x1 " cm " y1 " cm "
		   x1 " cm " (+ (- y (- x1 x0)) d) " cm drawpolygonbox")
	 (kont)))
   (define (drawhugebox-portrait kont)
      (let* ((x (- x1 (* 0.1 x1)))
	     (d (if (< (+ x margin) x1)
		    margin
		    (- x1 x))))
	 (ps-print
	  x0 " cm " y0 " cm " x " cm " y0 " cm "
	  (- x (- y1 y0)) " cm " y1 " cm "
	  x0 " cm " y1 " cm drawpolygonbox")
	 (kont)
	 (ps-print
	  (+ x d) " cm "  y0 " cm " x1 " cm " y0 " cm "
	  x1 " cm " y1 " cm "
	  (+ (- x (- y1 y0)) d) " cm " y1  " cm drawpolygonbox")
	 (kont)))
   (case *orientation*
      ((portrait)
       (drawhugebox-portrait (lambda ()
				(ps-gsave)
				(ps-fill color)
				(ps-grestore)))
       (drawhugebox-portrait (lambda ()
				(ps-stroke))))
      ((landscape)
       (drawhugebox-landscape (lambda ()
				 (ps-gsave)
				 (ps-fill color)
				 (ps-grestore)))
       (drawhugebox-landscape (lambda ()
				 (ps-stroke))))))

;*---------------------------------------------------------------------*/
;*    ps-set-color ...                                                 */
;*---------------------------------------------------------------------*/
(define (ps-set-color val)
   (ps-print val " " (if (number? val) "setgray" "setrgbcolor")))

;*---------------------------------------------------------------------*/
;*    ps-fill ...                                                      */
;*---------------------------------------------------------------------*/
(define (ps-fill val)
   (ps-set-color val)
   (ps-print " fill"))

;*---------------------------------------------------------------------*/
;*    ps-show ...                                                      */
;*---------------------------------------------------------------------*/
(define (show)
   (ps-print "show"))

;*---------------------------------------------------------------------*/
;*    ps-rotate ...                                                    */
;*---------------------------------------------------------------------*/
(define (ps-rotate angle)
   (ps-print angle " rotate"))

;*---------------------------------------------------------------------*/
;*    ps-set-font ...                                                  */
;*---------------------------------------------------------------------*/
(define (ps-set-font font size)
   (ps-print font " " size " choosefont"))

;*---------------------------------------------------------------------*/
;*    ps-setlinewidth ...                                              */
;*---------------------------------------------------------------------*/
(define (ps-setlinewidth val)
   (ps-print val " setlinewidth"))

;*---------------------------------------------------------------------*/
;*    ps-setdashline ...                                               */
;*---------------------------------------------------------------------*/
(define (ps-setdashline val)
   (ps-print "[" val " " val "] 0 setdash"))

;*---------------------------------------------------------------------*/
;*    ps-setplainline ...                                              */
;*---------------------------------------------------------------------*/
(define (ps-setplainline)
   (ps-print "[] 0 setdash"))

;*---------------------------------------------------------------------*/
;*    ps-gsave ...                                                     */
;*---------------------------------------------------------------------*/
(define (ps-gsave)
   (ps-print "gsave"))

;*---------------------------------------------------------------------*/
;*    ps-grestore ...                                                  */
;*---------------------------------------------------------------------*/
(define (ps-grestore)
   (ps-print "grestore"))

;*---------------------------------------------------------------------*/
;*    ps-string ...                                                    */
;*---------------------------------------------------------------------*/
(define (ps-string string::bstring)
   (string-append "(" string ")"))

;*---------------------------------------------------------------------*/
;*    ps-display-string ...                                            */
;*---------------------------------------------------------------------*/
(define (ps-display-string string x y . anchor)
   ;; if not anchor is specified, we display the string at the specified
   ;; location without any computation
   (let ((str (ps-string string)))
      (if (null? anchor)
	  (ps-print x " cm " y " cm moveto " str " show")
	  (case (car anchor)
	     ((sw)
	      (ps-print x " cm " y " cm moveto " str " show"))    
	     ((s)
	      (ps-prin x " cm "
		       str " stringwidth pop 2 div sub "
		       y " cm moveto ")
	      (ps-print str " show"))
	     ((n)
	      (ps-prin x " cm "
		       str " stringwidth pop 2 div sub "
		       y " cm "
		       str " stringwidth exch pop sub moveto ")
	      (ps-print str " show"))
	     ((c)
	      (ps-prin x " cm " y " cm moveto " str
		       " stringwidth 2 div exch 2 div exch rmoveto ")
	      (ps-print str " show"))
	     ((se)
	      (ps-prin x " cm " y " cm moveto 0 " str
		       " stringwidth pop sub 0 rmoveto ")
	      (ps-print str " show"))
	     ((e)
	      (ps-prin x " cm " y " cm moveto " str
		       " stringwidth 2 div neg exch neg exch rmoveto ")
	      (ps-print str " show"))
	     ((rotate)
	      (ps-print x " cm " y " cm moveto")
	      (ps-rotate (cadr anchor))
	      (ps-print str " show")
	      (ps-rotate (- (cadr anchor))))
	     (else
	      (bc-error "Illegal anchor" anchor))))))
   
;*---------------------------------------------------------------------*/
;*    ps-chart ...                                                     */
;*---------------------------------------------------------------------*/
(define (ps-chart chart)
;*    ;; in order to help debugging with GS, we translate every a little bit */
;*    (ps-print "1 cm 1 cm translate")                                 */
;*    ;; and we explictly draw the bounding box                        */
;*    (ps-setlinewidth 1)                                              */
;*    (ps-setdashline 4)                                               */
;*    (ps-set-color "1 0 0")                                           */
;*    (ps-drawbox -0.1 -0.1 (+ .1 *width*) (+ .1 *height*))            */
;*    (ps-stroke)                                                      */
   ;; if there is a margin color we display a use rectangle filled
   ;; with that color
   (if *margin-color*
       (begin
	  (ps-drawbox 0 0 *width* *height*)
	  (ps-gsave)
	  (ps-fill *margin-color*)
	  (ps-grestore)
	  (ps-drawbox *scale-x0* *scale-y0* *scale-x2* *scale-y2*)
	  (ps-gsave)
	  (ps-fill 1)
	  (ps-grestore)))
   ;; general line width and color
   (ps-setplainline)
   (ps-set-color 0)
   (ps-setlinewidth *line-width*)
   ;; we start drawing the the overall border
   (ps-drawbox *scale-x0* *scale-y0* *scale-x1* *scale-y1*)
   (ps-stroke)
   ;; the title of the barchart
   (ps-chart-title chart)
   ;; the legend of the barchart
   (ps-legend chart)
   ;; display the scale
   (if *scale* (ps-chart-scale (chart-max-scale-value chart)))
   (ps-stroke)
   ;; display the bars
   (ps-chart-bars chart))

;*---------------------------------------------------------------------*/
;*    ps-chart-title ...                                               */
;*---------------------------------------------------------------------*/
(define (ps-chart-title chart)
   ;; Set the text font in Title size
   (ps-set-font "TextBoldFont" (+ *font-size* 2))
   ;; display the title
   (ps-display-string (chart-title chart)
		      (/ *width* 2)
		      (- *height* (inch->cm (+ 4 *font-size*))) 's))

;*---------------------------------------------------------------------*/
;*    val->cm ...                                                      */
;*---------------------------------------------------------------------*/
(define (val->cm val)
   (define (val->cm val min max ratio)
      (cond
	 ((<= val *max-scale-value*)
	  (+ (/ (* (- max min) val) *max-scale-value*) min))
	 (else
	  (+ max (* (* (- max min) ratio)
		    (/ (- val *max-scale-value*)
		       (- *max-actual-value* *max-scale-value*)))))))
   (case *orientation*
      ((landscape)
       (let ((r (if (>= (+ *max-scale-value* (* 0.28 *max-scale-value*))
			*max-actual-value*)
		    (- 1 (/ *max-scale-value* *max-actual-value*))
		    0.28)))
	  (val->cm val *scale-y0* *scale-y1* r)))
      ((portrait)
       (let ((r (if (>= (+ *max-scale-value* (* 0.3 *max-scale-value*))
			*max-actual-value*)
		    (- 1 (/ *max-scale-value* *max-actual-value*))
		    0.3)))
	  (val->cm val *scale-x0* *scale-x1* r)))))

;*---------------------------------------------------------------------*/
;*    ps-chart-scale ...                                               */
;*---------------------------------------------------------------------*/
(define (ps-chart-scale max)
   (define (for init stop incr fun)
      (let loop ((i init))
	 (if (<= i stop)
	     (begin
		(fun i)
		(loop (+ i incr))))))
   (define (landscape-scale)
      (if *scale-lines*
	  ;; the main lines
	  (for *scale-init* max *scale-step*
	       (lambda (i)
		  (ps-drawline *scale-x0* (val->cm i)
			       *scale-x1* (val->cm i)))))
      (if *scale-rulers*
	  (for (+ *scale-init* (/ *scale-step* 2))
	       (- max (/ *scale-step* 2))
	       *scale-step*
	       (lambda (i)
		  (ps-drawline *scale-x0* (val->cm i)
			       (+ *scale-x0* 0.2) (val->cm i))
		  (ps-drawline (- *scale-x1* 0.2) (val->cm i)
			       *scale-x1* (val->cm i)))))
      (if *scale-numbering*
	  (begin
	     ;; the text values
	     (ps-set-font "NumberFont" *scale-font-size*)
	     (for *scale-init* max *scale-step*
		  (lambda (i)
		     (ps-display-string (number->string i)
					(- *scale-x0* 0.1)
					(- (val->cm i)
					   (inch->cm (/ *scale-font-size* 2)))
					'se))))))
   (define (portrait-scale)
      (if *scale-lines*
	  ;; the main lines
	  (for *scale-init* max *scale-step*
	       (lambda (i)
		  (ps-drawline (val->cm i) *scale-y0*
			       (val->cm i) *scale-y1*))))
      (if *scale-rulers*
	  (for (+ *scale-init* (/ *scale-step* 2))
	       (- max (/ *scale-step* 2))
	       *scale-step*
	       (lambda (i)
		  (ps-drawline (val->cm i) *scale-y0*
			       (val->cm i) (+ *scale-y0* 0.2))
		  (ps-drawline (val->cm i) (- *scale-y1* 0.2)
			       (val->cm i) *scale-y1*))))
      (if *scale-numbering*
	  (begin
	     ;; the text values
	     (ps-set-font "NumberFont" *scale-font-size*)
	     (for *scale-init* max *scale-step*
		  (lambda (i)
		     (ps-display-string (number->string i)
					(val->cm i)
					(+ *scale-y1* 0.2)
					's))))))
   (case *orientation*
      ((landscape)
       (landscape-scale))
      ((portrait)
       (portrait-scale))))

;*---------------------------------------------------------------------*/
;*    ps-chart-bars ...                                                */
;*---------------------------------------------------------------------*/
(define (ps-chart-bars chart)
   (with-access::chart chart (bar-names bar-values max-scale-value)
      (let loop ((num 0)
		 (vals (if (eq? *orientation* 'portrait)
			   (reverse bar-values)
			   bar-values)))
	 (if (pair? vals)
	     (begin
		(ps-bar-group num
			      (car vals)
			      bar-names
			      (pair? (cdr vals))
			      max-scale-value)
		(loop (+ 1 num) (cdr vals)))))))

;*---------------------------------------------------------------------*/
;*    ps-bar-group ...                                                 */
;*---------------------------------------------------------------------*/
(define (ps-bar-group num values names ruler max-scale-value)
   (define (ps-landscape-bar-group num values names ruler max-scale-value)
      ;; the group id
      (ps-print "%% " values)
      (let* ((name (car values))
	     (vals (cdr values))
	     (x0 (+ *scale-x0*
		    (/ *separator-width* 2)
		    (* (+ *group-width* *separator-width*) num)))
	     (group-x (+ x0 (/ *group-width* 2))))
	 ;; the group name
	 (ps-set-font "TextFont" *font-size*)
	 (if (not (= *name-rotation* 0))
	     (ps-display-string name group-x (inch->cm 2) 'rotate *name-rotation*)
	     (ps-display-string name group-x (inch->cm 2) 's))
	 ;; the ruler
	 (if (and *scale-rulers* ruler)
	     (let ((rx (- (+ x0 (+ *group-width* *separator-width*))
			  (/ *separator-width* 2)
			  (* *bar-width* (/ *bar-padding* 2)))))
		(ps-drawline rx *scale-y0* rx (+ *scale-y0* 0.2))))
	 ;; all the bars
	 (let loop ((names names)
		    (nnum 0))
	    (if (pair? names)
		(let ((val (assq (car (car names)) vals)))
		   (if (pair? val)
		       (ps-bar (+ x0 (* nnum *bar-width*))
			       *scale-y0*
			       (cadr val)
			       max-scale-value
			       (list-ref *colors* nnum)))
		   (loop (cdr names) (+ nnum 1)))))))
   (define (ps-portrait-bar-group num values names ruler max-scale-value)
      ;; the group id
      (ps-print "%% " values)
      (let* ((name (car values))
	     (vals (cdr values))
	     (y0 (+ *scale-y0*
		    (/ *separator-width* 2)
		    (* (+ *group-width* *separator-width*) num)))
	     (group-y (- (+ y0 (/ *group-width* 2))
			 (inch->cm (/ *font-size* 2)))))
	 ;; the group name
	 (ps-set-font "TextFont" *font-size*)
	 (if (not (= *name-rotation* 0))
	     (ps-display-string name
				(inch->cm
				 (* *font-size*
				    (cos (degree->radian *name-rotation*))))
				(- group-y
				   (inch->cm
				    (* (sin (degree->radian *name-rotation*))
				       (font-width *font-size*)
				       (string-length name)
				       0.5)))
				'rotate *name-rotation*)
	     (ps-display-string name (inch->cm 2) group-y))
	 ;; the ruler
	 (if (and *scale-rulers* ruler)
	     (let ((ry (- (+ y0 (+ *group-width* *separator-width*))
			  (/ *separator-width* 2)
			  (* *bar-width* (/ *bar-padding* 2)))))
		(ps-drawline *scale-x0* ry (+ *scale-x0* 0.2) ry)))
	 ;; all the bars
	 (let loop ((names (reverse names))
		    (nnum 0)
		    (cnum (- (length names) 1)))
	    (if (pair? names)
		(let ((val (assq (car (car names)) vals)))
		   (if (pair? val)
		       (ps-bar *scale-x0*
			       (+ y0 (* nnum *bar-width*))
			       (cadr val)
			       max-scale-value
			       (list-ref *colors* cnum)))
		   (loop (cdr names) (+ nnum 1) (- cnum 1)))))))
   (case *orientation*
      ((landscape)
       (ps-landscape-bar-group num values names ruler max-scale-value))
      ((portrait)
       (ps-portrait-bar-group num values names ruler max-scale-value))))

;*---------------------------------------------------------------------*/
;*    format-number ...                                                */
;*---------------------------------------------------------------------*/
(define (format-number num)
   (cond-expand
      (bigloo-c
       (if (fixnum? num)
	   (number->string num)
	   (let ((s::string (malloc 100)))
	      (cond
		 ((<fl num 1.0)
		  (sprintf s "%#.1g" num))
		 ((<fl num 10.0)
		  (sprintf s "%#.2g" num))
		 ((<fl num 100.0)
		  (sprintf s "%#.3g" num))
		 ((<fl num 1000.0)
		  (sprintf s "%#.4g" num))
		 ((<fl num 10000.0)
		  (sprintf s "%#.5g" num))
		 ((<fl num 100000.0)
		  (sprintf s "%#.6g" num))
		 ((<fl num 1000000.0)
		  (sprintf s "%#.7g" num))
		 ((<fl num 10000000.0)
		  (sprintf s "%#.8g" num))
		 ((<fl num 100000000.0)
		  (sprintf s "%#.9g" num)))
	      s)))
      (bigloo-jvm
       (number->string num))))

;*---------------------------------------------------------------------*/
;*    ps-bar ...                                                       */
;*    -------------------------------------------------------------    */
;*    Display one bar based at position X0, Y0, of value VALUE and     */
;*    color COLOR.                                                     */
;*---------------------------------------------------------------------*/
(define (ps-bar x0 y0 value max-value color)
   (define (ps-horizontal-bar x0 y0 value max-value color)
      (define (ps-regular-bar)
	 (let ((x1 (+ x0 (* (- 1. *bar-padding*) *bar-width*)))
	       (y1 (val->cm value)))
	    (ps-drawbox x0 y0 x1 y1)
	    (ps-gsave)
	    (ps-fill color)
	    (ps-grestore)
	    (ps-drawbox x0 y0 x1 y1)
	    (ps-stroke)
	    (if *bar-value*
		(let* ((snum (format-number value))
		       (str (if *bar-suffix*
				(string-append snum *bar-suffix*)
				snum)))
		   (ps-set-font "NumberFont" *value-font-size*)
		   (ps-display-string str
				      (+ x0 (/ (- x1 x0) 2))
				      (+ (val->cm value) 0.1)
				      's)))))
      (define (ps-large-bar)
	 (let ((x1 (+ x0 (* (- 1. *bar-padding*) *bar-width*)))
	       (y1 (val->cm value)))
	    (ps-drawhuge-box x0 y0 x1 y1 (/ *bar-width* 3.) color)
	    (if *bar-value*
		(let* ((snum (format-number value))
		       (str (if *bar-suffix*
				(string-append snum *bar-suffix*)
				snum)))
		   (ps-set-font "NumberFont" *value-font-size*)
		   (ps-display-string str
				      (+ x0 (/ (- x1 x0) 2))
				      (+ (val->cm value) 0.1)
				      's)))))
      (if (<= value max-value)
	  (ps-regular-bar)
	  (ps-large-bar)))
   (define (ps-vertical-bar x0 y0 value max-value color)
      (define (ps-regular-bar)
	 (let ((x1 (val->cm value))
	       (y1 (+ y0 (* (- 1. *bar-padding*) *bar-width*))))
	    (ps-drawbox x0 y0 x1 y1)
	    (ps-gsave)
	    (ps-fill color)
	    (ps-grestore)
	    (ps-drawbox x0 y0 x1 y1)
	    (ps-stroke)
	    (if *bar-value*
		(let ((y (if (> *bar-width* *value-font-size*)
			     (+ y0 (/ (- *bar-width* *value-font-size*) 2))
			     y0)))
		   (ps-set-font "NumberFont" *value-font-size*)
		   (ps-display-string (format-number value)
				      (+ (val->cm value) 0.1)
				      y
				      'sw)))))
      (define (ps-large-bar)
	 (let ((x1 (val->cm value))
	       (y1 (+ y0 (* (- 1. *bar-padding*) *bar-width*))))
	    (ps-drawhuge-box x0 y0 x1 y1 (/ *bar-width* 3.) color)
	    (if *bar-value*
		(let ((y (if (> *bar-width* *value-font-size*)
			     (+ y0 (/ (- *bar-width* *value-font-size*) 2))
			     y0)))
		   (ps-set-font "NumberFont" *value-font-size*)
		   (ps-display-string (format-number value)
				      (+ (val->cm value) 0.1)
				      y
				      'sw)))))
      (if (<= value max-value)
	  (ps-regular-bar)
	  (ps-large-bar)))
   (if (number? value)
       (case *orientation*
	  ((landscape)
	   (ps-horizontal-bar x0 y0 value max-value color))
	  ((portrait)
	   (ps-vertical-bar x0 y0 value max-value color)))))

;*---------------------------------------------------------------------*/
;*    legend-dimensions ...                                            */
;*---------------------------------------------------------------------*/
(define (legend-dimensions chart)
   (if (null? chart)
       0
       (let* ((names (map cadr (chart-bar-names chart)))
	      (lnames (length names)))
	  (define (horizontal-legend)
	     (let* ((width (inch->cm
			    (* lnames
			       (+ (* (font-width *font-size*)
				     (apply max (map string-length names)))
				  (* 1.5 (cm->inch *legend-sample-width*))))))
		    (fheight (inch->cm (+ 2 *font-size*)))
		    (height (* 1.5 fheight))
		    (x (/ *width* 2))
		    (y (- *height* (* 2 fheight) height)))
		(values (- x (/ width 2))
			y
			(+ x (/ width 2))
			(+ y height))))
	  (define (vertical-legend)
	     (let* ((width (inch->cm
			    (+ (* (font-width *font-size*)
				  (apply max (map string-length names)))
			       (* 2 (cm->inch *legend-sample-width*)))))
		    (fheight (inch->cm (+ 4 *font-size*)))
		    (height (+ (inch->cm 4) (* lnames fheight)))
		    (x (/ *width* 2))
		    (y (- *height* (* 2 fheight) height)))
		(values (- x (/ width 2))
			y
			(+ x (/ width 2))
			(+ y height))))
	  (case *legend*
	     ((horizontal)
	      (horizontal-legend))
	     ((vertical)
	      (vertical-legend))
	     (else
	      (values (/ *width* 2) *height* (/ *width* 2) *height*))))))
   
;*---------------------------------------------------------------------*/
;*    ps-legend ...                                                    */
;*---------------------------------------------------------------------*/
(define (ps-legend chart)
   (define (display-legend-sample color x y)
      (ps-drawbox x y (+ x *legend-sample-width*) (+ y (inch->cm *font-size*)))
      (ps-gsave)
      (ps-fill color)
      (ps-grestore))
   (define (horizontal-legend)
      (multiple-value-bind (x0 y0 x1 y1)
	 (legend-dimensions chart)
	 ;; display a white box for the legend
	 (if *margin-color*
	     (begin
		(ps-drawbox x0 y0 x1 y1)
		(ps-gsave)
		(ps-fill 1)
		(ps-grestore)))
	 ;; display the bar names
	 (let ((xstep (/ (- x1 x0) (length (chart-bar-names chart))))
	       (y (+ y0 (/ (- y1 y0 (inch->cm *font-size*)) 2))))
	    (ps-set-font "TextFont" *font-size*)
	    (let loop ((names (map cadr (chart-bar-names chart)))
		       (nnum 0)
		       (x (+ x0 (/ *legend-sample-width* 2))))
	       (if (pair? names)
		   (let ((color (list-ref *colors* nnum)))
		      (display-legend-sample color x y)
		      (ps-display-string (car names)
					 (+ x (* 1.5 *legend-sample-width*))
					 y)
		      (loop (cdr names)
			    (+ nnum 1)
			    (+ x xstep))))))))
   (define (vertical-legend)
      (multiple-value-bind (x0 y0 x1 y1)
	 (legend-dimensions chart)
	 ;; display a white box for the legend
	 (if *margin-color*
	     (begin
		(ps-drawbox x0 y0 x1 y1)
		(ps-gsave)
		(ps-fill 1)
		(ps-grestore)))
	 ;; display the bar names
	 (let ((ystep (inch->cm (+ *font-size* 4)))
	       (x (+ x0 (/ *legend-sample-width* 2))))
	    (ps-set-font "TextFont" *font-size*)
	    (let loop ((names (map cadr (chart-bar-names chart)))
		       (nnum 0)
		       (y (- y1 ystep)))
	       (if (pair? names)
		   (let ((color (list-ref *colors* nnum)))
		      (display-legend-sample color x y)
		      (ps-display-string (car names)
					 (+ x (* 1.5 *legend-sample-width*))
					 y)
		      (loop (cdr names)
			    (+ nnum 1)
			    (- y ystep))))))))
   (case *legend*
      ((horizontal)
       (horizontal-legend))
      ((vertical)
       (vertical-legend))))
