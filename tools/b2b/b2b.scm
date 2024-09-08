;*=====================================================================*/
;*    serrano/prgm/project/bglstone/tools/b2b/b2b.scm                  */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Jul 26 08:44:47 1995                          */
;*    Last change :  Tue Aug 21 17:06:14 2001 (serrano)                */
;*    -------------------------------------------------------------    */
;*    Barchart to bchart converter.                                    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module barchart
   (static  (class chart
	       legend::bstring
	       (bar-width (default *column-width*))
	       (sep-width (default *sep-width*))
	       (hline::int (default 0))
	       (hline-start (default 1))
	       (hline-increment (default 0))
	       (hline-fixnum?::bool (default #f))
	       (secondary-hline (default 0))
	       (actual-max (default 0))
	       (request-max (default 0))
	       (nb-bars::int (default 0))
	       (bars::pair-nil (default '()))
	       (suffix (default *suffix*))
	       (write-values (default *write-values*))))
   (export  *legend-font*
	    *legend-box-font*
	    *legend-box*
	    *legend-style*
	    *font-size*
	    *value-font*
	    *value-font-size*
	    *column-width*
	    *sep-width*
	    *suffix*
	    *hline-style*
	    *secondary-hline-style*
	    *vline?*
	    *color?*
	    *rotate-chart-names*
	    *write-values*
	    *rgb-colors*
	    *grey-colors*
	    *orientation*
	    *x-scale*)
   (eval    (export-all))
   (main    main))
 
;*---------------------------------------------------------------------*/
;*    global variables                                                 */
;*---------------------------------------------------------------------*/
(define *version*     "0.0")
(define *verbose*     0)
(define *barchartrc*  ".barchart")

(define *dest*        'stdout)
(define *src*         'stdin)
(define *inp*         (current-input-port))
(define *outp*        (current-output-port))
(define *home*        (let ((str (getenv "HOME")))
			 (if (string? str)
			     "/users/serrano"
			     "/users/serrano")))
(define-macro (get-pwd)
   (make-file-name (pwd) (dirname (car *src-files*))))
(define *vsize*       10)   ;; the vertical axis size (computed if portrait)
(define *hsize*       10)   ;; the horizontal axis size (computed if landscape)
(define *gwidth*      0.2)  ;; the graduation scale axis size (cm)
(define *sgwidth*     0.1)  ;; the graduation subscale axis size (cm)

(define *h-translate* 3)    ;; initial horizontal translation (cm)
(define *v-translate* 2)    ;; initial vertical translation (cm)

;*---------------------------------------------------------------------*/
;*    User global configuration variables                              */
;*---------------------------------------------------------------------*/
(define *column-width*           0.2)
(define *sep-width*              (/ *column-width* 2))
(define *color?*                 #f)
(define *palette*                #unspecified)
(define *rotate-chart-names*     0)
(define *write-values*           'none)
(define *legend-box*             'right)
(define *legend-style*           'plain)
(define *legend-font*            "Helvetica-Roman")
(define *value-font*             "Helvetica-Roman")
(define *value-font-size*        10)
(define *legend-box-font*        "Courier-Bold")
(define *font-size*              14)
(define *suffix*                 #f)
(define *hline-style*            'plain)
(define *secondary-hline-style*  'plain)
(define *vline?*                 #t)
(define *rgb-colors*             '("1 0 0"           ;; red
				   "0.99 0.9 0.0"    ;; yellow
				   "0.03 1.0 0.04"   ;; green
				   "0 1 1"           ;; cyan
				   "0 0 1"           ;; blue
				   "0.5 1 1"         ;; magenta
				   "0.72 0 0.5"))
(define *grey-colors*            '(0 0.9 0.4 0.8 0.3 0.7 0.2 0.6 0.1 0.5))
(define *orientation*            'landscape)
(define *x-scale*                1.0)

;*---------------------------------------------------------------------*/
;*    verbose ...                                                      */
;*---------------------------------------------------------------------*/
(define (verbose . args)
   (for-each (lambda (a) (display a (current-error-port))) args))

;*---------------------------------------------------------------------*/
;*    error ...                                                        */
;*---------------------------------------------------------------------*/
(define (barchart-error mes obj)
   (let ((proc "barchart"))
      (if (epair? obj)
	  (let ((cer (cer obj)))
	     (match-case cer
		((at ?fname ?pos)
		 (error/location proc mes obj fname pos))
		(else
		 ((@ error __error) proc mes obj))))
	  ((@ error __error) proc mes obj))))

;*---------------------------------------------------------------------*/
;*    load-barchartrc ...                                              */
;*---------------------------------------------------------------------*/
(define (load-barchartrc verb?)
   (let ((file (if (file-exists? *barchartrc*)
		   *barchartrc*
		   (let ((fname (make-file-name (getenv "HOME") *barchartrc*)))
		      (if (file-exists? fname)
			  fname
			  #f)))))
      (if (string? file)
	  (begin
	     (if verb?
		 (verbose "  loading rc file [" file #"]\n")
		 (load file))))))

;*---------------------------------------------------------------------*/
;*    parse-args ...                                                   */
;*---------------------------------------------------------------------*/
(define (parse-args argvs)
   (define (print-var name::symbol help values default)
      (let ((str (make-string 15))
	    (ssym (symbol->string name)))
	 (blit-string! ssym 0 str 0 (min 15 (string-length ssym)))
	 (display* "   " str " -- " help " " values " [default: ")
	 (write default)
	 (print "]")))
   (args-parse argvs
      (("-help" (help "This error message"))
       (print "barchart " *version*)
       (print "usage:")
       (args-parse-usage #f)
       (with-output-to-port (current-error-port)
	  (lambda ()
	     (newline)
	     (print "Runtime command file variables:")
	     (print-var '*legend-box* "Legend box position"
			'(#f right top) *legend-box*)
	     (print-var '*legend-font* "Font for legends"
			"" *legend-font*)
	     (print-var '*legend-box-font* "Font for legend box"
			"" *legend-box-font*)
	     (print-var '*value-font* "Font to be used when value display is enabled"
			"" *value-font*)
	     (print-var '*value-font-size* "Value Font size"
			"" *value-font-size*)
	     (print-var '*font-size* "Font size"
			"" *font-size*)
	     (print-var '*column-width* "Default column width"
			"" *column-width*)
	     (print-var '*sep-width* "Default separation width"
			"" *sep-width*)
	     (print-var '*suffix* "Suffix to be added on the vertical axis"
			"" *suffix*)
	     (print-var '*color?* "Use color or black and white?" 
			'(#t #f) *color?*)
	     (print-var '*write-values* "Write values on top of bars"
			'(none top middle) *write-values*)
	     (print-var '*hline-style* "Style for horizontal lines"
			'(plain dotted none) *hline-style*)
	     (print-var '*vline?* "Vertical tick"
			'(#t #f) *vline?*)
	     (print-var '*secondary-hline-style* "Style for secondary horizontal lines"
			'(plain dotted none) *secondary-hline-style*)
	     (print-var '*rgb-colors* "Color names"
			"" *rgb-colors*)
	     (print-var '*grey-colors* "Grey color values"
			"" *grey-colors*)
	     (print-var '*orientation* "Orientation"
			"" *orientation*)))
       (exit 0))
      (("-o" ?file (help "Output file"))
       (set! *dest* file))
      (("-s" ?suf (help "Suffix"))
       (set! *suffix* suf))
      (("--color" (help "Set color mode"))
       (set! *color?* #t))
      (("--gray" (help "Set gray mode (default)"))
       (set! *color?* #f))
      (("--eval" ?expr (help "Evaluate barchart expression"))
       (with-input-from-string expr
	  (lambda ()
	     (let ((expr (read)))
		(eval expr)))))
      (("-r" (help "Portrait orientation"))
       (set! *orientation* 'portrait)
       (set! *legend-font* 'top))
      (("-ls" ?style (help "Legend style"))
       (set! *legend-style* (string->symbol style)))
      (("--portrait" (help "Portrait orientation"))
       (set! *orientation* 'portrait)
       (set! *legend-font* 'top))
      (("--landscape" (help "Landscape orientation"))
       (set! *orientation* 'landscape))
      (("--xscale" ?val (help "X ratio (portrait mode)"))
       (set! *x-scale* (string->integer val)))
      (("--rotate" ?val (help "Rotate names (in degree)"))
       (set! *rotate-chart-names* (string->integer val)))
      (else
       (if (string? *src*)
	   (barchart-error "duplicated input source file argument" *src*)
	   (set! *src* else)))))

;*---------------------------------------------------------------------*/
;*    main ...                                                         */
;*---------------------------------------------------------------------*/
(define (main argv)
   ;; load the barchart file
   (if (not (member "-q" argv))
       (load-barchartrc (member "-v" argv)))
   ;; argument parsing
   (parse-args (cdr argv))
   (unwind-protect
      (begin
	 (start-io!)
	 (let ((charts (src->charts)))
	    (emit-prolog)
	    (for-each emit-bchart charts)))
      (stop-io!)))

;*---------------------------------------------------------------------*/
;*    actual-max ...                                                   */
;*---------------------------------------------------------------------*/
(define (actual-max charts)
   (let loop ((charts charts)
	      (max 1.0))
      (if (null? charts)
	  (if (> max 1.5) 1.5 max)
	  (with-access::chart (car charts) (actual-max request-max)
	     (let ((rat (/ actual-max request-max)))
		(loop (cdr charts)
		      (if (> rat max)
			  rat
			  max)))))))

;*---------------------------------------------------------------------*/
;*    init-colors! ...                                                 */
;*---------------------------------------------------------------------*/
(define (init-colors!)
   (let ((color (reverse! (reverse (if *color?* *rgb-colors* *grey-colors*)))))
      (set-cdr! (last-pair color) color)
      (set! *palette* color)))
   
;*---------------------------------------------------------------------*/
;*    get-next-color ...                                               */
;*---------------------------------------------------------------------*/
(define (get-next-color)
   (let ((col (car *palette*)))
      (set! *palette* (cdr *palette*))
      col))

;*---------------------------------------------------------------------*/
;*    *nb-items* ...                                                   */
;*---------------------------------------------------------------------*/
(define *nb-items* 0)
(define *items* '())

;*---------------------------------------------------------------------*/
;*    bound-item! ...                                                  */
;*---------------------------------------------------------------------*/
(define (bound-item! item::symbol name::bstring . color)
   (set! *nb-items* (+fx *nb-items* 1))
   (set! *items* (cons item *items*))
   (putprop! item 'item name)
   (if (pair? color)
       (putprop! item 'color (car color))
       (putprop! item 'color (get-next-color)))
   (putprop! item 'offset *nb-items*))

;*---------------------------------------------------------------------*/
;*    bounded-item? ...                                                */
;*---------------------------------------------------------------------*/
(define (bounded-item? item::symbol)
   (string? (getprop item 'item)))

;*---------------------------------------------------------------------*/
;*    item-name ...                                                    */
;*---------------------------------------------------------------------*/
(define (item-name item::symbol)
   (getprop item 'item))

;*---------------------------------------------------------------------*/
;*    max-item-name-length ...                                         */
;*---------------------------------------------------------------------*/
(define (max-item-name-length)
   (let loop ((items *items*)
	      (max   0))
      (if (null? items)
	  (/ max 4)
	  (let ((ilen (string-length (item-name (car items)))))
	     (if (>fx ilen max)
		 (loop (cdr items) ilen)
		 (loop (cdr items) max))))))
	  
;*---------------------------------------------------------------------*/
;*    max-chart-name ...                                               */
;*---------------------------------------------------------------------*/
(define (max-chart-name bars)
   (let loop ((items bars)
	      (max   0)
	      (str   ""))
      (if (null? items)
	  str
	  (let ((ilen (string-length (car (car items)))))
	     (if (>fx ilen max)
		 (loop (cdr items) ilen (car (car items)))
		 (loop (cdr items) max str))))))
	  
;*---------------------------------------------------------------------*/
;*    max-chart-name-length ...                                        */
;*---------------------------------------------------------------------*/
(define (max-chart-name-length bars)
   (let loop ((items bars)
	      (max  0))
      (if (null? items)
	  max
	  (let ((ilen (string-length (car (car items)))))
	     (if (>fx ilen max)
		 (loop (cdr items) ilen)
		 (loop (cdr items) max))))))
	  
;*---------------------------------------------------------------------*/
;*    parse-chart ...                                                  */
;*---------------------------------------------------------------------*/
(define (parse-chart chart-exp legend::bstring exps)
   (let ((chart (instantiate::chart
		   (legend legend))))
      (define (parse-chart-entry entry)
	 (match-case entry
	    ((bar-width ?val)
	     (if (number? val)
		 (with-access::chart chart (bar-width)
		    (set! bar-width val))
		 (barchart-error "Illegal bar-width value" (car exps))))
	    ((sep-width ?val)
	     (if (number? val)
		 (with-access::chart chart (sep-width)
		    (set! sep-width val))
		 (barchart-error "Illegal sep-width value" (car exps))))
	    (((or max max-value) ?val)
	     (if (number? val)
		 (with-access::chart chart (request-max)
		    (set! request-max val))
		 (barchart-error "Illegal max value" (car exps))))
	    (((or hline scale) ?val)
	     (if (number? val)
		 (with-access::chart chart (hline)
		    (set! hline val))
		 (barchart-error "Illegal hline-number value" (car exps))))
	    ((hline-start ?val)
	     (if (number? val)
		 (with-access::chart chart (hline-start)
		    (set! hline-start val))
		 (barchart-error "Illegal hline-start value" (car exps))))
	    ((hline-increment ?val)
	     (if (number? val)
		 (with-access::chart chart (hline-increment)
		    (set! hline-increment val))
		 (barchart-error "Illegal hline-increment value" (car exps))))
	    ((hline-fixnum? ?val)
	     (if (boolean? val)
		 (with-access::chart chart (hline-fixnum?)
		    (set! hline-fixnum? val))
		 (barchart-error "Illegal hline-fixnum? value" (car exps))))
	    (((or secondary-hline subscale) ?val)
	     (if (number? val)
		 (with-access::chart chart (secondary-hline)
		    (set! secondary-hline val))
		 (barchart-error "Illegal secondary-hline value" (car exps))))
	    ((suffix ?val)
	     (if (string? val)
		 (with-access::chart chart (suffix)
		    (set! suffix val))
		 (barchart-error "Illegal suffix value" (car exps))))
	    ((top-values)
	     (with-access::chart chart (write-values)
		(set! write-values 'top)))
	    ((middle-values)
	     (with-access::chart chart (write-values)
		(set! write-values 'middle)))
	    ((none-values)
	     (with-access::chart chart (write-values)
		(set! write-values 'none)))
	    (((and ?label (? string?)) . ?bs)
	     (with-access::chart chart (actual-max nb-bars bars)
		(let liip ((bs   bs)
			   (amax actual-max))
		   (if (null? bs)
		       (begin
			  (set! actual-max amax)
			  (set! nb-bars (+fx 1 nb-bars))
			  (set! bars (cons entry bars)))
		       (match-case (car bs)
			  (((and ?id (? symbol?)) (and ?val (? number?)))
			   (if (not (bounded-item? id))
			       (barchart-error "unbound item" (car bs))
			       (liip (cdr bs)
				     (if (> val amax) val amax))))
			  (((and ?id (? symbol?)) #unspecified)
			   (liip (cdr bs)
				 amax))
			  (else
			   (barchart-error "Illegal data" (car bs))))))))
	    (else
	     (barchart-error "Illegal expression" entry))))
      (for-each parse-chart-entry exps)
      (with-access::chart chart (nb-bars bar-width sep-width
					 request-max actual-max bars)
	 (cond
	    ((=fx nb-bars 0)
	     (barchart-error "No bars definition found for" chart-exp))
	    ((not (number? actual-max))
	     (barchart-error "No max found" chart-exp))
	    (else
	     (set! bars (reverse! bars))
	     (let ((sz (* nb-bars (+ sep-width (* *nb-items* bar-width)))))
		(if (eq? *orientation* 'landscape)
		    (set! *hsize* sz)
		    (begin
		       (set! *vsize* sz)
		       (set! *hsize* (* *vsize* *x-scale*)))))
	     chart)))))
   
;*---------------------------------------------------------------------*/
;*    src->charts ...                                                  */
;*---------------------------------------------------------------------*/
(define (src->charts)
   (init-colors!)
   (let loop ((exp    (read *inp* #t))
	      (charts '()))
      (if (eof-object? exp)
	  (reverse! charts)
	  (match-case exp
	     ((set! *color* ?val)
	      (set! *color?* (not (equal? val '(quote mono))))
	      (init-colors!)
	      (loop (read *inp* #t) charts))
	     ((set! ?var ?val)
	      (eval exp)
	      (loop (read *inp* #t) charts))
	     ((define-item (and ?item (? symbol?)) (and ?name (? string?)))
	      (bound-item! item name)
	      (loop (read *inp* #t) charts))
	     ((define-item (and ?item (? symbol?))
		 (and ?name (? string?)) (and ?color (? (lambda (x)
						 	   (or (number? x)
							       (string? x))))))
	      (bound-item! item name color)
	      (loop (read *inp* #t) charts))
	     ((define-chart (and ?legend (? string?)) . ?exps)
	      (loop (read *inp* #t)
		    (cons (parse-chart exp legend exps)
			  charts)))
	     (else
	      (barchart-error "Illegal expression" exp))))))
	     
;*---------------------------------------------------------------------*/
;*    start-io! ...                                                    */
;*---------------------------------------------------------------------*/
(define (start-io!) 
   (if (string? *src*)
       (begin
	  (set! *inp* (open-input-file *src*))
	  (if (not (input-port? *inp*))
	      (barchart-error "can't open file for input" *src*))))
   (if (string? *dest*)
       (begin
	  (set! *outp* (open-output-file *dest*))
	  (if (not (output-port? *outp*))
	      (barchart-error "can't open file for output" *dest*)))))

;*---------------------------------------------------------------------*/
;*    stop-io! ...                                                     */
;*---------------------------------------------------------------------*/
(define (stop-io!)
   (if (and (input-port? *inp*)
	    (not (eq? *inp* (current-input-port))))
       (close-input-port *inp*))
   (if (and (output-port? *outp*)
	    (not (eq? *outp* (current-output-port))))
       (close-output-port *outp*)))

;*---------------------------------------------------------------------*/
;*    emit-prolog ...                                                  */
;*---------------------------------------------------------------------*/
(define (emit-prolog)
   (if (eq? *orientation* 'landscape)
       (fprint *outp* `(set! *orientation* 'landscape)))
   (if (not *color?*)
       (fprint *outp* `(set! *color* #f)))
   (if (> *rotate-chart-names* 0)
       (fprint *outp* `(set! *name-rotation* ,*rotate-chart-names*)))
   (if (eq? *legend-style* 'line)
       (fprint *outp* '(set! *legend* 'horizontal))
       (fprint *outp* '(set! *legend* 'vertical)))
   (fprint *outp* `(set! *margin-color* #f))
   (if (eq? *orientation* 'landscape)
       (begin
	  (fprint *outp* `(set! *width* (* ,*x-scale* 20)))
	  (fprint *outp* '(set! *height* 10)))
       (begin
	  (fprint *outp* '(set! *width* 20))
	  (fprint *outp* `(set! *height* (* ,*x-scale* 10)))))
   #unspecified)

;*---------------------------------------------------------------------*/
;*    emit-bchart ...                                                  */
;*---------------------------------------------------------------------*/
(define (emit-bchart chart)
   (with-access::chart chart (legend
			      request-max actual-max
			      bars
			      hline-start hline hline-increment)
      (if (> hline 0)
	  (begin
	     (fprint *outp* `(set! *scale-init* ,hline-start))
	     (let* ((max (if (> request-max 0)
			     request-max
			     actual-max))
		    (step (/ max hline))
		    (step (if (> max 10)
			      (round step)
			      step)))
		(fprint *outp* `(set! *scale-step* ,step))))
	  (fprint *outp* '(set! *scale-lines* #f)))
      (pp `(barchart ,legend
		     ,@(if (> request-max 0)
			   `((max-scale-value ,request-max))
			   '())
		     ;; bar-names
		     (bar-names ,@(map (lambda (i)
					  (list i (getprop i 'item)))
				       *items*))
		     ;; bar-values
		     (bar-values ,@bars))
	  *outp*)
      (newline *outp*)))
