;*=====================================================================*/
;*    serrano/tmp/BGL/bglstone/tools/barchart/barchart.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Jul 26 08:44:47 1995                          */
;*    Last change :  Wed Mar 14 17:24:22 2012 (serrano)                */
;*    -------------------------------------------------------------    */
;*    Barchart is a program which produces postscript barcharts.       */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module barchart
   (include "barchart.sch")
   (extern  (macro sprintf::int (::string ::string ::double) "sprintf")
	    (macro malloc::string (::int) "(char *)GC_MALLOC"))
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
	    *template*
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
(define *version*     "0.1")
(define *verbose*     0)
(define *barchartrc*  ".barchart")

(define *dest*        'stdout)
(define *src*         'stdin)
(define *inp*         (current-input-port))
(define *outp*        (current-output-port))
(define *tplp*        #unspecified)
(define *home*        (let ((str (getenv "HOME")))
			 (if (string? str)
			     "/users/serrano"
			     "/users/serrano")))
(define-macro (get-pwd)
   (make-file-name (pwd) (dirname (car *src-files*))))
(define *template*    (make-file-name (get-pwd) "barchart.ps"))
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
	     (print-var '*template* "Name of the template file"
			"" *template*)
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
      (("-t" ?file (help "Template name"))
       (set! *template* file))
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
	    (emit-ps-prologue (actual-max charts) charts)
	    (for-each emit-ps-chart charts)
	    (if *legend-box*
		(emit-ps-legend-box charts))
	    (emit-ps-epilogue)))
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
	     (if (= request-max 0)
		 (set! request-max
		       (+ actual-max
			  (* 0.05 actual-max))))
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
   (set! *tplp* (open-input-file *template*))
   (if (not (input-port? *tplp*))
       (barchart-error "can't open file for input" *template*))
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
       (close-output-port *outp*))
   (if (input-port? *tplp*)
       (close-input-port *tplp*)))
	    
;*---------------------------------------------------------------------*/
;*    emit-ps-prologue ...                                             */
;*---------------------------------------------------------------------*/
(define (emit-ps-prologue vscale charts)
   (fprint *outp* "%!PS")
   (fprint *outp* "%%Title: " *dest*)
   (fprint *outp* "%%Creator: barchart (v " *version* "), M. Serrano")
   (display "%%CreationDate: " *outp*)
   (fprint *outp* (date))
   (fprint *outp* "%%For: " (getenv "USER"))
   (let ((vsize (* vscale *vsize*)))
      (if (eq? *orientation* 'landscape)
	  (case *legend-box*
	     ((right)
	      (fprint *outp* "%%BoundingBox: 0 0 "
		      (cm->inch (+ *h-translate* *hsize*
				   (+ 1 2 (max-item-name-length))))
		      " "
		      (cm->inch (+ *v-translate* vsize))))
	     ((top)
	      (fprint *outp* "%%BoundingBox: 0 0 "
		      (cm->inch (+ *h-translate* *hsize*)) " "
		      (cm->inch (+ *v-translate* vsize 0.2 vsize))))
	     (else
	      (fprint *outp* "%%BoundingBox: 0 0 "
		      (cm->inch (+ *h-translate* *hsize*)) " "
		      (cm->inch (+ *v-translate* vsize)))))
	  (let ((len (max-chart-name-length (chart-bars (car charts)))))
	     (fprint *outp* 
		     "%%BoundingBox: " (* 4 len) " 30 "
		     (cm->inch (+ *h-translate* (* (actual-max charts) *hsize*))) " "
		     (cm->inch vsize)))))
   (fprint *outp* "%%Pages: 1")
   (fprint *outp* "%%EndComments")
   (newline *outp*)
   (fprint *outp* "% Some global variables")
   (fprint *outp* "/LegendFont     /" *legend-font* " def")
   (fprint *outp* "/LegendBoxFont  /" *legend-box-font* " def")
   (fprint *outp* "/ValueFont  /" *value-font* " def")
   (newline *outp*)
   ;; template
   (let loop ((line (read-line *tplp*)))
      (if (eof-object? line)
	  #unspecified
	  (begin
	     (fprint *outp* line)
	     (loop (read-line *tplp*)))))
   ;; the translation
   (fprint *outp* *h-translate* " cm " *v-translate* " cm translate")
   (set-color (if *color?* "0 0 0" 0)))

;*---------------------------------------------------------------------*/
;*    emit-ps-epilogue ...                                             */
;*---------------------------------------------------------------------*/
(define (emit-ps-epilogue)
   (fprint *outp* #"showpage\n"))

;*---------------------------------------------------------------------*/
;*    cm->inch ...                                                     */
;*---------------------------------------------------------------------*/
(define (cm->inch cm)
   (inexact->exact (+ 1 (/ (* cm 72) 2.54))))

;*---------------------------------------------------------------------*/
;*    newpath ...                                                      */
;*---------------------------------------------------------------------*/
(define (newpath)
   (fprint *outp* "newpath"))

;*---------------------------------------------------------------------*/
;*    stroke ...                                                       */
;*---------------------------------------------------------------------*/
(define (stroke)
   (fprint *outp* "stroke"))

;*---------------------------------------------------------------------*/
;*    closepath ...                                                    */
;*---------------------------------------------------------------------*/
(define (closepath)
   (fprint *outp* "closepath"))

;*---------------------------------------------------------------------*/
;*    moveto ...                                                       */
;*---------------------------------------------------------------------*/
(define (moveto x y)
   (fprint *outp* x " cm " y " cm moveto"))
   
;*---------------------------------------------------------------------*/
;*    drawline ...                                                     */
;*---------------------------------------------------------------------*/
(define (drawline x0 y0 x1 y1)
   (fprint *outp* x0 " cm " y0 " cm " x1 " cm " y1 " cm drawline"))

;*---------------------------------------------------------------------*/
;*    drawbox ...                                                      */
;*---------------------------------------------------------------------*/
(define (drawbox x0 y0 x1 y1)
   (fprint *outp* x0 " cm " y0 " cm " x1 " cm " y1 " cm drawbox"))

;*---------------------------------------------------------------------*/
;*    drawhugebox-landscape ...                                        */
;*---------------------------------------------------------------------*/
(define (drawhugebox-landscape x0 y0 x1 y1 kont)
   (let ((y (- y1 (* 0.1 y1))))
      (fprint *outp*
	      x0 " cm " y0 " cm " x0 " cm " y " cm "
	      x1 " cm " (- y (- x1 x0)) " cm "
	      x1 " cm " y0 " cm drawpolygonbox")
      (kont)
      (fprint *outp*
	      x0 " cm " (+ y 0.2) " cm " x0 " cm " y1 " cm "
	      x1 " cm " y1 " cm "
	      x1 " cm " (+ (- y (- x1 x0)) 0.2) " cm drawpolygonbox")
      (kont)))

;*---------------------------------------------------------------------*/
;*    drawhugebox-portrait ...                                         */
;*---------------------------------------------------------------------*/
(define (drawhugebox-portrait x0 y0 x1 y1 kont)
   (let ((x (- x1 (* 0.1 x1))))
      (fprint *outp*
	      x0 " cm " y0 " cm " x " cm " y0 " cm "
	      (- x (- y1 y0)) " cm " y1 " cm "
	      x0 " cm " y1 " cm drawpolygonbox")
      (kont)
      (fprint *outp*
	      (+ x 0.2) " cm "  y0 " cm " x1 " cm " y0 " cm "
	      x1 " cm " y1 " cm "
	      (+ (- x (- y1 y0)) 0.2) " cm " y1  " cm drawpolygonbox")
      (kont)))

;*---------------------------------------------------------------------*/
;*    set-ps-color ...                                                 */
;*---------------------------------------------------------------------*/
(define (set-ps-color)
   (display (if *color?* "setrgbcolor" "setgray") *outp*))

;*---------------------------------------------------------------------*/
;*    set-color ...                                                    */
;*---------------------------------------------------------------------*/
(define (set-color val)
   (display val *outp*)
   (display " " *outp*)
   (set-ps-color)
   (newline *outp*))

;*---------------------------------------------------------------------*/
;*    fill ...                                                         */
;*---------------------------------------------------------------------*/
(define (fill val)
   (display val *outp*)
   (display " " *outp*)
   (set-ps-color)
   (fprint *outp* " fill"))

;*---------------------------------------------------------------------*/
;*    out ...                                                          */
;*---------------------------------------------------------------------*/
(define (out x y s)
   (fprint *outp* x " cm " y " cm " s " write"))

;*---------------------------------------------------------------------*/
;*    show ...                                                         */
;*---------------------------------------------------------------------*/
(define (show)
   (fprint *outp* "show"))

;*---------------------------------------------------------------------*/
;*    rotate ...                                                       */
;*---------------------------------------------------------------------*/
(define (rotate angle)
   (display angle *outp*)
   (display " rotate " *outp*))

;*---------------------------------------------------------------------*/
;*    choosefont ...                                                   */
;*---------------------------------------------------------------------*/
(define (choosefont font size)
   (fprint *outp* font " " size " choosefont"))

;*---------------------------------------------------------------------*/
;*    setlinewidth ...                                                 */
;*---------------------------------------------------------------------*/
(define (setlinewidth val)
   (fprint *outp* val " setlinewidth"))

;*---------------------------------------------------------------------*/
;*    gsave ...                                                        */
;*---------------------------------------------------------------------*/
(define (gsave)
   (fprint *outp* "gsave"))

;*---------------------------------------------------------------------*/
;*    grestore ...                                                     */
;*---------------------------------------------------------------------*/
(define (grestore)
   (fprint *outp* "grestore"))

;*---------------------------------------------------------------------*/
;*    val->cm ...                                                      */
;*---------------------------------------------------------------------*/
(define (val->cm val max)
   (/ (* *vsize* val) max))

;*---------------------------------------------------------------------*/
;*    ps-string ...                                                    */
;*---------------------------------------------------------------------*/
(define (ps-string string::bstring)
   (string-append "(" string ")"))

;*---------------------------------------------------------------------*/
;*    barchart.number->string                                          */
;*---------------------------------------------------------------------*/
(define (barchart.number->string num)
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
;*    emit-ps-chart ...                                                */
;*---------------------------------------------------------------------*/
(define (emit-ps-chart ch::chart)
   (if (eq? *orientation* 'landscape)
       (emit-ps-chart-landscape ch)
       (emit-ps-chart-portrait ch)))

;*---------------------------------------------------------------------*/
;*    emit-ps-chart-landscape ...                                      */
;*---------------------------------------------------------------------*/
(define (emit-ps-chart-landscape ch::chart)
   (with-access::chart ch (bar-width sep-width
				     request-max actual-max
				     suffix write-values legend
				     bars)
      (let* ((vsize  *vsize*)
	     (hsize  *hsize*)
	     (lscale (if (string? suffix)
			 (+ 5.8 (* 1.1 (string-length suffix)))
			 5.8))
	     (bwidth (* *nb-items* bar-width)))

	 ;; the function that is in charge of bars drawing
	 (define (draw-bar one xoffset)
	    (let* ((id     (car one))
		   (color  (getprop id 'color))
		   (offset (getprop id 'offset))
		   (xmin   (+ xoffset (* (- offset 1) bar-width)))
		   (val    (cadr one))
		   (value  (if (not (number? val)) 0 val))
 		   (ymax   (if (<= value (+ request-max
					    (/ request-max) 3))
			       (val->cm value request-max)
			       (val->cm (+ request-max
					   (* (/ request-max 3)
					      (/ value actual-max)))
					request-max))))
	       (if (<= value request-max)
		   (begin
		      (drawbox xmin 0 (+ xmin bar-width) ymax)
		      (gsave)
		      (fill color)
		      (grestore)
		      (drawbox xmin 0 (+ xmin bar-width) ymax)
		      (stroke))
		   (begin
		      (drawhugebox-landscape xmin 0 (+ xmin bar-width) ymax
					     (lambda ()
						(gsave)
						(fill color)
						(grestore)))
		      (drawhugebox-landscape xmin 0 (+ xmin bar-width) ymax
					     stroke)))
	       ;; if the top-values? is true, we also have to draw the value
	       ;; on top of the bar
	       (if (and (number? val) (not (eq? write-values 'none)))
		   (let* ((str (barchart.number->string value))
			  (pstr (ps-string str)))
		      (choosefont "ValueFont" *value-font-size*)
		      (if (eq? write-values 'middle)
			  (begin
			     (fprint *outp* (- (+ xmin bar-width) 0.1) " cm "
				     ymax " cm "
				     pstr
				     " stringwidth pop sub 2 div moveto "
				     pstr)
			     (rotate 90)
			     (show)
			     (rotate -90))
			  (begin
			     (fprint *outp* xmin " cm "
				     (+ ymax 0.1) " cm moveto "
				     pstr)
			     (show)))
		      (choosefont "LegendFont" *font-size*)))))
	 
	 ;; default line width
	 (setlinewidth 0.1)
	 (emit-ps-scale ch)
	 
	 ;; we write the chart name
	 (let ((str (ps-string legend)))
	    (fprint *outp* "-1.5 cm 10 cm "
		    str
		    " stringwidth pop sub 2 div moveto")
	    (display str *outp*)
	    (display #\space *outp*)
	    (rotate 90)
	    (show)
	    (rotate -90)
	    (newline *outp*))
	 
	 ;; we display the bars
	 (let loop ((bars bars)
		    (num  0))
	    (if (null? bars)
		'done
		(let* ((bar     (car bars))
		       (str     (ps-string (car bar)))
		       (xoffset (+ (/ sep-width 2)
				   (* num (+ sep-width bwidth)))))
		   (fprint *outp* "% " (car bar))
		   (fprint *outp* xoffset " cm "
			   bwidth " cm " str " stringwidth pop sub 2 div add"
			   " -0.5 cm moveto")
		   (if (not (= *rotate-chart-names* 0))
		       (begin
			  (fprint *outp* "0 0 " str
				  " stringwidth pop sub 2 div "
				  *rotate-chart-names* " sin "
				  *font-size* " mul sub rmoveto")
			  (rotate *rotate-chart-names*)
			  (display str *outp*)
			  (display #\space *outp*)
			  (show)
			  (rotate (- *rotate-chart-names*)))
		       (begin
			  (display str *outp*)
			  (display #\space *outp*)
			  (show)))
		   (if *vline?*
		       (let ((x (+ sep-width
				   bwidth
				   (* num (+ sep-width bwidth)))))
			  (drawline x 0 x 0.3)))
		   (for-each (lambda (one) (draw-bar one xoffset)) (cdr bar))
		   (loop (cdr bars) (+fx num 1)))))
	 
	 ;; we draw the two axis
	 (setlinewidth 1)
	 (drawbox 0 0 hsize vsize)
	 (stroke))))
	    
;*---------------------------------------------------------------------*/
;*    emit-ps-chart-portrait ...                                       */
;*---------------------------------------------------------------------*/
(define (emit-ps-chart-portrait ch::chart)
   (with-access::chart ch (bars
			   bar-width sep-width
			   request-max actual-max
			   suffix write-values legend)
      (let* ((lscale (if (string? suffix)
			 (+ 5.8 (* 1.1 (string-length suffix)))
			 5.8))
	     (bwidth (* *nb-items* bar-width))
	     (vsize  (* (length bars) (+ sep-width bwidth)))
	     (hsize  *hsize*))
	 
	 ;; the function that is in charge of bars drawing
	 (define (draw-bar one yoffset)
	    (let* ((id     (car one))
		   (color  (getprop id 'color))
		   (offset (getprop id 'offset))
		   (ymin   (+ yoffset (* (- offset 1) bar-width)))
		   (val    (cadr one))
		   (value  (if (not (number? val)) 0 val))
 		   (xmax   (if (<= value (+ request-max
					    (/ request-max) 3))
			       (val->cm value request-max)
			       (val->cm (+ request-max
					   (* (/ request-max 3)
					      (/ value actual-max)))
					request-max))))
	       (if (<= value request-max)
		   (begin
		      (drawbox 0 ymin (* *x-scale* xmax) (+ ymin bar-width))
		      (gsave)
		      (fill color)
		      (grestore)
		      (drawbox 0 ymin (* *x-scale* xmax) (+ ymin bar-width))
		      (stroke))
		   (begin
		      (drawhugebox-portrait 0 ymin
					    (* *x-scale* xmax)
					    (+ ymin bar-width)
					    (lambda ()
					       (gsave)
					       (fill color)
					       (grestore)))
		      (drawhugebox-portrait 0 ymin
					    (* *x-scale* xmax)
					    (+ ymin bar-width)
					    stroke)))
	       ;; if the top-values? is true, we also have to draw the value
	       ;; on top of the bar
	       (if (and (number? val) (not (eq? write-values 'none)))
		   (let* ((str (barchart.number->string value))
			  (pstr (ps-string str)))
		      (choosefont "ValueFont" *value-font-size*)
		      (if (eq? write-values 'middle)
			  (begin
			     (fprint *outp* 
				     (* *x-scale* (/ xmax 2)) " cm "
				     (+ ymin (/ bar-width 4)) " cm "
				     pstr
				     " stringwidth pop sub 2 div moveto "
				     pstr)
			     (show))
			  (begin
			     (fprint *outp* (* *x-scale* (+ xmax 0.1)) " cm "
				     (+ ymin (/ bar-width 4)) " cm moveto "
				     pstr)
			     (show)))
		      (choosefont "LegendFont" *font-size*)))))
	 
	 ;; default line width
	 (setlinewidth 0.1)
 	 (emit-ps-scale ch)
	 
	 ;; we write the chart name
	 (let ((str (ps-string legend)))
	    (fprint *outp* 
		    (/ hsize 2) " cm "
		    str
		    " stringwidth pop 2 div sub "
		    (+ vsize (* 2 sep-width)) " cm moveto"
		    str)
	    (display #\space *outp*)
	    (show)
	    (newline *outp*))
	 
	 ;; we display the bars
	 (let loop ((bars bars)
		    (num  0))
	    (if (null? bars)
		'done
		(let* ((bar     (car bars))
		       (str     (ps-string (car bar)))
		       (yoffset (+ (/ sep-width 2)
				   (* num (+ sep-width bwidth)))))
		   ;; the value names
		   (fprint *outp* "% " (car bar))
		   (fprint *outp*
			   (- sep-width)
			   " cm " str " stringwidth pop sub "
			   (+ (/ bwidth 2) yoffset) " cm "
			   " moveto")
		   (if (not (= *rotate-chart-names* 0))
		       (begin
			  (fprint *outp*
				  str " stringwidth pop "
				  str " stringwidth pop "
				  *rotate-chart-names* " cos mul "
				  " sub "
				  " 0 rmoveto")
			  (if (< *rotate-chart-names* 180)
			      (fprint *outp*
				      " 0 0 "
				      str " stringwidth pop "
				      *rotate-chart-names* " sin mul 2 div "
				      " sub "
				      "  rmoveto"))
			  (rotate *rotate-chart-names*)
			  (display str *outp*)
			  (display #\space *outp*)
			  (show)
			  (rotate (- *rotate-chart-names*)))
		       (begin
			  (display str *outp*)
			  (display #\space *outp*)
			  (show)))
		   (if *vline?*
		       (let ((y (+ sep-width
				   bwidth
				   (* num (+ sep-width bwidth)))))
			  (drawline 0 y 0.3 y)))
		   (for-each (lambda (one) (draw-bar one yoffset)) (cdr bar))
		   (loop (cdr bars) (+fx num 1)))))
	 
	 ;; we draw the two axis
	 (setlinewidth 1)
	 (drawbox 0 0 hsize vsize)
	 (stroke))))
	    
;*---------------------------------------------------------------------*/
;*    emit-ps-scale ...                                                */
;*---------------------------------------------------------------------*/
(define (emit-ps-scale ch::chart)
   (if (eq? *orientation* 'landscape)
       (emit-ps-scale-landscape ch)
       (emit-ps-scale-portrait ch)))

;*---------------------------------------------------------------------*/
;*    emit-ps-scale-landscape ...                                      */
;*---------------------------------------------------------------------*/
(define (emit-ps-scale-landscape ch::chart)
   (define (emit-scale inc)
      (with-access::chart ch (hline-start secondary-hline hline-fixnum?
					  sep-width request-max suffix)
	 (let* ((vsize *vsize*)
		(hsize *hsize*)
		(lscale (if (string? suffix)
			    (+ 5.8 (* 1.1 (string-length suffix)))
			    5.8))
		(draw-line? (not (eq? *hline-style* 'none))))
	    (define (drawg v)
	       (let* ((v (if hline-fixnum? (inexact->exact (ceiling v)) v))
		      (h (val->cm v request-max))
		      (vs (if (> request-max 100)
			      (inexact->exact v)
			      v))
		      (svs (barchart.number->string vs))
		      (ssvs (if (string? suffix)
				(string-append svs suffix)
				svs)))
		  (drawline 0 h (- *gwidth*) h)
		  (if draw-line?
		      (begin
			 (setlinewidth 0.02)
			 (drawline 0 h hsize h)
			 (setlinewidth 0.1)))
		  (out (* lscale (- *gwidth*))
		       (- h 0.1)
		       (ps-string ssvs))))
	    (define (drawsg v)
	       (let* ((v (if hline-fixnum? (inexact->exact (ceiling v)) v))
		      (h (val->cm v request-max)))
		  (drawline 0 h (- *sgwidth*) h)))
	    ;; choose the correct font
	    (choosefont "LegendFont" *font-size*)
	    ;; loop on scale
	    (let loop ((v hline-start))
	       (if (<= v request-max)
		   (begin
		      (drawg v)
		      (if (or (=fx secondary-hline 0) (>= v request-max))
			  (loop (+ v inc))
			  (let ((new-v (+ v inc))
				(ninc  (/ inc secondary-hline)))
			     (let liip ((v (+ v ninc)))
				;; loop on secondary hlines
				(if (or (>= v new-v) (>= v request-max))
				    (loop new-v)
				    (begin
				       (drawsg v)
				       (liip (+ v ninc)))))))))))))
   (with-access::chart ch (hline hline-increment request-max)
      (cond
	 ((> hline-increment 0)
	  (emit-scale hline-increment))
	 ((>fx hline 0)
	  (emit-scale (/ request-max hline))))))

;*---------------------------------------------------------------------*/
;*    emit-ps-scale-portrait ...                                       */
;*---------------------------------------------------------------------*/
(define (emit-ps-scale-portrait ch::chart)
   (define (emit-scale inc)
      (with-access::chart ch (hline-start
			      secondary-hline
			      hline-fixnum?
			      bar-width sep-width
			      request-max suffix
			      bars)
	 (let* ((bwidth (* *nb-items* bar-width))
		(vsize (* (length bars) (+ sep-width bwidth)))
		(hsize *hsize*)
		(lscale (if (string? suffix)
			    (+ 5.8 (* 1.1 (string-length suffix)))
			    5.8))
		(draw-line? (not (eq? *hline-style* 'none))))
	    (define (drawg v)
	       (let* ((v (if hline-fixnum? (inexact->exact (ceiling v)) v))
		      (h (* *x-scale* (val->cm v request-max)))
		      (vs (if (> request-max 100)
			      (inexact->exact v)
			      v))
		      (svs (barchart.number->string vs))
		      (ssvs (if (string? suffix)
				(string-append svs suffix)
				svs)))
		  (drawline h 0 h vsize)
		  (if draw-line?
		      (begin
			 (setlinewidth 0.02)
			 (drawline h 0 h vsize)
			 (setlinewidth 0.1)))
		  (out (cond
			  ((< v 10)
			   (- h 0.1))
			  ((< v 100)
			   (- h 0.3))
			  ((< v 1000)
			   (- h 0.5))
			  (else
			   (- h 0.8)))
		       -0.5
		       (ps-string ssvs))))
	    (define (drawsg v)
	       (let* ((v (if hline-fixnum? (inexact->exact (ceiling v)) v))
		      (h (* *x-scale* (val->cm v request-max)))
		      (vsize (* (length bars) (+ sep-width bwidth))))      
		  (drawline h 0 h (+ *sgwidth*))
		  (drawline h vsize h (- vsize *sgwidth*))))
	    ;; choose the correct font
	    (choosefont "LegendFont" *font-size*)
	    ;; loop on scale
	    (let loop ((v hline-start))
	       (if (<= v request-max)
		   (begin
		      (drawg v)
		      (if (or (=fx secondary-hline 0) (>= v request-max))
			  (loop (+ v inc))
			  (let ((new-v (+ v inc))
				(ninc  (/ inc secondary-hline)))
			     (let liip ((v (+ v ninc)))
				;; loop on secondary hlines
				(if (or (>= v new-v) (>= v request-max))
				    (loop new-v)
				    (begin
				       (drawsg v)
				       (liip (+ v ninc)))))))))))))
   (with-access::chart ch (hline hline-increment request-max)
      (cond
	 ((> hline-increment 0)
	  (emit-scale hline-increment))
	 ((>fx hline 0)
	  (emit-scale (/ request-max hline))))))

;*---------------------------------------------------------------------*/
;*    emit-ps-legend-box ...                                           */
;*---------------------------------------------------------------------*/
(define (emit-ps-legend-box charts)
   (if (eq? *orientation* 'landscape)
       (emit-ps-legend-box-landscape)
       (emit-ps-legend-box-portrait charts)))
       
;*---------------------------------------------------------------------*/
;*    emit-ps-legend-box-landscape ...                                 */
;*---------------------------------------------------------------------*/
(define (emit-ps-legend-box-landscape)
   (newline *outp*)
   (fprint *outp* "% legend box")
   (case *legend-box*
      ((top)
       (barchart-error "Not implemented yet" *legend-box*))
      ((right)
       (fprint *outp*
	       (+ *hsize* 1) " cm "
	       (- *vsize* (/ *vsize* 10)) " cm translate")))
   ;; choose the correct font
   (choosefont "LegendBoxFont" *font-size*)
   (let loop ((items (reverse! *items*))
	      (num   0.0))
      (if (null? items)
	  'done
	  (let* ((item  (car items))
		 (color (getprop item 'color)))
	     (drawbox 0 (- num) 0.8 (- 0.4 num))
	     (fill color)
	     (set-color (if *color?* "0 0 0" 0))
	     (out 0.9 (- num) (ps-string (getprop item 'item)))
	     (loop (cdr items) (+fl num 0.5))))))

;*---------------------------------------------------------------------*/
;*    emit-ps-legend-box-portrait ...                                  */
;*---------------------------------------------------------------------*/
(define (emit-ps-legend-box-portrait charts)
   (define (plain-legend)
      (newline *outp*)
      (fprint *outp* "% legend box")
      (case *legend-box*
	 ((top)
	  (fprint *outp*
		  (/ *hsize* 2) " cm "
		  (ps-string (max-chart-name (chart-bars (car charts))))
		  " stringwidth pop 2 div sub "
		  (+ *vsize* 1 (* 0.5 (length *items*))) " cm translate"))
	 ((right)
	  (barchart-error "Not implemented yet" *legend-box*)))
      ;; choose the correct font
      (choosefont "LegendBoxFont" *font-size*)
      (let loop ((items *items*)
		 (num   0.0))
	 (if (null? items)
	     'done
	     (let* ((item  (car items))
		    (color (getprop item 'color)))
		(drawbox 0 (- num) 0.8 (- 0.4 num))
		(fill color)
		(set-color (if *color?* "0 0 0" 0))
		(out 0.9 (- num) (ps-string (getprop item 'item)))
		(loop (cdr items) (+fl num 0.5))))))
   (define (line-legend)
      (let* ((ritems *items*)
	     (strings (map (lambda (item)
			      (getprop item 'item))
			   ritems))
	     (colors (map (lambda (item)
			     (getprop item 'color))
			  ritems))
	     (sstrings (map (lambda (s)
			       (string-append (make-string 6 #\space) s))
			    strings))
	     (astr (apply string-append sstrings)))
	 (newline *outp*)
	 (fprint *outp* "% legend box")
	 (case *legend-box*
	    ((top)
	     (fprint *outp*
		     (/ *hsize* 2) " cm "
		     (ps-string astr)
		     " stringwidth pop 2 div sub "
		     (+ *vsize* 1 0.5) " cm translate"))
	    ((right)
	     (barchart-error "Not implemented yet" *legend-box*)))
	 ;; choose the correct font
	 (choosefont "LegendBoxFont" *font-size*)
	 ;; display the legend string
	 (out 0 0 (ps-string astr))
	 (gsave)
	 (fprint *outp* (ps-string " ")
		 " stringwidth pop 0 translate")
	 (let loop ((strs sstrings)
		    (colors colors))
	    (if (null? strs)
		(grestore)
		(let ((str (car strs)))
		   (fprint *outp* "%" str)
		   (drawbox 0 -0.05 0.8 0.4)
		   (fill (car colors))
		   (set-color (if *color?* "0 0 0" 0))
		   (fprint *outp* (ps-string str)
			   " stringwidth pop 0 translate")
		   (loop (cdr strs) (cdr colors)))))))
   (if (eq? *legend-style* 'line)
       (line-legend)
       (plain-legend)))

