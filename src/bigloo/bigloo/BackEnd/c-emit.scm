;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/BackEnd/c-emit.scm          */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Mar 16 18:14:47 1995                          */
;*    Last change :  Wed Oct  6 14:24:36 2004 (serrano)                */
;*    Copyright   :  1995-2004 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The emission of the C code                                       */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module backend_c_emit
   (import  engine_param
	    engine_configure
	    tools_license
	    type_type
	    type_cache
	    ast_var
	    ast_node
	    cgen_cop
	    (emit-bdb-loc cgen_emit-cop)
	    type_tools)
   (export  (start-emission! suffix::bstring)
	    (stop-emission!)
	    (emit-comment ::bstring ::char)
	    (emit-header)
	    (emit-garbage-collector-selection)
	    (emit-include)
	    (emit-debug-activation)
	    (emit-main)
	    *c-port*
	    (emit-dlopen-init global)
	    (untrigraph::bstring ::bstring)
	    (emit-atom-value value)))

;*---------------------------------------------------------------------*/
;*    *dest-prefix* ...                                                */
;*---------------------------------------------------------------------*/
(define *dest-prefix* #f)
(define *c-port*      #f)

;*---------------------------------------------------------------------*/
;*    start-emission! ...                                              */
;*---------------------------------------------------------------------*/
(define (start-emission! suffix)
   (let* ((prefix (cond
		     ((and (string? *dest*)
			   (memq *pass* '(cgen distrib cc cindent hgen)))
		      (prefix *dest*))
		     ((and (pair? *src-files*) (string? (car *src-files*)))
		      (prefix (car *src-files*)))
		     (else
		      #f))))
      (if (or (eq? *dest* '--to-stdout) (not (string? prefix)))
	  (set! *c-port* (current-output-port))
	  (let ((f-name (string-append prefix suffix)))
	     (set! *dest-prefix* prefix)
	     (set! *c-port* (open-output-file f-name))
	     (if (not (output-port? *c-port*))
		 (error *bigloo-name* "Can't open file for output" f-name)
		 #unspecified)))))

;*---------------------------------------------------------------------*/
;*    stop-emission! ...                                               */
;*---------------------------------------------------------------------*/
(define (stop-emission!)
   (cond
      ((not (output-port? *c-port*))
       #f)
      ((eq? *c-port* (current-output-port))
       #f)
      (else
       (flush-output-port *c-port*)
       (close-output-port *c-port*)
       (set! *c-port* #f)
       *dest-prefix*)))

;*---------------------------------------------------------------------*/
;*    *max-col* ...                                                    */
;*---------------------------------------------------------------------*/
(define *max-col* 79)

;*---------------------------------------------------------------------*/
;*    emit-comment ...                                                 */
;*---------------------------------------------------------------------*/
(define (emit-comment string fill)
   (let ((string (if (>fx (string-length string) (-fx *max-col* 8))
		     (substring string 0 (-fx *max-col* 9))
		     string)))
      (display "/*" *c-port*)
      (let ((len (string-length string)))
	 (if (=fx len 0)
	     (fprint *c-port* (make-string (-fx *max-col* 4) fill) "*/")
	     (begin
		(display (make-string 2 fill) *c-port*)
		(display #\space *c-port*)
		(display string *c-port*)
		(display #\space *c-port*)
		(fprint *c-port*
			(make-string (-fx *max-col* (+ 8 len)) fill)
			"*/"))))))

;*---------------------------------------------------------------------*/
;*    emit-license ...                                                 */
;*---------------------------------------------------------------------*/
(define (emit-license)
   (let ((in (open-input-string (bigloo-license))))
      (let loop ((str (read-line in)))
	 (if (eof-object? str)
	     (close-input-port in)
	     (begin
		(emit-comment str #\space)
		(loop (read-line in)))))))

;*---------------------------------------------------------------------*/
;*    emit-header ...                                                  */
;*---------------------------------------------------------------------*/
(define (emit-header)
   (emit-comment "" #\=)
   (emit-comment (let ((p (open-output-string)))
		    (display *src-files* p)
		    (close-output-port p))
		 #\space)
   (emit-comment *bigloo-name* #\space)
   (emit-comment (string-append *bigloo-author* " (c)      " *bigloo-date*)
		 #\space)
   (if *bigloo-licensing?* (emit-license))
   (emit-comment "" #\=)
   (display "/* COMPILATION: " *c-port*)
   (display (command-line) *c-port*)
   (display "*/" *c-port*)
   (newline *c-port*))

;*---------------------------------------------------------------------*/
;*    emit-garbage-collector-selection ...                             */
;*---------------------------------------------------------------------*/
(define (emit-garbage-collector-selection)
   (fprint *c-port* "/* GC selection */")
   (case *garbage-collector*
      ((boehm)
       (fprint *c-port* #"#define THE_GC BOEHM_GC\n"))
      ((bumpy)
       (fprint *c-port* "#define THE_GC BOEHM_GC")
       (fprint *c-port* #"#define BUMPY_GC\n"))
      (else
       (error "emit-garbage-collector-selection"
	      "Can't emit code for gc"
	      *garbage-collector*))))
       
;*---------------------------------------------------------------------*/
;*    emit-include ...                                                 */
;*---------------------------------------------------------------------*/
(define (emit-include)
   ;; the regular include files
   (for-each (lambda (i) (fprint *c-port* "#include <" i ">"))
	     (reverse! *include-foreign*))
   ;; the user additional includes
   (for-each (lambda (i) (fprint *c-port* "#include <" i ">"))
	     (reverse! *additional-include-foreign*))
   (newline *c-port*))

;*---------------------------------------------------------------------*/
;*    emit-debug-activation ...                                        */
;*---------------------------------------------------------------------*/
(define (emit-debug-activation)
   (fprint *c-port* "/* debug mode */")
   (fprint *c-port* "#define BIGLOO_DEBUG 1")
   (newline *c-port*))

;*---------------------------------------------------------------------*/
;*    emit-main ...                                                    */
;*---------------------------------------------------------------------*/
(define (emit-main)
   (emit-bdb-loc #unspecified)
   ;; The bigloo_abort function is only used by the debugger
   ;; and the macro THE_FAILURE. It is used by the debugger as
   ;; a mark where to stop execution after an error. That function
   ;; cannot be located inside the library because of the difficulty
   ;; to stop inside a shared library.
   (fprint *c-port* "long bigloo_abort(long n) { return n; }")
   (emit-bdb-loc #unspecified)
   (fprint *c-port* "int BIGLOO_MAIN(int argc, char *argv[], char *env[])")
   (fprint *c-port*
	   "{"
	   (if (number? *user-heap-size*)
	       (string-append "extern long heap_size; heap_size = "
			      (integer->string *user-heap-size*)
			      ";")
	       "")
	   (if (and (number? *profile-mode*) (> *profile-mode* 0))
	       "GC_profile_start();"
	       ""))
   (when (pair? *bigloo-libraries-c-setup*)
      (fprint *c-port* "/* Libraries setup */")
      (for-each (lambda (f)
		   (fprint *c-port* f "(argc, argv, env);"))
		*bigloo-libraries-c-setup*)
      (newline *c-port*))
   (fprint *c-port* "_bigloo_main(argc, argv, env, &bigloo_main);}")
   (newline *c-port*))

;*---------------------------------------------------------------------*/
;*    emit-dlopen-init ...                                             */
;*---------------------------------------------------------------------*/
(define (emit-dlopen-init global)
   (emit-bdb-loc #unspecified)
   (fprint *c-port* "obj_t " bgl-foreign-dlopen-init "() {")
   (emit-bdb-loc #unspecified)
   (fprint *c-port* "  return " (global-name global) "( 0, \"dload\" );")
   (emit-bdb-loc #unspecified)
   (fprint *c-port* "}")
   (newline *c-port*))

;*---------------------------------------------------------------------*/
;*    emit-atom-value ...                                              */
;*---------------------------------------------------------------------*/
(define (emit-atom-value value)
   (define (emit-llong value macro)
      (let ((disp (open-output-string)))
	 (display value disp)
	 (display macro *c-port*)
	 (display "( " *c-port*)
	 (let ((str (close-output-port disp)))
	    (display (substring str 2 (string-length str)) *c-port*)
	    (display ")" *c-port*))))
   (cond
      ((boolean? value)
       (display "((" *c-port*)
       (display (string-sans-$ (type-name *bool*)) *c-port*)
       (display #\) *c-port*)
       (display (if value 1 0) *c-port*)
       (display #\) *c-port*))
      ((null? value)
       (display "BNIL" *c-port*))
      ((char? value)
       (display "((" *c-port*)
       (display (string-sans-$ (type-name *char*)) *c-port*)
       (display ")" *c-port*)
       (if (=fx (char->integer value) 0)
	   (display "'\\000'" *c-port*)
	   (begin
	      (write-char #\' *c-port*)
	      (if (=fx (char->integer value) 39)
		  (display "\\''" *c-port*)
		  (begin
		     (case value
			((#\return)
			 (write-char #\\ *c-port*)
			 (write-char #\r *c-port*))
			((#\tab)
			 (write-char #\\ *c-port*)
			 (write-char #\t *c-port*))
			((#\newline)
			 (write-char #\\ *c-port*)
			 (write-char #\n *c-port*))
			((#\\)
			 (write-char #\\ *c-port*)
			 (write-char #\\ *c-port*))
			(else
			 (write-char value *c-port*)))
		     (write-char #\' *c-port*)))))
       (write-char #\) *c-port*))
      ((eq? value #unspecified)
       (display "BUNSPEC" *c-port*))
      ((cnst? value)
       (display "BCNST(" *c-port*)
       (display (cnst->integer value) *c-port*)
       (display #\) *c-port*))
      ((string? value)
       (display #\" *c-port*)
       (display (untrigraph (string-for-read value)) *c-port*)
       (display #\" *c-port*))
      ((fixnum? value)
       (display "((" *c-port*)
       (display (string-sans-$ (type-name *long*)) *c-port*)
       (display ")" *c-port*)
       (display value *c-port*)
       (display ")" *c-port*))
      ((flonum? value)
       (display "((" *c-port*)
       (display (string-sans-$ (type-name *real*)) *c-port*)
       (display ")" *c-port*)
       (display value *c-port*)
       (display ")" *c-port*))
      ((elong? value)
       (display "((" *c-port*)
       (display (string-sans-$ (type-name *elong*)) *c-port*)
       (display ")" *c-port*)
       (display (elong->string value) *c-port*)
       (display ")" *c-port*))
      ((llong? value)
       (display "((" *c-port*)
       (display (string-sans-$ (type-name *llong*)) *c-port*)
       (display ")" *c-port*)
       (display (llong->string value) *c-port*)
       (display ")" *c-port*))
      (else
       (display value *c-port*))))

;*---------------------------------------------------------------------*/
;*    untrigraph ...                                                   */
;*    -------------------------------------------------------------    */
;*    We remove ?? and replace it by \077\077 (the octal ascii         */
;*    code of ?) in order to avoir C trigraph confusions.              */
;*---------------------------------------------------------------------*/
(define (untrigraph from)
   (let* ((len   (string-length from))
	  (len-2 (-fx len 2)))
      ;; first we count how many collisions we have
      (let ((nb-col (let loop ((i      0)
			       (nb-col 0))
		       (cond
			  ((>fx i len-2)
			   nb-col)
			  ((not (char=? (string-ref from i) #\?))
			   (loop (+fx i 1) nb-col))
			  ((not (char=? (string-ref from (+fx i 1)) #\?))
			   (loop (+fx i 2) nb-col))
			  ;; yes, we have one
			  (else
			   (loop (+fx i 2) (+fx nb-col 1)))))))
	 (if (=fx nb-col 0)
	     ;; there is no trigraph clashes
	     from
	     ;; there is some, we allocate a new string. Each trigraph
	     ;; require 4 times its size.
	     (let ((res   (make-string (+fx len (*fx 3 (*fx nb-col 2)))))
		   (len-1 (-fx len 1)))
		(let loop ((r 0)
			   (w 0))
		   (cond
		      ((=fx r len)
		       res)
		      ((or (not (char=? (string-ref from r) #\?))
			   (>fx r len-2))
		       (string-set! res w (string-ref from r))
		       (loop (+fx r 1) (+fx w 1)))
		      ((not (char=? (string-ref from (+fx r 1)) #\?))
		       (string-set! res w #\?)
		       (string-set! res (+fx w 1) (string-ref from (+fx r 1)))
		       (loop (+fx r 2) (+fx w 2)))
		      (else
		       ;; this is a trigraph
		       (string-set! res w #\\)
		       (string-set! res (+fx w 1) #\0)
		       (string-set! res (+fx w 2) #\7)
		       (string-set! res (+fx w 3) #\7)
		       (string-set! res (+fx w 4) #\\)
		       (string-set! res (+fx w 5) #\0)
		       (string-set! res (+fx w 6) #\7)
		       (string-set! res (+fx w 7) #\7)
		       (loop (+fx r 2) (+fx w 8))))))))))

