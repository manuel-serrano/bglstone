;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Expand/eps.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Dec 28 14:56:58 1994                          */
;*    Last change :  Thu Feb  3 10:25:43 2005 (serrano)                */
;*    Copyright   :  1994-2005 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The macro expanser inspired by:                                  */
;*    << Expansion-Passing Style: Beyond Conventional Macro >>,        */
;*    Dybvig, Friedman & Haynes  -- ACM 1986 (LFP) page 143            */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    Le module                                                        */
;*---------------------------------------------------------------------*/
(module expand_eps
   (include "Expand/expander.sch"
	    "Tools/trace.sch"
	    "Engine/pass.sch"
	    "Ast/unit.sch")
   (import  tools_speek
	    tools_error
	    tools_progn
	    tools_misc
	    type_type
	    ast_ident
	    expand_expander
	    (find-location tools_location))
   (export  (with-lexical          ::obj ::obj loc ::procedure)
	    (lexical-stack)
	    (add-macro-definition! ::obj)
	    (add-macro-alias!      ::symbol ::symbol)
	    (comptime-expand       ::obj)
	    (comptime-expand-once  ::obj)
	    (expand-units          ::obj)))

;*---------------------------------------------------------------------*/
;*    The user macro list.                                             */
;*---------------------------------------------------------------------*/
(define *macro* '())

;*---------------------------------------------------------------------*/
;*    add-macro! ...                                                   */
;*---------------------------------------------------------------------*/
(define (add-macro-definition! form)
   (set! *macro* (cons form *macro*)))

;*---------------------------------------------------------------------*/
;*    add-macro-alias! ...                                             */
;*---------------------------------------------------------------------*/
(define (add-macro-alias! sym1 sym2)
   (putprop! sym1 'macro-alias-key sym2))

;*---------------------------------------------------------------------*/
;*    *lexical-stack* ...                                              */
;*    -------------------------------------------------------------    */
;*    This global variable allow us to simulate a stack to know        */
;*    when a variable `over-define' a `O-macro'. It is not as          */
;*    beautiful as a extra parameter to `expand' but it is             */
;*    easier to implement.                                             */
;*---------------------------------------------------------------------*/
(define *lexical-stack* '())

;*---------------------------------------------------------------------*/
;*    lexical-stack ...                                                */
;*---------------------------------------------------------------------*/
(define (lexical-stack)
   *lexical-stack*)

;*---------------------------------------------------------------------*/
;*    with-lexical ...                                                 */
;*---------------------------------------------------------------------*/
(define (with-lexical new mark loc thunk)
   (let ((new-id (map (lambda (a) (fast-id-of-id a loc)) new))
	 (old-lexical-stack *lexical-stack*))
      (set! *lexical-stack*
	    (append (map (lambda (o) (cons o mark)) new-id) *lexical-stack*))
      (let ((res (bind-exit (escape)
		    (with-exception-handler
		       (lambda (e)
			  (if (&error? e)
			      (begin
				 (user-error-notify e 'expand)
				 (escape #unspecified))
			      (raise e)))
		       thunk))))
	 (set! *lexical-stack* old-lexical-stack)
	 res)))
    
;*---------------------------------------------------------------------*/
;*    expand-units ...                                                 */
;*    -------------------------------------------------------------    */
;*    We expand the user code and the produced codes.                  */
;*---------------------------------------------------------------------*/
(define (expand-units units)
   (pass-prelude "Expand")
   ;; We set all macros definitions seen in include files.
   (for-each comptime-expand (reverse! *macro*))
   ;; imported inlined functions which are not coming from library
   ;; have to be expanded. It is not obliged to perform macro-expansion
   ;; on library functions because they have alredy been expanded.
   (define (handler e)
      (if (&error? e)
	  (user-error-notify e 'expand)
	  (raise e)))
   ;; we scan all units
   (for-each (lambda (unit)
		(if (procedure? (unit-sexp* unit))
		    ;; a freezed unit (such as the eval unit)
		    ;; cannot be macro expanser.
		    'nothing
		    (let loop ((src (unit-sexp* unit))
			       (res '()))
		       (if (null? src)
			   (unit-sexp*-set! unit (reverse! res))
			   (match-case (car src)
			      ((define-macro . ?-)
			       (with-exception-handler
				  (lambda (e)
				     (handler e)
				     ''())
				  (lambda ()
				     (comptime-expand (car src))))
			       (loop (cdr src) res))
			      ((define-expander . ?-)
			       (with-exception-handler
				  (lambda (e)
				     (handler e)
				     ''())
				  (lambda ()
				     (comptime-expand (car src))))
			       (loop (cdr src) res))
			      (else
			       (let* ((obody (car src))
				      (nbody (bind-exit (skip)
						(with-exception-handler
						   (lambda (e)
						      (handler e)
						      (if (&error? e)
							  (skip ''())))
						   (lambda ()
						      (comptime-expand obody))))))
				  (loop (cdr src) (cons nbody res)))))))))
	     units)
   (pass-postlude units))
      
;*---------------------------------------------------------------------*/
;*    comptime-expand ...                                              */
;*---------------------------------------------------------------------*/
(define (comptime-expand x)
   (initial-expander x initial-expander))

;*---------------------------------------------------------------------*/
;*    (comptime-expand-once ...                                        */
;*---------------------------------------------------------------------*/
(define (comptime-expand-once x)
   (initial-expander x (lambda (x e) x)))

;*---------------------------------------------------------------------*/
;*    initial-expander ...                                             */
;*---------------------------------------------------------------------*/
(define (initial-expander x e::procedure)
   (trace expand "initial-expander: " x #\Newline)
   (let ((e1::procedure (cond
			   ((symbol? x)
			    *identifier-expander*)
			   ((null? x)
			    (error #f "Illegal form" '()))
			   ((not (pair? x))
			    (lambda (x e) x))
			   ((symbol? (car x))
			    (let ((id (fast-id-of-id (car x)
						     (find-location x))))
			       (let (b)
				  (cond
				     ((begin
					 (set! b (get-compiler-expander id))
					 b)
				      b)
				     ((begin
					 (set! b (get-eval-expander id))
					 b)
				      b)
				     ((pair? (assq id (lexical-stack)))
				      *application-expander*)
				     ((begin
					 (set! b (find-O-expander id))
					 b)
				      (expander-expander b))
				     ((begin
					 (set! b (find-G-expander id))
					 b)
				      (expander-expander b))
				     (else
				      *application-expander*)))))
			   (else
			    *application-expander*))))
      (let ((new (e1 x e)))
	 (if (and (pair? new)
		  (not (epair? new))
		  (epair? x))
	     (econs (car new) (cdr new) (cer x))
	     new))))

;*---------------------------------------------------------------------*/
;*    *identifier-expander* ...                                        */
;*---------------------------------------------------------------------*/
(define (*identifier-expander* id e)
   (if (pair? (assq id (lexical-stack)))
       id
       (let ((a (getprop id 'macro-alias-key)))
	  (if a
	      (e a e)
	      id))))

;*---------------------------------------------------------------------*/
;*    *application-expander* ...                                       */
;*---------------------------------------------------------------------*/
(define (*application-expander* x e)
   (let loop ((x* x))
      (cond
	 ((pair? x*)
	  (set-car! x* (e (car x*) e))
	  (set-cdr! x* (loop (cdr x*)))
	  x*)
	 ((null? x*)
	  '())
	 (else
	  (error #f "Illegal application form" x)))))

       
