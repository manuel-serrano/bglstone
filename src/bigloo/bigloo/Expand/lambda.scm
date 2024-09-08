;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Expand/lambda.scm           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Dec 28 15:44:53 1994                          */
;*    Last change :  Tue May 21 10:12:05 2002 (serrano)                */
;*    Copyright   :  1994-2002 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The lambda macro-expansion.                                      */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module expand_lambda
   (include "Tools/trace.sch")
   (import tools_args
	   tools_progn
	   tools_misc
	   tools_location
	   expand_eps
	   engine_param
	   (find-location tools_location))
   (export internal-definition?
	   (expand-args ::obj ::procedure)
	   (expand-lambda ::obj ::procedure)
	   (internal-begin-expander::procedure ::procedure)))

;*---------------------------------------------------------------------*/
;*    internal-definition? ...                                         */
;*---------------------------------------------------------------------*/
(define internal-definition? #f)

;*---------------------------------------------------------------------*/
;*    expand-args ...                                                  */
;*---------------------------------------------------------------------*/
(define (expand-args args e)
   (let loop ((args args))
      (cond
	 ((null? args)
	  '())
	 ((symbol? args)
	  args)
	 ((not (pair? args))
	  (error "expand" "Illegal argument" args))
	 ((not (and (pair? (car args))
		    (pair? (cdr (car args)))
		    (null? (cddr (car args)))))
	  (cons (car args) (loop (cdr args))))
	 (else
	  (cons (list (car (car args)) (e (cadr (car args)) e))
		(loop (cdr args)))))))
       
;*---------------------------------------------------------------------*/
;*    expand-lambda ...                                                */
;*---------------------------------------------------------------------*/
(define (expand-lambda x e)
   (let ((old-internal internal-definition?))
      (set! internal-definition? #t)
      (let ((res (match-case x
		    ((?lam ?args . (and ?body (not ())))
		     (with-lexical (args*->args-list args)
				   '_
				   (find-location x)
				   (lambda ()
				      (let ((e (internal-begin-expander e)))
					 `(,lam ,(expand-args args e)
					     ,(e (normalize-progn body) e))))))
		    (else
		     (error #f "Illegal `lambda' form" x)))))
	 (set! internal-definition? old-internal)
	 (replace! x res))))
  
;*---------------------------------------------------------------------*/
;*    internal-begin-expander ...                                      */
;*---------------------------------------------------------------------*/
(define (internal-begin-expander old-expander)
   (lambda (expr expander)
      (let ((res (if (and (pair? expr) (eq? (car expr) 'begin))
		     (if (not (null? (cdr expr)))
			 (let ((rest (with-lexical
				      (begin-bindings (cdr expr))
				      '_
				      (find-location expr)
				      (lambda ()
					 (lambda-defines
					  (emap (lambda (x) (expander x expander))
						(cdr expr)))))))
			    (if (epair? expr)
				(econs 'begin rest (cer expr))
				(cons 'begin rest)))
			 #unspecified)
		     (old-expander expr expander))))
	 (if (pair? res)
	     (replace! expr res)
	     res))))

;*---------------------------------------------------------------------*/
;*    begin-bindings ...                                               */
;*---------------------------------------------------------------------*/
(define (begin-bindings body)
   (trace expand "let-bindings: " body
	  #\Newline)
   (let loop ((oldforms  body)
	      (vars     '()))
      (if (pair? oldforms)
	  (let ((form (car oldforms)))
	     (match-case form
		((define (?var . ?-) . ?-)
		 (loop (cdr oldforms)
		       (cons var vars)))
		((define ?var . ?-)
		 (loop (cdr oldforms)
		       (cons var vars)))
		(else
		 (loop (cdr oldforms)
		       vars))))
	  vars)))

;*---------------------------------------------------------------------*/
;*    lambda-defines ...                                               */
;*---------------------------------------------------------------------*/
(define (lambda-defines body)
   (trace expand "lambda-defines: " body
	  #\Newline)
   (let loop ((oldforms  body)
	      (newforms '())
	      (vars     '())
	      (decls    '()))
      (if (pair? oldforms)
	  (let ((form (car oldforms)))
	     (match-case form
		((define ?var ?val)
		 (loop (cdr oldforms)
		       newforms
		       (cons var vars)
		       (cons `(,var ,val)
			     decls)))
		(else
		 (loop (cdr oldforms)
		       (cons form newforms)
		       vars
		       decls))))
	  (cond
	     ((not (null? vars))
	      (let ((nf (if (null? newforms)
			    #unspecified
			    (normalize-progn (reverse newforms)))))
		 `((letrec ,decls ,nf))))
	     (else
	      body)))))
