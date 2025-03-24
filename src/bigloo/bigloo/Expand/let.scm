;*=====================================================================*/
;*    .../prgm/project/bglstone/src/bigloo/bigloo/Expand/let.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Jun 19 08:29:58 1992                          */
;*    Last change :  Thu Mar  6 16:40:50 2025 (serrano)                */
;*    Copyright   :  1992-2025 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    Let expansions.                                                  */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module expand_let
   (include "Ast/node.sch" "Type/type.sch")
   (import  tools_progn
	    tools_args
	    tools_misc
	    expand_lambda
	    expand_eps
	    (find-location tools_location)
	    (dsssl-named-constant? tools_dsssl))
   (export  (expand-let*   ::obj ::procedure)
	    (expand-let    ::obj ::procedure)
	    (expand-letrec ::obj ::procedure)
	    (expand-labels ::obj ::procedure)))
 
;*---------------------------------------------------------------------*/
;*    expand-let* ...                                                  */
;*---------------------------------------------------------------------*/
(define (expand-let* x e)
   (let ((old-internal internal-definition?))
      (set! internal-definition? #t)
      (let* ((e   (internal-begin-expander e))
	     (res (match-case x
		     ((?- () . (and ?body (not ())))
		      (replace! x
				(cons* 'let
				       '()
				       (e (normalize-progn body) e)
				       '())))
		     ((?- (and (? pair?) ?bindings) . (and ?body (not ())))
		      (let ((sl (econs 'let*
				       (cons* (cdr bindings) body)
				       (find-location (car bindings)))))
			 (e (cons* 'let
				   (if (epair? bindings)
				       (econs (car bindings)
					      '()
					      (cer bindings))
				       (cons (car bindings)
					     '()))
				   sl
				   '())
			    e)))
		     (else
		      (error #f "Illegal `let*' form" x)))))
	 (set! internal-definition? old-internal)
	 (replace! x res))))

;*---------------------------------------------------------------------*/
;*    expand-let ...                                                   */
;*---------------------------------------------------------------------*/
(define (expand-let x e)
   (let ((old-internal internal-definition?))
      (set! internal-definition? #t)
      (let* ((e   (internal-begin-expander e))
	     (res (match-case x
		     ((?- () . (and ?body (not ())))
		      ;; we must left the construction (to uses with traces).
		      (replace! x
				(cons* 'let
				       '()
				       (e (normalize-progn body) e)
				       '())))
		     ((?- (and (? symbol?) ?loop)
			  ?bindings . (and ?body (not ())))
		      (if (not (or (null? bindings) (pair? bindings)))
			  (error #f "Illegal `let' form" x)
			  (e `(labels ((,loop ,(map
						(lambda (b)
						   (if (pair? b)
						       (car b)
						       (error
							#f
							"Illegal `let' form"
							x)))
						bindings)
					      ,@body))
				 (,loop ,@(map (lambda (b)
						  (if (and (pair? b)
							   (pair? (cdr b))
							   (null? (cddr b)))
						      (cadr b)
						      (error
						       #f
						       "Illegal `let' form"
						       x)))
					       bindings)))
			     e)))
		     ((?- (and (? pair?) ?bindings) . (and ?body (not ())))
		      `(let ,(let loop ((bindings bindings)
					(acc      '()))
				(if (null? bindings)
				    (reverse! acc)
				    (let ((pr (car bindings)))
				       (cond
					  ((not (pair? pr))
					   (loop (cdr bindings)
						 (cons (list
							pr
							'(unspecified))
						       acc)))
					  ((not (and (pair? (cdr pr))
						     (null? (cddr pr))))
					   (error #f "Illegal `let' form" x))
					  (else
					   (let ((bd (list 
						      (car pr)
						      (e (normalize-progn
							  (cdr pr))
							 e))))
					      (replace! pr bd)
					      (loop (cdr bindings)
						    (cons pr acc))))))))
			  ,(with-lexical
			    (map (lambda (v) (if (pair? v) (car v) v))
				 bindings)
			    '_
			    (find-location x)
			    (lambda ()
;* 			       (print "expand-let: " body)             */
;* 			       (print "       loc: " (find-location body)) */
			       (let ((nbody (normalize-progn body)))
;* 				  (print "     nbody: " nbody)         */
;* 				  (print "       loc: " loc)           */
				  (let ((body (e nbody e)))
				     body))))))
		     (else
		      (error #f "Illegal `let' form" x)))))
	 (set! internal-definition? old-internal)
	 (replace! x res))))

;*---------------------------------------------------------------------*/
;*    expand-letrec ...                                                */
;*---------------------------------------------------------------------*/
(define (expand-letrec x e)
   (let ((old-internal internal-definition?))
      (set! internal-definition? #t)
      (let* ((e   (internal-begin-expander e))
	     (res (match-case x
		     ((?- () . (and ?body (not ())))
		      (set-car! x 'let)
		      (e x e))
		     ((?- (and (? pair?) ?bindings) . (and ?body (not ())))
		      (with-lexical
		       (map (lambda (v) (if (pair? v) (car v) v)) bindings)
		       '_
		       (find-location x)
		       (lambda ()
			  `(letrec ,(let loop ((bindings bindings)
					       (acc      '()))
				       (if (null? bindings)
					   (reverse! acc)
					   (let ((pr (car bindings)))
					      (if (or (not (pair? pr))
						      (not (pair? (cdr pr))))
						  (error
						   #f
						   "Illegal `letrec' form"
						   x)
						  (let ((nb (list
							     (car pr)
							     (e (normalize-progn
								 (cdr pr))
								e))))
						     (replace! pr nb)
						     (loop
						      (cdr bindings)
						      (cons pr acc)))))))
			      ,(e (normalize-progn body) e)))))
		     (else
		      (error #f "Illegal `letrec' form" x)))))
	 (set! internal-definition? old-internal)
	 (replace! x res))))
	 
;*---------------------------------------------------------------------*/
;*      expand-labels ...                                              */
;*---------------------------------------------------------------------*/
(define (expand-labels x e)
   (let ((old-internal internal-definition?))
      (define (expd-lbls bindings body)
	 (let ((bdg (map (lambda (b)
			    (match-case b
			       ((?id () . ?-)
				id)
			       ((?id (? symbol?) . ?-)
				id)
			       ((?id ((or (? dsssl-named-constant?)
					  (? symbol?))
				      ...) . ?-)
				id)
			       ((?id ((? symbol?) ... . (? symbol?)) . ?-)
				id)
			       (else
				(error #f "Illegal `labels' form" x))))
			 bindings)))
	    (with-lexical
	     bdg
	     '_
	     (find-location x)
	     (lambda ()
		(let ((new
		       (let loop ((bindings bindings))
			  (cond
			     ((null? bindings)
			      '())
			     ((not (pair? bindings))
			      (error #f "Illegal `labels' form" x))
			     (else
			      (match-case (car bindings)
				 ((?name ?args)
				  (error #f "Illegal `labels' form" x))
				 ((?name ?args . ?lbody)
				  (with-lexical
				   (args*->args-list args)
				   '_
				   (find-location x)
				   (lambda ()
				      (let* ((b `(,name
						  ,args
						  ,(e (normalize-progn
						       lbody)
						      e))))
					 (cons
					  (if (epair? (car bindings))
					      (econs (car b)
						     (cdr b)
						     (cer (car bindings)))
					      b)
					  (loop (cdr bindings)))))))
				 (else
				  (error #f "Illegal `labels' form" x))))))))
		   `(labels ,new ,(e (normalize-progn body) e)))))))
      (set! internal-definition? #t)
      (let* ((e   (internal-begin-expander e))
	     (res (match-case x
		     ((?- () . (and ?body (not ())))
		      (set-car! x 'let)
		      (e x e))
		     ((?- (and (? pair?) ?bindings) . (and ?body (not ())))
		      (expd-lbls bindings body))
		     (else
		      (error #f "Illegal `labels' form" x)))))
	 (set! internal-definition? old-internal)
	 (replace! x res))))
