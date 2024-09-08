;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Ast/app.scm                 */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Jun 21 09:34:48 1996                          */
;*    Last change :  Wed Nov  3 09:46:15 2004 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The application compilation                                      */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module ast_app
   (include "Ast/node.sch"
	    "Tools/trace.sch")
   (import  tools_error
	    tools_location
	    tools_shape
	    type_cache
 	    ast_sexp
	    ast_ident)
   (export  (application->node::node ::obj ::obj ::obj ::symbol)
	    (make-app-node::node stack loc var::var args)
	    (correct-arity-app?::bool ::variable ::obj)))

;*---------------------------------------------------------------------*/
;*    correct-arity-app? ...                                           */
;*    -------------------------------------------------------------    */
;*    We check functional application arity in order to print, as      */
;*    soon as possible, user errors.                                   */
;*---------------------------------------------------------------------*/
(define (correct-arity-app? var::variable args)
   (let* ((fun     (variable-value var))
	  (nb-args (length args))
	  (arity   (fun-arity fun)))
      (cond
	 ((=fx arity -1)
	  #t)
	 ((>=fx arity 0)
	  (= arity nb-args))
	 (else
	  (<=fx (-fx (negfx arity) (+fx nb-args 1)) 0)))))

;*---------------------------------------------------------------------*/
;*    clean-user-node! ...                                             */
;*    -------------------------------------------------------------    */
;*    This function walk thru a let-var node to unset the user         */
;*    property of local variables.                                     */
;*---------------------------------------------------------------------*/
(define (clean-user-node! node)
   (let loop ((walk node))
      (if (let-var? walk)
	  (begin
	     (for-each (lambda (binding)
			  (local-user?-set! (car binding) #f))
		       (let-var-bindings walk))
	     (loop (let-var-body walk)))
	  node)))

;*---------------------------------------------------------------------*/
;*    application->node ...                                            */
;*    -------------------------------------------------------------    */
;*    Each parameters which is not a variable _is_ force to be bound   */
;*    to a variable.                                                   */
;*    -------------------------------------------------------------    */
;*    This function has been fixed on Aug 7, 2000 (see comments        */
;*    below). The previous version was correct but was compiling       */
;*    twice the function. This was causing a problem on an example     */
;*    of Bernard Serpette (this example was intented to test the       */
;*    CFA but it used to crash on the AST construction).               */
;*---------------------------------------------------------------------*/
(define (application->node exp stack loc site)
   (define (all-subexp-symbol? exp)
      (let loop ((exp exp))
	 (if (null? exp)
	     #t
	     (match-case (car exp)
		((atom ?-)
		 (loop (cdr exp)))
		(((kwote quote) . ?-)
		 (loop (cdr exp)))
		((@ (? symbol?) (? symbol?))
		 (loop (cdr exp)))
		(else
		 (or (atom? (car exp))
		     (var? (car exp))))))))
   (let* ((loc (find-location/loc exp loc))
	  (err-nb *nb-error-on-pass*)
	  (debugstamp (gensym))
	  (fun (sexp->node (car exp) stack loc 'app))
	  (fun-err? (>fx *nb-error-on-pass* err-nb)))
      (if (and (all-subexp-symbol? exp) (var? fun))
	  (let* ((args  (cdr exp))
		 (delta (check-user-app fun args)))
	     (cond
		((not (var? fun))
		 (sexp->node ''() stack loc 'value))
		((=fx delta 0)
		 (make-app-node stack loc fun args))
		(else
		 (wrong-number-of-arguments exp loc fun args))))
	  (let loop ((old-args (cdr exp))
		     (new-args '())
		     (bindings '()))
	     (cond
		((null? old-args)
		 (let ((old-fun      (car exp))
		       (make-the-app (lambda (fun)
					(if (pair? bindings)
					    `(let ,(reverse! bindings)
						(,fun ,@(reverse! new-args)))
					    `(,fun ,@(reverse! new-args))))))
		    
		    (if (var? fun)
			(begin
			   ;; fix of Aug 7, use to be
			   ;; (let ((node (sexp->node (make-the-app old-fun)
			   (let ((node (sexp->node (make-the-app fun)
						   stack
						   loc
						   site)))
			      (clean-user-node! node)))
			(let* ((new-fun (mark-symbol-non-user! (gensym 'fun)))
			       (lexp `(let ((,new-fun ,(if fun-err?
							   '(lambda l l)
							   fun)))
					 ,(make-the-app new-fun)))
			       (node (sexp->node lexp stack loc site)))
			   (clean-user-node! node)))))
		((or (symbol? (car old-args))
		     (cnst? (car old-args)))
		 (loop (cdr old-args)
		       (if (epair? old-args)
			   (econs (car old-args) new-args (cer old-args))
			   (cons (car old-args) new-args))
		       bindings))
		(else
		 (let ((new-arg (mark-symbol-non-user! (gensym 'arg))))
		    (loop (cdr old-args)
			  (if (epair? old-args)
			      (econs new-arg new-args (cer old-args))
			      (cons new-arg new-args))
			  (cons (list new-arg (car old-args)) bindings)))))))))

;*---------------------------------------------------------------------*/
;*    check-user-app ...                                               */
;*    -------------------------------------------------------------    */
;*    We check functional application arity in order to print, as      */
;*    soon as possible, user errors.                                   */
;*---------------------------------------------------------------------*/
(define (check-user-app fun args)
   (if (not (var? fun))
       ;; we may have found an error while compiling the
       ;; function and have compiled '() instead. In order
       ;; to not print several messages for the same error
       ;; we skip the arity one here.
       0
       (let* ((var     (var-variable fun))
	      (fun     (variable-value var))
	      (nb-args (length args))
	      (arity   (cond
			  ((fun? fun)
			   (fun-arity fun))
			  (else
			   -1))))
	  (cond
	     ((=fx arity -1)
	      0)
	     ((>=fx arity 0)
	      (-fx arity nb-args))
	     (else
	      (if (<=fx (-fx (negfx arity) (+fx nb-args 1)) 0)
		  0
		  1))))))

;*---------------------------------------------------------------------*/
;*    wrong-number-of-arguments ...                                    */
;*---------------------------------------------------------------------*/
(define (wrong-number-of-arguments exp loc fun args)
   (let* ((var     (var-variable fun))
	  (fun     (variable-value var))
	  (nb-args (length args))
	  (arity   (cond
		      ((fun? fun)
		       (fun-arity fun))
		      (else
		       -1)))
	  (expect  (cond
		      ((>=fx arity 0)
		       (string-append (number->string arity)
				      " arg(s) expected, "))
		      (else
		       (string-append (number->string (negfx (+fx arity 1)))
				      " or more arg(s) expected, "))))
	  (provide (string-append (number->string (length args)) " provided")))
      (error-sexp->node
       (string-append "Illegal application: " expect provide)
       (shape exp)
       loc)))
				   
;*---------------------------------------------------------------------*/
;*    make-app-node ...                                                */
;*---------------------------------------------------------------------*/
(define (make-app-node stack loc var args)
   (let ((fun (variable-value (var-variable var))))
      (if (or (not (fun? fun))
	      (>=fx (fun-arity fun) 0)
	      (cfun? fun))
	  (let ((args (let loop ((args args)
				 (res  '()))
			 (if (null? args)
			     (reverse! res)
			     (let ((a   (car args))
				   (loc (find-location/loc args loc)))
				(loop (cdr args)
				      (cons (sexp->node a stack loc 'value)
					    res)))))))
	     (make-fx-app-node loc var args))
	  (make-va-app-node (fun-arity fun) stack loc var args))))

;*---------------------------------------------------------------------*/
;*    make-fx-app-node ...                                             */
;*    -------------------------------------------------------------    */
;*    This function produces nodes that represent function call in     */
;*    the AST. If the function is special (mainly because it is an     */
;*    operator), this function emit an ad-hoc node. Otherwise, it      */
;*    emits an APP node if the function is constant and a FUNCALL      */
;*    node otherwise.                                                  */
;*---------------------------------------------------------------------*/
(define (make-fx-app-node loc var args)
   (let ((v (var-variable var)))
      (cond
	 ((and (cfun? (variable-value v)) (special-cfun? v))
	  ;; this is a special C function call, such as a vector-ref,
	  ;; a vector-set! or a vector creation
	  (make-special-app-node loc var args))
	 ((fun? (variable-value v))
	  ;; this is a regular direct call
	  (instantiate::app
	     (loc loc)
	     (type (variable-type v))
	     (fun  (if (closure? var)
		       (duplicate::var var)
		       var))
	     (args args)))
	 (else
	  ;; this is a computed function call (a call to an unkknown function)
	  (instantiate::funcall
	     (loc loc)
	     (type *_*)
	     (fun  var)
	     (args (cons (duplicate::var var) args)))))))

;*---------------------------------------------------------------------*/
;*    make-va-app-node ...                                             */
;*---------------------------------------------------------------------*/
(define (make-va-app-node arity stack loc var args)
   (define (make-args-list args)
      (if (null? args)
	  ''()
	  `((@ c-cons foreign) ,(car args) ,(make-args-list (cdr args)))))
   (let loop ((old-args args)
	      (arity    arity)
	      (f-args   '()))
      (if (=fx arity -1)
	  (let* ((l-arg  (mark-symbol-non-user! (gensym 'list)))
		 (l-exp  `(let ((,l-arg ,(make-args-list old-args)))
			     ,l-arg))
		 (l-node (sexp->node l-exp stack loc 'value))
		 (l-var  (let-var-body l-node))
		 (app    (make-fx-app-node loc
					   var
					   (reverse! (cons l-var f-args)))))
	     (let-var-body-set! l-node app)
	     (clean-user-node! l-node))
	  (loop (cdr old-args)
		(+fx arity 1)
		(cons (sexp->node (car old-args) stack loc 'value) f-args)))))

;*---------------------------------------------------------------------*/
;*    special-cfun? ...                                                */
;*---------------------------------------------------------------------*/
(define (special-cfun? global)
   (memq (global-id global)
	 '(c-vector-length %%vector-length
			   c-vector-ref %%vector-ref
			   c-vector-set! %%vector-set!
			   c-create-vector %%create-vector)))

;*---------------------------------------------------------------------*/
;*    make-special-app-node ...                                        */
;*    -------------------------------------------------------------    */
;*    We don't have to take care of arity checks because they have     */
;*    already been done.                                               */
;*---------------------------------------------------------------------*/
(define (make-special-app-node loc variable args)
   (let* ((var (var-variable variable))
	  (gname (global-name var)))
      (case (global-id var)
	 ((c-vector-length %%vector-length)
	  (instantiate::vlength
	     (loc loc)
	     (type (global-type var))
	     (c-format (string-append gname "($1)"))
	     (expr* args)
	     (vtype (car (cfun-args-type (global-value var))))))
	 ((c-vector-ref %%vector-ref)
	  (instantiate::vref
	     (loc loc)
	     (type (global-type var))
	     (c-format (string-append gname "($1,$2)"))
	     (expr* args)
	     (vtype (car (cfun-args-type (global-value var))))
	     (ftype (global-type var))
	     (otype (cadr (cfun-args-type (global-value var))))))
	 ((c-vector-set! %%vector-set!)
	  (instantiate::vset!
	     (loc loc)
	     (type (global-type var))
	     (c-format (string-append gname "($1,$2,$3)"))
	     (expr* args)
	     (vtype (car (cfun-args-type (global-value var))))
	     (otype (cadr (cfun-args-type (global-value var))))
	     (ftype (caddr (cfun-args-type (global-value var))))))
	 ((c-create-vector %%create-vector)
	  (let* ((stack-alloc (fun-stack-allocator (variable-value var)))
		 (heap-format (string-append gname "($1)"))
		 (stack-format (if (global? stack-alloc)
				   (string-append (global-name stack-alloc)
						  "($1)")
				   heap-format)))
	     (instantiate::valloc
		(loc loc)
		(type (global-type var))
		(c-heap-format heap-format)
		(otype (car (cfun-args-type (global-value var))))
		(expr* args))))
	 (else
	  (error "make-special-app-node"
		 "Illegal special application"
		 (shape var))))))
