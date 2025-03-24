;*=====================================================================*/
;*    .../prgm/project/bglstone/src/bigloo/bigloo/Module/eval.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Jun  4 16:28:03 1996                          */
;*    Last change :  Thu Mar  6 16:28:48 2025 (serrano)                */
;*    Copyright   :  1996-2025 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The eval clauses compilation.                                    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module module_eval
   (include "Ast/unit.sch"
	    "Ast/node.sch"
	    "Object/slots.sch")
   (import  module_module
	    module_include
	    engine_param
	    tools_shape
	    tools_error
	    tools_location
	    type_cache
	    type_env
	    object_class
	    object_slots
	    ast_env
	    ast_glo-decl
	    ast_sexp)
   (export  (make-eval-compiler)
	    *all-eval?*
	    *all-export-eval?*
	    *all-module-eval?*
	    *all-export-mutable?*))

;*---------------------------------------------------------------------*/
;*    make-eval-compiler ...                                           */
;*---------------------------------------------------------------------*/
(define (make-eval-compiler)
   (instantiate::ccomp (id 'eval)
		       (producer eval-producer)
		       (finalizer eval-finalizer)))

;*---------------------------------------------------------------------*/
;*    eval-producer ...                                                */
;*---------------------------------------------------------------------*/
(define (eval-producer clause)
   (match-case clause
      ((?- . ?protos)
       (for-each (lambda (proto) (eval-parser proto clause)) protos)
       '())
      (else
       (user-error "Parse error"
		   (string-append "Illegal `eval' clause")
		   clause
		   '()))))
   
;*---------------------------------------------------------------------*/
;*    eval-parser ...                                                  */
;*---------------------------------------------------------------------*/
(define (eval-parser proto clause)
   (match-case proto
      ((export-all)
       (set! *all-eval?* #t))
      ((export-exports)
       (set! *all-export-eval?* #t))
      ((export-module)
       (set! *all-module-eval?* #t))
      ((export (and (? symbol?) ?var))
       (set! *one-eval?* #t)
       (remember-eval-exported! var *module* proto))
      ((export (@ (and (? symbol?) ?var) (and (? symbol?) ?module)))
       (set! *one-eval?* #t)
       (remember-eval-exported! var module proto))
      ((class (and (? symbol?) ?class))
       (set! *eval-classes* (cons proto *eval-classes*)))
      ((import (and (? symbol?) ?var))
       (declare-global-svar! var 'eval 'eval clause #f))
      (else
       (user-error "Parse error" "Illegal `eval clause'" clause '()))))

;*---------------------------------------------------------------------*/
;*    *eval-exported* ...                                              */
;*---------------------------------------------------------------------*/
(define *eval-exported* '())

;*---------------------------------------------------------------------*/
;*    *eval-classes* ...                                               */
;*---------------------------------------------------------------------*/
(define *eval-classes* '())

;*---------------------------------------------------------------------*/
;*    remember-eval-exported! ...                                      */
;*---------------------------------------------------------------------*/
(define (remember-eval-exported! var::symbol module::symbol loc)
   (set! *eval-exported* (cons (list var module loc) *eval-exported*)))

;*---------------------------------------------------------------------*/
;*    *all-eval?* ...                                                  */
;*---------------------------------------------------------------------*/
(define *all-eval?* #f)
(define *all-export-eval?* #f)
(define *all-module-eval?* #f)
(define *all-export-mutable?* #f)

;*---------------------------------------------------------------------*/
;*    *one-eval?* ...                                                  */
;*---------------------------------------------------------------------*/
(define *one-eval?* #f)

;*---------------------------------------------------------------------*/
;*    eval-finalizer ...                                               */
;*---------------------------------------------------------------------*/
(define (eval-finalizer)
   (if (or *one-eval?* *all-eval?* *all-export-eval?* *all-module-eval?*
	   (pair? *eval-classes*))
       (list
	(unit
	 'eval
	 (-fx (get-toplevel-unit-weight) 2)
	 (delay
	    (let ((body (get-evaluated-class-macros)))
	       (let loop ((globals (get-evaluated-globals))
			  (init*  '(#unspecified)))
		  (if (null? globals)
		      `(begin ,@body ,@(reverse! init*))
		      (let ((g (car globals)))
			 (set-eval-types! g)
			 (loop (cdr globals)
			       (cons (cond
					((svar? (global-value g))
					 (variable-access-set! g 'write)
					 (define-primop-ref->node g
					    (location->node g)))
					((scnst? (global-value g))
					 (define-primop-ref->node g
					    (location->node g)))
					(else
					 (define-primop->node g)))
				     init*)))))))
	 #f))
       'void))

;*---------------------------------------------------------------------*/
;*    set-eval-types! ...                                              */
;*    -------------------------------------------------------------    */
;*    Global variables send to eval must be obj variable. This         */
;*    function enforce that.                                           */
;*---------------------------------------------------------------------*/
(define (set-eval-types! global)
   (let ((val (global-value global)))
      (if (not (sfun? val))
	  (let ((type (global-type global)))
	     (cond
		((eq? type *_*)
		 (global-type-set! global *obj*))
		((not (bigloo-type? type))
		 (error "eval"
			"Non bigloo prototyped value can't be evaluated"
			(global-id global))))))))
   
;*---------------------------------------------------------------------*/
;*    get-evaluated-globals ...                                        */
;*---------------------------------------------------------------------*/
(define (get-evaluated-globals)
   (let ((globals (get-evaluated-classes-accesses)))
      (if (or *all-eval?* *all-export-eval?* *all-module-eval?*)
	  (let ((scope-lst (cond
			      (*all-eval?* '(import static export))
			      (*all-module-eval?* '(static export))
			      (else '(export)))))
	     (for-each-global!
	      (lambda (g)
		 (if (and (memq (global-import g) scope-lst)
			  (global-evaluable? g)
			  (or *lib-mode* (not (global-library? g))))
		     (set! globals (cons g globals)))))))
      (let loop ((eval-exported *eval-exported*)
		 (res           globals))
	 (if (null? eval-exported)
	     res
	     (let ((var.module.pos (car eval-exported)))
		(let ((g (find-global/module (car var.module.pos)
					     (cadr var.module.pos))))
		   (cond
		      ((not (global? g))
		       (user-error/location (find-location
					     (caddr var.module.pos))
					    "eval-init"
					    "Unbound eval variable"
					    (car var.module.pos)
					    '())
		       (loop (cdr eval-exported) res))
		      ((and (not *lib-mode*) (global-library? g))
		       (loop (cdr eval-exported) res))
		      ((not (global-evaluable? g))
		       (user-error/location (find-location
					     (caddr var.module.pos))
					    "eval-init"
					    "This variable cannot be known by eval"
					    (car var.module.pos)
					    '())
		       (loop (cdr eval-exported) res))
		      (else
		       (loop (cdr eval-exported) (cons g res))))))))))

;*---------------------------------------------------------------------*/
;*    get-evaluated-classes-accesses ...                               */
;*---------------------------------------------------------------------*/
(define (get-evaluated-classes-accesses)
   (let ((err '())
	 (res '()))
      (with-exception-handler
	 (lambda (e)
	    (error-notify e)
	    (with-access::&error e (obj)
	       (set! err (cons obj err))))
	 (lambda ()
	    (if (null? *eval-classes*)
		'()
		(set! res
		      (append
		       (apply append (map get-evaluated-class-accesses
					  *eval-classes*))
		       res)))))
      (if (pair? err)
	  (error 'eval "Undefined classes found" err)
	  res)))

;*---------------------------------------------------------------------*/
;*    get-evaluated-class-accesses ...                                 */
;*---------------------------------------------------------------------*/
(define (get-evaluated-class-accesses ev)
   (define (get-global id)
      (let ((g (find-global id)))
	 (if (global? g)
	     g
	     (internal-error 'eval "Can't find global access" id))))
   (define (access s id)
      (let ((i (slot-id s)))
	 (if (slot-indexed s)
	     (let* ((gidl (symbol-append id '- i '-len))
		    (gid (symbol-append id '- i '-ref))
		    (sid (symbol-append id '- i '-set!))
		    (gl (get-global gidl))
		    (g (get-global gid)))
		(if (slot-read-only? s)
		    (list gl gl)
		    (list gl g (get-global sid))))
	     (let* ((gid (symbol-append id '- i))
		    (sid (symbol-append gid '-set!))
		    (g (get-global gid)))
		(if (slot-read-only? s)
		    (list g)
		    (list g (get-global sid)))))))
   (match-case ev
      ((class ?id)
       (let ((t (find-type/location id (find-location ev))))
	  (if (not (tclass? t))
	      (user-error/location (find-location ev)
				   'eval
				   "Referenced type is not a Bigloo class"
				   id)
	      (let* ((slots (map (lambda (s) (access s id))
				 (filter (lambda (s)
					    (eq? (slot-class-owner s) t))
					 (tclass-all-slots t))))
		     (commons (cons (get-global (class-predicate t))
				    (apply append slots))))
		 (if (tclass-abstract? t)
		     commons
		     (cons* (get-global (class-make t))
			    commons))))))
      (else
       (internal-error 'eval "(eval (class ...)) malformed" ev))))

;*---------------------------------------------------------------------*/
;*    get-evaluated-class-macros ...                                   */
;*---------------------------------------------------------------------*/
(define (get-evaluated-class-macros)
   (if (null? *eval-classes*)
       '()
       (map (lambda (s)
	       (let ((t (find-type/location (cadr s) (find-location s))))
		  `(begin
		      (eval ,(eval-expand-instantiate t))
		      (eval ,(eval-expand-with-access t))
		      ,@(map (lambda (e) `(eval ,e))
			    (eval-bind-super-access t)))))
	    *eval-classes*)))

;*---------------------------------------------------------------------*/
;*    eval-expand-instantiate ...                                      */
;*---------------------------------------------------------------------*/
(define (eval-expand-instantiate t)
   (let ((id (symbol-append 'instantiate:: (tclass-id t)))
	 (args (gensym 'args)))
      `'(define-macro (,id . ,args)
	   ,(if (tclass-abstract? t)
		`(error 'eval "Abstract classes can't be instantiated" ',id)
		(eval-instantiate->fill t args)))))

;*---------------------------------------------------------------------*/
;*    eval-instantiate->fill ...                                       */
;*---------------------------------------------------------------------*/
(define (eval-instantiate->fill class args)
   (let* ((mk (class-make class))
	  (slots (tclass-all-slots class))
	  (inst (symbol-append 'instantiate:: (tclass-id class)))
	  (nodef (slot-no-default-value-mark)))
      (let loop ((slots slots)
		 (vals '()))
	 (if (null? slots)
	     (list 'quasiquote
		   `(,mk ,@(map (lambda (v) (list 'unquote v))
				(reverse! vals))))
	     (let ((s (car slots)))
		(let ((id (slot-id s)))
		   `(let ((,id (let ((c (assq ',id ,args)))
				  (if (pair? c)
				      (cadr c)
				      ,(let ((d (slot-default-value s)))
					  (if (eq? d nodef)
					      `(error ',inst
						      "argument missing"
						      ',id)
					      (list 'quote d)))))))
		       ,(loop (cdr slots) (cons id vals)))))))))

;*---------------------------------------------------------------------*/
;*    eval-expand-with-access ...                                      */
;*---------------------------------------------------------------------*/
(define (eval-expand-with-access t)
   (let ((id (symbol-append 'with-access:: (tclass-id t))))
      `'(define-expander ,id
	   (lambda (x e)
	      ,(eval-with-access->let t)))))

;*---------------------------------------------------------------------*/
;*    eval-with-access->let ...                                        */
;*---------------------------------------------------------------------*/
(define (eval-with-access->let t)
   `(match-case x
       ((?- ?i ?vars . ?body)
	,(let ((tid (symbol-append (tclass-id t) '-))
	       (slots (map (lambda (s)
			      (list (slot-id s)
				    (if (slot-indexed s) #t #f)
				    (slot-read-only? s)))
			   (tclass-all-slots t))))
	    `(let* ((inst (gensym 'instance))
		    (e1 (lambda (x e2)
			   (match-case x
			      ((set! ?s ?v)
			       (let ((d (assq s ',slots)))
				  (if (or (not d) (caddr d))
				      (e x e2)
				      (e `(,(symbol-append ',tid s '-set!) ,inst ,v) e2))))
			      (else
			       (e x e2))))))
		(e `(let* ((,inst ,i))
			,(let loop ((vars vars))
			    (if (null? vars)
				`(begin ,@body)
				(let ((d (assq (car vars) ',slots)))
				   (cond
				      ((not d)
				       `(error ',(car x)
					       "Illegal attribute"
					       ',(car vars)))
				      ((cadr d)
				       ;; an indexed slot
				       (let ((v `(,(symbol-append ',tid (car vars) '-len) ,inst))
					     (r `(lambda (r) (,(symbol-append ',tid (car vars) '-ref) ,inst r)))
					     (s `(lambda (r v) (,(symbol-append ',tid (car vars) '-set!) ,inst r v))))
					  (if (caddr d)
					      `(let ((,(symbol-append (car vars) '-len) ,v)
						     (,(symbol-append (car vars) '-ref) ,r))
						  ,(loop (cdr vars)))
					      `(let ((,(symbol-append (car vars) '-len) ,v)
						     (,(symbol-append (car vars) '-ref) ,r)
						     (,(symbol-append (car vars) '-set!) ,s))
						  ,(loop (cdr vars))))))
				      (else
				       ;; a direct slot
				       (let ((v `(,(symbol-append ',tid (car vars)) ,inst)))
					  `(let ((,(car vars) ,v))
					      ,(loop (cdr vars))))))))))
		    e1))))
       (else
	(error (car x) "Illegal form" x))))

;*---------------------------------------------------------------------*/
;*    eval-bind-super-access ...                                       */
;*---------------------------------------------------------------------*/
(define (eval-bind-super-access t)
   (let ((slots (tclass-all-slots t))
	 (id (tclass-id t)))
      (define (slot-bind s)
	 (let* ((i (slot-id s))
		(ssi (symbol-append (tclass-id (slot-class-owner s)) '- i))
		(asi (symbol-append id '- i))
		(get `'(define (,asi o) (,ssi o))))
	    (if (slot-read-only? s)
		(list get)
		(let ((sssi (symbol-append ssi '-set!))
		      (sasi (symbol-append asi '-set!)))
		   (list get `'(define (,sasi o x) (,sssi o x)))))))
      (apply append
	     (map slot-bind
		  (filter (lambda (s)
			     (not (eq? (slot-class-owner s) t)))
			  (tclass-all-slots t))))))
	  
