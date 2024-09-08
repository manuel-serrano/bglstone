(module saw_unwide
   (import type_type ast_var ast_node
	   type_typeof
	   type_env
	   object_class
	   module_module )
   (export (unwide v::global)) )

(define (unwide v::global)
   (correct-var v)
   (let ( (fun (global-value v)) )
      (cond
	 ((cfun? fun)
	  (let cwalk ( (l (cfun-args-type fun)) )
	     (if (not (null? l))
		 (begin (set-car! l (real-type (car l)))
			(cwalk (cdr l)) ))))
	 ((sfun? fun)
	  (let ( (body (sfun-body fun)) )
	     (if (node? body) (follow body)) )
	  (let ( (l (sfun-args fun)) )
	     (if (and (not (null? l)) (type? (car l)))
		 (let walk ( (l l) )
		    (if (not (null? l))
			(begin (set-car! l (real-type (car l)))
			       (walk (cdr l)) )))
		 (for-each correct-var l) )))
	 (else 'ok) )))

(define (real-type t::type)
   (if (wide-class? t)
       (tclass-its-super t)
       t ))

(define (correct-var v)
   (let ( (t (variable-type v)) )
      (if (wide-class? t)
	  (variable-type-set! v (tclass-its-super t)) )))

(define (correct e::node)
   (let ( (t (typeof e)) )
      (if (wide-class? t)
	  (node-type-set! e (tclass-its-super t)) )))

;;
(define-generic (follow e::node)
   (error 'type-correction "forgotten method for"
	  (class-name (object-class e)) ))

(define-method (follow e::atom)
   'done )

(define-method (follow e::kwote)
   'done )

(define-method (follow e::var)
   'done )

(define-method (follow e::let-var)
   (correct e)
   (with-access::let-var e (bindings body)
      (for-each (lambda (b)
		   (correct-var (car b))
		   (follow (cdr b)) )
		bindings )
      (follow body) ))

(define-method (follow e::setq)
   (correct e)
   (with-access::setq e (value)
      (follow value) ))

(define-method (follow e::sequence)
   (correct e)
   (with-access::sequence e (nodes)
      (for-each follow nodes) ))

(define-method (follow e::conditional)
   (correct e)
   (with-access::conditional e (test true false)
      (follow test)
      (follow true)
      (follow false) ))

(define-method (follow e::select)
   (correct e)
   (with-access::select e (test clauses)
      (follow test)
      (for-each (lambda (c) (follow (cdr c))) clauses) ))

(define-method (follow e::let-fun)
   (correct e)
   (with-access::let-fun e (locals body)
      (for-each (lambda (v)
		   (correct-var v)
		   (for-each correct-var (sfun-args (local-value v)))
		   (follow (sfun-body (local-value v))) )
		locals )
      (follow body) ))

(define-method (follow e::app)
   (correct e)
   (with-access::app e (fun args)
      (for-each follow args) ))

(define-method (follow e::app-ly)
   (correct e)
   (with-access::app-ly e (fun arg)
      (follow fun)
      (follow arg) ))
	  
(define-method (follow e::funcall)
   (correct e)
   (with-access::funcall e (fun args strength)
      (follow fun)
      (for-each follow args) ))

(define-method (follow e::extern)
   (correct e)
   (with-access::extern e (expr*)
      (for-each follow expr*) ))

(define-method (follow e::new)
   (with-access::extern e (expr*)
      (for-each follow expr*) ))

(define-method (follow e::getfield)
   (with-access::extern e (expr*)
      (for-each follow expr*) ))

(define-method (follow e::setfield)
   (with-access::extern e (expr*)
      (for-each follow expr*) ))

(define-method (follow e::cast)
   (correct e)
   (with-access::cast e (arg)
      (follow arg) ))

(define-method (follow e::set-ex-it)
   (correct e)
   (with-access::set-ex-it e (var body)
      (follow body) ))

(define-method (follow e::jump-ex-it)
   (correct e)
   (with-access::jump-ex-it e (exit value)
      (follow exit)
      (follow value) ))

(define-method (follow e::fail)
   (correct e)
   (with-access::fail e (proc msg obj)
      (follow proc)
      (follow msg)
      (follow obj) ))

(define-method (follow e::make-box)
   (correct e)
   (with-access::make-box e (value)
      (follow value) ))

(define-method (follow e::box-ref)
   (correct e) )

(define-method (follow e::box-set!)
   (correct e)
   (with-access::box-set! e (var value)
      (follow value) ))
