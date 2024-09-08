(module saw_prelude
   (import type_type ast_var ast_node
	   type_typeof
	   type_env
	   module_module )
   (export (type-correction v::global)) )

(define (type-correction v::global)
   (let ( (r (prelude (sfun-body (global-value v)))) )
      (if (not (eq? (global-import v) 'export))
	  ;; Change only private function prototype
	  (correct-var v r) )))

(define (correct-var v type)
   (let ( (otype (variable-type v)) )
      (if (not (eq? otype type))
	  (if (eq? otype (find-type 'procedure))
	      (begin
		 ;(print "\t" (variable-id v) " " (type-id otype)
		;	" -> " (type-id type) )
		 (variable-type-set! v type) )))))

;;
(define-generic (prelude e::node)
   (error 'type-correction "forgotten method for"
	  (class-name (object-class e)) ))

(define-method (prelude e::atom)
   (typeof e) )

(define-method (prelude e::var)
   (typeof e) )

(define-method (prelude e::let-var)
   (with-access::let-var e (bindings body)
      (for-each (lambda (b) (correct-var (car b) (prelude (cdr b))))
		bindings )
      (prelude body) ))

(define-method (prelude e::setq)
   (with-access::setq e (value)
      (prelude value) ))

(define-method (prelude e::sequence)
   (with-access::sequence e (nodes)
      (for-each prelude nodes)
      (typeof e) ))

(define-method (prelude e::conditional)
   (with-access::conditional e (test true false)
      (prelude test)
      (prelude true)
      (prelude false) ))

(define-method (prelude e::select)
   (with-access::select e (test clauses)
      (prelude test)
      (for-each (lambda (c) (prelude (cdr c))) clauses)
      (typeof e) ))

(define-method (prelude e::let-fun)
   (with-access::let-fun e (locals body)
      (for-each (lambda (v)
		   (correct-var v (prelude (sfun-body (local-value v)))) )
		locals )
      (prelude body) ))

(define-method (prelude e::app)
   (with-access::app e (fun args)
      (let ( (v (var-variable fun)) )
	 (check-args-params v args)
	 (if (local? v)
	     (typeof e)
	     (let ( (id (global-id v)) (vect (find-type 'vector))
				       (obj (find-type 'obj)) )
		(cond
		   ((eq? id 'make-el-procedure)
		    (global-type-set! v vect)
		    vect )
		   ((eq? id 'procedure-el-ref)
		    (set-car! (cfun-args-type (global-value v)) vect)
		    obj )
		   ((eq? id 'procedure-el-set!)
		    (set-car! (cfun-args-type (global-value v)) vect)
		    (typeof e) )
		   ((eq? id 'make-el-procedure-1)
		    (global-type-set! v obj)
		    obj )
		   ((eq? id 'procedure-1-el-ref)
		    (set-car! (cfun-args-type (global-value v)) obj)
		    obj )
		   ((eq? id 'procedure-1-el-set!)
		    (set-car! (cfun-args-type (global-value v)) obj)
		    (typeof e) )
		   (else (typeof e)) ))))))

(define (check-args-params v l)
   (if (local? v)
       (for-each prelude l)
       (with-access::global v (module value)
	  (if (and (eq? module *module*) (sfun? value))
	      (begin
		 ;(print "calling " (global-id v))
		 (for-each correct-var (sfun-args value) (map prelude l)) )
	      (for-each prelude l) ))))

(define-method (prelude e::app-ly)
   (with-access::app-ly e (fun arg)
      (prelude fun)
      (prelude arg)
      (typeof e) ))
	  
(define-method (prelude e::funcall)
   (with-access::funcall e (fun args strength)
      (if (eq? strength 'elight)
	  (begin (check-args-params (var-variable fun) args)
		 (typeof e) )
	  (begin (prelude fun)
		 (for-each prelude args)
		 (typeof e) ))))

(define-method (prelude e::extern)
   (with-access::extern e (expr*)
      (for-each (lambda (e) (prelude e)) expr*)
      (typeof e) ))

(define-method (prelude e::cast)
   (with-access::cast e (arg)
      (prelude arg)
      (typeof e) ))

(define-method (prelude e::set-ex-it)
   (with-access::set-ex-it e (var body)
      (prelude body)
      (typeof e) ))

(define-method (prelude e::jump-ex-it)
   (with-access::jump-ex-it e (exit value)
      (prelude exit)
      (prelude value)
      (typeof e) ))

(define-method (prelude e::fail)
   (with-access::fail e (proc msg obj)
      (prelude proc)
      (prelude msg)
      (prelude obj)
      (typeof e) ))

(define-method (prelude e::make-box)
   (with-access::make-box e (value)
      (prelude value)
      (typeof e) ))

(define-method (prelude e::box-ref)
   (typeof e) )

(define-method (prelude e::box-set!)
   (with-access::box-set! e (var value)
      (prelude value)
      (typeof e) ))

      
(module msil_prelude
   (export (type-correction
