;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Ast/unit.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Jun  3 08:35:53 1996                          */
;*    Last change :  Thu Apr  3 14:10:30 2003 (serrano)                */
;*    -------------------------------------------------------------    */
;*    A module is composed of several unit (for instance, the user     */
;*    unit (also called the toplevel unit), the foreign unit, the      */
;*    constant unit, ...). This module takes in charge the production  */
;*    of an ast for a unit.                                            */
;*    -------------------------------------------------------------    */
;*    This module does not build node. Nodes can't be until, all       */
;*    body has been scanned and all global definitions seens. This     */
;*    is done only when all unit have been converted.                  */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module ast_unit
   (include "Ast/unit.sch"
	    "Ast/node.sch"
	    "Tools/trace.sch")
   (import  ast_find-gdefs
	    ast_glo-def
	    ast_ident
	    ast_env
	    ast_local
	    ast_sexp
	    object_class
	    object_generic
	    object_method
	    tools_progn
	    tools_args
	    tools_misc
	    tools_speek
	    tools_shape
	    tools_location
	    tools_error
	    tools_dsssl
	    engine_param
	    module_module
	    module_class
	    module_include
	    type_cache)
   (export  (unit-sexp*-add! <unit> ::obj)
	    (unit->defs      <unit>)
	    (unit-initializers)
	    (unit-initializer-id id)
	    (unit-init-calls)))

;*---------------------------------------------------------------------*/
;*    unit-sexp*-add! ...                                              */
;*---------------------------------------------------------------------*/
(define (unit-sexp*-add! unit sexp)
   (if (null? (unit-sexp* unit))
       (unit-sexp*-set! unit sexp)
       (set-cdr! (last-pair (unit-sexp* unit)) sexp)))

;*---------------------------------------------------------------------*/
;*    unit->defs ...                                                   */
;*---------------------------------------------------------------------*/
(define (unit->defs unit)
   (verbose 2
	    "      [" (string-downcase (symbol->string (unit-id unit)))
	    "]" #\Newline)
   (trace ast "unit->defs: " unit #\Newline)
   (trace (ast 2) "  " (unit-sexp* unit) #\Newline)
   (let ((id     (unit-id unit))
	 (weight (unit-weight unit))
	 (sexp*  (unit-sexp* unit)))
      ;; we now compute the global definitions
      (let loop ((aexp* (if (procedure? sexp*)
			    (force sexp*)
			    (toplevel*->ast sexp* (find-global-defs sexp*))))
		 (init* '())
		 (def*  '()))
	 (if (null? aexp*)
	     (if (pair? init*)
		 (let ((init (def-global-sfun-no-warning!
				(symbol-append (unit-initializer-id id) '::obj)
				'()
				'()
				*module*
				'snifun
				*module-clause*
				'cgen
				(normalize-progn (reverse! init*)))))
		    ;; all init functions but the toplevel ones are
		    ;; compiler functions.
		    (if (not (eq? unit (get-toplevel-unit)))
			(global-user?-set! init #f))
		    ;; we declare the unit for the late module
		    ;; initialization (unit initializer are called
		    ;; in order they are declared).
		    (declare-unit! id weight)
		    ;; init functions cannot be sent to eval
		    (global-evaluable?-set! init #f)
		    ;; we are done, we return now a list of
		    ;; global function definitions.
		    (cons init (reverse! def*)))
		 (reverse! def*))
	     (if (global? (car aexp*))
		 (loop (cdr aexp*)
		       init*
		       (cons (car aexp*) def*))
		 (loop (cdr aexp*)
		       (cons (car aexp*) init*)
		       def*))))))
 
;*---------------------------------------------------------------------*/
;*    *unit-list* ...                                                  */
;*---------------------------------------------------------------------*/
(define *unit-list* '())

;*---------------------------------------------------------------------*/
;*    declare-unit! ...                                                */
;*---------------------------------------------------------------------*/
(define (declare-unit! id::symbol weight::long)
   (if (or (null? *unit-list*) (<fx weight (cdr (car *unit-list*))))
       (set! *unit-list* (cons (cons id weight) *unit-list*))
       (let loop ((ulist *unit-list*))
	  (cond
	     ((<fx weight (cdr (car ulist)))
	      (set-cdr! ulist (cons (car ulist) (cdr ulist)))
	      (set-car! ulist (cons id weight)))
	     ((null? (cdr ulist))
	      (set-cdr! ulist (list (cons id weight))))
	     (else
	      (loop (cdr ulist)))))))

;*---------------------------------------------------------------------*/
;*    unit-initializer-id ...                                          */
;*---------------------------------------------------------------------*/
(define (unit-initializer-id id)
   (symbol-append id '-init))

;*---------------------------------------------------------------------*/
;*    unit-initializers ...                                            */
;*---------------------------------------------------------------------*/
(define (unit-initializers)
   (map (lambda (unit)
	   (find-global/module (unit-initializer-id (car unit)) *module*))
	*unit-list*))

;*---------------------------------------------------------------------*/
;*    unit-init-calls ...                                              */
;*---------------------------------------------------------------------*/
(define (unit-init-calls)
   (map (lambda (unit) `((@ ,(unit-initializer-id (car unit)) ,*module*)))
	*unit-list*))

;*---------------------------------------------------------------------*/
;*    toplevel*->ast ...                                               */
;*---------------------------------------------------------------------*/
(define (toplevel*->ast::pair-nil sexp*::pair gdefs)
   (let loop ((sexp* sexp*)
	      (aexp* '()))
      (if (null? sexp*)
	  (reverse! aexp*)
	  (loop (cdr sexp*)
		(append (toplevel->ast (car sexp*) gdefs) aexp*)))))
 
;*---------------------------------------------------------------------*/
;*    toplevel->ast ...                                                */
;*    -------------------------------------------------------------    */
;*    !!! WARNING: this function must build a reversed list !!!        */
;*---------------------------------------------------------------------*/
(define (toplevel->ast sexp gdefs)
   (match-case sexp
      ((begin)
       (list sexp))
      ((begin . ?nsexp*)
       (reverse! (toplevel*->ast nsexp* gdefs)))
      ((define (?var . ?args) . ?exp)
       (let* ((id     (id-of-id var (find-location sexp)))
	      (def    (assq id gdefs))
	      (global (find-global/module id *module*)))
	  ;; exported variable are set to be written hence, we don't
	  ;; have to check here if the variable has been declared has
	  ;; exported (as a variable vs a function). We just have to
	  ;; check if it is written.
	  ;; We may not find global in def because it can has been
	  ;; introduced after the computation of gdefs (for instance
	  ;; if it is a default method of a generic).
	  (if (or (not (pair? def))
		  (and (eq? (car (cdr def)) 'read)
		       (or (not (global? global))
			   (eq? (global-access global) 'read))))
	      (make-sfun-definition var
				    *module*
				    args
				    (normalize-progn/error exp
							   sexp
							   (find-location
							    (cddr sexp)))
				    sexp
				    'sfun)
	      (let ((new-sexp `(set! ,var (lambda ,args ,@exp))))
		 (replace! sexp new-sexp)
		 (make-svar-definition var sexp)))))
      ((define ?var (lambda ?args . ?exp))
       (let* ((id     (id-of-id var (find-location sexp)))
	      (def    (assq id gdefs))
	      (global (find-global/module id *module*)))
	  ;; same remark as in the previous match (variables vs functions)
	  (if (and (eq? (car (cdr def)) 'read)
		   (or (not (global? global))
		       (eq? (global-access global) 'read)))
	      (make-sfun-definition var
				    *module*
				    args
				    (normalize-progn/error exp
							   sexp
							   (find-location
							    (cddr sexp)))
				    sexp
				    'sfun)
	      (make-svar-definition var sexp))))
      ((define ?var (begin ?1-exp))
       (set-car! (cddr sexp) 1-exp)
       (toplevel->ast sexp gdefs))
      ((define ?var (and (? symbol?) ?var2))
       (let ((def (assq (id-of-id var (find-location sexp)) gdefs)))
	  (if (eq? (car (cdr def)) 'read)
	      (let ((arity (get-global-arity var2 #f gdefs)))
		 (if (fixnum? arity)
		     (let ((def (eta-expanse sexp arity)))
			(toplevel->ast def gdefs))
		     (make-svar-definition var sexp)))
	      (make-svar-definition var sexp))))
      ((define ?var (@ (and (? symbol?) ?var2) (and (? symbol?) ?module)))
       (let ((def (assq (id-of-id var (find-location sexp)) gdefs)))
	  (if (eq? (car (cdr def)) 'read)
	      (let ((arity (get-global-arity var2 module gdefs)))
		 (if (fixnum? arity)
		     (let ((def (eta-expanse sexp arity)))
			(toplevel->ast def gdefs))
		     (make-svar-definition var sexp)))
	      (make-svar-definition var sexp))))
      ((define ?var . ?exp)
       (make-svar-definition var sexp))
      ((define-inline ((@ ?var ?module) . ?args) . ?exp)
       (make-sfun-definition var
			     module
			     args
			     (normalize-progn/error exp
						    sexp
						    (find-location (cddr sexp)))
			     
			     sexp
			     'sifun))
      ((define-inline (?var . ?args) . ?exp)
       (make-sfun-definition var
			     *module*
			     args
			     (normalize-progn/error exp
						    sexp
						    (find-location (cddr sexp)))
			     sexp
			     'sifun))
      ((define-generic ((@ ?var ?module) . ?args) . ?exp)
       (make-sgfun-definition var
			      module
			      args
			      exp
			      sexp
			      gdefs))
      ((define-generic (?var . ?args) . ?exp)
       (make-sgfun-definition var
			      *module*
			      args
			      exp
			      sexp
			      gdefs))
      ((define-method (?var . ?args) . ?exp)
       (make-method-definition var
			       args
			       (normalize-progn/error exp
						      sexp
						      (find-location (cddr sexp)))
			       sexp))
      (else
       (list sexp))))

;*---------------------------------------------------------------------*/
;*    normalize-progn/error ...                                        */
;*---------------------------------------------------------------------*/
(define (normalize-progn/error exp src loc)
   (if (null? exp)
       (error-sexp->node "Illegal '() expression" src (find-location src))
       (let ((exp (normalize-progn exp)))
	  (cond
	     ((not loc)
	      exp)
	     ((epair? exp)
	      exp)
	     ((pair? exp)
	      (econs (car exp) (cdr exp) loc))
	     (else
	      (econs 'begin (list exp) loc))))))

;*---------------------------------------------------------------------*/
;*    get-global-arity ...                                             */
;*---------------------------------------------------------------------*/
(define (get-global-arity id module gdefs)
   (let ((global (if (symbol? module)
		     (find-global/module id module)
		     (find-global id))))
      (if (not (global? global))
	  #f
	  (if (fun? (global-value global))
	      (fun-arity (global-value global))
	      #f))))

;*---------------------------------------------------------------------*/
;*    eta-expanse ...                                                  */
;*---------------------------------------------------------------------*/
(define (eta-expanse sexp arity)
   (let ((args (make-n-proto arity)))
      (define (do-eta-expanse/module var id2 module)
	 (cond
	    ((>=fx arity 0)
	     `(define ,(cons var args)
		 ((@ ,id2 ,module) ,@args)))
	    ((=fx arity -1)
	     `(define ,(cons var args)
		 (apply (@ ,id2 ,module) ,args)))
	    (else
	     `(define ,(cons var args)
		 (apply (@ ,id2 ,module)
			(cons* ,@(args*->args-list args)))))))
      (define (do-eta-expanse var id2)
	 (cond
	    ((>=fx arity 0)
	     `(define ,(cons var args)
		 (,id2 ,@args)))
	    ((=fx arity -1)
	     `(define ,(cons var args)
		 (apply ,id2 ,args)))
	    (else
	     `(define ,(cons var args)
		 (apply ,id2 (cons* ,@(args*->args-list args)))))))
      (match-case sexp
	 ((define ?var1 ?var2)
	  (let ((id2 (id-of-id var2 (find-location sexp))))
	     (replace! sexp (do-eta-expanse var1 id2))))
	 ((define ?var1 (@ ?var2 ?module2))
	  (let* ((id2 (id-of-id var2 (find-location sexp))))
	     (replace! sexp (do-eta-expanse/module var2 id2 module2)))))))
	  
;*---------------------------------------------------------------------*/
;*    make-sfun-definition ...                                         */
;*---------------------------------------------------------------------*/
(define (make-sfun-definition id::symbol module::symbol args body src class)
   (let* ((loc      (find-location src))
	  (locals   (let loop ((args  args)
			       (res   '()))
		       (cond
			  ((null? args)
			   (reverse! res))
			  ((not (pair? args)) 
			   (let* ((pid  (check-id (parse-id args loc) src))
				  (id   (car pid))
				  (type (cdr pid)))
			      ;; there is no need to check the last
			      ;; n-ary formal argument because it will
			      ;; be checked when defining the global variable
			      (reverse! (cons (make-user-local-svar id type)
					      res))))
			  ((dsssl-named-constant? (car args))
			   (let ((arg (dsssl-find-first-formal args)))
			      (if arg
				  (reverse!
				   (cons (make-user-local-svar arg *obj*) res))
				  (reverse! res))))
			  (else
			   (let* ((pid  (check-id (parse-id (car args) loc)
						  src))
				  (id   (car pid))
				  (type (cdr pid)))
			      (loop (cdr args)
				    (cons (if (user-symbol? id)
					      (make-user-local-svar id type)
					      (make-local-svar id type))
					  res)))))))
	  (body     (make-dsssl-function-prelude id args body user-error)))
      (list (def-global-sfun! id args locals module class src 'now body))))

;*---------------------------------------------------------------------*/
;*    make-svar-definition ...                                         */
;*---------------------------------------------------------------------*/
(define (make-svar-definition id src)
   (def-global-svar! id *module* src 'now)
   ;; without the Inline global variable optimization once should except
   ;; to find here `(set-car! src 'set!)'
   (set-car! (cdr src) (car (check-id (parse-id id (find-location src)) src)))
   (list src))

;*---------------------------------------------------------------------*/
;*    make-sgfun-default ...                                           */
;*---------------------------------------------------------------------*/
(define (make-sgfun-default name type args body src gdefs)
   (trace ast "make-sgfun-default: " name " " args " " body #\Newline)
   (let* ((default-id   name)
	  (default-tid  (if (eq? type *_*)
			    default-id
			    (make-typed-ident default-id (type-id type))))
	  (default-body (if (pair? body)
			    (normalize-progn body)
			    `(error ',name
				    "No method for this object"
				    ,(id-of-id (car args)
					       (find-location src)))))
	  (form (let ((tmp `(define ,(cons default-tid args) ,default-body)))
		   (if (epair? *module-clause*)
		       (econs (car tmp) (cdr tmp) (cer *module-clause*))
		       tmp))))
      (trace (ast 2) "  le body: " default-body #\Newline)
      (let ((ast (toplevel->ast form gdefs)))
	 (trace (ast 2) "  l'ast: " ast #\Newline)
	 (if (pair? ast)
	     (if (global? (car ast))
		 ;; we mark the function as a user one so the default
		 ;; function will appears as is in the profiler or the
		 ;; debugger.
		 (global-user?-set! (car ast) #t)))
	 ast)))

;*---------------------------------------------------------------------*/
;*    make-sgfun-definition ...                                        */
;*---------------------------------------------------------------------*/
(define (make-sgfun-definition id module args body src gdefs)
   (trace ast "make-sgfun-definition: " id " " module " " args " " body
	  #\Newline)
   (let* ((loc    (find-location src))
	  (locals (if (null? args)
		      (user-error id
				  "Illegal generic definition (first argument missing)"
				  src)
		      (let loop ((args args)
				 (res  '()))
			 (cond
			    ((null? args)
			     (reverse! res))
			    ((not (pair? args))
			     (let* ((pid  (check-id (parse-id args loc) src))
				    (id   (car pid))
				    (type (cdr pid)))
				;; there is no need to check the last
				;; n-ary formal argument because it will
				;; be checked when defining the global variable
				(reverse! (cons (make-user-local-svar id type)
						res))))
			    (else
			     (let* ((pid  (check-id (parse-id (car args) loc)
						    src))
				    (id   (car pid))
				    (type (cdr pid)))
				(loop (cdr args)
				      (cons (make-user-local-svar id type)
					    res))))))))
	  (pid     (check-id (parse-id id loc) src))
	  (name    (car pid))
	  (type    (cdr pid))
	  (dname   (gensym (symbol-append name '-default)))
	  (default (if (eq? module *module*)
		       (make-sgfun-default dname type args body src gdefs)
		       '()))
	  (body    (make-generic-body id locals args src))
	  (generic (def-global-sfun! id
		      args
		      locals
		      module
		      'sgfun
		      src
		      'now
		      body)))
      (trace (ast 2) "  body: " body #\Newline)
      (let ((o-unit (get-method-unit))
	    (sexp*  (cons generic
			  (append (list `(add-generic! (@ ,(global-id generic)
							  ,module)
						       (@ ,dname ,*module*)))
				  default))))
	 (if (not (unit? o-unit))
	     sexp*
	     (begin
		(unit-sexp*-add! o-unit sexp*)
		(list #unspecified))))))
	 	
;*---------------------------------------------------------------------*/
;*    make-method-definition ...                                       */
;*---------------------------------------------------------------------*/
(define (make-method-definition id args body src)
   (let* ((loc    (find-location src))
	  (locals (let loop ((args args)
			     (res  '()))
		     (cond
			((null? args)
			 (reverse! res))
			((not (pair? args))
			 (let* ((pid  (check-id (parse-id args loc) src))
				(id   (car pid))
				(type (cdr pid)))
			    ;; there is no need to check the last
			    ;; n-ary formal argument because it will
			    ;; be checked when defining the global variable
			    (reverse! (cons (make-local-svar id type) res))))
			(else
			 (let* ((pid  (check-id (parse-id (car args) loc) src))
				(id   (car pid))
				(type (cdr pid)))
			    (loop (cdr args)
				  (cons (make-local-svar id type) res))))))))
      (if (not (check-method-definition id args	locals src))
	  (list #unspecified)
	  (let ((o-unit (get-method-unit))
		(sexp*  (make-method-body id args locals body src)))
	     (if (not (unit? o-unit))
		 sexp*
		 (begin
		    (unit-sexp*-add! o-unit sexp*)
		    (list #unspecified)))))))
	     
