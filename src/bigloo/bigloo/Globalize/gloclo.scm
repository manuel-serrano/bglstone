;*=====================================================================*/
;*    .../project/bglstone/src/bigloo/bigloo/Globalize/gloclo.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Feb  3 09:56:11 1995                          */
;*    Last change :  Fri Mar  7 07:32:42 2025 (serrano)                */
;*    Copyright   :  1995-2025 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The global closure creation                                      */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module globalize_global-closure
   (include "Ast/node.sch" "Type/type.sch")
   (include "Tools/trace.sch")
   (import  tools_shape
	    tools_args
	    tools_error
	    engine_param
	    type_type
	    type_cache
	    ast_var
	    ast_node
	    ast_sexp
	    ast_local
	    ast_glo-def
	    ast_ident
	    module_module
	    globalize_ginfo
	    globalize_node
	    globalize_free)
    (export (global-closure::global      ::global <loc>)
	    (make-global-closure::global ::global)
	    (foreign-closures)))

;*---------------------------------------------------------------------*/
;*    global-closure ...                                               */
;*---------------------------------------------------------------------*/
(define (global-closure global loc)
   (the-global-closure global loc)
   (make-global-closure global))
      
;*---------------------------------------------------------------------*/
;*    make-global-closure ...                                          */
;*---------------------------------------------------------------------*/
(define (make-global-closure global)
   (let ((glo (global/Ginfo-global-closure global)))
      (if (global? glo)
	  glo
	  (let* ((old-fun  (global-value global))
		 (env      (let ((var (make-local-svar 'env *procedure*)))
			      (widen!::local/Ginfo var)
			      var))
		 (new-args (map (lambda (old)
				   (let ((new (make-local-svar
					       (if (local? old)
						   (local-id old)
						   (gensym))
					       *obj*)))
				      (if (local? old)
					  (local-user?-set! new
							    (local-user? old)))
				      (widen!::svar/Ginfo (local-value new))
				      (widen!::local/Ginfo new)
				      new))
				(if (sfun? old-fun)
				    (sfun-args old-fun)
				    (if (cfun? old-fun)
					;; cfun-args-type is a list of type
					;; not a list of local. It doesn't
					;; matter. What is important here
					;; is just the list.
					(cfun-args-type old-fun)
					(internal-error "make-global-closure"
							"Unexpected value"
							old-fun)))))
		 (loc      (if (sfun? old-fun)
			       (if (and (node? (sfun-body old-fun))
					(node-loc (sfun-body old-fun)))
				   (node-loc (sfun-body old-fun))
				   (sfun-loc old-fun))))
		 (gloclo   (gloclo global env new-args))
		 (new-fun  (global-value gloclo)))
	     ;; we must set now the info slot of env
	     (widen!::svar/Ginfo (local-value env))
	     ;; we ajust the function definition
	     (widen!::global/Ginfo gloclo (escape? #t))
	     (sfun-body-set! new-fun
			     (instantiate::app
				(loc loc)
				(type *obj*)
				(fun (instantiate::var
					(loc loc)
					(type *_*)
					(variable global)))
				;; we have to ignore the addition environment
				;; parameters, so we just take the cdr of the
				;; formals list.
				(args (map (lambda (v)
					      (instantiate::var
						 (loc loc)
						 (type *obj*)
						 (variable v)))
					   new-args))))
	     (trace (globalize 2) "=======> J'ai cree le corps:"
		    (shape (sfun-body new-fun))
		    #\Newline)
	     gloclo))))

;*---------------------------------------------------------------------*/
;*    *foreign-closures* ...                                           */
;*---------------------------------------------------------------------*/
(define *foreign-closures* '())

;*---------------------------------------------------------------------*/
;*    foreign-closures ...                                             */
;*---------------------------------------------------------------------*/
(define (foreign-closures)
   (let ((res *foreign-closures*))
      (set! *foreign-closures* '())
      res))
   
;*---------------------------------------------------------------------*/
;*    gloclo ...                                                       */
;*---------------------------------------------------------------------*/
(define (gloclo global env::local args)   
   (let* ((arity  (fun-arity (global-value global)))
	  (id     (let ((str (string-append
			      "_"
			      (symbol->string (global-id global)))))
		     (if (symbol-exists? str)
			 (gensym (symbol-append '_ (global-id global)))
			 (symbol-append '_ (global-id global)))))
	  (gloclo (def-global-sfun-no-warning! (make-typed-ident id 'obj)
		     (make-n-proto (+-arity arity 1))
		     (cons env args)
		     (if (eq? (global-import global) 'foreign)
			 *module*
			 (global-module global))
		     (global-import global)
		     'sfun
		     'now
		     #unspecified)))
      (global/Ginfo-global-closure-set! global gloclo)
      ;; we have to propagate the location definition
      ;; of the local variable
      (if (sfun? (global-value global))
	  (sfun-loc-set! (global-value gloclo)
			 (sfun-loc (global-value global))))
      ;; I made a modification here at Tue Oct 27 08:05:04.
      ;; Closure function are now always static. I don't know I did
      ;; it different before. If this turn out to be wrong, I will
      ;; be necessary to restore the old code that was:
      ;;     (global-import-set! gloclo (global-import global))
      ;; For that modification I have added a new kind of removable
      ;; property: NEVER.
      ;; The Cgen prototyper (Cgen/proto.scm) checks for value prior
      ;; to decide not to emit a C function prototype.
      (if (eq? (global-import global) 'foreign)
	  (begin
	     (set! *foreign-closures* (cons gloclo *foreign-closures*))
	     (global-removable-set! gloclo 'never)
	     (global-import-set! gloclo 'static))
	  (begin
	     (if (not (eq? (global-import global) 'static))
		 (global-removable-set! gloclo 'never))
	     (global-import-set! gloclo 'static)))
      (fun-side-effect?-set! (global-value gloclo)
			     (fun-side-effect? (global-value global)))
      (if (not (global? gloclo))
	  (internal-error "global-closure"
			  "Can't allocate global closure"
			  gloclo)
	  gloclo)))
