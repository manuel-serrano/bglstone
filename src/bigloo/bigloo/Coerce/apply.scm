;*=====================================================================*/
;*    .../prgm/project/bglstone/src/bigloo/bigloo/Coerce/apply.scm     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Jan 20 17:21:26 1995                          */
;*    Last change :  Fri Mar  7 07:31:54 2025 (serrano)                */
;*    Copyright   :  1995-2025 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The `apply' coercion                                             */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module coerce_apply
   (include "Ast/node.sch" "Type/type.sch")
   (include "Tools/trace.sch"
	    "Tools/location.sch")
   (import  engine_param
	    tools_shape
	    tools_location
	    tools_error
	    type_type
	    type_cache
	    ast_var
	    ast_node
	    ast_sexp
	    ast_local
	    coerce_coerce
	    coerce_convert))

;*---------------------------------------------------------------------*/
;*    coerce! ::app-ly ...                                             */
;*---------------------------------------------------------------------*/
(define-method (coerce! node::app-ly caller to)
   (trace coerce "coerce-apply!: " (shape node) #\Newline)
   (let ((error-msg (list 'quote (shape node))))
      ;; we coerce the arguments
      (app-ly-arg-set! node (coerce! (app-ly-arg node) caller *obj*))
      ;; we coerce the procedure
      (let ((c-fun (coerce! (app-ly-fun node) caller *procedure*)))
	 ;; we check arity
	 (if *unsafe-arity*
	     (begin
		(if (var? c-fun)
		    (begin
		       (app-ly-fun-set! node c-fun)
		       (convert! node *obj* to))
		    (let ((fun (make-local-svar 'fun *procedure*)))
		       (app-ly-fun-set! node (instantiate::var
						(loc (node-loc c-fun))
						(type *procedure*)
						(variable fun)))
		       (instantiate::let-var
			  (loc (node-loc node))
			  (type (node-type node))
			  (bindings (list (cons fun c-fun)))
			  (body (convert! node *obj* to))))))
	     (let* ((fun   (make-local-svar 'fun *procedure*))
		    (val   (make-local-svar 'val *pair-nil*))
		    (loc   (node-loc node))
		    (lval  (top-level-sexp->node `(length ,val) loc))
		    (len   (gensym 'len))
		    (lnode (instantiate::let-var
			      (loc loc)
			      (type *obj*)
			      (bindings (list (cons fun c-fun)
					      (cons val (app-ly-arg node))))
			      (body     (top-level-sexp->node
					 `(let ((,(symbol-append len '::int)
						 ,(coerce! lval caller *int*)))
					     (if (correct-arity? ,fun ,len)
						 ,(convert! node *obj* to)
						 ,(make-error-node error-msg
								   loc
								   caller 
								   to)))
					 loc)))))
		   ;; we set the new apply value
		(app-ly-fun-set! node (instantiate::var
					(loc loc)
					(type *procedure*)
					(variable fun)))
		(app-ly-arg-set! node (instantiate::var
					(loc loc)
					(type *obj*)
					(variable val)))
		lnode)))))

;*---------------------------------------------------------------------*/
;*    make-error-node ...                                              */
;*---------------------------------------------------------------------*/
(define (make-error-node error-msg loc caller to)
   (let ((ut *unsafe-type*))
      (set! *unsafe-type* #t)
      (let ((node (coerce!
		   (top-level-sexp->node 
		    (if (and (or (>fx *bdb-debug* 0)
				 (>fx *compiler-debug* 0))
			     (location? loc))
			`(begin
			    ((@ error/location __error)
			     ,(list 'quote (current-function))
			     "Wrong number of arguments"
			     ,error-msg
			     ,(location-full-fname loc)
			     ,(location-pos loc))
			    (failure '_ '_ '_))
			`(failure ,(list 'quote (current-function))
				  "Wrong number of arguments"
				  ,error-msg))
		    loc)
		   caller
		   to)))
	 (set! *unsafe-type* ut)
	 node)))

