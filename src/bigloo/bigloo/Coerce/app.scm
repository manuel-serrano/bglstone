;*=====================================================================*/
;*    .../prgm/project/bglstone/src/bigloo/bigloo/Coerce/app.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jan 19 11:51:05 1995                          */
;*    Last change :  Fri Mar  7 07:31:45 2025 (serrano)                */
;*    Copyright   :  1995-2025 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    A little module which implement application arity checks.        */
;*=====================================================================*/
 
;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module coerce_app
   (include "Ast/node.sch" "Type/type.sch")
   (include "Tools/trace.sch")
   (import  engine_param
	    tools_shape
	    type_type
	    type_cache
	    ast_var
	    ast_node
	    coerce_coerce
	    coerce_convert))

;*---------------------------------------------------------------------*/
;*    coerce! ::app ...                                                */
;*---------------------------------------------------------------------*/
(define-method (coerce! node::app caller to)
   (trace (coerce 2) "coerce-app!: " (shape node) " -> " (shape to)
	  #\Newline)
   (let ((fun (var-variable (app-fun node))))
      (if (and (global? fun) (cfun? (variable-value fun)))
	  (coerce-foreign-app! fun caller node to)
	  (coerce-bigloo-app! fun caller node to))))

;*---------------------------------------------------------------------*/
;*    coerce-foreign-app! ...                                          */
;*---------------------------------------------------------------------*/
(define (coerce-foreign-app! callee::variable caller node to)
   (trace (coerce 2) "coerce-foreign-app!: " (shape node) " -> " (shape to)
	  #\Newline)
   (let* ((ffun  (variable-value callee))
	  (arity (fun-arity ffun)))
      (if (>=fx arity 0)
	  (coerce-foreign-fx-app! ffun callee caller node to)
	  (coerce-foreign-va-app! ffun callee caller node to))))

;*---------------------------------------------------------------------*/
;*    coerce-foreign-fx-app! ...                                       */
;*---------------------------------------------------------------------*/
(define (coerce-foreign-fx-app! fun callee::variable caller node to)
   (let loop ((actuals (app-args node))
	      (types   (cfun-args-type fun)))
      (if (null? actuals)
	  (convert! node (variable-type callee) to)
	  (begin
	     (set-car! actuals (coerce! (car actuals) caller (car types)))
	     (loop (cdr actuals) (cdr types))))))

;*---------------------------------------------------------------------*/
;*    coerce-foreign-va-app! ...                                       */
;*---------------------------------------------------------------------*/
(define (coerce-foreign-va-app! fun callee::variable caller node to)
   (let loop ((actuals (app-args node))
	      (types   (cfun-args-type fun))
	      (counter (fun-arity fun)))
      (if (=fx counter -1)
	  ;; this is the formals of a foreign va-args
	  (let loop ((actuals actuals))
	     (if (null? actuals)
		 (convert! node (variable-type callee) to)
		 (begin
		    (set-car! actuals (coerce! (car actuals)
					       caller
					       (car types)))
		    (loop (cdr actuals)))))
	  (begin
	     (set-car! actuals (coerce! (car actuals)
					caller
					(car types)))
	     (loop (cdr actuals) (cdr types) (+fx counter 1))))))

;*---------------------------------------------------------------------*/
;*    coerce-bigloo-app! ...                                           */
;*---------------------------------------------------------------------*/
(define (coerce-bigloo-app! callee::variable caller node to)
   (trace (coerce 2) "coerce-bigloo-app!: " (shape node) " "
	  (shape (variable-type callee))
	  "(" (shape (node-type node)) ") -> " (shape to) #\Newline)
   (if (and (global? callee)
	    (eq? (global-import callee) 'import)
	    (pair? (sfun-args (variable-value callee)))
	    (type? (car (sfun-args (variable-value callee)))))
       (coerce-bigloo-extern-app! callee caller node to)
       (coerce-bigloo-intern-app! callee caller node to)))

;*---------------------------------------------------------------------*/
;*    coerce-bigloo-intern-app! ...                                    */
;*---------------------------------------------------------------------*/
(define (coerce-bigloo-intern-app! callee::variable caller node to)
   (let* ((fun   (variable-value callee))
	  (arity (sfun-arity fun))
	  (sh    (shape callee)))
      (let loop ((actuals (app-args node))
		 (formals (sfun-args fun)))
	 [assert (actuals formals sh) (=fx (length actuals) (length formals))]
	 (if (null? actuals)
	     (if (and (eq? caller callee) (not *unsafe-type*))
		 ;; As suggested by J.G Malecki, there is no need to
		 ;; type check the result of a self recursive call
		 (begin
		    (set! *unsafe-type* #t)		    
		    (let ((res (convert! node (variable-type callee) to)))
		       (set! *unsafe-type* #f)
		       res))
		 (convert! node (variable-type callee) to))
	     (let ((type (local-type (car formals))))
		(set-car! actuals (coerce! (car actuals)
					   caller
					   type))
		(loop (cdr actuals) (cdr formals)))))))

;*---------------------------------------------------------------------*/
;*    coerce-bigloo-extern-app! ...                                    */
;*---------------------------------------------------------------------*/
(define (coerce-bigloo-extern-app! callee::variable caller node to)
   (let* ((fun (variable-value callee))
	  (arity (sfun-arity fun)))
      (let loop ((actuals (app-args node))
		 (formals (sfun-args fun)))
	 (if (null? actuals)
	     (convert! node (variable-type callee) to)
	     (let ((type (car formals)))
		(set-car! actuals (coerce! (car actuals)
					   caller
					   type))
		(loop (cdr actuals) (cdr formals)))))))
