;*=====================================================================*/
;*    .../prgm/project/bglstone/src/bigloo/bigloo/Cfa/funcall.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Jun 25 07:47:42 1996                          */
;*    Last change :  Fri Mar  7 07:46:50 2025 (serrano)                */
;*    Copyright   :  1996-2025 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The funcall management.                                          */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module cfa_funcall
   (include "Ast/node.sch" "Type/type.sch" "Cfa/cinfo.sch" "Cfa/cinfo2.sch" "Cfa/cinfo3.sch")
   (include "Tools/trace.sch")
   (import  tools_shape
	    tools_args
	    tools_error
	    type_type
	    type_cache
	    ast_var
	    ast_node
	    cfa_info
	    cfa_info2
	    cfa_info3
	    cfa_cfa
	    cfa_loose
	    cfa_approx
	    cfa_app))

;*---------------------------------------------------------------------*/
;*    cfa! ...                                                         */
;*    -------------------------------------------------------------    */
;*    First, we compute the possible function called. Then for         */
;*    each of them, we compute the result of evaluating its            */
;*    body with the actual approximations.                             */
;*    -------------------------------------------------------------    */
;*    In this function, we always ignore the first argument which      */
;*    is the closure itself. Instead, it uses the approximation        */
;*    computed by the Cfa.                                             */
;*---------------------------------------------------------------------*/
(define-method (cfa! node::funcall/Cinfo)
   (trace (cfa 2) "  funcall: " (shape node) #\Newline)
   (with-access::funcall/Cinfo node (approx fun args)
      (let* ((fun-approx  (cfa! fun))
	     (args-approx (map cfa! (cdr args))))
	 (trace (cfa 2)
	     "      fun: " (shape fun-approx) #\Newline
	     "     args: " (shape args-approx) #\Newline)
	 ;; we check for a possible type error
	 (let ((fun-type (approx-type fun-approx)))
	    (if (and (not (eq? fun-type *_*))
		     (not (eq? fun-type *obj*))
		     (not (eq? fun-type *procedure*)))
		(funcall-type-error node fun-type)))
	 ;; we check the type...
	 (if (or (not (eq? (approx-type fun-approx) *procedure*))
		 (approx-top? fun-approx))
	     (approx-set-type! approx *obj*))
	 ;; and we compute the approximations
	 (if (or (approx-top? fun-approx)
		 (not (eq? (approx-type fun-approx) *procedure*)))
	     (begin
		(for-each (lambda (approx) (loose! approx 'all)) args-approx)
		(for-each-approx-alloc
		 (lambda (alloc)
		    (if (make-procedure-app? alloc)
			(let ((env-approx (make-procedure-app-approx alloc)))
			   (union-approx! approx
					  (funcall! alloc
						    (cons env-approx
							  args-approx)
						    node)))
			(make-empty-approx)))
		 fun-approx)
		(approx-set-top! approx))
	     (for-each-approx-alloc
	      (lambda (alloc)
		 (if (make-procedure-app? alloc)
		     (let ((env-approx (make-procedure-app-approx alloc)))
			(union-approx! approx
				       (funcall! alloc
						 (cons env-approx args-approx)
						 node)))
		     (make-empty-approx)))
	      fun-approx))
	 (trace (cfa 2) "       ->: " (shape approx) #\Newline)
	 approx)))

;*---------------------------------------------------------------------*/
;*    funcall! ...                                                     */
;*---------------------------------------------------------------------*/
(define (funcall!::approx alloc::make-procedure-app args-approx node)
   (let* ((callee (car (make-procedure-app-args alloc)))
	  (v      (var-variable callee))
	  (fun    (variable-value v))
	  (arity  (fun-arity fun)))
      (trace (cfa 3) " funcall!: " (shape callee) " arity: " arity " "
	     (shape args-approx) #\Newline)
      (cond 
	 ((not (sound-arity? arity args-approx))
	  ;; arity error
	  (funcall-arity-error node v arity args-approx))
	 ((>=fx arity 0)
	  ;; fix arity call
	  (trace (cfa 3) "   ->app!: " #\Newline)
	  (app! fun callee args-approx))
	 (else
	  ;; va arity call
	  (let loop ((old-args-approx args-approx)
		     (new-args-approx '())
		     (arity           arity))
	     (if (=fx arity -1)
		 (begin
		    (for-each (lambda (approx) (loose! approx 'all))
			      old-args-approx)
		    (app! fun
			  callee
			  (reverse! (cons (funcall/Cinfo-va-approx node)
					  new-args-approx))))
		 (loop (cdr old-args-approx)
		       (cons (car old-args-approx) new-args-approx)
		       (+fx arity 1))))))))

;*---------------------------------------------------------------------*/
;*    funcall-type-error ...                                           */
;*---------------------------------------------------------------------*/
(define (funcall-type-error node type)
   (trace (cfa 3) " *** type error *** " (shape node) #\Newline)
   (with-access::funcall/Cinfo node (type-error-noticed? loc)
      (if (not type-error-noticed?)
	  (begin
	     (set! type-error-noticed? #t)
	     (user-warning/location loc
				    "cfa"
				    "Possible funcall type error"
				    (shape type))))))

;*---------------------------------------------------------------------*/
;*    funcall-arity-error ...                                          */
;*---------------------------------------------------------------------*/
(define (funcall-arity-error node v arity args-approx)
   (with-access::funcall/Cinfo node (arity-error-noticed? loc)
      (if (not arity-error-noticed?)
	  (let ((len-prov (-fx (length args-approx) 1)))
	     (set! arity-error-noticed? #t)
	     (user-warning/location loc
				    ;; we duplicate to avoid the printing
				    ;; of the approximation informations.
				    (shape (cond
					      ((local? v)
					       (duplicate::local v))
					      ((global? v)
 					       (duplicate::global v))
					      (else
					       v)))
				    "Possible funcall arity error"
				    (string-append
				     (integer->string (-fx arity 1))
				     " arg(s) expected, "
				     (integer->string len-prov)
				     " provided")))))
   (trace (cfa 3) " *** arity error *** " (shape node) #\Newline)
   (make-empty-approx))
     
			  
