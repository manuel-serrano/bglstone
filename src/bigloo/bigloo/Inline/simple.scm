;*=====================================================================*/
;*    .../project/bglstone/src/bigloo/bigloo/Inline/simple.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Jun 17 14:01:30 1996                          */
;*    Last change :  Thu Mar  6 16:43:59 2025 (serrano)                */
;*    Copyright   :  1996-2025 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The inlining of simple functions (non recursive functions).      */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module inline_simple
   (include "Ast/node.sch" "Type/type.sch")
   (include "Tools/trace.sch"
	    "Tools/verbose.sch")
   (import  engine_param
	    type_type
	    type_cache
	    ast_var
	    ast_node
	    ast_local
	    tools_shape
	    tools_speek
	    tools_error
	    inline_inline
	    inline_size
	    ast_alphatize
	    ast_sexp
	    effect_effect
	    effect_spread)
   (export  (inline-app-simple::node ::node ::long ::obj ::bstring)))

;*---------------------------------------------------------------------*/
;*    inline-app-simple ...                                            */
;*    -------------------------------------------------------------    */
;*    Thanks to normalization of the ast building, all actuals         */
;*    values are placed into variables. Hence, we don't have to        */
;*    build a let construction. We just make an alpha-conversion.      */
;*---------------------------------------------------------------------*/
(define (inline-app-simple node kfactor stack msg)
   (trace (inline 2) "inline-app-simple: " (shape node) #\Newline)
   (trace (inline+ 2) "inline-app-simple: " (shape node) #\Newline)
   (let* ((callee      (var-variable (app-fun node)))
	  (sfun        (variable-value callee))
	  (formals     (sfun-args sfun))
	  (actuals     (app-args node))
	  (reductors   (map (lambda (f a)
			       (if (and (closure? a)
					(eq? (local-access f) 'read)
					(or (eq? (local-type f) *procedure*)
					    (eq? (local-type f) *_*)
					    (eq? (local-type f) *obj*)))
				   ;; We are given a closure and
				   ;; the formal binding is only read.
				   ;; Hence we do not use intermediate
				   ;; variable
				   (closure-variable a)
				   (clone-local
				    f
				    (duplicate::svar (local-value f)))))
			    formals
			    actuals))
	  (bindings    (let loop ((reductors reductors)
				  (actuals   actuals)
				  (res       '()))
			  (cond
			     ((null? actuals)
			      (reverse! res))
			     ((and (closure? (car actuals))
				   (eq? (car reductors)
					(closure-variable (car actuals))))
			      (loop (cdr reductors)
				    (cdr actuals)
				    res))
			     (else
			      (loop (cdr reductors)
				    (cdr actuals)
				    (cons (cons (car reductors)
						(car actuals))
					  res))))))
	  (body        (if (isfun? sfun)
			   (isfun-original-body sfun)
			   (sfun-body sfun)))
	  (arity       (sfun-arity sfun))
	  (new-kfactor (*inlining-reduce-kfactor* kfactor))
	  (loc         (node-loc node))
	  (type        (variable-type callee)))
      ;; some verbing small ...
      (if (not (memq (sfun-class sfun) '(sifun sgfun)))
	  (verbose 3 "         "
		   (shape callee) " --> " (current-function)
		   " (" msg #\)
		   #\Newline))
      ;; we compute the new body
      (trace (inline 3) "J'alphatize: " (shape formals) " " (shape reductors)
	     #\Newline
	     "        sur le body: " (shape body) #\Newline)
      (trace (inline+ 3) "J'alphatize: " (shape formals) " " (shape reductors)
	     #\Newline
	     "        sur le body: " (shape body) #\Newline)
      (let ((alpha-body (alphatize formals reductors loc body)))
	 ;; we spread side effect for incoming inlines (such as
	 ;; null? which is translated into c-null?).
	 (spread-side-effect! alpha-body)
	 (let ((inline-body (instantiate::let-var
			       (loc loc)
			       (type type)
			       (side-effect? (side-effect? alpha-body))
			       (bindings bindings)
			       (body (inline-node alpha-body
						  new-kfactor
						  (cons callee stack))))))
	    ;; we mark each new variable as a compiler variable
	    (for-each (lambda (binding formal)
			 (local-user?-set! (car binding) (local-user? formal)))
		      bindings
		      formals)
	    ;; if the result type of the inlined function is not *_* or
	    ;; *obj* we use a local variable in order to ensure that the
	    ;; type checking of the result will be implemented even after
	    ;; inlining. This fix a bug a the release 1.9b
 	    (if (or (eq? type *_*)
		    (eq? type *obj*)
		    (eq? type (node-type alpha-body)))
		inline-body
		(let ((var (make-local-svar (gensym 'res) type)))
		   (instantiate::let-var
		      (loc loc)
		      (type type)
		      (side-effect? (side-effect? inline-body))
		      (bindings (list (cons var inline-body)))
		      (body (instantiate::var
			       (loc loc)
			       (type type)
			       (variable var))))))))))


