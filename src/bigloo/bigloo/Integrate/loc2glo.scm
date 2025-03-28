;*=====================================================================*/
;*    .../project/bglstone/src/bigloo/bigloo/Integrate/loc2glo.scm     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Mar 15 17:29:48 1995                          */
;*    Last change :  Fri Mar  7 07:33:43 2025 (serrano)                */
;*    Copyright   :  1995-2025 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    We translate a local function definition into a global one.      */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module integrate_local->global
   (include "Ast/node.sch" "Type/type.sch" "Integrate/iinfo.sch")
   (include "Tools/trace.sch")
   (import  tools_shape
	    tools_args
	    module_module
	    type_type
	    type_cache
	    ast_var
	    ast_node
	    ast_glo-def
	    ast_env
	    ast_local
	    integrate_info
	    integrate_node)
   (export  (local->global ::local)
	    (the-global    ::local)))

;*---------------------------------------------------------------------*/
;*    local->global ...                                                */
;*---------------------------------------------------------------------*/
(define (local->global local)
   (trace (integrate 2) (shape local) #\: #\Newline)
   (let* ((global   (the-global local))
	  (kaptured (sfun/Iinfo-kaptured (local-value local)))
	  (add-args (map (lambda (old)
			    (clone-local old
					 (duplicate::svar/Iinfo
					       (local-value old))))
			 kaptured))
	  (old-fun  (local-value local))
	  (new-fun  (duplicate::sfun old-fun
		       (arity (+-arity (sfun-arity old-fun) (length add-args)))
		       (args (append (reverse add-args)
				     (sfun-args old-fun))))))
      ;; we set the result type
      (global-type-set! global (local-type local))
      (for-each (lambda (l)
		   (if (integrate-celled? l)
		       (local-type-set! l *obj*)))
		(sfun-args new-fun))
      (sfun-body-set! new-fun
		      (globalize! (sfun-body old-fun)
				  local
				  (map cons kaptured add-args)))
      (global-value-set! global new-fun)
      global))

;*---------------------------------------------------------------------*/
;*    symbol-quote ...                                                 */
;*---------------------------------------------------------------------*/
(define symbol-quote (string->symbol "'"))

;*---------------------------------------------------------------------*/
;*    local-id->global-id ...                                          */
;*    -------------------------------------------------------------    */
;*    Generates a new global name for the globalized local function.   */
;*---------------------------------------------------------------------*/
(define (local-id->global-id local)
   (let ((id (local-id local)))
      (let loop ((id (local-id local)))
	 (if (global? (find-global/module id *module*))
	     (loop (symbol-append id symbol-quote))
	     id))))

;*---------------------------------------------------------------------*/
;*    the-global ...                                                   */
;*---------------------------------------------------------------------*/
(define (the-global local::local)
   (let ((value (local-value local)))
      (if (global? (sfun/Iinfo-global value))
	  (sfun/Iinfo-global value)
	  (let* ((id     (local-id->global-id local))
		 (global (def-global-sfun-no-warning! id
			   ;; we set dummy empty args-id 
			   ;; and dummy empty args because a new-fun
			   ;; will be allocated.
			   '()
			   '()
			   *module*
			   'sfun
			   'a-integrated-body
			   'now
			   #unspecified)))
	     ;; we have to propagate the location definition
	     ;; of the local variable
	     (sfun-loc-set! (global-value global) (sfun-loc value))
	     ;; we check if the function is a user one
	     (if (not (local-user? local))
		 (global-user?-set! global #f))
	     (sfun/Iinfo-global-set! value global)
	     (sfun-side-effect?-set! (global-value global)
				     (sfun-side-effect? value))
	     global))))
