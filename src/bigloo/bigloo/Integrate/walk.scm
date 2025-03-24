;*=====================================================================*/
;*    .../project/bglstone/src/bigloo/bigloo/Integrate/walk.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Feb 21 08:37:48 1995                          */
;*    Last change :  Fri Mar  7 07:34:28 2025 (serrano)                */
;*    Copyright   :  1995-2025 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The `integration' pass.                                          */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module integrate_walk
   (include "Ast/node.sch" "Type/type.sch")
   (include "Engine/pass.sch")
   (import  tools_error
	    engine_pass
	    type_type
	    ast_var
	    ast_remove
	    integrate_info
	    integrate_definition)
   (export  (integrate-walk! globals)))

;*---------------------------------------------------------------------*/
;*    integrate-walk! ...                                              */
;*---------------------------------------------------------------------*/
(define (integrate-walk! globals)
   (pass-prelude "Integration")
   (let loop ((old globals)
	      (new '()))
      (if (null? old)
	  (pass-postlude (remove-var '(integrate cfa) new))
	  (let ((global (car old)))
	     (enter-function (global-id global))
	     (loop (cdr old)
		   (append (integrate-definition! global) new))))))

