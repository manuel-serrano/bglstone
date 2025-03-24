;*=====================================================================*/
;*    .../prgm/project/bglstone/src/bigloo/bigloo/Coerce/walk.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jan 19 09:43:37 1995                          */
;*    Last change :  Fri Mar  7 07:31:35 2025 (serrano)                */
;*    Copyright   :  1995-2025 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    We perform now coercions.                                        */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    the module                                                       */
;*---------------------------------------------------------------------*/
(module coerce_walk
   (include "Ast/node.sch" "Type/type.sch")
   (include "Engine/pass.sch")
   (import  tools_speek
	    tools_shape
	    tools_error
	    type_type
	    type_cache
	    engine_param
	    ast_var
	    ast_env
	    ast_node
	    ast_remove
	    coerce_pproto
	    coerce_coerce)
   (export  (coerce-walk! ast)))

;*---------------------------------------------------------------------*/
;*    coerce-walk! ...                                                 */
;*---------------------------------------------------------------------*/
(define (coerce-walk! ast)
   (pass-prelude "Coercions & Checks")
   (for-each (lambda (global)
		(reset-ppmarge!)
		(enter-function (global-id global))
		(coerce-function! global)
		(leave-function))
	     ast)
   (reset-ppmarge!)
   (for-each-global! (lambda (global)
			(if (and (not (fun? (global-value global)))
				 (or (eq? (global-import global) 'static)
				     (eq? (global-import global) 'export)))
			    (pvariable-proto 3 global))))
   (pass-postlude (remove-var 'coerce ast)))

