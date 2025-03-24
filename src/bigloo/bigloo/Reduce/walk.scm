;*=====================================================================*/
;*    .../prgm/project/bglstone/src/bigloo/bigloo/Reduce/walk.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jul 13 10:25:23 1995                          */
;*    Last change :  Fri Mar  7 08:23:38 2025 (serrano)                */
;*    Copyright   :  1995-2025 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The Reduction optimizations                                      */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module reduce_walk
   (include "Engine/pass.sch")
   (import  tools_shape
	    tools_error
	    engine_param
	    type_type
	    tvector_tvector
	    object_class
	    ast_var
	    ast_node
	    reduce_copy
	    reduce_cse
	    reduce_cond
	    reduce_typec
	    reduce_1occ
	    reduce_beta
	    ast_remove)
   (export  (reduce-walk! globals . obj)))

;*---------------------------------------------------------------------*/
;*    reduce-walk ...                                                  */
;*---------------------------------------------------------------------*/
(define (reduce-walk! globals . type-unsafe)
   (cond
      ((and (pair? type-unsafe) (car type-unsafe))
       (pass-prelude "Reduction (trivial)")
       (reduce-1occ! globals)
       (pass-postlude (remove-var 'reduce globals)))
      (*optim-dataflow?*
       (pass-prelude "Reduction")
       (reduce-copy! globals)
       (when (>=fx *optim* 2)
	  (reduce-cse! globals))
       (reduce-type-check! globals)
       (reduce-copy! globals)
       (reduce-conditional! globals)
       (reduce-1occ! globals)
       (when *optim-reduce-beta?*
	  (reduce-beta! globals))
       (pass-postlude (remove-var 'reduce globals)))
      (else
       globals)))
