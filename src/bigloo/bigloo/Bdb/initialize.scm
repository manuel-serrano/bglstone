;*=====================================================================*/
;*    .../project/bglstone/src/bigloo/bigloo/Bdb/initialize.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Apr  9 14:53:02 1998                          */
;*    Last change :  Fri Mar  7 08:38:28 2025 (serrano)                */
;*    Copyright   :  1992-2025 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The module specific Bdb runtime initialization.                  */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module bdb_initialize
   (include "Ast/node.sch" "Type/type.sch")
   (include "Tools/trace.sch"
	    "Ast/unit.sch")
   (import  tools_shape
	    tools_speek
	    tools_error
	    engine_param
	    module_module
	    type_type
	    type_cache
	    ast_var
	    ast_node
	    ast_env
	    ast_build
	    ast_lvtype
	    ast_glo-def
	    coerce_coerce 
	    cnst_alloc
	    cnst_node)
   (export  (initialize-ast)))

;*---------------------------------------------------------------------*/
;*    initialize-ast ...                                               */
;*    -------------------------------------------------------------    */
;*    Now, we have finished the ast walk, we can set the               */
;*    initialization function definition.                              */
;*---------------------------------------------------------------------*/
(define (initialize-ast)
   ;; and now on we can produce bdb initialization function
   (let ((body `((bdb-set-module-info! ',*module*
				       (pragma::obj "((obj_t)__bdb_info)")))))
      (let ((unit (unit 'bdb 9 body #t)))
	 (let ((ast (build-ast (list unit))))
	    (for-each (lambda (global)
			 (coerce! (sfun-body (global-value global))
				  global
				  (global-type global)))
		      ast)
	    ast))))

