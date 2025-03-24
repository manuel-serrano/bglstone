;*=====================================================================*/
;*    .../prgm/project/bglstone/src/bigloo/bigloo/Prof/unit.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Apr  8 17:21:15 1998                          */
;*    Last change :  Fri Mar  7 08:38:47 2025 (serrano)                */
;*    Copyright   :  1998-2025 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The debugging information code production.                       */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module prof_walk
   (include "Ast/node.sch" "Type/type.sch")
   (include "Engine/pass.sch"
	    "Ast/unit.sch")
   (import  tools_shape
	    tools_error
	    engine_param
	    type_type
	    module_include
	    ast_var
	    ast_node)
   (export  (make-prof-unit)))

;*---------------------------------------------------------------------*/
;*    make-prof-unit ...                                               */
;*---------------------------------------------------------------------*/
(define (make-prof-unit)
   (pass-prelude "Prof")
   (unit 'prof
	 (+ 100 (get-toplevel-unit-weight))
	 '(begin (pragma "write_bprof_table()")) #t))


