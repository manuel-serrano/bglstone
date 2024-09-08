;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Prof/unit.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Apr  8 17:21:15 1998                          */
;*    Last change :  Mon May 15 07:59:37 2000 (serrano)                */
;*    Copyright   :  1998-2000 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The debugging information code production.                       */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module prof_walk
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


