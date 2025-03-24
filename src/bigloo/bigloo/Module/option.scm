;*=====================================================================*/
;*    .../project/bglstone/src/bigloo/bigloo/Module/option.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Feb 28 10:20:55 1998                          */
;*    Last change :  Thu Mar  6 16:33:19 2025 (serrano)                */
;*    Copyright   :  1998-2025 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The compiler option clause compilation                           */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module module_option
   (include "Ast/node.sch")
   (include "Ast/unit.sch")
   (import  module_module
	    tools_error
	    engine_param)
   (export  (make-option-compiler)))

;*---------------------------------------------------------------------*/
;*    make-option-compiler ...                                         */
;*---------------------------------------------------------------------*/
(define (make-option-compiler)
   (instantiate::ccomp
      (id 'option)
      (producer option-producer)))

;*---------------------------------------------------------------------*/
;*    option-producer ...                                              */
;*---------------------------------------------------------------------*/
(define (option-producer clause)
   (match-case clause
      ((?- . ?protos)
       (for-each eval protos)
       '())
      (else
       (user-error "Parse error" "Illegal `option' clause" clause '()))))
