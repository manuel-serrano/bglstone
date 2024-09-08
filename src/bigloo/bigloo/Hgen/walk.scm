;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Hgen/walk.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Feb 21 08:37:48 1995                          */
;*    Last change :  Tue Sep 16 12:11:42 2003 (serrano)                */
;*    Copyright   :  1995-2003 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The `C generation' pass.                                         */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module hgen_walk
   (include "Engine/pass.sch")
   (import  tools_error
	    type_type
	    object_class
	    ast_var
	    ast_node
	    backend_c_emit)
   (export  (hgen-walk)))

;*---------------------------------------------------------------------*/
;*    hgen-walk ...                                                    */
;*---------------------------------------------------------------------*/
(define (hgen-walk)
   (pass-prelude "C headers generation" (lambda () (start-emission! ".h")))
   
   ;; a very little comment 
   (emit-header)
   ;; we emit the generated type for the classes
   (emit-class-types *c-port*)
   
   (stop-emission!))
