;*=====================================================================*/
;*    .../prgm/project/bglstone/src/bigloo/bigloo/Integrate/u.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Mar 15 10:30:52 1995                          */
;*    Last change :  Fri Mar  7 07:34:12 2025 (serrano)                */
;*    Copyright   :  1995-2025 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The `U' property                                                 */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module integrate_u
   (include "Ast/node.sch" "Type/type.sch" "Integrate/iinfo.sch")
   (include "Tools/trace.sch")
   (import  tools_shape
	    type_type
	    ast_var
	    ast_node
	    integrate_info
	    integrate_a)
   (export  (U!)))

;*---------------------------------------------------------------------*/
;*    U! ...                                                           */
;*---------------------------------------------------------------------*/
(define (U!)
   (let loop ((Phi *phi*))
      (if (null? Phi)
	  (trace-U)
	  (let* ((p    (car Phi))
		 (ifun (variable-value p)))
	     (sfun/Iinfo-U-set! ifun (=fx (length (sfun/Iinfo-K* ifun)) 1))
	     (loop (cdr Phi))))))

;*---------------------------------------------------------------------*/
;*    trace-U ...                                                      */
;*---------------------------------------------------------------------*/
(define (trace-U)
   (trace (integrate 2)
	  (begin
	     (fprint *trace-port* "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
	     (fprint *trace-port* "U: " #\Newline)
	     (for-each (lambda (p)
			  (let ((ifun (variable-value p)))
			     (fprint *trace-port*
				     " --> " (shape p) #\:
				     (sfun/Iinfo-U ifun))))
		       *phi*)
	     "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
	  #\Newline))
