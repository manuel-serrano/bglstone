;*=====================================================================*/
;*    .../prgm/project/bglstone/src/bigloo/bigloo/Tools/trace.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Jun 15 15:04:42 1992                          */
;*    Last change :  Fri Mar  7 08:35:55 2025 (serrano)                */
;*    Copyright   :  1992-2025 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The trace management                                             */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    Le module                                                        */
;*---------------------------------------------------------------------*/
(module tools_trace
   (include "Ast/node.sch" "Type/type.sch")
   (import engine_param
	   object_slots
	   tools_error)
   (export (start-trace level pass)
	   (stop-trace)
	   *trace-port*
	   (trace-satisfy? mask level)
	   (print-trace . args)))

;*---------------------------------------------------------------------*/
;*    L'initialisation des variables locales                           */
;*---------------------------------------------------------------------*/
(define *trace-port* #f)
(define *trace-pass* 'none)
(define *level*      0)
(define *trace-mode* #t)

;*---------------------------------------------------------------------*/
;*    start-trace ...                                                  */
;*---------------------------------------------------------------------*/
(define (start-trace level pass)
   (if *trace-mode*
       (let ((trace-name *trace-name*))
	  (set! *trace-pass* pass)
	  (set! *trace-port* (open-output-file trace-name))
	  (if (not (output-port? *trace-port*))
	      (internal-error "start-trace"
			      "Can't open trace file"
			      trace-name)
	      (set! *level* level)))))
      
;*---------------------------------------------------------------------*/
;*    stop-trace ...                                                   */
;*---------------------------------------------------------------------*/
(define (stop-trace)
   (if *trace-mode*
       (if (output-port? *trace-port*)
	   (close-output-port *trace-port*))))

;*---------------------------------------------------------------------*/
;*    trace-satisfy? ...                                               */
;*---------------------------------------------------------------------*/
(define (trace-satisfy? pass level)
   (and *trace-mode*
	(or (eq? pass *trace-pass*) (memq pass *additional-traces*))
	(<=fx level *level*)))
	      
;*---------------------------------------------------------------------*/
;*    print-trace ...                                                  */
;*---------------------------------------------------------------------*/
(define (print-trace . exp)
   (if *trace-mode*
       (begin
	  (for-each (lambda (e) (display-circle e *trace-port*)) exp)
	  (flush-output-port *trace-port*))))
       
	 
