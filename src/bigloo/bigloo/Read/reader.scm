;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Read/reader.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Nov 20 06:48:17 1998                          */
;*    Last change :  Mon May 15 08:00:31 2000 (serrano)                */
;*    Copyright   :  1998-2000 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The implementation of the function that physically reads on      */
;*    input ports.                                                     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module read_reader
   (import engine_param)
   (export (compiler-read . args)))

;*---------------------------------------------------------------------*/
;*    compiler-read ...                                                */
;*---------------------------------------------------------------------*/
(define (compiler-read . args)
   (let ((value (apply read args)))
      (if (eq? *reader* 'intern)
	  (cond
	     ((eof-object? value)
	      value)
	     ((string? value)
	      (string->obj value))
	     (else
	      (error "" "Illegal intern value" value)))
	  value)))
