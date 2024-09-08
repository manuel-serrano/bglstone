;*=====================================================================*/
;*    .../prgm/project/bglstone/src/cgc/bigloo/Ast/location.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Jan 27 09:14:37 1998                          */
;*    Last change :  Tue Nov 24 16:08:31 2015 (serrano)                */
;*    -------------------------------------------------------------    */
;*    Location managment                                               */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module ast_location

   (import engine_param)

   (include "Ast/location.sch")
   
   (static (class source
	      fname::bstring
	      point::bint))
   
   (export (make-location ::bstring ::bint)
	   (source-error  ::obj ::obj ::obj)))

;*---------------------------------------------------------------------*/
;*    make-location ...                                                */
;*---------------------------------------------------------------------*/
(define (make-location fname point)
   (instantiate::source
      (fname fname)
      (point point)))
	     
;*---------------------------------------------------------------------*/
;*    source-error ...                                                 */
;*---------------------------------------------------------------------*/
(define (source-error loc msg obj)
   (if (source? loc)
       (with-access::source loc (fname point)
	  (error/location *cgc-name* msg obj fname point))
       (error *cgc-name* msg obj)))
