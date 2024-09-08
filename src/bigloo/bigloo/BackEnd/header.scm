;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/BackEnd/header.scm          */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Mar 16 18:14:47 1995                          */
;*    Last change :  Mon Jan 28 16:02:17 2002 (serrano)                */
;*    Copyright   :  1995-2002 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The emission of the bigloo header                                */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module backend_header
   (import  engine_param
	    ;engine_configure
	    tools_license
	    backend_backend
	    )
   (export  (emit-header backend::backend)))

;*---------------------------------------------------------------------*/
;*    *max-col* ...                                                    */
;*---------------------------------------------------------------------*/
(define *max-col* 79)

;*---------------------------------------------------------------------*/
;*    emit-comment ...                                                 */
;*---------------------------------------------------------------------*/
(define (emit-comment backend string fill)
   (let ((string (if (>fx (string-length string) (-fx *max-col* 8))
		     (substring string 0 (-fx *max-col* 9))
		     string)))
      (backend-comment
       backend
       (let ((len (string-length string)))
	  (if (=fx len 0)
	      (make-string (-fx *max-col* 4) fill)
	      (string-append
	       (make-string 2 fill)
	       " "
	       string
	       " "
	       (make-string (-fx *max-col* (+ 8 len)) fill)))))))

;*---------------------------------------------------------------------*/
;*    emit-license ...                                                 */
;*---------------------------------------------------------------------*/
(define (emit-license backend)
   (let ((in (open-input-string (bigloo-license))))
      (let loop ((str (read-line in)))
	 (if (eof-object? str)
	     (close-input-port in)
	     (begin
		(emit-comment backend str #\space)
		(loop (read-line in)))))))

;*---------------------------------------------------------------------*/
;*    emit-header ...                                                  */
;*---------------------------------------------------------------------*/
(define (emit-header backend::backend)
   (emit-comment backend "" #\=)
   (emit-comment backend
		 (let ((p (open-output-string)))
		    (display *src-files* p)
		    (close-output-port p))
		 #\space)
   (emit-comment backend *bigloo-name* #\space)
   (emit-comment backend
		 (string-append *bigloo-author* " (c)      " *bigloo-date*)
		 #\space)
   (if *bigloo-licensing?* (emit-license backend))
   (emit-comment backend "" #\=))
