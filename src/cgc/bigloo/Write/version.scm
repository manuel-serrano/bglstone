;*=====================================================================*/
;*    serrano/uni/97-98/maitrise/compil/cgc/Write/version.scm          */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Jan 26 09:30:07 1998                          */
;*    Last change :  Fri Jan 30 09:31:49 1998 (serrano)                */
;*    -------------------------------------------------------------    */
;*    Release outputing                                                */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module write_version 
   (import engine_param
	   tools_speek
	   tools_date)
   (export (version)
	   (short-version)))

;*---------------------------------------------------------------------*/
;*    short-version ...                                                */
;*---------------------------------------------------------------------*/
(define (short-version)
   (print *cgc-name* (if (char? *cgc-level*)
			 (let ((s " (level 0)"))
			    (string-set! s 8 *cgc-level*)
			    s)
			 "")))

;*---------------------------------------------------------------------*/
;*    version ...                                                      */
;*---------------------------------------------------------------------*/
(define (version)
   (display-to-column (string-append *cgc-name*
				     (if (char? *cgc-level*)
					 (let ((s " (level 0) "))
					    (string-set! s 8 *cgc-level*)
					    s)
					 " ")
				     (cgc-date))
		      78
		      #\space)
   (newline))

;*---------------------------------------------------------------------*/
;*    display-to-column ...                                            */
;*---------------------------------------------------------------------*/
(define (display-to-column string column char)
   (display string)
   (let loop ((l (+fx 1 (string-length string))))
      (if (>=fx l column)
	  'done
	  (begin
	     (write-char char)
	     (loop (+fx l 1))))))   
      
