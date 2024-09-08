;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Init/setrc.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri May 31 07:53:05 1996                          */
;*    Last change :  Mon May 15 07:27:40 2000 (serrano)                */
;*    Copyright   :  1992-2000 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The reading of the `runtime-command' file.                       */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module init_setrc
   (import engine_param
	   tools_speek)
   (export (setup-default-values)
	   (setup-library-values ::bstring)
	   (load-library-init)))

;*---------------------------------------------------------------------*/
;*    setup-default-values ...                                         */
;*---------------------------------------------------------------------*/
(define (setup-default-values)
   (let* ((path  (let ((home (getenv "HOME")))
		    (if (string? home)
			(cons home *lib-dir*)
			*lib-dir*)))
	  (fname (find-file/path ".bigloorc" path)))
      (if fname
	  (loadq fname))))

;*---------------------------------------------------------------------*/
;*    setup-library-values ...                                         */
;*---------------------------------------------------------------------*/
(define (setup-library-values library)
   (let* ((init-name (string-append library ".init"))
	  (fname     (find-file/path init-name *lib-dir*)))
      (if fname
	  (set! *library-init* (cons fname *library-init*)))))

;*---------------------------------------------------------------------*/
;*    *library-init* ...                                               */
;*    -------------------------------------------------------------    */
;*    The list of init file that will have to be loaded for libraries. */
;*---------------------------------------------------------------------*/
(define *library-init* '())

;*---------------------------------------------------------------------*/
;*    load-library-init ...                                            */
;*---------------------------------------------------------------------*/
(define (load-library-init)
   (for-each (lambda (fname)
		(verbose 2 "      [reading " fname "]" #\Newline)
		(loadq fname))
	     *library-init*)
   (set! *library-init* '()))
      


