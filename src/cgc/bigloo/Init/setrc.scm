;*=====================================================================*/
;*    serrano/uni/97-98/maitrise/compil/cgc/Init/setrc.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Jul 11 11:49:35 1995                          */
;*    Last change :  Mon Jan 26 09:22:54 1998 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The runtime-command file loading.                                */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    Le module                                                        */
;*---------------------------------------------------------------------*/
(module init_setrc
   (import engine_param)
   (export (setup-default-values)))

;*---------------------------------------------------------------------*/
;*    setup-default-values ...                                         */
;*---------------------------------------------------------------------*/
(define (setup-default-values)
   (if (file-exists? ".cgcrc")
       (loadq ".cgcrc")
       (let ((home (getenv "HOME")))
	  (if (string? home)
	      (begin
		 (let ((cgcrc (string-append home "/.cgcrc")))
		    (if (file-exists? cgcrc)
			(loadq cgcrc)
			'done)))))))


		     



