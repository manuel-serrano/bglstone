;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Init/extend.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Oct 28 08:37:41 1994                          */
;*    Last change :  Mon May 15 07:27:19 2000 (serrano)                */
;*    Copyright   :  1992-2000 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    La lecture des extensions (isole'es pour ne pas avoir le pbm     */
;*    des PRIMOPs trop nombreuses).                                    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    Le module                                                        */
;*---------------------------------------------------------------------*/
(module init_extend
    (export (load-extend ::bstring))
    (import engine_param))

;*---------------------------------------------------------------------*/
;*    load-extend ...                                                  */
;*---------------------------------------------------------------------*/
(define (load-extend extend-name)
   (let ((fname (find-file/path extend-name *lib-dir*)))
      (if fname
	  (loadq fname)
	  (error "parse-args" "Can't find extend file" extend-name))))
