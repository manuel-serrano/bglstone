;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Engine/engine.scm           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Mar 17 10:13:23 1993                          */
;*    Last change :  Tue Sep 16 11:57:57 2003 (serrano)                */
;*    Copyright   :  1993-2003 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The system's engine (some kind of dispatcher between the linker, */
;*    the interpreter and the compiler).                               */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    Le module                                                        */
;*---------------------------------------------------------------------*/
(module engine_engine
   (export (engine)
	   (hello-world))
   (import tools_speek
	   tools_trace
	   tools_error
	   write_version
           engine_param
	   engine_compiler
	   engine_interp
	   engine_link
	   backend_c))

;*---------------------------------------------------------------------*/
;*    engine ...                                                       */
;*---------------------------------------------------------------------*/
(define (engine)
   (cond
      ((and (pair? *src-files*) (not *interpreter*))
       (profile comp (compiler)))
      ((pair? *c-files*)
       (if (pair? (cdr *c-files*))
	   (user-warning "engine" "Ignoring additional files" (cdr *c-files*)))
       (set! *rm-tmp-files* #f)
       (profile ccomp (cc-compiler (prefix (car *c-files*))
				   (if (string? *dest*)
				       (prefix *dest*)
				       #f))))
      ((null? *o-files*)
       (profile interp (interp version
			       *verbose*
			       *src-files*
			       *startup-file*
			       (string-append (car *lib-dir*) "/scheme-files")
			       *bigloo-args*)))
      (else
       (link))))

;*---------------------------------------------------------------------*/
;*    hello-world ...                                                  */
;*---------------------------------------------------------------------*/
(define (hello-world)
   (if *hello*
       (let loop ((src (reverse! *src-files*))
		  (str ""))
	  (if (null? src)
	      (begin
		 (string-set! str (-fx (string-length str) 1) #\:)
		 (verbose 0 str #\Newline))
	      (loop (cdr src)
		    (string-append (if (string? (car src))
				       (car src)
				       (symbol->string (car src)))
				   " "
				   str))))))


