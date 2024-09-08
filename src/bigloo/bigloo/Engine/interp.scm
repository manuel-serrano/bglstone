;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Engine/interp.scm           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri May 31 08:03:39 1996                          */
;*    Last change :  Mon Jun 23 15:20:08 2003 (serrano)                */
;*    Copyright   :  1996-2003 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The inner interpreter.                                           */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module engine_interp
   (import engine_param)
   (export (interp version verbose file startup path args)))

;*---------------------------------------------------------------------*/
;*    interp ...                                                       */
;*---------------------------------------------------------------------*/
(define (interp version verbose files startup path args)
   (set! *bigloo-interpreter* #t)
   (if (and (>=fx verbose 0) (not (pair? files)))
       (begin
	  (version)
	  (print "Welcome to the interpreter")))
   ;; on definie la variable `(command-line)'
   (set! *the-command-line* (interp-parse-args args))
   ;; on charge le fichier de startup
   (if (string? startup)
       (let ((path (let ((home (getenv "HOME")))
		      (if (string? home)
			  (cons home path)
			  path))))
	  (let ((fstartup (find-file/path startup path)))
	     (if fstartup
		 (loadq startup)
		 (warning "interp"
			  #\Newline
			  "Can't file startup file -- " startup)))))
   ;; on rentre dans l'interprete
   (if (pair? files)
       (for-each (lambda (f) (if (string? f) (loadq f) (load-stdin))) files)
       (repl))
   0)

;*---------------------------------------------------------------------*/
;*    load-stdin ...                                                   */
;*---------------------------------------------------------------------*/
(define (load-stdin)
   (let loop ((e (read)))
      (if (not (eof-object? e))
	  (begin
	     (eval e)
	     (loop (read))))))

;*---------------------------------------------------------------------*/
;*    interp-parse-args ...                                            */
;*---------------------------------------------------------------------*/
(define (interp-parse-args args)
   (let loop ((args (cdr args))
	      (res  '()))
      (cond
	 ((null? args)
	  (append *src-files* (reverse res)))
	 ((string=? (car args) "-i")
	  (loop (cdr args) res))
	 ((member (car args) *src-files*)
	  (loop (cdr args) res))
	 (else
	  (loop (cdr args) (cons (car args) res))))))
			   

