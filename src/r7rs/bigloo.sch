;*=====================================================================*/
;*    serrano/prgm/project/bglstone/src/r7rs/bigloo.sch                */
;*    -------------------------------------------------------------    */
;*    Author      :  manuel serrano                                    */
;*    Creation    :  Sun Nov  3 12:19:09 2024                          */
;*    Last change :  Sun Nov  3 12:47:35 2024 (serrano)                */
;*    Copyright   :  2024 manuel serrano                               */
;*    -------------------------------------------------------------    */
;*    Commont script to be include in all r7rs tests                   */
;*=====================================================================*/

(define (import . l) #unspecified)
(define (scheme . l) #unspecified)
(define (base) #unspecified)

(define inexact exact->inexact)
(define exact inexact->exact)

(define (hide a b)
   (car (list b)))

(define (run-r7rs-benchmark name count thunk ok?)
   (display* name "...")
   (flush-output-port (current-output-port))
   (let loop ((i 0)
	      (result #f))
      (cond
	 ((<fx i count) (loop (+fx i 1) (thunk)))
	 ((ok? result) (print "ok") (exit 0))
	 (else (print "error") (exit 1)))))
	 
(define (main x)
   (if (pair? (cdr x))
       (if (string=? (cadr x) "pmem")
           (with-input-from-string (pmem) run-benchmark)
           (with-input-from-string (cadr x) run-benchmark))
       (with-input-from-file (default-input) run-benchmark)))
