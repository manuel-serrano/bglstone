;*=====================================================================*/
;*    serrano/prgm/project/bglstone/src/r7rs/bigloo.sch                */
;*    -------------------------------------------------------------    */
;*    Author      :  manuel serrano                                    */
;*    Creation    :  Sun Nov  3 12:19:09 2024                          */
;*    Last change :  Tue Nov  5 09:12:47 2024 (serrano)                */
;*    Copyright   :  2024 manuel serrano                               */
;*    -------------------------------------------------------------    */
;*    Commont script to be include in all r7rs tests                   */
;*=====================================================================*/

(define (import . l) #unspecified)
(define (scheme . l) #unspecified)
(define (base) #unspecified)
(define (file) #unspecified)
(define (cxr) #unspecified)
(define (char) #unspecified)
(define (complex) #unspecified)

(define inexact exact->inexact)
(define exact inexact->exact)

(define (exact-integer? n) (or (fixnum? n) (exact? n)))

(define (integer-length n)
   (if (bignum? n)
       (let loop ((c 1)
		  (m #z1))
	  (if (<bx m n)
	      (loop (+fx c 1) (*bx m #z2))
	      (-fx c 1)))
       (inexact->exact (ceiling (/ (log n) (log 2))))))

;; taken from mit-scheme, src/runtime/primitive-arithmetic.scm
(define (exact-integer-sqrt n)
  (if (= 0 n)
      (values 0 0)
      (let loop
	  ((i
	    (expt 2
		  (let ((n-bits (integer-length n)))
		    (if (= 0 (remainder n-bits 2))
			(quotient n-bits 2)
			(+ (quotient n-bits 2) 1))))))
	(let ((j (quotient (+ i (quotient n i)) 2)))
	  (if (>= j i)
	      (values i (- n (* i i)))
	      (loop j))))))

(define (square x) (* x x))

(define (real-part o) o)
(define (imag-part o) o)

(define (numerator o) 1)
(define (denominator o) 1)

(define (hide a b)
   (car (list b)))

(define (call-with-current-continuation proc)
   (bind-exit (exit)
      (proc exit)))

(define-expander error
   (lambda (x e)
      (match-case x
	 ((?a) (e `((@ error __error) "bigloo" ,a ,#f) e))
	 ((?a ?b) (e `((@ error __error) "bigloo" ,a ,b) e))
	 ((?a ?b ?c) (e `((@ error __error) ,a ,b ,c) e))
	 ((?a ?b ?c . ?-) (e `((@ error __error) ,a ,b ,c) e))
	 (else (e `((@ error __error) ,a ,b ,c) e)))))

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
