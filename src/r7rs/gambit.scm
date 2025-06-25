;; File: bglstone/src/r7rs/gambit.scm

(declare (standard-bindings) (extended-bindings) (block) (safe))

(define-syntax import (lambda (stx) #f)) ;; ignore (import ...)

(##meta-info script-line "gsi") ;; force a call to main at end

;; exact is standard
;; inexact is standard
;; exact-integer? is standard
;; integer-length is standard
;; exact-integer-sqrt is standard
;; square is standard
;; real-part is standard
;; imag-part is standard
;; numerator is standard
;; denominator is standard
;; call-with-current-continuation
;; error is standard

(define (make-directories path) ;; only does one level
  (with-exception-catcher list (lambda () (create-directory path))))

(define (make-file-name dir base)
  (path-expand base dir))

(define exit-function list)

(define (register-exit-function! proc)
  (set! exit-function proc))

(define directory->list directory-files)

(define (hide a b)
   (car (list b)))

(define (substring=? v p)
   (let ((lv (string-length v))
	 (lp (string-length p)))
      (and (>= lv lp)
	   (letrec ((loop (lambda (i)
			     (cond
				((= i lp) #t)
				((char=? (string-ref lv i) (string-ref lp i)) (loop (+ i 1)))
				(else #f)))))
	      (loop 0)))))
	      
(define heap-reserve
   (let ((v (getenv "BGLSTONE_FILLER")))
      (cond
	 ((not v) #f)
	 ((string->number v)
	  =>
	  (lambda (n) (make-vector (quotient n 8))))
	 ((substring=? v "(make-vector ")
	  (make-vector
	     (string->number
		(substring v (string-length "(make-vector ")
		   (- (string-length v) 1)))))
	 ((substring=? v "(make-string ")
	  (make-string
	     (string->number
		(substring v (string-length "(make-string ")
		   (- (string-length v) 1)))))
	 (else
	  (print "error")
	  (exit 1)))))

(define (run-r7rs-benchmark name count thunk ok?)
  (print name "...")
  (force-output)
  (let loop ((i 0) (result #f))
    (cond ((fx< i count) (loop (fx+ i 1) (thunk)))
          ((ok? result) (print "ok") (exit-function #f) (exit 0))
          (else (print "error") (exit-function #f) (exit 1)))))

(##main-set!
 (lambda args
   (with-input-from-file (default-input) run-benchmark)))
