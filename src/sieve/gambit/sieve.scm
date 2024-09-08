;*---------------------------------------------------------------------*/
;*    Arithmetic                                                       */
;*---------------------------------------------------------------------*/
;*--- entiere ---------------------------------------------------------*/
(define-macro (<fx x y)
   `(##fixnum.< ,x ,y))

(define-macro (<=fx x y)
   `(##fixnum.<= ,x ,y))

(define-macro (-fx x y)
   `(##fixnum.- ,x ,y))

(define-macro (+fx x y)
   `(##fixnum.+ ,x ,y))

(define-macro (*fx x y)
   `(##fixnum.* ,x ,y))

(define-macro (/fx x y)
   `(##fixnum.quotient ,x ,y))

(define-macro (>fx x y)
   `(##fixnum.> ,x ,y))

(define-macro (>=fx x y)
   `(##fixnum.>= ,x ,y))

(define-macro (=fx x y)
   `(##fixnum.= ,x ,y))

(define-macro (modulo x y)
   `(##fixnum.modulo ,x ,y))

(define-macro (quotient x y)
   `(##fixnum.quotient ,x ,y))

(define-macro (quotientfx x y)
   `(##fixnum.quotient ,x ,y))

(define-macro (zerofx? x)
   `(##fixnum.zero? ,x))

;*--- reelle ----------------------------------------------------------*/
(define-macro (<fl x y)
   `(##FLONUM.< ,x ,y))

(define-macro (<=fl x y)
   `(##FLONUM.<= ,x ,y))

(define-macro (negfl x)
   `(##FLONUM.- ,x))

(define-macro (-fl x y)
   `(##FLONUM.- ,x ,y))

(define-macro (*fl x y)
   `(##FLONUM.* ,x ,y))

(define-macro (+fl x y)
   `(##FLONUM.+ ,x ,y))

(define-macro (/fl x y)
   `(##FLONUM./ ,x ,y))

(define-macro (cosfl x)
   `(##FLONUM.cos ,x))

(define-macro (sinfl x)
   `(##FLONUM.sin ,x))

(define-macro (atanfl x)
   `(##FLONUM.atan ,x))

(define-macro (sqrtfl x)
   `(##FLONUM.sqrt ,x))

(define-macro (>fl x y)
   `(##FLONUM.> ,x ,y))

(define-macro (>fl x y)
   `(##FLONUM.>= ,x ,y))

(define-macro (=fl x y)
   `(##FLONUM.= ,x ,y))

;*---------------------------------------------------------------------*/
;*    Logical operations                                               */
;*---------------------------------------------------------------------*/
(define-macro (bit-or x y)
   `(##fixnum.logior ,x ,y))

(define-macro (bit-not x)
   `(##fixnum.lognot ,x))

(define-macro (bit-and x y)
   `(##fixnum.logand ,x ,y))

(define-macro (bit-xor x y)
   `(##fixnum.logxor ,x ,y))

;*---------------------------------------------------------------------*/
;*    print ...                                                        */
;*---------------------------------------------------------------------*/
(define (print . l)
   (for-each display l) (newline))

;*---------------------------------------------------------------------*/
;*    display* ...                                                     */
;*---------------------------------------------------------------------*/
(define (display* . l)
   (for-each display l))

;*---------------------------------------------------------------------*/
;*    bind-exit ...                                                    */
;*---------------------------------------------------------------------*/
(define-macro (bind-exit exit . body)
   `(call-with-current-continuation (lambda ,exit ,@body)))

(define (interval min max)
   (if (>fx min max)
       '()
       (cons min (interval (+fx min 1) max))))

(define (filter p l)
   (if (null? l)
       l
       (let ((a (car l))
	     (r (cdr l)))
	  (if (p a)
	      (cons a (filter p r))
	      (filter p r)))))

(define (remove-multiples-of n l)
   (filter (lambda (m) (not (=fx (modulo m n) 0))) l))

(define (sieve max)
   (define (filter-again l)
      (if (null? l)
	  l
	  (let ((n (car l))
		(r (cdr l)))
	     (if (>fx (*fx n n) max)
		 l
		 (cons n (filter-again
			  (remove-multiples-of n r)))))))
   (filter-again (interval 2 max)))

(define (do-list f lst)
   (for-each f lst))

(define (doit num thunk)
   (define (loop num res)
      (if (> num 0)
	  (loop (- num 1) (+ res (thunk)))
	  res))
   (loop num 0))

(define (main argv)
   (print "doit: " (doit 5000 (lambda () (length (sieve 3000)))))
   (do-list (lambda (n)
	       (display n)
	       (display " "))
	    (sieve 3000))
   (newline))

   
(main '("a.gambit"))
