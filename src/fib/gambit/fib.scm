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


(define (doit num thunk)
   (define (loop num res)
      (if (> num 0)
	  (loop (- num 1) (+ res (thunk)))
	  res))
   (loop num 0))

(define (fib x)
   (if (<fx x 2)
       1
       (+fx (fib (-fx x 1)) (fib (-fx x 2)))))

(define (main argv)
   (doit 300 (lambda () (fib 30)))
   (print (fib 30)))

(main '("a.gambit"))
