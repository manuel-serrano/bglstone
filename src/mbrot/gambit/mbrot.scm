;*---------------------------------------------------------------------*/
;*    arithmetic                                                       */
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
   `(##flonum.< ,x ,y))

(define-macro (<=fl x y)
   `(##flonum.<= ,x ,y))

(define-macro (negfl x)
   `(##flonum.- ,x))

(define-macro (-fl x y)
   `(##flonum.- ,x ,y))

(define-macro (*fl x y)
   `(##flonum.* ,x ,y))

(define-macro (+fl x y)
   `(##flonum.+ ,x ,y))

(define-macro (/fl x y)
   `(##flonum./ ,x ,y))

(define-macro (cosfl x)
   `(##flonum.cos ,x))

(define-macro (sinfl x)
   `(##flonum.sin ,x))

(define-macro (atanfl x)
   `(##flonum.atan ,x))

(define-macro (sqrtfl x)
   `(##flonum.sqrt ,x))

(define-macro (>fl x y)
   `(##flonum.> ,x ,y))

(define-macro (>fl x y)
   `(##flonum.>= ,x ,y))

(define-macro (=fl x y)
   `(##flonum.= ,x ,y))

;*---------------------------------------------------------------------*/
;*    logical operations                                               */
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



(define (count cr ci)

   (let ((max-count 64)
         (radius^2  16.0))

      (let loop ((c 0)
                 (zr cr)
                 (zi ci))
         (if (=fx c max-count)
             c
             (let ((zr^2 (*fl zr zr))
                   (zi^2 (*fl zi zi)))
                (if (>fl (+fl zr^2 zi^2) radius^2)
                    c
                    (loop (+fx c 1)
                          (+fl (-fl zr^2 zi^2) cr)
                          (+fl (*fl 2.0 (*fl zr zi)) ci))))))))

(define *nn 0)

(define (mbrot r i n step p?)
  (let loop1 ((y n) (ci i))
    (if (>fx y 0)
      (let loop2 ((x n) (cr r))
        (if (>fx x 0)
          (let ((c (count cr ci)))
             (if p?
                 (write-char (integer->char (+fx c 32)))
                 (set! *nn (+ c *nn)) )
            (loop2 (-fx x 1) (+fl cr step)))
          (begin
             (if p? 
                 (newline))
            (loop1 (-fx y 1) (+fl ci step))))))))

(define (run num)
   (let loop ((num num)
              (res (mbrot *r *i *n *step (=fx num 1))))
      (if (=fx num 1)
          (print 'done)
          (loop (-fx num 1) (mbrot *r *i *n *step (=fx num 2))))))

(define (do-bench num)
   (if (>fx num 0)
       (run num)))

(define *r -1.0)
(define *i -0.5)
(define *n 75)
(define *step 0.005)

(define (main argv)
   (if (=fx (length argv) 3)
       (begin (print "setting new values")
              (set! *r -1.1)
              (set! *i -0.4)
              (set! *n 50)
              (set! *step 0.001) ))
   (do-bench 10000))

(main '("a.gambit"))
