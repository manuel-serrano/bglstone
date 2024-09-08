(define (print . l)
   (for-each display l)
   (newline))

(define (count cr ci)

   (let ((max-count 64)
         (radius^2  16.0))

      (let loop ((c 0)
                 (zr cr)
                 (zi ci))
         (if (= c max-count)
             c
             (let ((zr^2 (* zr zr))
                   (zi^2 (* zi zi)))
                (if (> (+ zr^2 zi^2) radius^2)
                    c
                    (loop (+ c 1)
                          (+ (- zr^2 zi^2) cr)
                          (+ (* 2.0 (* zr zi)) ci))))))))

(define *nn 0)

(define (mbrot r i n step p?)
  (let loop1 ((y n) (ci i))
    (if (> y 0)
      (let loop2 ((x n) (cr r))
        (if (> x 0)
          (let ((c (count cr ci)))
             (if p?
                 (write-char (integer->char (+ c 32)))
                 (set! *nn (+ c *nn)) )
            (loop2 (- x 1) (+ cr step)))
          (begin
             (if p? 
                 (newline))
            (loop1 (- y 1) (+ ci step))))))))

(define (run num)
   (let loop ((num num)
              (res (mbrot *r *i *n *step (= num 1))))
      (if (= num 1)
          (print 'done)
          (loop (- num 1) (mbrot *r *i *n *step (= num 2))))))

(define (do-bench num)
   (if (> num 0)
       (run num)))

(define *r -1.0)
(define *i -0.5)
(define *n 75)
(define *step 0.005)

(define (main argv)
;*    (if (= (length argv) 3)                                          */
;*        (begin (print "setting new values")                          */
;*               (set! *r -1.1)                                        */
;*               (set! *i -0.4)                                        */
;*               (set! *n 50)                                          */
;*               (set! *step 0.001) ))                                 */
   (do-bench 10000))

(main '("toto"))
