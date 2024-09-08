(define (doit num thunk)
   (define (loop num res)
      (if (> num 0)
	  (loop (- num 1) (+ res (thunk)))))
   (loop num 0))

(define (print . l)
   (for-each display l)
   (newline))
   
(define (fib x)
   (if (< x 2)
       1
       (+ (fib (- x 1)) (fib (- x 2)))))

(define (main argv)
   (doit 300 (lambda () (fib 30)))
   (print (fib 30)))

(main '("fib"))
