(module fib
   (option (when (string>=? (bigloo-config 'release-number) "3.4b")
	      (set! *optim-cfa-fixnum-arithmetic?* #t)
	      (set! *arithmetic-overflow* #f)))
   (main main))

(define (doit num thunk)
   (define (loop num res)
      (if (>fx num 0)
	  (loop (-fx num 1) (cons (thunk) res))
	  res))
   (loop num '()))

(define (fib x)
   (if (< x 2)
       1
       (+ (fib (- x 1)) (fib (- x 2)))))

(define (main argv)
   (let ((n (if (pair? (cdr argv))
		(string->integer (cadr argv))
		900)))
      (if (= n 1)
	  (let ((v (fib 35)))
	     (print v)
	     (if (= v 14930352) 0 1))
	  (let ((res (doit n (lambda () (fib 35))))
		(e (fib 35)))
	     (if (every (lambda (v) (=fx e v)) res) 0 1)))))

