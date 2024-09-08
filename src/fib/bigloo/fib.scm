(module fib
   (option (when (string>=? (bigloo-config 'release-number) "3.4b")
	      (set! *optim-cfa-fixnum-arithmetic?* #t)
	      (set! *arithmetic-overflow* #f)))
   (main main))

(define (doit num thunk)
   (define (loop num res)
      (if (>fx num 0)
	  (loop (-fx num 1) (+fx res (thunk)))
	  res))
   (loop num 0))

(define (fib x)
   (if (< x 2)
       1
       (+ (fib (- x 1)) (fib (- x 2)))))

(define (main argv)
   (let ((n (if (pair? (cdr argv))
		(string->integer (cadr argv))
		300)))
      (if (= n 1)
	  (let ((v (fib 30)))
	     (print "fib(30)=" v)
	     (if (= v 1346269) 0 1))
	  (let ((res (doit n (lambda () (fib 30)))))
	     (if (= (fib 30) (/ res n)) 0 1)))))
