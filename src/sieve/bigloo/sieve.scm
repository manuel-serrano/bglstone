(module sieve
   (main main))

(define (interval min max)
   (if (>fx min max)
       '()
       (cons min (interval (+fx min 1) max))))

(define (sfilter p l)
   (if (null? l)
       l
       (let ((a (car l))
	     (r (cdr l)))
	  (if (p a)
	      (cons a (sfilter p r))
	      (sfilter p r)))))

(define (remove-multiples-of n l)
   (sfilter (lambda (m) (not (=fx (modulo m n) 0))) l))

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

(define (doit num)
   (define (loop num res)
      (if (>fx num 0)
	  (loop (-fx num 1) (+fx res (length (sieve 3000))))
	  res))
   (loop num 0))

(define expected-result
   '(2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97))

(define (main argv)
   (let ((s100 (sieve 100)))
      (let ((n (if (pair? (cdr argv)) (string->integer (cadr argv)) 5000)))
	 (when (=fx n 1) (print s100))
	 (doit n)
	 (exit (if (equal? s100 expected-result) 0 1)))))

   
