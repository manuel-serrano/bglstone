(define (interval min max)
   (if (> min max)
       '()
       (cons min (interval (+ min 1) max))))

(define (filter p l)
   (if (null? l)
       l
       (let ((a (car l))
	     (r (cdr l)))
	  (if (p a)
	      (cons a (filter p r))
	      (filter p r)))))

(define (remove-multiples-of n l)
   (filter (lambda (m) (not (= (modulo m n) 0))) l))

(define (sieve max)
   (define (filter-again l)
      (if (null? l)
	  l
	  (let ((n (car l))
		(r (cdr l)))
	     (if (> (* n n) max)
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

(define (print . l)
   (for-each display l)
   (newline))

(define (main argv)
   (print "doit: " (doit 5000 (lambda () (length (sieve 3000)))))
   (do-list (lambda (n)
	       (display n)
	       (display " "))
	    (sieve 3000))
   (newline))

(main '("toto"))
