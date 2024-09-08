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



(define (qsort lo hi a)
   (if (<fx lo hi)
       (let ((i lo)
	     (j hi)
	     (pivot (vector-ref a hi)))
	  (let loop1 ()
	     (if (<fx i j)
		 (begin
		    (let loop2 ()
		       (if (and (<fx i hi)
				(<=fx (vector-ref a i) pivot))
			   (begin
			      (set! i (+fx i 1))
			      (loop2))))
		    (let loop3 ()
		       (if (and (>fx j lo)
				(>=fx (vector-ref a j) pivot))
			   (begin
			      (set! j (-fx j 1))
			      (loop3))))
		    (if (<fx i j)
			(let ((temp (vector-ref a i)))
			   (vector-set! a i (vector-ref a j))
			   (vector-set! a j temp)))
		    (loop1))))
	  (let ((temp (vector-ref a i)))
	     (vector-set! a i (vector-ref a hi))
	     (vector-set! a hi temp))
	  (qsort lo (-fx i 1) a)
	  (qsort (+fx i 1) hi a))))

(define (cmp i j)
   (-fx i j))

(define (qsort2 lo hi a)
   (if (<fx lo hi)
       (let ((i lo)
	     (j hi)
	     (pivot (vector-ref a hi)))
	  (let loop1 ()
	     (if (<fx i j)
		 (begin
		    (let loop2 ()
		       (if (and (<fx i hi)
				(<= (cmp (vector-ref a i) pivot) 0))
			   (begin
			      (set! i (+fx i 1))
			      (loop2))))
		    (let loop3 ()
		       (if (and (>fx j lo)
				(>=fx (cmp (vector-ref a j) pivot) 0))
			   (begin
			      (set! j (-fx j 1))
			      (loop3))))
		    (if (<fx i j)
			(let ((temp (vector-ref a i)))
			   (vector-set! a i (vector-ref a j))
			   (vector-set! a j temp)))
		    (loop1))))
	  (let ((temp (vector-ref a i)))
	     (vector-set! a i (vector-ref a hi))
	     (vector-set! a hi temp))
	  (qsort2 lo (-fx i 1) a)
	  (qsort2 (+fx i 1) hi a))))

(define seed 0)

(define (rand)
   (set! seed (bit-and (+fx (*fx seed 25173) 17431) 4095))
   seed)

(define (test-sort sort-fun size)
   (let ((a (make-vector size 0))
	 (check (make-vector 4096 0)))
      (let ((to (-fx size 1)))
	 (let for.1 ((i 0))
	    (if (<=fx i to)
		(let ((n (rand)))
		   (vector-set! a i n)
		   (vector-set! check n (+fx 1 (vector-ref check n)))
		   (for.1 (+fx i 1)))))
	 (if (eq? sort-fun 'qsort)
	     (qsort 0 (-fx size 1) a)
	     (qsort2 0 (-fx size 1) a))
	 (begin
	    (vector-set! check
			 (vector-ref a 0)
			 (-fx (vector-ref check (vector-ref a 0)) 1))
	    (let for.2 ((i 1))
	       (if (<=fx i to)
		   (begin
		      (if (>fx (vector-ref a (-fx i 1))
			       (vector-ref a i))
			  (error "test-sort" "illegal sort" i)
			  (begin
			     (vector-set! check
					  (vector-ref a i)
					  (-fx (vector-ref check
							   (vector-ref a i))
					       1))
			     (for.2 (+fx i 1)))))))
	    (let for.3 ((i 0))
	       (if (<=fx i 4095)
		   (begin
		      (if (not (=fx (vector-ref check i) 0))
			  (error "test-sort" "illegal sort" i)
			  (for.3 (+fx i 1))))))
	    (vector-ref a 0)))))

(define (runit n)
   (+ (test-sort 'qsort n)
      (test-sort 'qsort2 n)))
   
(define (testit n)
   (and (integer? (test-sort 'qsort n)) (integer? (test-sort 'qsort2 n))))
	  
(define (doit n)
   (let loop ((i 40))
      (when (>fx i 1)
	 (runit n)
	 (loop (-fx i 1))))
   (testit n))
      
(define (main argv)
   (let ((n (if (pair? (cdr argv)) (string->number (cadr argv)) 520000)))
       (exit (if (eq? (doit n) #t) 0 1))))

(main '("a.gambit"))
