;*---------------------------------------------------------------------*/
;*    .../diffusion/article/bjvm/bench/queens/gambit/queens.scm        */
;*                                                                     */
;*    author      :  manuel serrano                                    */
;*    creation    :  tue may 12 09:19:00 1992                          */
;*    Last change :  Sun Feb 11 18:49:08 2001 (serrano)                */
;*                                                                     */
;*    la resolution des huits reine d'apres l. augustsson (lml)        */
;*---------------------------------------------------------------------*/
 
;*---------------------------------------------------------------------*/
;*    le module                                                        */
;*---------------------------------------------------------------------*/
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



;*---------------------------------------------------------------------*/
;*    succ ...                                                         */
;*---------------------------------------------------------------------*/
(define (succ x)
   (+fx 1 x))

;*---------------------------------------------------------------------*/
;*    mmap ...                                                         */
;*---------------------------------------------------------------------*/
(define (mmap f l)
   (define (map_rec l)
      (cond ((null? l)
	     '())
	    (else
	     (cons (f (car l)) (map_rec (cdr l))))))
   (map_rec l))

;*---------------------------------------------------------------------*/
;*    filter ...                                                       */
;*---------------------------------------------------------------------*/
(define (filter p l)
   (cond
      ((null? l)
       '())
      ((p (car l))
       (cons (car l) (filter p (cdr l))))
      (else
       (filter p (cdr l)))))

;*---------------------------------------------------------------------*/
;*    count ...                                                        */
;*---------------------------------------------------------------------*/
(define (count from to)
   (if (>fx from to)
       '()
       (cons from (count (succ from) to))))

;*---------------------------------------------------------------------*/
;*    concmap ...                                                      */
;*---------------------------------------------------------------------*/
(define (concmap f l)
   (if (null? l)
       '()
       (append (f (car l)) (concmap f (cdr l)))))

;*---------------------------------------------------------------------*/
;*    nsoln ...                                                        */
;*---------------------------------------------------------------------*/
(define (nsoln nq)
   (define (safe d x l)
      (if (null? l)
	  #t
	  (let ((q (car l)))
	     (and (not (=fx x q))
		  (and (not (=fx x (+fx q d)))
		       (and (not (=fx x (-fx q d))) 
			    (safe (+fx d 1) x (cdr l))))))))
   (define (ok l)
      (if (null? l)
	  #t
	  (safe 1 (car l) (cdr l))))
   (let ((pos_l (count 1 nq)))
      (letrec ((testcol (lambda (b) (filter ok (mmap (lambda (q) (cons q b)) 
						     pos_l)))))
	 (letrec ((gen (lambda (n)
			  (if (=fx n 0)
			      '(())
			      (let ((g (gen (-fx n 1))))
				 (concmap testcol g))))))
	    (length (gen nq))))))

;*---------------------------------------------------------------------*/
;*    nsola ...                                                        */
;*---------------------------------------------------------------------*/
(define (nsoln_a nq)
   (letrec ((ok (lambda (l)
		   (if (null? l)
		       #t
		       (let* ((x (car l))
			      (l (cdr l)))
			  (letrec ((safe (lambda (x d l)
					    (if
					     (null? l)
					     #t
					     (let* ((q (car l))
						    (l (cdr l)))
						(and
						 (not (=fx x q))
						 (and (not (=fx x (+fx q d)))
						      (and (not (=fx x (-fx q d)))
							   (safe x (+fx d 1) l)))))))))
			     (safe x 1 l)))))))
      (letrec ((gen (lambda (n)
		       (if (=fx n 0)
			   '(())
			   (concmap (lambda (b)
				       (filter ok
					       (mmap (lambda (q) (cons q b))
						     (count 1 nq))))
				    (gen (-fx n 1)))))))
	 (length (gen nq)))))

;*---------------------------------------------------------------------*/
;*    les formes top-level                                             */
;*---------------------------------------------------------------------*/
(define (doit num thunk)
   (define (loop num res)
      (if (> num 0)
	  (loop (- num 1) (+ res (thunk)))
	  res))
   (loop num 0))

(define (testit)
   (-fx (nsoln 10) (nsoln_a 10)))

;*---------------------------------------------------------------------*/
;*    les formes top-level                                             */
;*---------------------------------------------------------------------*/
(define (main argv)
   (print (doit 40 testit)))

(main '("a.gambit"))
