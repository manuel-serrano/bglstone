;*---------------------------------------------------------------------*/
;*    serrano/diffusion/article/bjvm/bench/queens/kawa/queens.scm      */
;*                                                                     */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue May 12 09:19:00 1992                          */
;*    Last change :  Wed Feb 14 08:04:22 2001 (serrano)                */
;*                                                                     */
;*    La resolution des huits reine d'apres L. Augustsson (lml)        */
;*---------------------------------------------------------------------*/
 
;*---------------------------------------------------------------------*/
;*    succ ...                                                         */
;*---------------------------------------------------------------------*/
(define (succ x)
   (+ 1 x))

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
   (if (> from to)
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
	     (and (not (= x q))
		  (and (not (= x (+ q d)))
		       (and (not (= x (- q d))) 
			    (safe (+ d 1) x (cdr l))))))))
   (define (ok l)
      (if (null? l)
	  #t
	  (safe 1 (car l) (cdr l))))
   (let ((pos_l (count 1 nq)))
      (define (testcol b)
	 (filter ok (mmap (lambda (q) (cons q b)) pos_l)))
      (define (gen n)
	 (if (= n 0)
	     '(())
	     (let ((g (gen (- n 1))))
		(concmap testcol g))))
      (length (gen nq))))

;*---------------------------------------------------------------------*/
;*    nsola ...                                                        */
;*---------------------------------------------------------------------*/
(define (nsoln_a nq)
   (define (ok l)
      (if (null? l)
	  #t
	  (let* ((x (car l))
		 (l (cdr l)))
	     (define (safe x d l)
		(if (null? l)
		    #t
		    (let* ((q (car l))
			   (l (cdr l)))
		       (and
			(not (= x q))
			(and (not (= x (+ q d)))
			     (and (not (= x (- q d)))
				  (safe x (+ d 1) l)))))))
	     (safe x 1 l))))
   (define (gen n)
      (if (= n 0)
	  '(())
	  (concmap (lambda (b)
		      (filter ok
			      (mmap (lambda (q) (cons q b)) (count 1 nq))))
		   (gen (- n 1)))))
   (length (gen nq)))

(define (doit num thunk)
   (define (loop num res)
      (if (> num 0)
	  (loop (- num 1) (+ res (thunk)))
	  res))
   (loop num 0))

(define (testit)
   (- (nsoln 10) (nsoln_a 10)))

(define (print . l)
   (for-each display l)
   (newline))

;*---------------------------------------------------------------------*/
;*    les formes top-level                                             */
;*---------------------------------------------------------------------*/
(define (main argv)
   (print (doit 40 testit)))

(main '(toto))
	    




