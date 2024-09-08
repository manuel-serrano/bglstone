;*---------------------------------------------------------------------*/
;*    serrano/prgm/project/bglstone/src/queens/bigloo/queens.scm       */
;*                                                                     */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue May 12 09:19:00 1992                          */
;*    Last change :  Sun Sep  8 11:12:30 2024 (serrano)                */
;*                                                                     */
;*    La resolution des huits reine d'apres L. Augustsson (lml)        */
;*---------------------------------------------------------------------*/
 
;*---------------------------------------------------------------------*/
;*    Le module                                                        */
;*---------------------------------------------------------------------*/
(module reine
   (main main))

;*---------------------------------------------------------------------*/
;*    succ ...                                                         */
;*---------------------------------------------------------------------*/
(define (succ x)
   (+fx 1 x))

;*---------------------------------------------------------------------*/
;*    qmap ...                                                         */
;*---------------------------------------------------------------------*/
(define (qmap f l)
   (labels ((map_rec (l)
		     (cond ((null? l)
			    '())
			   (else
			    (cons (f (car l)) (map_rec (cdr l)))))))
      (map_rec l)))

;*---------------------------------------------------------------------*/
;*    qfilter ...                                                      */
;*---------------------------------------------------------------------*/
(define (qfilter p l)
   (cond
      ((null? l)
       '())
      ((p (car l))
       (cons (car l) (qfilter p (cdr l))))
      (else
       (qfilter p (cdr l)))))

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
   (labels ((safe (d x l)
		  (if (null? l)
		      #t
		      (let ((q (car l)))
			 (and (not (=fx x q))
			      (and (not (=fx x (+fx q d)))
				   (and (not (=fx x (-fx q d))) 
					(safe (+fx d 1) x (cdr l))))))))
	    (ok (l)
		(if (null? l)
		    #t
		    (safe 1 (car l) (cdr l)))))
      (let ((pos_l (count 1 nq)))
	 (labels ((testcol (b) (qfilter ok (qmap (lambda (q) (cons q b)) 
						pos_l))))
	    (labels ((gen (n)
			  (if (=fx n 0)
			      '(())
			      (let ((g (gen (-fx n 1))))
				 (concmap testcol g)))))
	       (length (gen nq)))))))

;*---------------------------------------------------------------------*/
;*    nsola ...                                                        */
;*---------------------------------------------------------------------*/
(define (nsoln_a nq)
   (labels ((ok (l)
		(if (null? l)
		    #t
		    (let* ((x (car l))
			   (l (cdr l)))
		       (labels ((safe (x d l)
				      (if
				       (null? l)
				       #t
				       (let* ((q (car l))
					      (l (cdr l)))
					  (and
					   (not (=fx x q))
					   (and (not (=fx x (+fx q d)))
						(and (not (=fx x (-fx q d)))
						     (safe x (+fx d 1) l))))))))
			  (safe x 1 l))))))
      (labels ((gen (n)
		    (if (=fx n 0)
			'(())
			(concmap (lambda (b)
				    (qfilter ok
					    (qmap (lambda (q) (cons q b))
						  (count 1 nq))))
				 (gen (-fx n 1))))))
	 (length (gen nq)))))

(define (doit num)
   (define (loop num res)
      (if (> num 0)
	  (loop (- num 1) (+ res (-fx (nsoln 10) (nsoln_a 10))))
	  res))
   (loop num 0))

;*---------------------------------------------------------------------*/
;*    les formes top-level                                             */
;*---------------------------------------------------------------------*/
(define (main argv)
   (let ((n (if (pair? (cdr argv)) (string->integer (cadr argv)) 450)))
      (if (=fx n 1)
	  (print "nsoln(10)=" (nsoln 10) ", nsoln_a(10)=" (nsoln_a 10))
	  (doit n))
      (exit (if (and (=fx (nsoln 10) 724) (=fx (nsoln_a 10) 724)) 0 1))))
