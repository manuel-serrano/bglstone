(module fibfx
   (main main))

(define (main argv)
   (let ((num (if (pair? (cdr argv))
		  (fixnum->flonum (string->integer (cadr argv)))
		  35.)))
      (print (fibfl num))))


(define (fibfl x)
   (if (<fl x 2.)
       1.
       (+fl (fibfl (-fl x 1.)) (fibfl (-fl x 2.)))))
