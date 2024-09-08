(module fibfx
   (main main))

(define (main argv)
   (let ((num (if (pair? (cdr argv))
		  (string->integer (cadr argv))
		  35)))
      (print (fibfx num))))


(define (fibfx x)
   (if (<fx x 2)
       1
       (+fx (fibfx (-fx x 1)) (fibfx (-fx x 2)))))
