(module r7rs-matrix
   (include "../../r7rs/bigloo.sch")
   (include "../../r7rs/src/matrix.scm")
   (main main))

(define (default-input) "../../r7rs/inputs/matrix.input")
(define (pmem)
   (with-output-to-string
      (lambda ()
	 (call-with-input-file (default-input)
	    (lambda (ip)
	       (for-each (lambda (e)
			    (write e)
			    (newline))
		  (cons 250 (cdr (port->list ip)))))))))
