(module r7rs-peval
   (include "../../r7rs/bigloo.sch")
   (include "../../r7rs/src/peval.scm")
   (main main))

(define (default-input) "../../r7rs/inputs/peval.input")
(define (pmem)
   (with-output-to-string
      (lambda ()
	 (call-with-input-file (default-input)
	    (lambda (ip)
	       (for-each (lambda (e)
			    (write e)
			    (newline))
		  (cons 200 (cdr (port->list ip)))))))))
