(module r7rs-dynamic
   (include "../../r7rs/bigloo.sch")
   (include "../../r7rs/src/dynamic.scm")
   (main main))

(define (default-input) "../../r7rs/inputs/dynamic.input")
(define (pmem)
   (with-output-to-string
      (lambda ()
	 (call-with-input-file (default-input)
	    (lambda (ip)
	       (for-each (lambda (e)
			    (write e)
			    (newline))
		  (cons 50 (cdr (port->list ip)))))))))
