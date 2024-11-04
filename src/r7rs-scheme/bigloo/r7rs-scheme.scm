(module r7rs-scheme
   (include "../../r7rs/bigloo.sch")
   (include "../../r7rs/src/scheme.scm")
   (main main))

(define (default-input) "../../r7rs/inputs/scheme.input")
(define (pmem)
   (with-output-to-string
      (lambda ()
	 (call-with-input-file (default-input)
	    (lambda (ip)
	       (for-each (lambda (e)
			    (write e)
			    (newline))
		  (cons 10000 (cdr (port->list ip)))))))))
