(module r7rs-pi
   (include "../../r7rs/bigloo.sch")
   (include "../../r7rs/src/pi.scm")
   (main main))

(define (default-input) "../../r7rs/inputs/pi.input")
(define (pmem)
   (with-output-to-string
      (lambda ()
	 (call-with-input-file (default-input)
	    (lambda (ip)
	       (for-each (lambda (e)
			    (write e)
			    (newline))
		  (cons 10 (cdr (port->list ip)))))))))
