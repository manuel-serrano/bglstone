(module r7rs-maze
   (include "../../r7rs/bigloo.sch")
   (include "../../r7rs/src/maze.scm")
   (main main))

(define (default-input) "../../r7rs/inputs/maze.input")
(define (pmem)
   (with-output-to-string
      (lambda ()
	 (call-with-input-file (default-input)
	    (lambda (ip)
	       (for-each (lambda (e)
			    (write e)
			    (newline))
		  (cons 1000 (cdr (port->sexp-list ip)))))))))
