(module r7rs-slatex
   (include "../../r7rs/bigloo.sch")
   (include "../../r7rs/src/slatex.scm")
   (main main))

(define (default-input) "../../r7rs/inputs/slatex.input")
(define (pmem) "50
\"inputs/slatex-data/test\"
ignored
ignored")

(make-directories "outputs")

(register-exit-function!
   (lambda (r)
      (let ((dir "outputs"))
	 (for-each (lambda (f)
		      (delete-file (make-file-name dir f)))
	    (directory->list dir))
	 (delete-directory "outputs"))))
