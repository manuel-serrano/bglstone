(module r7rs-ray
   (include "../../r7rs/bigloo.sch")
   (include "../../r7rs/src/ray.scm")
   (main main))

(make-directories "outputs")

(define (default-input) "../../r7rs/inputs/ray.input")
(define (pmem) "5 1 \"outputs/ray.output\" ok")

(register-exit-function!
   (lambda (r)
      (let ((dir "outputs"))
	 (for-each (lambda (f)
		      (delete-file (make-file-name dir f)))
	    (directory->list dir))
	 (delete-directory "outputs"))))
