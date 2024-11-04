(module r7rs-ray
   (include "../../r7rs/bigloo.sch")
   (include "../../r7rs/src/ray.scm")
   (main main))

(define (default-input) "../../r7rs/inputs/ray.input")
(define (pmem) "5 1 \"outputs/ray.output\" ok")
