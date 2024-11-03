(module r7rs-mbrot
   (include "../../r7rs/bigloo.sch")
   (include "../../r7rs/src/mbrot.scm")
   (main main))

(define (default-input) "../../r7rs/inputs/mbrot.input")
(define (pmem) "100 75 5")
