(module r7rs-ray
   (include "../../r7rs/bigloo.sch")
   (include "../../r7rs/src/simplex.scm")
   (main main))

(define (default-input) "../../r7rs/inputs/simplex.input")
(define (pmem) "100000
740.0
(#(4 1 3 2) #(0 5 7 6))")
