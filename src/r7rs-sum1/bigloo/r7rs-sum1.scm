(module r7rs-sum1
   (include "../../r7rs/bigloo.sch")
   (include "../../r7rs/src/sum1.scm")
   (main main))

(define (default-input) "../../r7rs/inputs/sum1.input")
(define (pmem) "100
\"../../r7rs/inputs/sum1.data\"
15794.975")

