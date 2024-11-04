(module r7rs-sboyer
   (include "../../r7rs/bigloo.sch")
   (include "../../r7rs/src/sboyer.scm")
   (main main))

(define (default-input) "../../r7rs/inputs/sboyer.input")
(define (pmem) "1
5
51507739 ; if the input is 5
16445406 ; if the input is 4")
