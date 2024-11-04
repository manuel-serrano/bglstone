(module r7rs-nboyer
   (include "../../r7rs/bigloo.sch")
   (include "../../r7rs/src/nboyer.scm")
   (main main))

(define (default-input) "../../r7rs/inputs/nboyer.input")
(define (pmem) "1
5 ; was 4
51507739 ; if the input is 5
16445406 ; if the input is 4


; The old inputs were:

1
4
16445406 ; if the input is 4
51507739 ; if the input is 5")
