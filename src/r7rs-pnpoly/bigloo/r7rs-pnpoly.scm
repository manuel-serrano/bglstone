(module r7rs-pnpoly
   (include "../../r7rs/bigloo.sch")
   (include "../../r7rs/src/pnpoly.scm")
   (main main))

(define (default-input) "../../r7rs/inputs/pnpoly.input")
(define (pmem) "100000
#(0. 1. 1. 0. 0. 1. -.5 -1. -1. -2. -2.5 -2. -1.5 -.5 1. 1. 0. -.5 -1. -.5)
#(0. 0. 1. 1. 2. 3. 2. 3. 0. -.5 -1.  -1.5 -2. -2. -1.5 -1. -.5 -1. -1. -.5)
6")
