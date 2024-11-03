(module r7rs-fft
   (include "../../r7rs/bigloo.sch")
   (include "../../r7rs/src/fft.scm")
   (main main))

(define (default-input) "../../r7rs/inputs/fft.input")
(define (pmem) "100 65536 0.0 0.0")
