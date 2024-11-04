(module r7rs-earley
   (include "../../r7rs/bigloo.sch")
   (include "../../r7rs/src/earley.scm")
   (main main))

(define (default-input) "../../r7rs/inputs/earley.input")
(define (pmem) "1 15 2674440")
