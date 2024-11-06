(module r7rs-earley
   (include "../../r7rs/bigloo.sch")
   (include "../../r7rs/src/earley.scm")
   (main main))

(define (default-input) "../../r7rs/inputs/earley.input")
(define (pmem)
   (setrlimit! 'STACK +inf.0 +inf.0)
    "1 15 2674440")
