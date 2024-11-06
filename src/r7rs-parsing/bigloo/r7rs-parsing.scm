(module r7rs-parsing
   (include "../../r7rs/bigloo.sch")
   (include "../../r7rs/src/parsing.scm")
   (main main))

(define (default-input) "../../r7rs/inputs/parsing.input")
(define (pmem) "250
\"../../inputs/parsing.data\"
(should return this list)")
