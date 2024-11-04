(module r7rs-compiler
   (include "../../r7rs/bigloo.sch")
   (include "../../r7rs/src/compiler.scm")
   (main main))

(define (default-input) "../../r7rs/inputs/compiler.input")
(define (pmem) (call-with-input-file (default-input) read-string))
