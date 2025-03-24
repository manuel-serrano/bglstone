;; ==========================================================
;; Class accessors
;; Bigloo (4.7a)
;; Inria -- Sophia Antipolis     Thu Mar 6 07:34:14 AM CET 2025 
;; (bigloo -classgen Module/module.scm)
;; ==========================================================

;; The directives
(directives

;; ccomp
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-ccomp::ccomp id1104::symbol producer1105::procedure consumer1106::procedure finalizer1107::procedure)
    (inline ccomp?::bool ::obj)
    (ccomp-nil::ccomp)
    (inline ccomp-finalizer::procedure ::ccomp)
    (inline ccomp-consumer::procedure ::ccomp)
    (inline ccomp-producer::procedure ::ccomp)
    (inline ccomp-id::symbol ::ccomp)))))

;; The definitions
(cond-expand (bigloo-class-sans
;; ccomp
(define-inline (make-ccomp::ccomp id1104::symbol producer1105::procedure consumer1106::procedure finalizer1107::procedure) (instantiate::ccomp (id id1104) (producer producer1105) (consumer consumer1106) (finalizer finalizer1107)))
(define-inline (ccomp?::bool obj::obj) ((@ isa? __object) obj (@ ccomp module_module)))
(define (ccomp-nil::ccomp) (class-nil (@ ccomp module_module)))
(define-inline (ccomp-finalizer::procedure o::ccomp) (-> |#!bigloo_wallow| o finalizer))
(define-inline (ccomp-finalizer-set! o::ccomp v::procedure) (set! (-> |#!bigloo_wallow| o finalizer) v))
(define-inline (ccomp-consumer::procedure o::ccomp) (-> |#!bigloo_wallow| o consumer))
(define-inline (ccomp-consumer-set! o::ccomp v::procedure) (set! (-> |#!bigloo_wallow| o consumer) v))
(define-inline (ccomp-producer::procedure o::ccomp) (-> |#!bigloo_wallow| o producer))
(define-inline (ccomp-producer-set! o::ccomp v::procedure) (set! (-> |#!bigloo_wallow| o producer) v))
(define-inline (ccomp-id::symbol o::ccomp) (-> |#!bigloo_wallow| o id))
(define-inline (ccomp-id-set! o::ccomp v::symbol) (set! (-> |#!bigloo_wallow| o id) v))
))
