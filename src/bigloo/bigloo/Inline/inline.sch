;; ==========================================================
;; Class accessors
;; Bigloo (4.7a)
;; Inria -- Sophia Antipolis     Thu Mar 6 07:34:14 AM CET 2025 
;; (bigloo -classgen Inline/inline.scm)
;; ==========================================================

;; The directives
(directives

;; isfun
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-isfun::isfun arity1119::long side-effect?1120::obj predicate-of1121::obj stack-allocator1122::obj top?1123::bool the-closure1124::obj effect1125::obj property1126::obj args1127::obj body1128::obj class1129::obj dsssl-keywords1130::obj loc1131::obj original-body1132::node recursive-calls1133::obj)
    (inline isfun?::bool ::obj)
    (isfun-nil::isfun)
    (inline isfun-recursive-calls::obj ::isfun)
    (inline isfun-recursive-calls-set! ::isfun ::obj)
    (inline isfun-original-body::node ::isfun)
    (inline isfun-loc::obj ::isfun)
    (inline isfun-loc-set! ::isfun ::obj)
    (inline isfun-dsssl-keywords::obj ::isfun)
    (inline isfun-dsssl-keywords-set! ::isfun ::obj)
    (inline isfun-class::obj ::isfun)
    (inline isfun-class-set! ::isfun ::obj)
    (inline isfun-body::obj ::isfun)
    (inline isfun-body-set! ::isfun ::obj)
    (inline isfun-args::obj ::isfun)
    (inline isfun-args-set! ::isfun ::obj)
    (inline isfun-property::obj ::isfun)
    (inline isfun-property-set! ::isfun ::obj)
    (inline isfun-effect::obj ::isfun)
    (inline isfun-effect-set! ::isfun ::obj)
    (inline isfun-the-closure::obj ::isfun)
    (inline isfun-the-closure-set! ::isfun ::obj)
    (inline isfun-top?::bool ::isfun)
    (inline isfun-top?-set! ::isfun ::bool)
    (inline isfun-stack-allocator::obj ::isfun)
    (inline isfun-stack-allocator-set! ::isfun ::obj)
    (inline isfun-predicate-of::obj ::isfun)
    (inline isfun-predicate-of-set! ::isfun ::obj)
    (inline isfun-side-effect?::obj ::isfun)
    (inline isfun-side-effect?-set! ::isfun ::obj)
    (inline isfun-arity::long ::isfun)))))

;; The definitions
(cond-expand (bigloo-class-sans
;; isfun
(define-inline (make-isfun::isfun arity1119::long side-effect?1120::obj predicate-of1121::obj stack-allocator1122::obj top?1123::bool the-closure1124::obj effect1125::obj property1126::obj args1127::obj body1128::obj class1129::obj dsssl-keywords1130::obj loc1131::obj original-body1132::node recursive-calls1133::obj) (instantiate::isfun (arity arity1119) (side-effect? side-effect?1120) (predicate-of predicate-of1121) (stack-allocator stack-allocator1122) (top? top?1123) (the-closure the-closure1124) (effect effect1125) (property property1126) (args args1127) (body body1128) (class class1129) (dsssl-keywords dsssl-keywords1130) (loc loc1131) (original-body original-body1132) (recursive-calls recursive-calls1133)))
(define-inline (isfun?::bool obj::obj) ((@ isa? __object) obj (@ isfun inline_inline)))
(define (isfun-nil::isfun) (class-nil (@ isfun inline_inline)))
(define-inline (isfun-recursive-calls::obj o::isfun) (-> |#!bigloo_wallow| o recursive-calls))
(define-inline (isfun-recursive-calls-set! o::isfun v::obj) (set! (-> |#!bigloo_wallow| o recursive-calls) v))
(define-inline (isfun-original-body::node o::isfun) (-> |#!bigloo_wallow| o original-body))
(define-inline (isfun-original-body-set! o::isfun v::node) (set! (-> |#!bigloo_wallow| o original-body) v))
(define-inline (isfun-loc::obj o::isfun) (-> |#!bigloo_wallow| o loc))
(define-inline (isfun-loc-set! o::isfun v::obj) (set! (-> |#!bigloo_wallow| o loc) v))
(define-inline (isfun-dsssl-keywords::obj o::isfun) (-> |#!bigloo_wallow| o dsssl-keywords))
(define-inline (isfun-dsssl-keywords-set! o::isfun v::obj) (set! (-> |#!bigloo_wallow| o dsssl-keywords) v))
(define-inline (isfun-class::obj o::isfun) (-> |#!bigloo_wallow| o class))
(define-inline (isfun-class-set! o::isfun v::obj) (set! (-> |#!bigloo_wallow| o class) v))
(define-inline (isfun-body::obj o::isfun) (-> |#!bigloo_wallow| o body))
(define-inline (isfun-body-set! o::isfun v::obj) (set! (-> |#!bigloo_wallow| o body) v))
(define-inline (isfun-args::obj o::isfun) (-> |#!bigloo_wallow| o args))
(define-inline (isfun-args-set! o::isfun v::obj) (set! (-> |#!bigloo_wallow| o args) v))
(define-inline (isfun-property::obj o::isfun) (-> |#!bigloo_wallow| o property))
(define-inline (isfun-property-set! o::isfun v::obj) (set! (-> |#!bigloo_wallow| o property) v))
(define-inline (isfun-effect::obj o::isfun) (-> |#!bigloo_wallow| o effect))
(define-inline (isfun-effect-set! o::isfun v::obj) (set! (-> |#!bigloo_wallow| o effect) v))
(define-inline (isfun-the-closure::obj o::isfun) (-> |#!bigloo_wallow| o the-closure))
(define-inline (isfun-the-closure-set! o::isfun v::obj) (set! (-> |#!bigloo_wallow| o the-closure) v))
(define-inline (isfun-top?::bool o::isfun) (-> |#!bigloo_wallow| o top?))
(define-inline (isfun-top?-set! o::isfun v::bool) (set! (-> |#!bigloo_wallow| o top?) v))
(define-inline (isfun-stack-allocator::obj o::isfun) (-> |#!bigloo_wallow| o stack-allocator))
(define-inline (isfun-stack-allocator-set! o::isfun v::obj) (set! (-> |#!bigloo_wallow| o stack-allocator) v))
(define-inline (isfun-predicate-of::obj o::isfun) (-> |#!bigloo_wallow| o predicate-of))
(define-inline (isfun-predicate-of-set! o::isfun v::obj) (set! (-> |#!bigloo_wallow| o predicate-of) v))
(define-inline (isfun-side-effect?::obj o::isfun) (-> |#!bigloo_wallow| o side-effect?))
(define-inline (isfun-side-effect?-set! o::isfun v::obj) (set! (-> |#!bigloo_wallow| o side-effect?) v))
(define-inline (isfun-arity::long o::isfun) (-> |#!bigloo_wallow| o arity))
(define-inline (isfun-arity-set! o::isfun v::long) (set! (-> |#!bigloo_wallow| o arity) v))
))
