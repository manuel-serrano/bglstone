;; ==========================================================
;; Class accessors
;; Bigloo (4.7a)
;; Inria -- Sophia Antipolis     Thu Mar 6 07:34:14 AM CET 2025 
;; (bigloo -classgen Cnst/node.scm)
;; ==========================================================

;; The directives
(directives

;; local/bvalue
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (static
    (inline make-local/bvalue::local/bvalue id1116::symbol name1117::obj type1118::type value1119::value access1120::obj fast-alpha1121::obj removable1122::obj occurrence1123::long user?1124::bool key1125::long binding-value1126::node)
    (inline local/bvalue?::bool ::obj)
    (local/bvalue-nil::local/bvalue)
    (inline local/bvalue-binding-value::node ::local/bvalue)
    (inline local/bvalue-key::long ::local/bvalue)
    (inline local/bvalue-user?::bool ::local/bvalue)
    (inline local/bvalue-user?-set! ::local/bvalue ::bool)
    (inline local/bvalue-occurrence::long ::local/bvalue)
    (inline local/bvalue-occurrence-set! ::local/bvalue ::long)
    (inline local/bvalue-removable::obj ::local/bvalue)
    (inline local/bvalue-removable-set! ::local/bvalue ::obj)
    (inline local/bvalue-fast-alpha::obj ::local/bvalue)
    (inline local/bvalue-fast-alpha-set! ::local/bvalue ::obj)
    (inline local/bvalue-access::obj ::local/bvalue)
    (inline local/bvalue-access-set! ::local/bvalue ::obj)
    (inline local/bvalue-value::value ::local/bvalue)
    (inline local/bvalue-value-set! ::local/bvalue ::value)
    (inline local/bvalue-type::type ::local/bvalue)
    (inline local/bvalue-type-set! ::local/bvalue ::type)
    (inline local/bvalue-name::obj ::local/bvalue)
    (inline local/bvalue-name-set! ::local/bvalue ::obj)
    (inline local/bvalue-id::symbol ::local/bvalue)))))

;; The definitions
(cond-expand (bigloo-class-sans
;; local/bvalue
(define-inline (make-local/bvalue::local/bvalue id1116::symbol name1117::obj type1118::type value1119::value access1120::obj fast-alpha1121::obj removable1122::obj occurrence1123::long user?1124::bool key1125::long binding-value1126::node) (instantiate::local/bvalue (id id1116) (name name1117) (type type1118) (value value1119) (access access1120) (fast-alpha fast-alpha1121) (removable removable1122) (occurrence occurrence1123) (user? user?1124) (key key1125) (binding-value binding-value1126)))
(define-inline (local/bvalue?::bool obj::obj) ((@ isa? __object) obj (@ local/bvalue cnst_node)))
(define (local/bvalue-nil::local/bvalue) (class-nil (@ local/bvalue cnst_node)))
(define-inline (local/bvalue-binding-value::node o::local/bvalue) (-> |#!bigloo_wallow| o binding-value))
(define-inline (local/bvalue-binding-value-set! o::local/bvalue v::node) (set! (-> |#!bigloo_wallow| o binding-value) v))
(define-inline (local/bvalue-key::long o::local/bvalue) (-> |#!bigloo_wallow| o key))
(define-inline (local/bvalue-key-set! o::local/bvalue v::long) (set! (-> |#!bigloo_wallow| o key) v))
(define-inline (local/bvalue-user?::bool o::local/bvalue) (-> |#!bigloo_wallow| o user?))
(define-inline (local/bvalue-user?-set! o::local/bvalue v::bool) (set! (-> |#!bigloo_wallow| o user?) v))
(define-inline (local/bvalue-occurrence::long o::local/bvalue) (-> |#!bigloo_wallow| o occurrence))
(define-inline (local/bvalue-occurrence-set! o::local/bvalue v::long) (set! (-> |#!bigloo_wallow| o occurrence) v))
(define-inline (local/bvalue-removable::obj o::local/bvalue) (-> |#!bigloo_wallow| o removable))
(define-inline (local/bvalue-removable-set! o::local/bvalue v::obj) (set! (-> |#!bigloo_wallow| o removable) v))
(define-inline (local/bvalue-fast-alpha::obj o::local/bvalue) (-> |#!bigloo_wallow| o fast-alpha))
(define-inline (local/bvalue-fast-alpha-set! o::local/bvalue v::obj) (set! (-> |#!bigloo_wallow| o fast-alpha) v))
(define-inline (local/bvalue-access::obj o::local/bvalue) (-> |#!bigloo_wallow| o access))
(define-inline (local/bvalue-access-set! o::local/bvalue v::obj) (set! (-> |#!bigloo_wallow| o access) v))
(define-inline (local/bvalue-value::value o::local/bvalue) (-> |#!bigloo_wallow| o value))
(define-inline (local/bvalue-value-set! o::local/bvalue v::value) (set! (-> |#!bigloo_wallow| o value) v))
(define-inline (local/bvalue-type::type o::local/bvalue) (-> |#!bigloo_wallow| o type))
(define-inline (local/bvalue-type-set! o::local/bvalue v::type) (set! (-> |#!bigloo_wallow| o type) v))
(define-inline (local/bvalue-name::obj o::local/bvalue) (-> |#!bigloo_wallow| o name))
(define-inline (local/bvalue-name-set! o::local/bvalue v::obj) (set! (-> |#!bigloo_wallow| o name) v))
(define-inline (local/bvalue-id::symbol o::local/bvalue) (-> |#!bigloo_wallow| o id))
(define-inline (local/bvalue-id-set! o::local/bvalue v::symbol) (set! (-> |#!bigloo_wallow| o id) v))
))
