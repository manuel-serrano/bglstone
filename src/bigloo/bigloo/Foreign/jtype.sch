;; ==========================================================
;; Class accessors
;; Bigloo (4.7a)
;; Inria -- Sophia Antipolis     Thu Mar 6 07:34:14 AM CET 2025 
;; (bigloo -classgen Foreign/jtype.scm)
;; ==========================================================

;; The directives
(directives

;; jarray
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-jarray::jarray id1126::symbol name1127::obj size1128::obj class1129::obj coerce-to1130::obj parents1131::obj init?1132::bool magic?1133::bool $1134::obj alias1135::obj pointed-to-by1136::obj tvector1137::obj location1138::obj import-location1139::obj item-type1140::type)
    (inline jarray?::bool ::obj)
    (jarray-nil::jarray)
    (inline jarray-item-type::type ::jarray)
    (inline jarray-import-location::obj ::jarray)
    (inline jarray-import-location-set! ::jarray ::obj)
    (inline jarray-location::obj ::jarray)
    (inline jarray-location-set! ::jarray ::obj)
    (inline jarray-tvector::obj ::jarray)
    (inline jarray-tvector-set! ::jarray ::obj)
    (inline jarray-pointed-to-by::obj ::jarray)
    (inline jarray-pointed-to-by-set! ::jarray ::obj)
    (inline jarray-alias::obj ::jarray)
    (inline jarray-alias-set! ::jarray ::obj)
    (inline jarray-$::obj ::jarray)
    (inline jarray-$-set! ::jarray ::obj)
    (inline jarray-magic?::bool ::jarray)
    (inline jarray-magic?-set! ::jarray ::bool)
    (inline jarray-init?::bool ::jarray)
    (inline jarray-init?-set! ::jarray ::bool)
    (inline jarray-parents::obj ::jarray)
    (inline jarray-parents-set! ::jarray ::obj)
    (inline jarray-coerce-to::obj ::jarray)
    (inline jarray-coerce-to-set! ::jarray ::obj)
    (inline jarray-class::obj ::jarray)
    (inline jarray-class-set! ::jarray ::obj)
    (inline jarray-size::obj ::jarray)
    (inline jarray-size-set! ::jarray ::obj)
    (inline jarray-name::obj ::jarray)
    (inline jarray-name-set! ::jarray ::obj)
    (inline jarray-id::symbol ::jarray)))))

;; The definitions
(cond-expand (bigloo-class-sans
;; jarray
(define-inline (make-jarray::jarray id1126::symbol name1127::obj size1128::obj class1129::obj coerce-to1130::obj parents1131::obj init?1132::bool magic?1133::bool $1134::obj alias1135::obj pointed-to-by1136::obj tvector1137::obj location1138::obj import-location1139::obj item-type1140::type) (instantiate::jarray (id id1126) (name name1127) (size size1128) (class class1129) (coerce-to coerce-to1130) (parents parents1131) (init? init?1132) (magic? magic?1133) ($ $1134) (alias alias1135) (pointed-to-by pointed-to-by1136) (tvector tvector1137) (location location1138) (import-location import-location1139) (item-type item-type1140)))
(define-inline (jarray?::bool obj::obj) ((@ isa? __object) obj (@ jarray foreign_jtype)))
(define (jarray-nil::jarray) (class-nil (@ jarray foreign_jtype)))
(define-inline (jarray-item-type::type o::jarray) (-> |#!bigloo_wallow| o item-type))
(define-inline (jarray-item-type-set! o::jarray v::type) (set! (-> |#!bigloo_wallow| o item-type) v))
(define-inline (jarray-import-location::obj o::jarray) (-> |#!bigloo_wallow| o import-location))
(define-inline (jarray-import-location-set! o::jarray v::obj) (set! (-> |#!bigloo_wallow| o import-location) v))
(define-inline (jarray-location::obj o::jarray) (-> |#!bigloo_wallow| o location))
(define-inline (jarray-location-set! o::jarray v::obj) (set! (-> |#!bigloo_wallow| o location) v))
(define-inline (jarray-tvector::obj o::jarray) (-> |#!bigloo_wallow| o tvector))
(define-inline (jarray-tvector-set! o::jarray v::obj) (set! (-> |#!bigloo_wallow| o tvector) v))
(define-inline (jarray-pointed-to-by::obj o::jarray) (-> |#!bigloo_wallow| o pointed-to-by))
(define-inline (jarray-pointed-to-by-set! o::jarray v::obj) (set! (-> |#!bigloo_wallow| o pointed-to-by) v))
(define-inline (jarray-alias::obj o::jarray) (-> |#!bigloo_wallow| o alias))
(define-inline (jarray-alias-set! o::jarray v::obj) (set! (-> |#!bigloo_wallow| o alias) v))
(define-inline (jarray-$::obj o::jarray) (-> |#!bigloo_wallow| o $))
(define-inline (jarray-$-set! o::jarray v::obj) (set! (-> |#!bigloo_wallow| o $) v))
(define-inline (jarray-magic?::bool o::jarray) (-> |#!bigloo_wallow| o magic?))
(define-inline (jarray-magic?-set! o::jarray v::bool) (set! (-> |#!bigloo_wallow| o magic?) v))
(define-inline (jarray-init?::bool o::jarray) (-> |#!bigloo_wallow| o init?))
(define-inline (jarray-init?-set! o::jarray v::bool) (set! (-> |#!bigloo_wallow| o init?) v))
(define-inline (jarray-parents::obj o::jarray) (-> |#!bigloo_wallow| o parents))
(define-inline (jarray-parents-set! o::jarray v::obj) (set! (-> |#!bigloo_wallow| o parents) v))
(define-inline (jarray-coerce-to::obj o::jarray) (-> |#!bigloo_wallow| o coerce-to))
(define-inline (jarray-coerce-to-set! o::jarray v::obj) (set! (-> |#!bigloo_wallow| o coerce-to) v))
(define-inline (jarray-class::obj o::jarray) (-> |#!bigloo_wallow| o class))
(define-inline (jarray-class-set! o::jarray v::obj) (set! (-> |#!bigloo_wallow| o class) v))
(define-inline (jarray-size::obj o::jarray) (-> |#!bigloo_wallow| o size))
(define-inline (jarray-size-set! o::jarray v::obj) (set! (-> |#!bigloo_wallow| o size) v))
(define-inline (jarray-name::obj o::jarray) (-> |#!bigloo_wallow| o name))
(define-inline (jarray-name-set! o::jarray v::obj) (set! (-> |#!bigloo_wallow| o name) v))
(define-inline (jarray-id::symbol o::jarray) (-> |#!bigloo_wallow| o id))
(define-inline (jarray-id-set! o::jarray v::symbol) (set! (-> |#!bigloo_wallow| o id) v))
))
