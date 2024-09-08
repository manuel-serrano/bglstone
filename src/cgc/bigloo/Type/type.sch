;; ==========================================================
;; Class accessors
;; Bigloo (3.8a)
;; Inria -- Sophia Antipolis     Wed Mar 14 07:20:00 CET 2012 
;; (bigloo Type/type.scm -classgen)
;; ==========================================================

;; The directives
(directives

;; type
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-type::type id1105::ident)
    (inline type?::bool ::obj)
    (type-nil::type)
    (inline type-id::ident ::type))))

;; alias
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-alias::alias id1102::ident of1103::obj)
    (inline alias?::bool ::obj)
    (alias-nil::alias)
    (inline alias-of::obj ::alias)
    (inline alias-of-set! ::alias ::obj)
    (inline alias-id::ident ::alias))))

;; structure
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-structure::structure id1099::ident fields1100::obj)
    (inline structure?::bool ::obj)
    (structure-nil::structure)
    (inline structure-fields::obj ::structure)
    (inline structure-fields-set! ::structure ::obj)
    (inline structure-id::ident ::structure))))

;; array
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-array::array id1096::ident of1097::obj)
    (inline array?::bool ::obj)
    (array-nil::array)
    (inline array-of::obj ::array)
    (inline array-of-set! ::array ::obj)
    (inline array-id::ident ::array)))))

;; The definitions
(cond-expand (bigloo-class-sans
;; type
(define-inline (make-type::type id1105::ident) (instantiate::type (id id1105)))
(define-inline (type?::bool obj::obj) ((@ isa? __object) obj (@ type type_type)))
(define (type-nil::type) (class-nil (@ type type_type)))
(define-inline (type-id::ident o::type) (with-access::type o (id) id))
(define-inline (type-id-set! o::type v::ident) (with-access::type o (id) (set! id v)))

;; alias
(define-inline (make-alias::alias id1102::ident of1103::obj) (instantiate::alias (id id1102) (of of1103)))
(define-inline (alias?::bool obj::obj) ((@ isa? __object) obj (@ alias type_type)))
(define (alias-nil::alias) (class-nil (@ alias type_type)))
(define-inline (alias-of::obj o::alias) (with-access::alias o (of) of))
(define-inline (alias-of-set! o::alias v::obj) (with-access::alias o (of) (set! of v)))
(define-inline (alias-id::ident o::alias) (with-access::alias o (id) id))
(define-inline (alias-id-set! o::alias v::ident) (with-access::alias o (id) (set! id v)))

;; structure
(define-inline (make-structure::structure id1099::ident fields1100::obj) (instantiate::structure (id id1099) (fields fields1100)))
(define-inline (structure?::bool obj::obj) ((@ isa? __object) obj (@ structure type_type)))
(define (structure-nil::structure) (class-nil (@ structure type_type)))
(define-inline (structure-fields::obj o::structure) (with-access::structure o (fields) fields))
(define-inline (structure-fields-set! o::structure v::obj) (with-access::structure o (fields) (set! fields v)))
(define-inline (structure-id::ident o::structure) (with-access::structure o (id) id))
(define-inline (structure-id-set! o::structure v::ident) (with-access::structure o (id) (set! id v)))

;; array
(define-inline (make-array::array id1096::ident of1097::obj) (instantiate::array (id id1096) (of of1097)))
(define-inline (array?::bool obj::obj) ((@ isa? __object) obj (@ array type_type)))
(define (array-nil::array) (class-nil (@ array type_type)))
(define-inline (array-of::obj o::array) (with-access::array o (of) of))
(define-inline (array-of-set! o::array v::obj) (with-access::array o (of) (set! of v)))
(define-inline (array-id::ident o::array) (with-access::array o (id) id))
(define-inline (array-id-set! o::array v::ident) (with-access::array o (id) (set! id v)))
))
