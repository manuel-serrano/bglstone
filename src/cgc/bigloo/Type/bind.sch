;; ==========================================================
;; Class accessors
;; Bigloo (3.8a)
;; Inria -- Sophia Antipolis     Wed Mar 14 07:20:00 CET 2012 
;; (bigloo Type/bind.scm -classgen)
;; ==========================================================

;; The directives
(directives

;; type-entry
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (static
    (inline make-type-entry::type-entry name1086::bstring type1087::obj)
    (inline type-entry?::bool ::obj)
    (type-entry-nil::type-entry)
    (inline type-entry-type::obj ::type-entry)
    (inline type-entry-type-set! ::type-entry ::obj)
    (inline type-entry-name::bstring ::type-entry)))))

;; The definitions
(cond-expand (bigloo-class-sans
;; type-entry
(define-inline (make-type-entry::type-entry name1086::bstring type1087::obj) (instantiate::type-entry (name name1086) (type type1087)))
(define-inline (type-entry?::bool obj::obj) ((@ isa? __object) obj (@ type-entry type_bind)))
(define (type-entry-nil::type-entry) (class-nil (@ type-entry type_bind)))
(define-inline (type-entry-type::obj o::type-entry) (with-access::type-entry o (type) type))
(define-inline (type-entry-type-set! o::type-entry v::obj) (with-access::type-entry o (type) (set! type v)))
(define-inline (type-entry-name::bstring o::type-entry) (with-access::type-entry o (name) name))
(define-inline (type-entry-name-set! o::type-entry v::bstring) (with-access::type-entry o (name) (set! name v)))
))
