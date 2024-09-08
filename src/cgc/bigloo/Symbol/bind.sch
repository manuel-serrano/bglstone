;; ==========================================================
;; Class accessors
;; Bigloo (3.8a)
;; Inria -- Sophia Antipolis     Wed Mar 14 07:20:00 CET 2012 
;; (bigloo Symbol/bind.scm -classgen)
;; ==========================================================

;; The directives
(directives

;; symbol-entry
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (static
    (inline make-symbol-entry::symbol-entry name1086::bstring decl1087::decl)
    (inline symbol-entry?::bool ::obj)
    (symbol-entry-nil::symbol-entry)
    (inline symbol-entry-decl::decl ::symbol-entry)
    (inline symbol-entry-name::bstring ::symbol-entry)))))

;; The definitions
(cond-expand (bigloo-class-sans
;; symbol-entry
(define-inline (make-symbol-entry::symbol-entry name1086::bstring decl1087::decl) (instantiate::symbol-entry (name name1086) (decl decl1087)))
(define-inline (symbol-entry?::bool obj::obj) ((@ isa? __object) obj (@ symbol-entry symbol_bind)))
(define (symbol-entry-nil::symbol-entry) (class-nil (@ symbol-entry symbol_bind)))
(define-inline (symbol-entry-decl::decl o::symbol-entry) (with-access::symbol-entry o (decl) decl))
(define-inline (symbol-entry-decl-set! o::symbol-entry v::decl) (with-access::symbol-entry o (decl) (set! decl v)))
(define-inline (symbol-entry-name::bstring o::symbol-entry) (with-access::symbol-entry o (name) name))
(define-inline (symbol-entry-name-set! o::symbol-entry v::bstring) (with-access::symbol-entry o (name) (set! name v)))
))
