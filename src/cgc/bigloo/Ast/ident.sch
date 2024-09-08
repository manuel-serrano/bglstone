;; ==========================================================
;; Class accessors
;; Bigloo (3.8a)
;; Inria -- Sophia Antipolis     Wed Mar 14 07:20:00 CET 2012 
;; (bigloo Ast/ident.scm -classgen)
;; ==========================================================

;; The directives
(directives

;; ident
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-ident::ident name1051::bstring location1052::obj)
    (inline ident?::bool ::obj)
    (ident-nil::ident)
    (inline ident-location::obj ::ident)
    (inline ident-name::bstring ::ident)))))

;; The definitions
(cond-expand (bigloo-class-sans
;; ident
(define-inline (make-ident::ident name1051::bstring location1052::obj) (instantiate::ident (name name1051) (location location1052)))
(define-inline (ident?::bool obj::obj) ((@ isa? __object) obj (@ ident ast_ident)))
(define (ident-nil::ident) (class-nil (@ ident ast_ident)))
(define-inline (ident-location::obj o::ident) (with-access::ident o (location) location))
(define-inline (ident-location-set! o::ident v::obj) (with-access::ident o (location) (set! location v)))
(define-inline (ident-name::bstring o::ident) (with-access::ident o (name) name))
(define-inline (ident-name-set! o::ident v::bstring) (with-access::ident o (name) (set! name v)))
))
