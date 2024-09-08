;; ==========================================================
;; Class accessors
;; Bigloo (3.8a)
;; Inria -- Sophia Antipolis     Wed Mar 14 07:20:00 CET 2012 
;; (bigloo Ast/location.scm -classgen)
;; ==========================================================

;; The directives
(directives

;; source
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (static
    (inline make-source::source fname1051::bstring point1052::bint)
    (inline source?::bool ::obj)
    (source-nil::source)
    (inline source-point::bint ::source)
    (inline source-fname::bstring ::source)))))

;; The definitions
(cond-expand (bigloo-class-sans
;; source
(define-inline (make-source::source fname1051::bstring point1052::bint) (instantiate::source (fname fname1051) (point point1052)))
(define-inline (source?::bool obj::obj) ((@ isa? __object) obj (@ source ast_location)))
(define (source-nil::source) (class-nil (@ source ast_location)))
(define-inline (source-point::bint o::source) (with-access::source o (point) point))
(define-inline (source-point-set! o::source v::bint) (with-access::source o (point) (set! point v)))
(define-inline (source-fname::bstring o::source) (with-access::source o (fname) fname))
(define-inline (source-fname-set! o::source v::bstring) (with-access::source o (fname) (set! fname v)))
))
