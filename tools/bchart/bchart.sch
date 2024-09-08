;; ==========================================================
;; Class accessors
;; Bigloo (3.8a)
;; Inria -- Sophia Antipolis     Wed Mar 14 07:20:00 CET 2012 
;; (bigloo bchart.scm -classgen)
;; ==========================================================

;; The directives
(directives

;; chart
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (static
    (inline make-chart::chart title1055::bstring max-scale-value1056::obj max-value1057::obj bar-names1058::obj bar-names-length1059::obj bar-values1060::obj)
    (inline chart?::bool ::obj)
    (chart-nil::chart)
    (inline chart-bar-values::obj ::chart)
    (inline chart-bar-values-set! ::chart ::obj)
    (inline chart-bar-names-length::obj ::chart)
    (inline chart-bar-names-length-set! ::chart ::obj)
    (inline chart-bar-names::obj ::chart)
    (inline chart-bar-names-set! ::chart ::obj)
    (inline chart-max-value::obj ::chart)
    (inline chart-max-value-set! ::chart ::obj)
    (inline chart-max-scale-value::obj ::chart)
    (inline chart-max-scale-value-set! ::chart ::obj)
    (inline chart-title::bstring ::chart)
    (inline chart-title-set! ::chart ::bstring)))))

;; The definitions
(cond-expand (bigloo-class-sans
;; chart
(define-inline (make-chart::chart title1055::bstring max-scale-value1056::obj max-value1057::obj bar-names1058::obj bar-names-length1059::obj bar-values1060::obj) (instantiate::chart (title title1055) (max-scale-value max-scale-value1056) (max-value max-value1057) (bar-names bar-names1058) (bar-names-length bar-names-length1059) (bar-values bar-values1060)))
(define-inline (chart?::bool obj::obj) ((@ isa? __object) obj (@ chart bchart)))
(define (chart-nil::chart) (class-nil (@ chart bchart)))
(define-inline (chart-bar-values::obj o::chart) (with-access::chart o (bar-values) bar-values))
(define-inline (chart-bar-values-set! o::chart v::obj) (with-access::chart o (bar-values) (set! bar-values v)))
(define-inline (chart-bar-names-length::obj o::chart) (with-access::chart o (bar-names-length) bar-names-length))
(define-inline (chart-bar-names-length-set! o::chart v::obj) (with-access::chart o (bar-names-length) (set! bar-names-length v)))
(define-inline (chart-bar-names::obj o::chart) (with-access::chart o (bar-names) bar-names))
(define-inline (chart-bar-names-set! o::chart v::obj) (with-access::chart o (bar-names) (set! bar-names v)))
(define-inline (chart-max-value::obj o::chart) (with-access::chart o (max-value) max-value))
(define-inline (chart-max-value-set! o::chart v::obj) (with-access::chart o (max-value) (set! max-value v)))
(define-inline (chart-max-scale-value::obj o::chart) (with-access::chart o (max-scale-value) max-scale-value))
(define-inline (chart-max-scale-value-set! o::chart v::obj) (with-access::chart o (max-scale-value) (set! max-scale-value v)))
(define-inline (chart-title::bstring o::chart) (with-access::chart o (title) title))
(define-inline (chart-title-set! o::chart v::bstring) (with-access::chart o (title) (set! title v)))
))
