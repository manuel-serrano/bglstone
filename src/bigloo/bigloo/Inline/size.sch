;; ==========================================================
;; Class accessors
;; Bigloo (4.7a)
;; Inria -- Sophia Antipolis     Thu Mar 6 07:34:14 AM CET 2025 
;; (bigloo -classgen Inline/size.scm)
;; ==========================================================

;; The directives
(directives

;; sized-sequence
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (static
    (inline make-sized-sequence::sized-sequence loc1179::obj type1180::type side-effect?1181::obj key1182::obj nodes1183::obj size1184::long)
    (inline sized-sequence?::bool ::obj)
    (sized-sequence-nil::sized-sequence)
    (inline sized-sequence-size::long ::sized-sequence)
    (inline sized-sequence-nodes::obj ::sized-sequence)
    (inline sized-sequence-key::obj ::sized-sequence)
    (inline sized-sequence-key-set! ::sized-sequence ::obj)
    (inline sized-sequence-side-effect?::obj ::sized-sequence)
    (inline sized-sequence-side-effect?-set! ::sized-sequence ::obj)
    (inline sized-sequence-type::type ::sized-sequence)
    (inline sized-sequence-type-set! ::sized-sequence ::type)
    (inline sized-sequence-loc::obj ::sized-sequence))))

;; sized-select
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (static
    (inline make-sized-select::sized-select loc1168::obj type1169::type side-effect?1170::obj key1171::obj test1172::node clauses1173::obj item-type1174::type size1176::long)
    (inline sized-select?::bool ::obj)
    (sized-select-nil::sized-select)
    (inline sized-select-size::long ::sized-select)
    (inline sized-select-item-type::type ::sized-select)
    (inline sized-select-clauses::obj ::sized-select)
    (inline sized-select-test::node ::sized-select)
    (inline sized-select-test-set! ::sized-select ::node)
    (inline sized-select-key::obj ::sized-select)
    (inline sized-select-key-set! ::sized-select ::obj)
    (inline sized-select-side-effect?::obj ::sized-select)
    (inline sized-select-side-effect?-set! ::sized-select ::obj)
    (inline sized-select-type::type ::sized-select)
    (inline sized-select-type-set! ::sized-select ::type)
    (inline sized-select-loc::obj ::sized-select))))

;; sized-let-fun
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (static
    (inline make-sized-let-fun::sized-let-fun loc1160::obj type1161::type side-effect?1162::obj key1163::obj locals1164::obj body1165::node size1166::long)
    (inline sized-let-fun?::bool ::obj)
    (sized-let-fun-nil::sized-let-fun)
    (inline sized-let-fun-size::long ::sized-let-fun)
    (inline sized-let-fun-body::node ::sized-let-fun)
    (inline sized-let-fun-body-set! ::sized-let-fun ::node)
    (inline sized-let-fun-locals::obj ::sized-let-fun)
    (inline sized-let-fun-locals-set! ::sized-let-fun ::obj)
    (inline sized-let-fun-key::obj ::sized-let-fun)
    (inline sized-let-fun-key-set! ::sized-let-fun ::obj)
    (inline sized-let-fun-side-effect?::obj ::sized-let-fun)
    (inline sized-let-fun-side-effect?-set! ::sized-let-fun ::obj)
    (inline sized-let-fun-type::type ::sized-let-fun)
    (inline sized-let-fun-type-set! ::sized-let-fun ::type)
    (inline sized-let-fun-loc::obj ::sized-let-fun))))

;; sized-let-var
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (static
    (inline make-sized-let-var::sized-let-var loc1150::obj type1151::type side-effect?1152::obj key1153::obj bindings1154::obj body1155::node removable?1156::bool size1158::long)
    (inline sized-let-var?::bool ::obj)
    (sized-let-var-nil::sized-let-var)
    (inline sized-let-var-size::long ::sized-let-var)
    (inline sized-let-var-removable?::bool ::sized-let-var)
    (inline sized-let-var-removable?-set! ::sized-let-var ::bool)
    (inline sized-let-var-body::node ::sized-let-var)
    (inline sized-let-var-body-set! ::sized-let-var ::node)
    (inline sized-let-var-bindings::obj ::sized-let-var)
    (inline sized-let-var-bindings-set! ::sized-let-var ::obj)
    (inline sized-let-var-key::obj ::sized-let-var)
    (inline sized-let-var-key-set! ::sized-let-var ::obj)
    (inline sized-let-var-side-effect?::obj ::sized-let-var)
    (inline sized-let-var-side-effect?-set! ::sized-let-var ::obj)
    (inline sized-let-var-type::type ::sized-let-var)
    (inline sized-let-var-type-set! ::sized-let-var ::type)
    (inline sized-let-var-loc::obj ::sized-let-var)))))

;; The definitions
(cond-expand (bigloo-class-sans
;; sized-sequence
(define-inline (make-sized-sequence::sized-sequence loc1179::obj type1180::type side-effect?1181::obj key1182::obj nodes1183::obj size1184::long) (instantiate::sized-sequence (loc loc1179) (type type1180) (side-effect? side-effect?1181) (key key1182) (nodes nodes1183) (size size1184)))
(define-inline (sized-sequence?::bool obj::obj) ((@ isa? __object) obj (@ sized-sequence inline_size)))
(define (sized-sequence-nil::sized-sequence) (class-nil (@ sized-sequence inline_size)))
(define-inline (sized-sequence-size::long o::sized-sequence) (-> |#!bigloo_wallow| o size))
(define-inline (sized-sequence-size-set! o::sized-sequence v::long) (set! (-> |#!bigloo_wallow| o size) v))
(define-inline (sized-sequence-nodes::obj o::sized-sequence) (-> |#!bigloo_wallow| o nodes))
(define-inline (sized-sequence-nodes-set! o::sized-sequence v::obj) (set! (-> |#!bigloo_wallow| o nodes) v))
(define-inline (sized-sequence-key::obj o::sized-sequence) (-> |#!bigloo_wallow| o key))
(define-inline (sized-sequence-key-set! o::sized-sequence v::obj) (set! (-> |#!bigloo_wallow| o key) v))
(define-inline (sized-sequence-side-effect?::obj o::sized-sequence) (-> |#!bigloo_wallow| o side-effect?))
(define-inline (sized-sequence-side-effect?-set! o::sized-sequence v::obj) (set! (-> |#!bigloo_wallow| o side-effect?) v))
(define-inline (sized-sequence-type::type o::sized-sequence) (-> |#!bigloo_wallow| o type))
(define-inline (sized-sequence-type-set! o::sized-sequence v::type) (set! (-> |#!bigloo_wallow| o type) v))
(define-inline (sized-sequence-loc::obj o::sized-sequence) (-> |#!bigloo_wallow| o loc))
(define-inline (sized-sequence-loc-set! o::sized-sequence v::obj) (set! (-> |#!bigloo_wallow| o loc) v))

;; sized-select
(define-inline (make-sized-select::sized-select loc1168::obj type1169::type side-effect?1170::obj key1171::obj test1172::node clauses1173::obj item-type1174::type size1176::long) (instantiate::sized-select (loc loc1168) (type type1169) (side-effect? side-effect?1170) (key key1171) (test test1172) (clauses clauses1173) (item-type item-type1174) (size size1176)))
(define-inline (sized-select?::bool obj::obj) ((@ isa? __object) obj (@ sized-select inline_size)))
(define (sized-select-nil::sized-select) (class-nil (@ sized-select inline_size)))
(define-inline (sized-select-size::long o::sized-select) (-> |#!bigloo_wallow| o size))
(define-inline (sized-select-size-set! o::sized-select v::long) (set! (-> |#!bigloo_wallow| o size) v))
(define-inline (sized-select-item-type::type o::sized-select) (-> |#!bigloo_wallow| o item-type))
(define-inline (sized-select-item-type-set! o::sized-select v::type) (set! (-> |#!bigloo_wallow| o item-type) v))
(define-inline (sized-select-clauses::obj o::sized-select) (-> |#!bigloo_wallow| o clauses))
(define-inline (sized-select-clauses-set! o::sized-select v::obj) (set! (-> |#!bigloo_wallow| o clauses) v))
(define-inline (sized-select-test::node o::sized-select) (-> |#!bigloo_wallow| o test))
(define-inline (sized-select-test-set! o::sized-select v::node) (set! (-> |#!bigloo_wallow| o test) v))
(define-inline (sized-select-key::obj o::sized-select) (-> |#!bigloo_wallow| o key))
(define-inline (sized-select-key-set! o::sized-select v::obj) (set! (-> |#!bigloo_wallow| o key) v))
(define-inline (sized-select-side-effect?::obj o::sized-select) (-> |#!bigloo_wallow| o side-effect?))
(define-inline (sized-select-side-effect?-set! o::sized-select v::obj) (set! (-> |#!bigloo_wallow| o side-effect?) v))
(define-inline (sized-select-type::type o::sized-select) (-> |#!bigloo_wallow| o type))
(define-inline (sized-select-type-set! o::sized-select v::type) (set! (-> |#!bigloo_wallow| o type) v))
(define-inline (sized-select-loc::obj o::sized-select) (-> |#!bigloo_wallow| o loc))
(define-inline (sized-select-loc-set! o::sized-select v::obj) (set! (-> |#!bigloo_wallow| o loc) v))

;; sized-let-fun
(define-inline (make-sized-let-fun::sized-let-fun loc1160::obj type1161::type side-effect?1162::obj key1163::obj locals1164::obj body1165::node size1166::long) (instantiate::sized-let-fun (loc loc1160) (type type1161) (side-effect? side-effect?1162) (key key1163) (locals locals1164) (body body1165) (size size1166)))
(define-inline (sized-let-fun?::bool obj::obj) ((@ isa? __object) obj (@ sized-let-fun inline_size)))
(define (sized-let-fun-nil::sized-let-fun) (class-nil (@ sized-let-fun inline_size)))
(define-inline (sized-let-fun-size::long o::sized-let-fun) (-> |#!bigloo_wallow| o size))
(define-inline (sized-let-fun-size-set! o::sized-let-fun v::long) (set! (-> |#!bigloo_wallow| o size) v))
(define-inline (sized-let-fun-body::node o::sized-let-fun) (-> |#!bigloo_wallow| o body))
(define-inline (sized-let-fun-body-set! o::sized-let-fun v::node) (set! (-> |#!bigloo_wallow| o body) v))
(define-inline (sized-let-fun-locals::obj o::sized-let-fun) (-> |#!bigloo_wallow| o locals))
(define-inline (sized-let-fun-locals-set! o::sized-let-fun v::obj) (set! (-> |#!bigloo_wallow| o locals) v))
(define-inline (sized-let-fun-key::obj o::sized-let-fun) (-> |#!bigloo_wallow| o key))
(define-inline (sized-let-fun-key-set! o::sized-let-fun v::obj) (set! (-> |#!bigloo_wallow| o key) v))
(define-inline (sized-let-fun-side-effect?::obj o::sized-let-fun) (-> |#!bigloo_wallow| o side-effect?))
(define-inline (sized-let-fun-side-effect?-set! o::sized-let-fun v::obj) (set! (-> |#!bigloo_wallow| o side-effect?) v))
(define-inline (sized-let-fun-type::type o::sized-let-fun) (-> |#!bigloo_wallow| o type))
(define-inline (sized-let-fun-type-set! o::sized-let-fun v::type) (set! (-> |#!bigloo_wallow| o type) v))
(define-inline (sized-let-fun-loc::obj o::sized-let-fun) (-> |#!bigloo_wallow| o loc))
(define-inline (sized-let-fun-loc-set! o::sized-let-fun v::obj) (set! (-> |#!bigloo_wallow| o loc) v))

;; sized-let-var
(define-inline (make-sized-let-var::sized-let-var loc1150::obj type1151::type side-effect?1152::obj key1153::obj bindings1154::obj body1155::node removable?1156::bool size1158::long) (instantiate::sized-let-var (loc loc1150) (type type1151) (side-effect? side-effect?1152) (key key1153) (bindings bindings1154) (body body1155) (removable? removable?1156) (size size1158)))
(define-inline (sized-let-var?::bool obj::obj) ((@ isa? __object) obj (@ sized-let-var inline_size)))
(define (sized-let-var-nil::sized-let-var) (class-nil (@ sized-let-var inline_size)))
(define-inline (sized-let-var-size::long o::sized-let-var) (-> |#!bigloo_wallow| o size))
(define-inline (sized-let-var-size-set! o::sized-let-var v::long) (set! (-> |#!bigloo_wallow| o size) v))
(define-inline (sized-let-var-removable?::bool o::sized-let-var) (-> |#!bigloo_wallow| o removable?))
(define-inline (sized-let-var-removable?-set! o::sized-let-var v::bool) (set! (-> |#!bigloo_wallow| o removable?) v))
(define-inline (sized-let-var-body::node o::sized-let-var) (-> |#!bigloo_wallow| o body))
(define-inline (sized-let-var-body-set! o::sized-let-var v::node) (set! (-> |#!bigloo_wallow| o body) v))
(define-inline (sized-let-var-bindings::obj o::sized-let-var) (-> |#!bigloo_wallow| o bindings))
(define-inline (sized-let-var-bindings-set! o::sized-let-var v::obj) (set! (-> |#!bigloo_wallow| o bindings) v))
(define-inline (sized-let-var-key::obj o::sized-let-var) (-> |#!bigloo_wallow| o key))
(define-inline (sized-let-var-key-set! o::sized-let-var v::obj) (set! (-> |#!bigloo_wallow| o key) v))
(define-inline (sized-let-var-side-effect?::obj o::sized-let-var) (-> |#!bigloo_wallow| o side-effect?))
(define-inline (sized-let-var-side-effect?-set! o::sized-let-var v::obj) (set! (-> |#!bigloo_wallow| o side-effect?) v))
(define-inline (sized-let-var-type::type o::sized-let-var) (-> |#!bigloo_wallow| o type))
(define-inline (sized-let-var-type-set! o::sized-let-var v::type) (set! (-> |#!bigloo_wallow| o type) v))
(define-inline (sized-let-var-loc::obj o::sized-let-var) (-> |#!bigloo_wallow| o loc))
(define-inline (sized-let-var-loc-set! o::sized-let-var v::obj) (set! (-> |#!bigloo_wallow| o loc) v))
))
