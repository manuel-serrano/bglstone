;; ==========================================================
;; Class accessors
;; Bigloo (4.7a)
;; Inria -- Sophia Antipolis     Thu Mar 6 07:34:14 AM CET 2025 
;; (bigloo -classgen Module/java.scm)
;; ==========================================================

;; The directives
(directives

;; jklass
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (static
    (inline make-jklass::jklass src1143::pair loc1144::obj id1145::symbol idd1146::symbol jname1147::obj package1148::obj fields1149::pair-nil methods1150::pair-nil constructors1151::pair-nil abstract?1152::bool module1153::obj)
    (inline jklass?::bool ::obj)
    (jklass-nil::jklass)
    (inline jklass-module::obj ::jklass)
    (inline jklass-module-set! ::jklass ::obj)
    (inline jklass-abstract?::bool ::jklass)
    (inline jklass-abstract?-set! ::jklass ::bool)
    (inline jklass-constructors::pair-nil ::jklass)
    (inline jklass-constructors-set! ::jklass ::pair-nil)
    (inline jklass-methods::pair-nil ::jklass)
    (inline jklass-methods-set! ::jklass ::pair-nil)
    (inline jklass-fields::pair-nil ::jklass)
    (inline jklass-fields-set! ::jklass ::pair-nil)
    (inline jklass-package::obj ::jklass)
    (inline jklass-package-set! ::jklass ::obj)
    (inline jklass-jname::obj ::jklass)
    (inline jklass-jname-set! ::jklass ::obj)
    (inline jklass-idd::symbol ::jklass)
    (inline jklass-id::symbol ::jklass)
    (inline jklass-loc::obj ::jklass)
    (inline jklass-src::pair ::jklass))))

;; jmethod
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (static
    (inline make-jmethod::jmethod src1137::pair id1138::symbol args1139::pair-nil jname1140::bstring modifiers1141::pair-nil)
    (inline jmethod?::bool ::obj)
    (jmethod-nil::jmethod)
    (inline jmethod-modifiers::pair-nil ::jmethod)
    (inline jmethod-jname::bstring ::jmethod)
    (inline jmethod-args::pair-nil ::jmethod)
    (inline jmethod-id::symbol ::jmethod)
    (inline jmethod-src::pair ::jmethod))))

;; jconstructor
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (static
    (inline make-jconstructor::jconstructor src1131::pair id1132::symbol args1133::pair-nil jname1134::bstring modifiers1135::pair-nil)
    (inline jconstructor?::bool ::obj)
    (jconstructor-nil::jconstructor)
    (inline jconstructor-modifiers::pair-nil ::jconstructor)
    (inline jconstructor-jname::bstring ::jconstructor)
    (inline jconstructor-args::pair-nil ::jconstructor)
    (inline jconstructor-id::symbol ::jconstructor)
    (inline jconstructor-src::pair ::jconstructor)))))

;; The definitions
(cond-expand (bigloo-class-sans
;; jklass
(define-inline (make-jklass::jklass src1143::pair loc1144::obj id1145::symbol idd1146::symbol jname1147::obj package1148::obj fields1149::pair-nil methods1150::pair-nil constructors1151::pair-nil abstract?1152::bool module1153::obj) (instantiate::jklass (src src1143) (loc loc1144) (id id1145) (idd idd1146) (jname jname1147) (package package1148) (fields fields1149) (methods methods1150) (constructors constructors1151) (abstract? abstract?1152) (module module1153)))
(define-inline (jklass?::bool obj::obj) ((@ isa? __object) obj (@ jklass module_java)))
(define (jklass-nil::jklass) (class-nil (@ jklass module_java)))
(define-inline (jklass-module::obj o::jklass) (-> |#!bigloo_wallow| o module))
(define-inline (jklass-module-set! o::jklass v::obj) (set! (-> |#!bigloo_wallow| o module) v))
(define-inline (jklass-abstract?::bool o::jklass) (-> |#!bigloo_wallow| o abstract?))
(define-inline (jklass-abstract?-set! o::jklass v::bool) (set! (-> |#!bigloo_wallow| o abstract?) v))
(define-inline (jklass-constructors::pair-nil o::jklass) (-> |#!bigloo_wallow| o constructors))
(define-inline (jklass-constructors-set! o::jklass v::pair-nil) (set! (-> |#!bigloo_wallow| o constructors) v))
(define-inline (jklass-methods::pair-nil o::jklass) (-> |#!bigloo_wallow| o methods))
(define-inline (jklass-methods-set! o::jklass v::pair-nil) (set! (-> |#!bigloo_wallow| o methods) v))
(define-inline (jklass-fields::pair-nil o::jklass) (-> |#!bigloo_wallow| o fields))
(define-inline (jklass-fields-set! o::jklass v::pair-nil) (set! (-> |#!bigloo_wallow| o fields) v))
(define-inline (jklass-package::obj o::jklass) (-> |#!bigloo_wallow| o package))
(define-inline (jklass-package-set! o::jklass v::obj) (set! (-> |#!bigloo_wallow| o package) v))
(define-inline (jklass-jname::obj o::jklass) (-> |#!bigloo_wallow| o jname))
(define-inline (jklass-jname-set! o::jklass v::obj) (set! (-> |#!bigloo_wallow| o jname) v))
(define-inline (jklass-idd::symbol o::jklass) (-> |#!bigloo_wallow| o idd))
(define-inline (jklass-idd-set! o::jklass v::symbol) (set! (-> |#!bigloo_wallow| o idd) v))
(define-inline (jklass-id::symbol o::jklass) (-> |#!bigloo_wallow| o id))
(define-inline (jklass-id-set! o::jklass v::symbol) (set! (-> |#!bigloo_wallow| o id) v))
(define-inline (jklass-loc::obj o::jklass) (-> |#!bigloo_wallow| o loc))
(define-inline (jklass-loc-set! o::jklass v::obj) (set! (-> |#!bigloo_wallow| o loc) v))
(define-inline (jklass-src::pair o::jklass) (-> |#!bigloo_wallow| o src))
(define-inline (jklass-src-set! o::jklass v::pair) (set! (-> |#!bigloo_wallow| o src) v))

;; jmethod
(define-inline (make-jmethod::jmethod src1137::pair id1138::symbol args1139::pair-nil jname1140::bstring modifiers1141::pair-nil) (instantiate::jmethod (src src1137) (id id1138) (args args1139) (jname jname1140) (modifiers modifiers1141)))
(define-inline (jmethod?::bool obj::obj) ((@ isa? __object) obj (@ jmethod module_java)))
(define (jmethod-nil::jmethod) (class-nil (@ jmethod module_java)))
(define-inline (jmethod-modifiers::pair-nil o::jmethod) (-> |#!bigloo_wallow| o modifiers))
(define-inline (jmethod-modifiers-set! o::jmethod v::pair-nil) (set! (-> |#!bigloo_wallow| o modifiers) v))
(define-inline (jmethod-jname::bstring o::jmethod) (-> |#!bigloo_wallow| o jname))
(define-inline (jmethod-jname-set! o::jmethod v::bstring) (set! (-> |#!bigloo_wallow| o jname) v))
(define-inline (jmethod-args::pair-nil o::jmethod) (-> |#!bigloo_wallow| o args))
(define-inline (jmethod-args-set! o::jmethod v::pair-nil) (set! (-> |#!bigloo_wallow| o args) v))
(define-inline (jmethod-id::symbol o::jmethod) (-> |#!bigloo_wallow| o id))
(define-inline (jmethod-id-set! o::jmethod v::symbol) (set! (-> |#!bigloo_wallow| o id) v))
(define-inline (jmethod-src::pair o::jmethod) (-> |#!bigloo_wallow| o src))
(define-inline (jmethod-src-set! o::jmethod v::pair) (set! (-> |#!bigloo_wallow| o src) v))

;; jconstructor
(define-inline (make-jconstructor::jconstructor src1131::pair id1132::symbol args1133::pair-nil jname1134::bstring modifiers1135::pair-nil) (instantiate::jconstructor (src src1131) (id id1132) (args args1133) (jname jname1134) (modifiers modifiers1135)))
(define-inline (jconstructor?::bool obj::obj) ((@ isa? __object) obj (@ jconstructor module_java)))
(define (jconstructor-nil::jconstructor) (class-nil (@ jconstructor module_java)))
(define-inline (jconstructor-modifiers::pair-nil o::jconstructor) (-> |#!bigloo_wallow| o modifiers))
(define-inline (jconstructor-modifiers-set! o::jconstructor v::pair-nil) (set! (-> |#!bigloo_wallow| o modifiers) v))
(define-inline (jconstructor-jname::bstring o::jconstructor) (-> |#!bigloo_wallow| o jname))
(define-inline (jconstructor-jname-set! o::jconstructor v::bstring) (set! (-> |#!bigloo_wallow| o jname) v))
(define-inline (jconstructor-args::pair-nil o::jconstructor) (-> |#!bigloo_wallow| o args))
(define-inline (jconstructor-args-set! o::jconstructor v::pair-nil) (set! (-> |#!bigloo_wallow| o args) v))
(define-inline (jconstructor-id::symbol o::jconstructor) (-> |#!bigloo_wallow| o id))
(define-inline (jconstructor-id-set! o::jconstructor v::symbol) (set! (-> |#!bigloo_wallow| o id) v))
(define-inline (jconstructor-src::pair o::jconstructor) (-> |#!bigloo_wallow| o src))
(define-inline (jconstructor-src-set! o::jconstructor v::pair) (set! (-> |#!bigloo_wallow| o src) v))
))
