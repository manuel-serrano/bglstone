;------------------------------------------------------------------------------
;
;                            A simple partial evaluator
;
;                              Marc Feeley (05/15/88)
;
;------------------------------------------------------------------------------

(module peval
   (main main))

; Utilities

(define (pevery? pred? l)
  (let loop ((l l))
    (or (null? l) (and (pred? (car l)) (loop (cdr l))))))

(define (some? pred? l)
  (let loop ((l l))
    (if (null? l) #f (or (pred? (car l)) (loop (cdr l))))))

(define (map2 f l1 l2)
  (let loop ((l1 l1) (l2 l2))
    (if (pair? l1)
      (cons (f (car l1) (car l2)) (loop (cdr l1) (cdr l2)))
      '())))

(define (plast-pair l)
  (let loop ((l l))
    (let ((x (cdr l))) (if (pair? x) (loop x) l))))

(define (proper-list? l) (list? l))

;------------------------------------------------------------------------------
;
; The partial evaluator.

(define (partial-evaluate proc args)
  (peval (alphatize proc '()) args))

(define (alphatize exp env) ; return a copy of 'exp' where each bound var has
  (define (alpha exp)       ; been renamed (to prevent aliasing problems)
    (cond ((const-expr? exp)
           (quot (const-value exp)))
          ((symbol? exp)
           (let ((x (assq exp env))) (if x (cdr x) exp)))
          ((or (eq? (car exp) 'if) (eq? (car exp) 'begin))
           (cons (car exp) (map alpha (cdr exp))))
          ((or (eq? (car exp) 'let) (eq? (car exp) 'letrec))
           (let ((new-env (new-variables (map car (cadr exp)) env)))
             (list (car exp)
                   (map (lambda (x)
                          (list (cdr (assq (car x) new-env))
                                (if (eq? (car exp) 'let)
                                  (alpha (cadr x))
                                  (alphatize (cadr x) new-env))))
                        (cadr exp))
                   (alphatize (caddr exp) new-env))))
          ((eq? (car exp) 'lambda)
           (let ((new-env (new-variables (cadr exp) env)))
             (list 'lambda
                   (map (lambda (x) (cdr (assq x new-env))) (cadr exp))
                   (alphatize (caddr exp) new-env))))
          (else
           (map alpha exp))))
  (alpha exp))

(define (const-expr? expr) ; is 'expr' a constant expression?
   (and (not (symbol? expr))
	(or (not (pair? expr))
	    (eq? (car expr) 'quote))))

(define (const-value expr) ; return the value of a constant expression
  (if (pair? expr) ; then it must be a quoted constant
    (cadr expr)
    expr))

(define (quot val) ; make a quoted constant whose value is 'val'
  (list 'quote val))

(define (new-variables parms env)
  (append (map (lambda (x) (cons x (new-variable x))) parms) env))

(define *current-num* 0)

(define (new-variable name)
  (set! *current-num* (+ *current-num* 1))
  (string->symbol
    (string-append (symbol->string name)
                   "_"
                   (number->string *current-num*))))

;------------------------------------------------------------------------------
;
; (peval proc args) will transform a procedure that is known to be called
; with constants as some of its arguments into a specialized procedure that
; is 'equivalent' but accepts only the non-constant parameters.  'proc' is the
; list representation of a lambda-expression and 'args' is a list of values,
; one for each parameter of the lambda-expression.  A special value (i.e.
; 'not-constant') is used to indicate an argument that is not a constant.
; The returned procedure is one that has as parameters the parameters of the
; original procedure which are NOT passed constants.  Constants will have been
; substituted for the constant parameters that are referenced in the body
; of the procedure.
;
; For example:
;
;   (peval
;     '(lambda (x y z) (f z x y)) ; the procedure
;     (list 1 not-constant #t))   ; the knowledge about x, y and z
;
; will return: (lambda (y) (f '#t '1 y))

(define (peval proc args)
  (simplify!
    (let ((parms (cadr proc))  ; get the parameter list
          (body (caddr proc))) ; get the body of the procedure
      (list 'lambda
            (remove-constant parms args) ; remove the constant parameters
            (beta-subst ; in the body, replace variable refs to the constant
              body      ; parameters by the corresponding constant
              (map2 (lambda (x y) (if (not-constant? y)
				      '(()) (cons x (quot y))))
                    parms
                    args))))))

(define not-constant (list '?)) ; special value indicating non-constant parms.

(define (not-constant? x) (eq? x not-constant))

(define (remove-constant l a) ; remove from list 'l' all elements whose
  (cond ((null? l)            ; corresponding element in 'a' is a constant
         '())
        ((not-constant? (car a))
         (cons (car l) (remove-constant (cdr l) (cdr a))))
        (else
         (remove-constant (cdr l) (cdr a)))))

(define (extract-constant l a) ; extract from list 'l' all elements whose
  (cond ((null? l)             ; corresponding element in 'a' is a constant
         '())
        ((not-constant? (car a))
         (extract-constant (cdr l) (cdr a)))
        (else
         (cons (car l) (extract-constant (cdr l) (cdr a))))))

(define (beta-subst exp env) ; return a modified 'exp' where each var named in
  (define (bs exp)           ; 'env' is replaced by the corresponding expr (it
    (cond ((const-expr? exp) ; is assumed that the code has been alphatized)
           (quot (const-value exp)))
          ((symbol? exp)
           (let ((x (assq exp env))) 
             (if x (cdr x) exp)))
          ((or (eq? (car exp) 'if) (eq? (car exp) 'begin))
           (cons (car exp) (map bs (cdr exp))))
          ((or (eq? (car exp) 'let) (eq? (car exp) 'letrec))
           (list (car exp)
                 (map (lambda (x) (list (car x) (bs (cadr x)))) (cadr exp))
                 (bs (caddr exp))))
          ((eq? (car exp) 'lambda)
           (list 'lambda
                 (cadr exp)
                 (bs (caddr exp))))
          (else
           (map bs exp))))
  (bs exp))

;------------------------------------------------------------------------------
;
; The expression simplifier.

(define (simplify! exp)     ; simplify the expression 'exp' destructively (it
                            ; is assumed that the code has been alphatized)
  (define (simp! where env)

    (define (s! where)
      (let ((exp (car where)))

        (cond ((const-expr? exp))  ; leave constants the way they are

              ((symbol? exp))      ; leave variable references the way they are

              ((eq? (car exp) 'if) ; dead code removal for conditionals
               (s! (cdr exp))      ; simplify the predicate
               (if (const-expr? (cadr exp)) ; is the predicate a constant?
                 (begin
                   (set-car! where
                     (if (memq (const-value (cadr exp)) '(#f ())) ; false?
                       (if (= (length exp) 3) ''() (cadddr exp))
                       (caddr exp)))
                   (s! where))
                 (for-each! s! (cddr exp)))) ; simplify consequent and alt.

              ((eq? (car exp) 'begin)
               (for-each! s! (cdr exp))
               (let loop ((exps exp)) ; remove all useless expressions
                 (if (not (null? (cddr exps))) ; not last expression?
                   (let ((x (cadr exps)))
                     (loop (if (or (const-expr? x)
                                   (symbol? x)
                                   (and (pair? x) (eq? (car x) 'lambda)))
                             (begin (set-cdr! exps (cddr exps)) exps)
                             (cdr exps))))))
               (if (null? (cddr exp)) ; only one expression in the begin?
                 (set-car! where (cadr exp))))

              ((or (eq? (car exp) 'let) (eq? (car exp) 'letrec))
               (let ((new-env (cons exp env)))
                 (define (keep i)
                   (if (>= i (length (cadar where)))
                     '()
                     (let* ((var (car (list-ref (cadar where) i)))
                            (val (cadr (assq var (cadar where))))
                            (refs (ref-count (car where) var))
                            (self-refs (ref-count val var))
                            (total-refs (- (car refs) (car self-refs)))
                            (oper-refs (- (cadr refs) (cadr self-refs))))
                       (cond ((= total-refs 0)
                              (keep (+ i 1)))
                             ((or (const-expr? val)
                                  (symbol? val)
                                  (and (pair? val)
                                       (eq? (car val) 'lambda)
                                       (= total-refs 1)
                                       (= oper-refs 1)
                                       (= (car self-refs) 0))
                                  (and (caddr refs)
                                       (= total-refs 1)))
                              (set-car! where
                                (beta-subst (car where)
                                            (list (cons var val))))
                              (keep (+ i 1)))
                             (else
                              (cons var (keep (+ i 1))))))))
                 (simp! (cddr exp) new-env)
                 (for-each! (lambda (x) (simp! (cdar x) new-env)) (cadr exp))
                 (let ((to-keep (keep 0)))
                   (if (< (length to-keep) (length (cadar where)))
                     (begin
                       (if (null? to-keep)
                         (set-car! where (caddar where))
                         (set-car! (cdar where)
                           (map (lambda (v) (assq v (cadar where))) to-keep)))
                       (s! where))
                     (if (null? to-keep)
                       (set-car! where (caddar where)))))))

              ((eq? (car exp) 'lambda)
               (simp! (cddr exp) (cons exp env)))

              (else
               (for-each! s! exp)
               (cond ((symbol? (car exp)) ; is the operator position a var ref?
                      (let ((frame (binding-frame (car exp) env)))
                        (if frame ; is it a bound variable?
                          (let ((proc (bound-expr (car exp) frame)))
                            (if (and (pair? proc)
                                     (eq? (car proc) 'lambda)
                                     (some? const-expr? (cdr exp)))
                              (let* ((args (arg-pattern (cdr exp)))
                                     (new-proc (peval proc args))
                                     (new-args (remove-constant (cdr exp)
								args)))
                                (set-car! where
                                  (cons (add-binding new-proc frame (car exp))
                                        new-args)))))
                          (set-car! where
                            (constant-fold-global (car exp) (cdr exp))))))
                     ((not (pair? (car exp))))
                     ((eq? (caar exp) 'lambda)
                      (set-car! where
                        (list 'let
                              (map2 list (cadar exp) (cdr exp))
                              (caddar exp)))
                      (s! where)))))))

    (s! where))

  (define (remove-empty-calls! where env)

    (define (rec! where)
      (let ((exp (car where)))

        (cond ((const-expr? exp))
              ((symbol? exp))
              ((eq? (car exp) 'if)
               (rec! (cdr exp))
               (rec! (cddr exp))
               (rec! (cdddr exp)))
              ((eq? (car exp) 'begin)
               (for-each! rec! (cdr exp)))
              ((or (eq? (car exp) 'let) (eq? (car exp) 'letrec))
               (let ((new-env (cons exp env)))
                 (remove-empty-calls! (cddr exp) new-env)
                 (for-each! (lambda (x) (remove-empty-calls! (cdar x) new-env))
                            (cadr exp))))
              ((eq? (car exp) 'lambda)
               (rec! (cddr exp)))
              (else
               (for-each! rec! (cdr exp))
               (if (and (null? (cdr exp)) (symbol? (car exp)))
                 (let ((frame (binding-frame (car exp) env)))
                   (if frame ; is it a bound variable?
                     (let ((proc (bound-expr (car exp) frame)))
                       (if (and (pair? proc)
                                (eq? (car proc) 'lambda))
                         (begin
                           (set! changed? #t)
                           (set-car! where (caddr proc))))))))))))

    (rec! where))

  (define changed? #f)

  (let ((x (list exp)))
    (let loop ()
      (set! changed? #f)
      (simp! x '())
      (remove-empty-calls! x '())
      (if changed? (loop) (car x)))))

(define (ref-count exp var) ; compute how many references to variable 'var'
  (let ((total 0)           ; are contained in 'exp'
        (oper 0)
        (always-evaled #t))
    (define (rc exp ae)
      (cond ((const-expr? exp))
            ((symbol? exp)
             (if (eq? exp var)
               (begin
                 (set! total (+ total 1))
                 (set! always-evaled (and ae always-evaled)))))
            ((eq? (car exp) 'if)
             (rc (cadr exp) ae)
             (for-each (lambda (x) (rc x #f)) (cddr exp)))
            ((eq? (car exp) 'begin)
             (for-each (lambda (x) (rc x ae)) (cdr exp)))
            ((or (eq? (car exp) 'let) (eq? (car exp) 'letrec))
             (for-each (lambda (x) (rc (cadr x) ae)) (cadr exp))
             (rc (caddr exp) ae))
            ((eq? (car exp) 'lambda)
             (rc (caddr exp) #f))
            (else
             (for-each (lambda (x) (rc x ae)) exp)
             (if (symbol? (car exp))
               (if (eq? (car exp) var) (set! oper (+ oper 1)))))))
    (rc exp #t)
    (list total oper always-evaled)))

(define (binding-frame var env)
  (cond ((null? env) #f)
        ((or (eq? (caar env) 'let) (eq? (caar env) 'letrec))
         (if (assq var (cadar env)) (car env) (binding-frame var (cdr env))))
        ((eq? (caar env) 'lambda)
         (if (memq var (cadar env)) (car env) (binding-frame var (cdr env))))
        (else
         (error '() '() "ill-formed environment"))))

(define (bound-expr var frame)
  (cond ((or (eq? (car frame) 'let) (eq? (car frame) 'letrec))
         (cadr (assq var (cadr frame))))
        ((eq? (car frame) 'lambda)
         not-constant)
        (else
         (error '() '() "ill-formed frame"))))

(define (add-binding val frame name)
  (define (find-val val bindings)
    (cond ((null? bindings) #f)
          ((equal? val (cadar bindings)) ; *kludge* equal? is not exactly what
           (caar bindings))              ; we want...
          (else
           (find-val val (cdr bindings)))))
  (or (find-val val (cadr frame))
      (let ((var (new-variable name)))
        (set-cdr! (plast-pair (cadr frame)) (list (list var val)))
        var)))

(define (for-each! proc! l) ; call proc! on each CONS CELL in the list 'l'
  (if (not (null? l))
    (begin (proc! l) (for-each! proc! (cdr l)))))

(define (arg-pattern exps) ; return the argument pattern (i.e. the list of
  (if (null? exps)         ; constants in 'exps' but with the not-constant
    '()                    ; value wherever the corresponding expression in
    (cons (if (const-expr? (car exps)) ; 'exps' is not a constant)
            (const-value (car exps))
            not-constant)
          (arg-pattern (cdr exps)))))

;------------------------------------------------------------------------------
;
; Knowledge about primitive procedures.

(define *primitives*
  (list
    (cons 'car (lambda (args)
                 (and (= (length args) 1)
                      (pair? (car args))
                      (quot (car (car args))))))
    (cons 'cdr (lambda (args)
                 (and (= (length args) 1)
                      (pair? (car args))
                      (quot (cdr (car args))))))
    (cons '+ (lambda (args)
               (and (pevery? number? args) (quot (apply + args)))))
    (cons '* (lambda (args)
               (and (pevery? number? args) (quot (apply * args)))))
    (cons '- (lambda (args)
               (and (> (length args) 0)
                    (pevery? number? args)
                    (quot (apply - args)))))
    (cons '/ (lambda (args)
               (and (> (length args) 1)
                    (pevery? number? args)
                    (quot (apply / args)))))
    (cons '< (lambda (args)
               (and (= (length args) 2)
                    (pevery? number? args)
                    (quot (< (car args) (cadr args))))))
    (cons '= (lambda (args)
               (and (= (length args) 2)
                    (pevery? number? args)
                    (quot (= (car args) (cadr args))))))
    (cons '> (lambda (args)
               (and (= (length args) 2)
                    (pevery? number? args)
                    (quot (> (car args) (cadr args))))))
    (cons 'eq? (lambda (args)
                 (and (= (length args) 2)
                      (quot (eq? (car args) (cadr args))))))
    (cons 'not (lambda (args)
                 (and (= (length args) 1)
                      (quot (not (car args))))))
    (cons 'null? (lambda (args)
                   (and (= (length args) 1)
                        (quot (null? (car args))))))
    (cons 'pair? (lambda (args)
                   (and (= (length args) 1)
                        (quot (pair? (car args))))))
    (cons 'symbol? (lambda (args)
                     (and (= (length args) 1)
                          (quot (symbol? (car args))))))
    (cons 'length (lambda (args)
                    (and (= (length args) 1)
                         (proper-list? (car args))
                         (quot (length (car args))))))
  )
)

(define (reduce-global name args)
  (let ((x (assq name *primitives*)))
    (and x ((cdr x) args))))

(define (constant-fold-global name exprs)

  (define (flatten args op)
    (cond ((null? args)
           '())
          ((and (pair? (car args)) (eq? (caar args) op))
           (append (flatten (cdar args) op) (flatten (cdr args) op)))
          (else
           (cons (car args) (flatten (cdr args) op)))))

  (let ((args (if (or (eq? name '+) (eq? name '*)) ; associative ops
                (flatten exprs name)
                exprs)))
    (or (and (pevery? const-expr? args)
             (reduce-global name (map const-value args)))
        (let ((pattern (arg-pattern args)))
          (let ((non-const (remove-constant args pattern))
                (const (map const-value (extract-constant args pattern))))
            (cond ((eq? name '+) ; + is commutative
                   (let ((x (reduce-global '+ const)))
                     (if x
                       (let ((y (const-value x)))
                         (cons '+
                               (if (= y 0) non-const (cons x non-const))))
                       (cons name args))))
                  ((eq? name '*) ; * is commutative
                   (let ((x (reduce-global '* const)))
                     (if x
                       (let ((y (const-value x)))
                         (cons '*
                               (if (= y 1) non-const (cons x non-const))))
                       (cons name args))))
                  ((eq? name 'cons)
                   (cond ((and (const-expr? (cadr args))
                               (null? (const-value (cadr args))))
                          (list 'list (car args)))
                         ((and (pair? (cadr args))
                               (eq? (car (cadr args)) 'list))
                          (cons 'list (cons (car args) (cdr (cadr args)))))
                         (else
                          (cons name args))))
                  (else
                   (cons name args))))))))

;------------------------------------------------------------------------------
;
; Examples:

(define (my-try proc args)
  (partial-evaluate proc args))

; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

(define example1
  '(lambda (a b c)
     (if (null? a) b (+ (car a) c))))

;(my-try example1 (list '(10 11) not-constant '1))

; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

(define example2
  '(lambda (x y)
     (let ((q (lambda (a b) (if (< a 0) b (- 10 b)))))
       (if (< x 0) (q (- y) (- x)) (q y x)))))

;(my-try example2 (list not-constant '1))

; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

(define example3
  '(lambda (l n)
     (letrec ((add-list
               (lambda (l n)
                 (if (null? l)
                   '()
                   (cons (+ (car l) n) (add-list (cdr l) n))))))
       (add-list l n))))

;(my-try example3 (list not-constant '1))

;(my-try example3 (list '(1 2 3) not-constant))

; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

(define example4
  '(lambda (exp env)
     (letrec ((eval
               (lambda (exp env)
                 (letrec ((eval-list
                            (lambda (l env)
                              (if (null? l)
                                '()
                                (cons (eval (car l) env)
                                      (eval-list (cdr l) env))))))
                   (if (symbol? exp) (lookup exp env)
                     (if (not (pair? exp)) exp
                       (if (eq? (car exp) 'quote) (car (cdr exp))
                         (apply (eval (car exp) env)
                                (eval-list (cdr exp) env)))))))))
       (eval exp env))))

;(my-try example4 (list 'x not-constant))

;(my-try example4 (list '(f 1 2 3) not-constant))

; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

(define example5
  '(lambda (a b)
     (letrec ((funct
               (lambda (x)
                 (+ x b (if (< x 1) 0 (funct (- x 1)))))))
       (funct a))))

;(my-try example5 (list '5 not-constant))

; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

(define example6
  '(lambda ()
     (letrec ((fib
               (lambda (x)
                 (if (< x 2) x (+ (fib (- x 1)) (fib (- x 2)))))))
       (fib 10))))

;(my-try example6 '())

; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

(define example7
  '(lambda (input)
     (letrec ((copy (lambda (in)
		      (if (pair? in)
                        (cons (copy (car in))
			      (copy (cdr in)))
			in))))
       (copy input))))

;(my-try example7
;        (list '(a b c d e f g h i j k l m n o p q r s t u v w x y z)))

; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

(define example8
  '(lambda (input)
     (letrec ((reverse (lambda (in result)
			 (if (pair? in)
			   (reverse (cdr in) (cons (car in) result))
			   result))))
       (reverse input '()))))

;(my-try example8
;        (list '(a b c d e f g h i j k l m n o p q r s t u v w x y z)))

; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

(define (test)
  (list (my-try example1 (list '(10 11) not-constant '1))
	(my-try example2 (list not-constant '1))
	(my-try example3 (list not-constant '1))
	(my-try example3 (list '(1 2 3) not-constant))
	(my-try example4 (list 'x not-constant))
	(my-try example4 (list '(f 1 2 3) not-constant))
	(my-try example5 (list '5 not-constant))
	(my-try example6 '())
	(my-try example7
             (list '(a b c d e f g h i j k l m n o p q r s t u v w x y z)))
        (my-try example8
             (list '(a b c d e f g h i j k l m n o p q r s t u v w x y z)))))

(define expected-result
   '((lambda (b_2) (quote 11)) (lambda (x_4) (if (< x_4 (quote 0)) (- x_4) (- (quote 10) x_4))) (lambda (l_11) (letrec ((add-list_13_16 (lambda (l_14) (if (null? l_14) (quote ()) (cons (+ (quote 1) (car l_14)) (add-list_13_16 (cdr l_14))))))) (add-list_13_16 l_11))) (lambda (n_18) (list (+ (quote 1) n_18) (+ (quote 2) n_18) (+ (quote 3) n_18))) (lambda (env_27) (lookup (quote x) env_27)) (lambda (env_36) (apply (lookup (quote f) env_36) (list (quote 1) (quote 2) (quote 3)))) (lambda (b_53) (+ (quote 15) b_53 b_53 b_53 b_53 b_53 b_53)) (lambda () (quote 55)) (lambda () (list (quote a) (quote b) (quote c) (quote d) (quote e) (quote f) (quote g) (quote h) (quote i) (quote j) (quote k) (quote l) (quote m) (quote n) (quote o) (quote p) (quote q) (quote r) (quote s) (quote t) (quote u) (quote v) (quote w) (quote x) (quote y) (quote z))) (lambda () (list (quote z) (quote y) (quote x) (quote w) (quote v) (quote u) (quote t) (quote s) (quote r) (quote q) (quote p) (quote o) (quote n) (quote m) (quote l) (quote k) (quote j) (quote i) (quote h) (quote g) (quote f) (quote e) (quote d) (quote c) (quote b) (quote a)))))

(define (main argv)
   (let ((r (test))
	 (n (if (pair? (cdr argv))
		(if (string=? (cadr argv) "pmem")
		    480
		    (string->number (cadr argv)))
		4800)))
      (let loop ((i n))
	 (if (= i 1)
	     (begin
		(when (=fx n 1) (print r))
		(print "benchmark "
		   (if (equal? r expected-result) "success" "failed"))
		(exit (if (equal? r expected-result) 0 1)))
	     (begin
		(test)
		(loop (- i 1)))))))

