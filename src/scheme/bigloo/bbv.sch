
(define-expander DEAD-END
   (lambda (x e)
      (match-case x
	 ((?- ?msg)
	  (if (epair? x)
	      (match-case (cer x)
		 ((?- ?fname ?loc)
		  `(error/location "bbv" "error" ,msg ,fname ,loc))
		 (else
		  `(error "bbv" "error" ,msg)))
	      `(error "bbv" "error" ,msg))))))

(define-macro (define-macro-arithmetic proto body)
   `(define-macro ,proto
      (define arithmetic
        (cond-expand
          (arithmeticG 'G)
          (arithmeticS 'S)
          (else 'S)))
      (define macros-constant-folding
        (cond-expand
          (macros-constant-folding #t)
          (else #f)))
      ,body))

(define-macro-arithmetic (FLop op . args)
   (case op
      ((atan2) `(atan-2fl ,@args))
      ((zero?) `(zerofl? ,@args))
      (else `(,(symbol-append op 'fl) ,@args))))
(define-macro-arithmetic (FXop op . args)
   (case op
      ((zero?) `(zerofx? ,@args))
      ((bit-lsh bit-and bit-or bit-not)
        `(,op ,@args))
      (else `(,(symbol-append op 'fx) ,@args))))

(define-macro-arithmetic (PRIMop op . args)
   (case op
      ((vector-set!) `(vector-set-ur! ,@args))
      ((vector-ref) `(vector-ref-ur ,@args))
      ((atan2) `(atan-2fl ,@args))
      ((car cdr set-car! set-cdr!
	  fixnum? flonum?
	  sqrt cos sin
	  vector-length string-ref string-set!
	  string-length substring)
       `(,(symbol-append '$ op) ,@args))
      ((string->symbol symbol->string
	  fixnum->flonum truncate
	  char<? char<=? char>=? char>? char=? char->integer)
       `(,(symbol-append 'c- op) ,@args))
      ((modulo quotient)
       `(,(symbol-append op 'fx) ,@args))
      ((make-vector) `($make-vector ,@args ,@(if (pair? (cdr args)) '() '(#f))))
      ((make-string) `($make-string ,@args ,@(if (pair? (cdr args)) '() '(#\space))))
      ((fixnum->string) `($fixnum->string ,@args ,@(if (pair? (cdr args)) '() '(10))))
      ((string->number)
        ;`(strtol ,(car args) 0 ,(if (pair? (cdr args)) (cadr args) 10))
        ;; TODO: implement an unsafe primitive that returns #f when not a number
        `(string->number ,(car args) ,(if (pair? (cdr args)) (cadr args) 10)))
      ((bit-lsh) `($bitlsh ,@args))
      ((bit-rsh) `($bitrsh ,@args))
      ((bit-and) `($bitand ,@args))
      ((bit-or) `($bitor ,@args))
      ((bit-not) `($bitnot ,@args))
      ((remainder) `(remainderfx ,@args))
      ((string->list) `((@ string->list __r4_strings_6_7) ,@args))
      ((vector->list) `((@ vector->list __r4_vectors_6_8) ,@args))
      ((vector-map) `((@ vector-map __r4_vectors_6_8) ,@args))
      ((+ / - * < <= > >= =)
       (case (length args)
	  ((2) `(,(symbol-append '|2| op) ,@args))
	  ((0 1) `((@ ,op __r4_numbers_6_5) ,@args)
	   (else `(,(symbol-append '|2| op) ,(car args) (,op ,@(cdr args)))))))
      (else `(,op ,@args))))

(define (unknown x . rest)
   (cond-expand
      (bbvcountprimitive ((car (list (lambda () (if (pair? rest) (car rest) x))))))
      (else ((car (list (lambda () x)))))))

(define-macro-arithmetic (MAPop kind op . args)
  (cond
   ((eq? kind 'FL)
    `(PRIMop ,(symbol-append 'fl op) ,@args))
   ((eq? kind 'FX)
    `(PRIMop ,(symbol-append 'fx op) ,@args))
   ((or (eq? kind 'GEN) (eq? arithmetic 'G)) ;; force generic?
    `(,(symbol-append 'BBV op) ,@args))
   ((eq? kind 'SFL)
    `(,(symbol-append 'FL op) ,@args))
   ((eq? kind 'SFX)
    `(,(symbol-append 'FX op) ,@args))))

(define-macro-arithmetic (GEN+ x y)         `(MAPop GEN + ,x ,y))
(define-macro-arithmetic (GEN- x y)         `(MAPop GEN - ,x ,y))
(define-macro-arithmetic (GEN* x y)         `(MAPop GEN * ,x ,y))
(define-macro-arithmetic (GEN/ x y)         `(MAPop GEN / ,x ,y))
(define-macro-arithmetic (GENquotient x y)  `(MAPop GEN quotient ,x ,y))
(define-macro-arithmetic (GENremainder x y) `(MAPop GEN remainder ,x ,y))
(define-macro-arithmetic (GENmodulo x y)    `(MAPop GEN modulo ,x ,y))
(define-macro-arithmetic (GEN= x y)         `(MAPop GEN = ,x ,y))
(define-macro-arithmetic (GEN< x y)         `(MAPop GEN < ,x ,y))
(define-macro-arithmetic (GEN> x y)         `(MAPop GEN > ,x ,y))
(define-macro-arithmetic (GEN<= x y)        `(MAPop GEN <=,x ,y))
(define-macro-arithmetic (GEN>= x y)        `(MAPop GEN >= ,x ,y))
(define-macro-arithmetic (GENzero? x)       `(MAPop GEN zero? ,x))
(define-macro-arithmetic (GENsqrt x)        `(MAPop GEN sqrt ,x))
(define-macro-arithmetic (GENsin x)         `(MAPop GEN sin ,x))
(define-macro-arithmetic (GENcos x)         `(MAPop GEN cos ,x))
(define-macro-arithmetic (GENatan2 x y)     `(MAPop GEN atan2 ,x ,y))
(define-macro-arithmetic (GENatan x)        `(MAPop GEN atan ,x))
(define-macro-arithmetic (GENmin . args)    `(MAPop GEN min ,@args))
(define-macro-arithmetic (GENmax . args)    `(MAPop GEN max ,@args))

(define-macro-arithmetic (SFL+ x y . z)     (if (pair? z)
						`(SFL+ (SFL+ ,x ,y) ,@z)
						`(MAPop SFL + ,x ,y)))
(define-macro-arithmetic (SFL- x . z)       (cond
					       ((null? z)
						`(SFL- 0. ,x))
					       ((null? (cdr z))
						`(MAPop SFL - ,x ,(car z)))
					       (else
						`(SFL- (SFL- ,x ,(car z)) ,@(cdr z)))))
(define-macro-arithmetic (SFL* x y)         `(MAPop SFL * ,x ,y))
(define-macro-arithmetic (SFL/ x y)         `(MAPop SFL / ,x ,y))
(define-macro-arithmetic (SFLquotient x y)  `(MAPop SFL quotient ,x ,y))
(define-macro-arithmetic (SFLremainder x y) `(MAPop SFL remainder ,x ,y))
(define-macro-arithmetic (SFLmodulo x y)    `(MAPop SFL modulo ,x ,y))
(define-macro-arithmetic (SFL= x y)         `(MAPop SFL = ,x ,y))
(define-macro-arithmetic (SFL< x y)         `(MAPop SFL < ,x ,y))
(define-macro-arithmetic (SFL> x y)         `(MAPop SFL > ,x ,y))
(define-macro-arithmetic (SFL<= x y)        `(MAPop SFL <= ,x ,y))
(define-macro-arithmetic (SFL>= x y)        `(MAPop SFL >= ,x ,y))
(define-macro-arithmetic (SFLzero? x)       `(MAPop SFL zero? ,x))
(define-macro-arithmetic (SFLsqrt x)        `(MAPop SFL sqrt ,x))
(define-macro-arithmetic (SFLsin x)         `(MAPop SFL sin ,x))
(define-macro-arithmetic (SFLcos x)         `(MAPop SFL cos ,x))
(define-macro-arithmetic (SFLasin x)        `(MAPop SFL asin ,x))
(define-macro-arithmetic (SFLacos x)        `(MAPop SFL acos ,x))
(define-macro-arithmetic (SFLatan2 x y)     `(MAPop SFL atan2 ,x ,y))
(define-macro-arithmetic (SFLatan x)        `(MAPop SFL atan ,x))
(define-macro-arithmetic (SFLmin . args)    `(MAPop SFL min ,@args))
(define-macro-arithmetic (SFLmax . args)    `(MAPop SFL max ,@args))

(define-macro-arithmetic (SFX+ x y)         `(MAPop SFX + ,x ,y))
(define-macro-arithmetic (SFX2- x y)        `(MAPop SFX - ,x ,y))
(define-macro-arithmetic (SFX- x . rest)    (if (null? rest) `(MAPop SFX - 0 ,x) `(SFX2- ,x ,@rest)))
(define-macro-arithmetic (SFX* x y)         `(MAPop SFX * ,x ,y))
(define-macro-arithmetic (SFX/ x y)         `(MAPop SFX / ,x ,y))
(define-macro-arithmetic (SFXquotient x y)  `(MAPop SFX quotient ,x ,y))
(define-macro-arithmetic (SFXremainder x y) `(MAPop SFX remainder ,x ,y))
(define-macro-arithmetic (SFXmodulo x y)    `(MAPop SFX modulo ,x ,y))
(define-macro-arithmetic (SFX= x y)         `(MAPop SFX = ,x ,y))
(define-macro-arithmetic (SFX< x y)         `(MAPop SFX < ,x ,y))
(define-macro-arithmetic (SFX> x y)         `(MAPop SFX > ,x ,y))
(define-macro-arithmetic (SFX<= x y)        `(MAPop SFX <= ,x ,y))
(define-macro-arithmetic (SFX>= x y)        `(MAPop SFX >= ,x ,y))
(define-macro-arithmetic (SFXzero? x)       `(MAPop SFX zero? ,x))
(define-macro-arithmetic (SFXodd? x)        `(MAPop SFX odd? ,x))
(define-macro-arithmetic (SFXeven? x)       `(MAPop SFX even? ,x))
(define-macro-arithmetic (SFXmin . args)    `(MAPop SFX min ,@args))
(define-macro-arithmetic (SFXmax . args)    `(MAPop SFX max ,@args))

(define-macro-arithmetic (SFXbit-and x y)   `(MAPop SFX bit-and ,x ,y))
(define-macro-arithmetic (SFXbit-or x y)    `(MAPop SFX bit-or ,x ,y))
(define-macro-arithmetic (SFXbit-not x)     `(MAPop SFX bit-not ,x))
(define-macro-arithmetic (SFXbit-lsh x y)   `(MAPop SFX bit-lsh ,x ,y))

(define-macro-arithmetic (FL+ x y)         `(FLop + ,x ,y))
(define-macro-arithmetic (FL- x y)         `(FLop - ,x ,y))
(define-macro-arithmetic (FL* x y)         `(FLop * ,x ,y))
(define-macro-arithmetic (FL/ x y)         `(FLop / ,x ,y))
(define-macro-arithmetic (FLquotient x y)  `(FLop quotient ,x ,y))
(define-macro-arithmetic (FLremainder x y) `(FLop remainder ,x ,y))
(define-macro-arithmetic (FLmodulo x y)    `(FLop modulo ,x ,y))
(define-macro-arithmetic (FL= x y)         `(FLop = ,x ,y))
(define-macro-arithmetic (FL< x y)         `(FLop < ,x ,y))
(define-macro-arithmetic (FL> x y)         `(FLop > ,x ,y))
(define-macro-arithmetic (FL<= x y)        `(FLop <= ,x ,y))
(define-macro-arithmetic (FL>= x y)        `(FLop >= ,x ,y))
(define-macro-arithmetic (FLzero? x)       `(FLop zero? ,x))
(define-macro-arithmetic (FLsqrt x)        `(FLop sqrt ,x))
(define-macro-arithmetic (FLsin x)         `(FLop sin ,x))
(define-macro-arithmetic (FLasin x)        `(FLop asin ,x))
(define-macro-arithmetic (FLcos x)         `(FLop cos ,x))
(define-macro-arithmetic (FLacos x)        `(FLop acos ,x))
(define-macro-arithmetic (FLatan2 x y)     `(FLop atan2 ,x ,y))
(define-macro-arithmetic (FLatan x)        `(FLop atan ,x))
(define-macro-arithmetic (FLmin . args)    `(FLop min ,@args))
(define-macro-arithmetic (FLmax . args)    `(FLop max ,@args))

(define-macro-arithmetic (FX+ x y)         `(FXop + ,x ,y))
(define-macro-arithmetic (FX- x y)         `(FXop - ,x ,y))
(define-macro-arithmetic (FX* x y)         `(FXop * ,x ,y))
(define-macro-arithmetic (FX/ x y)         `(FXop / ,x ,y))
(define-macro-arithmetic (FXquotient x y)  `(FXop quotient ,x ,y))
(define-macro-arithmetic (FXremainder x y) `(FXop remainder ,x ,y))
(define-macro-arithmetic (FXmodulo x y)    `(FXop modulo ,x ,y))
(define-macro-arithmetic (FX= x y)         `(FXop = ,x ,y))
(define-macro-arithmetic (FX< x y)         `(FXop < ,x ,y))
(define-macro-arithmetic (FX> x y)         `(FXop > ,x ,y))
(define-macro-arithmetic (FX<= x y)        `(FXop <= ,x ,y))
(define-macro-arithmetic (FX>= x y)        `(FXop >= ,x ,y))
(define-macro-arithmetic (FXzero? x)       `(FXop zero? ,x))
(define-macro-arithmetic (FXodd? x)        `(FXop odd? ,x))
(define-macro-arithmetic (FXeven? x)       `(FXop even? ,x))
(define-macro-arithmetic (FXmin . args)    `(FXop min ,@args))
(define-macro-arithmetic (FXmax . args)    `(FXop max ,@args))

(define-macro-arithmetic (FXbit-lsh x y)   `(FXop bit-lsh ,x ,y))
(define-macro-arithmetic (FXbit-and x y)   `(FXop bit-and ,x ,y))
(define-macro-arithmetic (FXbit-or x y)    `(FXop bit-or ,x ,y))
(define-macro-arithmetic (FXbit-not x)     `(FXop bit-not ,x))

(define-macro-arithmetic (FLONUM? x) `(PRIMop flonum? ,x))
(define-macro-arithmetic (FIXNUM? x) `(PRIMop fixnum? ,x))

(define-macro-arithmetic (BBVop op x y)
  (cond
    ((and macros-constant-folding (flonum? x))
      (let ((b (gensym)))
        `(let ((,b ,y))
          (cond
            ((FLONUM? ,b)
            (FLop ,op ,x ,b))
            (else
            (PRIMop ,op ,x ,b))))))
    ((and macros-constant-folding (flonum? y))
      (let ((a (gensym)))
        `(let ((,a ,x))
          (cond
            ((FLONUM? ,a)
            (FLop ,op ,a ,y))
            (else
            (PRIMop ,op ,a ,y))))))
    (else
      (let ((a (gensym))
            (b (gensym)))
        `(let ((,a ,x)
              (,b ,y))
          (cond
            ((and (FIXNUM? ,a) (FIXNUM? ,b))
            (,(if (eq? op '/) '/fx (symbol-append op 'fx/ov)) ,a ,b))
            ((and (FLONUM? ,a) (FLONUM? ,b))
            (FLop ,op ,a ,b))
            (else
            (PRIMop ,op ,a ,b))))))))

(define-macro-arithmetic (BBVcmp op x y)
  (let ((a (gensym))
        (b (gensym)))
    `(let ((,a ,x)
           (,b ,y))
       (cond
        ((and (FIXNUM? ,a) (FIXNUM? ,b))
         (FXop ,op ,a ,b))
        ((and (FLONUM? ,a) (FLONUM? ,b))
         (FLop ,op ,a ,b))
        (else
         (PRIMop ,op ,a ,b))))))

(define-macro-arithmetic (BBV+ x y) `(BBVop + ,x ,y))
(define-macro-arithmetic (BBV- x y) `(BBVop - ,x ,y))
(define-macro-arithmetic (BBV* x y) `(BBVop * ,x ,y))

(define-macro-arithmetic (BBV/ x y)
  (let ((a (gensym))
        (b (gensym)))
    `(let ((,a ,x)
           (,b ,y))
       (cond
        ((and (FLONUM? ,a) (FLONUM? ,b))
         (FL/ ,a ,b))
        (else
         (PRIMop / ,a ,b))))))

(define-macro-arithmetic (BBVquotient x y)
  (let ((a (gensym))
        (b (gensym)))
    `(let ((,a ,x)
           (,b ,y))
       (cond
        ((and (FIXNUM? ,a) (FIXNUM? ,b))
         (FXquotient ,a ,b)) ;; not correct when b = -1 or 0
        ((and (FLONUM? ,a) (FLONUM? ,b))
         (FLquotient ,a ,b))
        (else
         (PRIMop quotient ,a ,b))))))

(define-macro-arithmetic (BBVremainder x y)
  (let ((a (gensym))
        (b (gensym)))
    `(let ((,a ,x)
           (,b ,y))
       (cond
        ((and (FIXNUM? ,a) (FIXNUM? ,b))
         (FXremainder ,a ,b))
        ((and (FLONUM? ,a) (FLONUM? ,b))
         (FLremainder ,a ,b))
        (else
         (PRIMop remainder ,a ,b))))))

(define-macro-arithmetic (BBVmodulo x y)
  (let ((a (gensym))
        (b (gensym)))
    `(let ((,a ,x)
           (,b ,y))
       (cond
        ((and (FIXNUM? ,a) (FIXNUM? ,b))
         (FXmodulo ,a ,b))
        ((and (FLONUM? ,a) (FLONUM? ,b))
         (FLmodulo ,a ,b))
        (else
         (PRIMop modulo ,a ,b))))))

(define-macro-arithmetic (BBV= x y) `(BBVcmp = ,x ,y))
(define-macro-arithmetic (BBV< x y) `(BBVcmp < ,x ,y))
(define-macro-arithmetic (BBV<= x y) `(BBVcmp <= ,x ,y))
(define-macro-arithmetic (BBV>= x y) `(BBVcmp >= ,x ,y))
(define-macro-arithmetic (BBV> x y) `(BBVcmp > ,x ,y))

(define-macro-arithmetic (BBVzero? x)
  (let ((a (gensym)))
    `(let ((,a ,x))
       (cond
        ((FIXNUM? ,a) (FXzero? ,a))
        ((FLONUM? ,a) (FLzero? ,a))
        (else (PRIMop zero? ,a))))))

(define-macro-arithmetic (BBVodd? x)
  (let ((a (gensym)))
    `(let ((,a ,x))
       (cond
        ((FIXNUM? ,a) (FXodd? ,a))
        (else (PRIMop odd? ,a))))))

(define-macro-arithmetic (BBVeven? x)
  (let ((a (gensym)))
    `(let ((,a ,x))
       (cond
        ((FIXNUM? ,a) (FXeven? ,a))
        (else (PRIMop even? ,a))))))

(define-macro-arithmetic (BBVsqrt x)
  (let ((a (gensym)))
    `(let ((,a ,x))
       (cond
        ((FLONUM? ,a) (FLsqrt ,a))
        (else (PRIMop sqrt ,a))))))

(define-macro-arithmetic (BBVcos x)
  (let ((a (gensym)))
    `(let ((,a ,x))
       (if (FLONUM? ,a)
           (FLcos ,a)
           (PRIMop cos ,a)))))

(define-macro-arithmetic (BBVsin x)
  (let ((a (gensym)))
    `(let ((,a ,x))
       (if (FLONUM? ,a)
           (FLsin ,a)
           (PRIMop sin ,a)))))

(define-macro-arithmetic (BBVasin x)
  (let ((a (gensym)))
    `(let ((,a ,x))
       (if (FLONUM? ,a)
           (FLasin ,a)
           (PRIMop asin ,a)))))

(define-macro-arithmetic (BBVacos x)
  (let ((a (gensym)))
    `(let ((,a ,x))
       (if (FLONUM? ,a)
           (FLacos ,a)
           (PRIMop acos ,a)))))

(define-macro-arithmetic (BBVatan2 x y)
  (let ((a (gensym))
        (b (gensym)))
    `(let ((,a ,x)
           (,b ,y))
       (cond
        ((and (FLONUM? ,a) (FLONUM? ,b))
         (FLatan2 ,a ,b))
        (else
         (PRIMop atan2 ,a ,b))))))

(define-macro (BBVmax . args)
  (let* ((best (gensym))
         (vars (cons best (map (lambda (_) (gensym)) (cdr args))))
         (bindings (map (lambda (var arg) `(,var ,arg)) vars args)))
    `(let (,@bindings)
      ,(let loop ((vars (cdr vars)))
          (cond 
            ((null? vars) best)
            ((null? (cdr vars))
              `(if (GEN< ,best ,(car vars)) ,(car vars) ,best))
            (else
              `(begin (if (GEN< ,best ,(car vars)) (set! ,best ,(car vars)))
                      ,(loop (cdr vars)))))))))

(define-macro (BBVmin . args)
  (let* ((best (gensym))
         (vars (cons best (map (lambda (_) (gensym)) (cdr args))))
         (bindings (map (lambda (var arg) `(,var ,arg)) vars args)))
    `(let (,@bindings)
      ,(let loop ((vars (cdr vars)))
          (cond 
            ((null? vars) best)
            ((null? (cdr vars))
              `(if (GEN> ,best ,(car vars)) ,(car vars) ,best))
            (else
              `(begin (if (GEN< ,best ,(car vars)) (set! ,best ,(car vars)))
                      ,(loop (cdr vars)))))))))

(define-macro-arithmetic (BBVatan x)
  (let ((a (gensym)))
    `(let ((,a ,x))
       (cond
        ((FLONUM? ,a)
         (FLatan ,a))
        (else
         (PRIMop atan ,a))))))

(define-macro-arithmetic (BBVbit-and x y)
   (let ((a (gensym))
	 (b (gensym)))
      `(let ((,a ,x)
	     (,b ,y))
	  (if (and (FIXNUM? ,a) (FIXNUM? ,b))
	      (bit-and ,a ,b)
	      (DEAD-END "bit-and")))))

(define-macro-arithmetic (BBVbit-or x y)
   (let ((a (gensym))
	 (b (gensym)))
      `(let ((,a ,x)
	     (,b ,y))
	  (if (and (FIXNUM? ,a) (FIXNUM? ,b))
	      (bit-or ,a ,b)
	      (DEAD-END "bit-and")))))

(define-macro-arithmetic (BBVbit-lsh x y)
   (let ((a (gensym))
	 (b (gensym)))
      `(let ((,a ,x)
	     (,b ,y))
	  (if (and (FIXNUM? ,a) (FIXNUM? ,b))
	      (bit-lsh ,a ,b)
	      (DEAD-END "bit-lsh")))))

(define-macro-arithmetic (BBVbit-not x)
   (let ((a (gensym)))
      `(let ((,a ,x))
	  (if (FIXNUM? ,a)
	      (bit-not ,a)
	      (DEAD-END "bit-not")))))

(define-inline (odd?fx x)
   (oddfx? x))

(define-inline (even?fx x)
   (evenfx? x))

(define-inline (modulofl x y)
   (error "modulofl" "not implemented" #f))

(define-inline (quotientfl x y)
   (error "modulofl" "not implemented" #f))

(define-macro-arithmetic (Scar x)
  (let ((a (gensym)))
    `(let ((,a ,x))
       ,(if (eq? arithmetic 'S)
            `(PRIMop car ,a)
            `(if (pair? ,a)
                 (PRIMop car ,a)
                 (DEAD-END "car type error"))))))

(define-macro-arithmetic (Scdr x)
  (let ((a (gensym)))
    `(let ((,a ,x))
       ,(if (eq? arithmetic 'S)
            `(PRIMop cdr ,a)
            `(if (pair? ,a)
                 (PRIMop cdr ,a)
                 (DEAD-END "cdr type error"))))))

(define-macro-arithmetic (Sset-car! x y)
  (let ((a (gensym))
        (b (gensym)))
    `(let ((,a ,x)
           (,b ,y))
       ,(if (eq? arithmetic 'S)
            `(PRIMop set-car! ,a ,b)
            `(if (pair? ,a)
                 (PRIMop set-car! ,a ,b)
                 (DEAD-END "set-car! type error"))))))

(define-macro-arithmetic (Sset-cdr! x y)
  (let ((a (gensym))
        (b (gensym)))
    `(let ((,a ,x)
           (,b ,y))
       ,(if (eq? arithmetic 'S)
            `(PRIMop set-cdr! ,a ,b)
            `(if (pair? ,a)
                 (PRIMop set-cdr! ,a ,b)
                 (DEAD-END "set-cdr! type error"))))))

(define-macro-arithmetic (Scaar x) `(Scar (Scar ,x)))
(define-macro-arithmetic (Scadr x) `(Scar (Scdr ,x)))
(define-macro-arithmetic (Scdar x) `(Scdr (Scar ,x)))
(define-macro-arithmetic (Scddr x) `(Scdr (Scdr ,x)))
(define-macro-arithmetic (Scaadr x) `(Scar (Scar (Scdr ,x))))
(define-macro-arithmetic (Scadar x) `(Scar (Scdr (Scar ,x))))
(define-macro-arithmetic (Scdadr x) `(Scdr (Scar (Scdr ,x))))
(define-macro-arithmetic (Scaddr x) `(Scar (Scdr (Scdr ,x))))
(define-macro-arithmetic (Scdddr x) `(Scdr (Scdr (Scdr ,x))))
(define-macro-arithmetic (Scadddr x) `(Scar (Scdr (Scdr (Scdr ,x)))))

(define-macro-arithmetic (Sstring->symbol x)
  (let ((a (gensym)))
    `(let ((,a ,x))
       ,(if (eq? arithmetic 'S)
            `(PRIMop string->symbol ,a)
            `(if (string? ,a)
                 (PRIMop string->symbol ,a)
                 (DEAD-END "string->symbol type error"))))))

(define-macro-arithmetic (Ssymbol->string x)
  (let ((a (gensym)))
    `(let ((,a ,x))
       ,(if (eq? arithmetic 'S)
            `(PRIMop symbol->string ,a)
            `(if (symbol? ,a)
                 (PRIMop symbol->string ,a)
                 (DEAD-END "symbol->string type error"))))))

(define-macro-arithmetic (Sstring->number s)
  (let ((a (gensym)))
    `(let ((,a ,s))
       ,(if (eq? arithmetic 'S)
            `(PRIMop string->number ,a)
            `(if (string? ,a)
                 (PRIMop string->number ,a)
                 (DEAD-END "string->number type error"))))))

(define-macro-arithmetic (Sstring->number2 s base)
  (let ((a (gensym))
        (b (gensym)))
    `(let ((,a ,s)
           (,b ,base))
       ,(if (eq? arithmetic 'S)
            `(PRIMop string->number ,a ,b)
            `(if (and (string? ,a) (FIXNUM? ,b) (FX>= ,b 2) (FX< ,b 16))
                 (PRIMop string->number ,a ,b)
                 (DEAD-END "string->number type error"))))))

(define-macro-arithmetic (SFXnumber->string x)
  (let ((a (gensym)))
    `(let ((,a ,x))
       ,(if (eq? arithmetic 'S)
            `(PRIMop fixnum->string ,a)
            `(if (FIXNUM? ,a)
                 (PRIMop fixnum->string ,a)
                 (PRIMop number->string ,a))))))

(define-macro-arithmetic (SFXinexact x)
  (let ((a (gensym)))
    `(let ((,a ,x))
       ,(if (eq? arithmetic 'S)
            `(PRIMop fixnum->flonum ,a)
            `(if (FIXNUM? ,a)
                 (PRIMop fixnum->flonum ,a)
                 (PRIMop inexact ,a))))))

(define-macro-arithmetic (SFLexact x)
  (let ((a (gensym)))
    `(let ((,a ,x))
       ,(if (eq? arithmetic 'S)
            `(PRIMop flonum->fixnum ,a)
            `(if (FLONUM? ,a)
                 (PRIMop flonum->fixnum ,a)
                 (PRIMop exact ,a))))))

(define-macro-arithmetic (SFLtruncate x)
  (let ((a (gensym)))
    `(let ((,a ,x))
       ,(if (eq? arithmetic 'S)
            `(FLop truncate ,a)
            `(if (FLONUM? ,a)
                 (FLop truncate ,a)
                 (PRIMop truncate ,a))))))

(define-macro-arithmetic (Scall-with-current-continuation f)
   (match-case f
      ((lambda (?esc) ?body)
       `(bind-exit (,esc) ,body))
      (else
       (error "$call-with-current-continuation" "bad form" ',f))))

(define-macro-arithmetic (call-with-current-continuation f)
   (match-case f
      ((lambda (?esc) . ?body)
       `(bind-exit (,esc) ,@body))
      (else
       (error "call-with-current-continuation" "bad form" `',f))))

(define-macro-arithmetic (Schar->integer x)
  (let ((a (gensym)))
    `(let ((,a ,x))
       ,(if (eq? arithmetic 'S)
            `(PRIMop char->integer ,a)
            `(if (char? ,a)
                 (PRIMop char->integer ,a)
                 (DEAD-END "char->integer type error"))))))

(define-macro-arithmetic (Sinteger->char x)
  (let ((a (gensym)))
    `(let ((,a ,x))
       ,(if (eq? arithmetic 'S)
            `(PRIMop integer->char ,a)
            `(if (and (FIXNUM? ,a) (FX>= ,a 0) (FX< ,a #x110000))
                 (PRIMop integer->char ,a)
                 (DEAD-END "integer->char type error"))))))

(define-macro-arithmetic (Schar<? x y)
  (let ((a (gensym))
        (b (gensym)))
    `(let ((,a ,x)
           (,b ,y))
       ,(if (eq? arithmetic 'S)
            `(PRIMop char<? ,a ,b)
            `(if (and (char? ,a) (char? ,b))
                 (PRIMop char<? ,a ,b)
                 (DEAD-END "char<? type error"))))))

(define-macro-arithmetic (Schar>? x y)
  (let ((a (gensym))
        (b (gensym)))
    `(let ((,a ,x)
           (,b ,y))
       ,(if (eq? arithmetic 'S)
            `(PRIMop char>? ,a ,b)
            `(if (and (char? ,a) (char? ,b))
                 (PRIMop char>? ,a ,b)
                 (DEAD-END "char>? type error"))))))


(define-macro-arithmetic (Schar<=? x y)
  (let ((a (gensym))
        (b (gensym)))
    `(let ((,a ,x)
           (,b ,y))
       ,(if (eq? arithmetic 'S)
            `(PRIMop char<=? ,a ,b)
            `(if (and (char? ,a) (char? ,b))
                 (PRIMop char<=? ,a ,b)
                 (DEAD-END "char<=? type error"))))))

(define-macro-arithmetic (Schar>=? x y)
  (let ((a (gensym))
        (b (gensym)))
    `(let ((,a ,x)
           (,b ,y))
       ,(if (eq? arithmetic 'S)
            `(PRIMop char>=? ,a ,b)
            `(if (and (char? ,a) (char? ,b))
                 (PRIMop char>=? ,a ,b)
                 (DEAD-END "char>=? type error"))))))

(define-macro-arithmetic (Schar=? x y)
   (let ((a (gensym))
	 (b (gensym)))
      `(let ((,a ,x)
	     (,b ,y))
	  ,(if (eq? arithmetic 'S)
	       `(PRIMop char=? ,a ,b)
	       `(if (and (char? ,a) (char? ,b))
		    (PRIMop char=? ,a ,b)
		    (DEAD-END "char=? type error"))))))

(define-macro-arithmetic (Schar=?.orig x y)
  (let ((a (gensym))
        (b (gensym)))
    `(let ((,a ,x)
           (,b ,y))
       ,(if (eq? arithmetic 'S)
            `(PRIMop char=? ,a ,b)
            `(if (and (char? ,a) (char? ,b))
                 (PRIMop char=? ,a ,b)
                 (DEAD-END "char=? type error"))))))

(define-macro-arithmetic (Schar-downcase c)
  `(let ((c ,c))
     (if (Schar<=? c #\Z)
         (if (Schar>=? c #\A)
             (Sinteger->char (SFX+ (Schar->integer c) 32))
             c)
         c)))

(define-macro-arithmetic (Schar-alphabetic? c)
  `(let ((c ,c))
     (if (Schar>=? c #\a)
         (Schar<=? c #\z)
         (and (Schar>=? c #\A) (Schar<=? c #\Z)))))

(define-macro-arithmetic (Schar-whitespace? c)
  `(Schar<=? ,c #\space))

(define-macro-arithmetic (Schar-numeric? c)
  `(let ((c ,c))
     (and (Schar>=? c #\0) (Schar<=? c #\9))))

(define-macro-arithmetic (Sstring=? str1 str2)
  `(LIBstring=? ,str1 ,str2))

(define-macro-arithmetic (Sstring<? str1 str2)
  `(LIBstring<? ,str1 ,str2))

(define-macro-arithmetic (Sstring-ci=? str1 str2)
  `(LIBstring-ci=? ,str1 ,str2))

(define-macro-arithmetic (fatal-error msg . obj)
   `(DEAD-END ,msg))

(define-macro-arithmetic (Sequal? x y)
  `(LIBequal? ,x ,y))

(define-macro-arithmetic (Slist? l)
  `(let ((l ,l))
    (cond
     ((null? l) #t)
     ((not (pair? l)) #f)
     (else
      (letrec
        ((loop1
          (lambda (fast slow)
            (cond
              ((null? fast) #t)
              ((or (not (pair? fast)) (eq? fast slow)) #f)
              (else (loop2 (Scdr fast) slow)))))
         (loop2
          (lambda (fast slow)
            (cond
              ((null? fast) #t)
              ((or (not (pair? fast)) (eq? fast slow)) #f)
              (else (loop1 (Scdr fast) (Scdr slow)))))))
        (loop1 (Scdr l) l))))))

(define-macro-arithmetic (Slist-tail lst i)
  `(let ((lst ,lst) (i ,i))
     (let loop ((lst lst) (i i))
       (if (SFX<= i 0)
           lst
           (loop (Scdr lst) (SFX- i 1))))))

(define-macro-arithmetic (Slist-ref lst i)
  `(let ((lst ,lst) (i ,i))
     (let loop ((lst lst) (i i))
       (if (SFX<= i 0)
           (Scar lst)
           (loop (Scdr lst) (SFX- i 1))))))

(define-macro-arithmetic (Slength lst)
  `(let ((lst ,lst))
     (let loop ((lst lst) (len 0))
       (if (pair? lst)
           (loop (Scdr lst) (SFX+ len 1))
           len))))

(define-macro-arithmetic (Smemq key lst)
  `(let ((key ,key) (lst ,lst))
     (let loop ((lst lst))
       (and (pair? lst)
            (if (eq? key (Scar lst))
                lst
                (loop (Scdr lst)))))))

(define-macro-arithmetic (Smemv key lst)
  `(let ((key ,key) (lst ,lst))
     (let loop ((lst lst))
       (and (pair? lst)
            (if (eqv? key (Scar lst))
                lst
                (loop (Scdr lst)))))))

(define-macro-arithmetic (Smember key lst)
  `(let ((key ,key) (lst ,lst))
     (let loop ((lst lst))
       (and (pair? lst)
            (if (Sequal? key (Scar lst))
                lst
                (loop (Scdr lst)))))))

(define-macro-arithmetic (Sassq key lst)
  `(let ((key ,key) (lst ,lst))
     (let loop ((lst lst))
       (and (pair? lst)
            (let ((x (Scar lst)))
              (if (eq? key (Scar x))
                  x
                  (loop (Scdr lst))))))))

(define-macro-arithmetic (Sassv key lst)
  `(let ((key ,key) (lst ,lst))
     (let loop ((lst lst))
       (and (pair? lst)
            (let ((x (Scar lst)))
              (if (eqv? key (Scar x))
                  x
                  (loop (Scdr lst))))))))

(define-macro-arithmetic (Sassoc key lst)
  `(let ((key ,key) (lst ,lst))
     (let loop ((lst lst))
       (and (pair? lst)
            (let ((x (Scar lst)))
              (if (Sequal? key (Scar x))
                  x
                  (loop (Scdr lst))))))))

(define-macro-arithmetic (Sappend lst1 lst2)
  `(let ((lst1 ,lst1) (lst2 ,lst2))
     (let loop ((lst lst1))
       (if (pair? lst)
           (cons (Scar lst) (loop (Scdr lst)))
           lst2))))

(define-macro-arithmetic (Smap2 f lst) ;; 2 parameter map
  `(let ((f ,f) (lst ,lst))
      (if (procedure? ,f)
	  (letrec ((map2
		      (lambda (f lst)
			 (if (pair? lst)
			     (cons (f (Scar lst)) (map2 f (Scdr lst)))
			     '()))))
	     (map2 f lst))
	  (DEAD-END "map type error"))))

(define-macro-arithmetic (Smap3 f lst1 lst2) ;; 3 parameter map
  `(let ((f ,f) (lst1 ,lst1) (lst2 ,lst2))
     (if (procedure? f)
      (letrec ((map3
                (lambda (f lst1 lst2)
                  (if (and (pair? lst1) (pair? lst2))
                      (cons (f (Scar lst1) (Scar lst2)) (map3 f (Scdr lst1) (Scdr lst2)))
                      '()))))
        (map3 f lst1 lst2))
      (DEAD-END "map type error"))))

(define-macro-arithmetic (Sfor-each2 f lst) ;; 2 parameter map
  `(let ((f ,f) (lst ,lst))
    (if (procedure? ,f)
      (letrec ((for-each2
                (lambda (f lst)
                  (if (pair? lst)
                      (begin
                        (f (Scar lst))
                        (for-each2 f (Scdr lst)))
                      #f))))
        (for-each2 f lst))
      (DEAD-END "for-each! type error"))))

(define-macro-arithmetic (Sfor-each3 f lst1 lst2) ;; 3 parameter map
  `(let ((f ,f) (lst1 ,lst1) (lst2 ,lst2))
    (if (procedure? ,f)
      (letrec ((for-each3
                (lambda (f lst1 lst2)
                  (if (and (pair? lst1) (pair? lst2))
                      (begin
                        (f (Scar lst1) (Scar lst2))
                        (for-each3 f (Scdr lst1) (Scdr lst2)))
                      #f))))
        (for-each3 f lst1 lst2))
      (DEAD-END "for-each! type error"))))

(define-macro-arithmetic (Sreverse lst)
  `(let ((lst ,lst))
     (let loop ((lst lst) (result '()))
       (if (pair? lst)
           (loop (Scdr lst) (cons (Scar lst) result))
           result))))

(define-macro-arithmetic (Slist->vector lst)
  `(LIBlist->vector ,lst))

(define-macro-arithmetic (Slist->string lst)
  `(LIBlist->string ,lst))

(define-macro-arithmetic (Svector-map2 f vect)
  (let ((a (gensym))
        (b (gensym)))
    `(let ((,a ,f)
           (,b ,vect))
       ,(if (eq? arithmetic 'S)
            `(PRIMop vector-map ,a ,b)
            `(if (and (procedure? ,a) (vector? ,b))
                 (PRIMop vector-map ,a ,b)
                 (DEAD-END "vector-map type error"))))))

(define-macro-arithmetic (Svector->list vect)
  (let ((a (gensym)))
    `(let ((,a ,vect))
       ,(if (eq? arithmetic 'S)
            `(PRIMop vector->list ,a)
            `(if (vector? ,a)
                 (PRIMop vector->list ,a)
                 (DEAD-END "vector->list type error"))))))

(define-macro-arithmetic (Sstring->list s)
  (let ((a (gensym)))
    `(let ((,a ,s))
       ,(if (eq? arithmetic 'S)
            `(PRIMop string->list ,a)
            `(if (string? ,a)
                 (PRIMop string->list ,a)
                 (DEAD-END "string->list type error"))))))

(define-macro-arithmetic (Smake-vector1 n)
  (let ((a (gensym)))
    `(let ((,a ,n))
       ,(if (eq? arithmetic 'S)
            `(PRIMop make-vector ,a)
            `(if (and (FIXNUM? ,a) (FX>= ,a 0))
                 (PRIMop make-vector ,a)
                 (DEAD-END "make-vector type error"))))))

(define-macro-arithmetic (Smake-vector2 n init)
  (let ((a (gensym))
        (b (gensym)))
    `(let ((,a ,n)
           (,b ,init))
       ,(if (eq? arithmetic 'S)
            `(PRIMop make-vector ,a ,b)
            `(if (and (FIXNUM? ,a) (FX>= ,a 0))
                 (PRIMop make-vector ,a ,b)
                 (DEAD-END "make-vector type error"))))))

(define-macro-arithmetic (Svector-ref v i)
  (let ((a (gensym))
        (b (gensym)))
    `(let ((,a ,v)
           (,b ,i))
       ,(if (eq? arithmetic 'S)
            `(PRIMop vector-ref ,a ,b)
            `(if (and (vector? ,a) (FIXNUM? ,b) (FX>= ,b 0) (FX< ,b (PRIMop vector-length ,a)))
                 (PRIMop vector-ref ,a ,b)
                 (DEAD-END "vector-ref type error"))))))

(define-macro-arithmetic (Svector-set! v i x)
  (let ((a (gensym))
        (b (gensym))
        (c (gensym)))
    `(let ((,a ,v)
           (,b ,i)
           (,c ,x))
       ,(if (eq? arithmetic 'S)
            `(PRIMop vector-set! ,a ,b ,c)
            `(if (and (vector? ,a) (FIXNUM? ,b) (FX>= ,b 0) (FX< ,b (PRIMop vector-length ,a)))
                 (PRIMop vector-set! ,a ,b ,c)
                 (DEAD-END "vector-set! type error"))))))

(define-macro-arithmetic (Svector-length v)
  (let ((a (gensym)))
    `(let ((,a ,v))
       ,(if (eq? arithmetic 'S)
            `(PRIMop vector-length ,a)
            `(if (vector? ,a)
                 (PRIMop vector-length ,a)
                 (DEAD-END "vector-length type error"))))))

(define-macro-arithmetic (Smake-string1 n)
   (let ((a (gensym)))
      `(let ((,a ,n))
	  ,(if (eq? arithmetic 'S)
	       `(PRIMop make-string ,a)
	       `(if (and (FIXNUM? ,a) (FX>= ,a 0))
		    (PRIMop make-string ,a)
		    (DEAD-END "make-string type error"))))))

(define-macro-arithmetic (Smake-string2 n init)
  (let ((a (gensym))
        (b (gensym)))
    `(let ((,a ,n)
           (,b ,init))
       ,(if (eq? arithmetic 'S)
            `(PRIMop make-string ,a ,b)
            `(if (and (FIXNUM? ,a) (FX>= ,a 0) (char? ,b))
                 (PRIMop make-string ,a ,b)
                 (DEAD-END "make-string type error"))))))

(define-macro-arithmetic (Sstring-ref s i)
  (let ((a (gensym))
        (b (gensym)))
    `(let ((,a ,s)
           (,b ,i))
       ,(if (eq? arithmetic 'S)
            `(PRIMop string-ref ,a ,b)
            `(if (and (string? ,a) (FIXNUM? ,b) (FX>= ,b 0) (FX< ,b (PRIMop string-length ,a)))
                 (PRIMop string-ref ,a ,b)
                 (DEAD-END "string-ref type error"))))))

(define-macro-arithmetic (Sstring-set! s i x)
  (let ((a (gensym))
        (b (gensym))
        (c (gensym)))
    `(let ((,a ,s)
           (,b ,i)
           (,c ,x))
       ,(if (eq? arithmetic 'S)
            `(PRIMop string-set! ,a ,b ,c)
            `(if (and (string? ,a) (FIXNUM? ,b) (FX>= ,b 0) (FX< ,b (PRIMop string-length ,a)) (char? ,c))
                 (PRIMop string-set! ,a ,b ,c)
                 (DEAD-END "string-set! type error"))))))

(define-macro-arithmetic (Sstring-length s)
   (let ((a (gensym)))
      `(let ((,a ,s))
	  ,(if (eq? arithmetic 'S)
	       `(PRIMop string-length ,a)
	       `(if (string? ,a)
		    (PRIMop string-length ,a)
		    (DEAD-END "string-length type error"))))))

(define-macro-arithmetic (Sstring-append . args)
   (let ((vars (map (lambda (_) (gensym)) args)))
      `(let ,(map list vars args)
	  ,(if (eq? arithmetic 'S)
	       `(PRIMop string-append ,@vars)
	       `(if (and ,@(map (lambda (var) `(string? ,var)) vars))
		    (PRIMop string-append ,@vars)
		    (DEAD-END "string-append type error"))))))

(define-macro-arithmetic (Ssubstring str start end)
  (let ((a (gensym))
        (b (gensym))
        (c (gensym)))
    `(let ((,a ,str)
           (,b ,start)
           (,c ,end))
       ,(if (eq? arithmetic 'S)
            `(PRIMop substring ,a ,b ,c)
            `(if (and (string? ,a)
                      (FIXNUM? ,b) (FX>= ,b 0) (FX< ,b (PRIMop string-length ,a))
                      (FIXNUM? ,c) (FX>= ,c ,b) (FX< ,c (PRIMop string-length ,a)))
                 `(PRIMop substring ,a ,b ,c)
                 (DEAD-END "substring type error"))))))

(define-macro (Sdefine-record-TBR-5mar2024 name . fields)
   (let ((make-func-name (symbol-append 'make- name))
	 (test-func-name (symbol-append name '?))
	 (get-accessor-name
	    (lambda (field)
	       (symbol-append name '|:| field)))
	 (get-mutator-name
	    (lambda (field)
	       (symbol-append 'set- name '|:| field))))
      `(begin
	  ;; constructor macro
	  (define-macro (,make-func-name . fields)
	     `(vector ',',name ,@fields))
	  
	  (define-macro (,test-func-name obj)
	     `(vector? ,obj))
	  
	  ;; accessors and mutators for each field
	  ,@(apply append
	       (map
		  (lambda (field index)
		     (let ((accessor-name (get-accessor-name field))
			   (mutator-name (get-mutator-name field)))
			`((define-macro (,accessor-name o)
			     `(let ((o ,o))
				 (if (,',test-func-name o)
				     (vector-ref-ur o ,,index)
				     (DEAD-END "record type error"))))
			  (define-macro (,mutator-name o v)
			     `(let ((o ,o) (v ,v))
				 (if (,',test-func-name o)
				     (vector-set-ur! o ,,index v)
				     (DEAD-END "record type error")))))))
		  fields
		  (iota (length fields) 1))))))

(define-macro (Sdefine-record name . fields)
  (let* ((macro? (memq macro: fields))
         (fields (filter (lambda (x) (not (eq? x macro:))) fields))
         (make-func-name (symbol-append 'make- name))
         (test-func-name (symbol-append name '?))
         (get-field-name
          (lambda (field)
            (if (pair? field) (car field) field)))
         (field-names (map get-field-name fields))
         (get-accessor-name
          (lambda (field)
            (if (pair? field)
                (cadr field)
                (symbol-append name '|:| field))))
         (get-mutator-name
          (lambda (field)
            (if (pair? field)
                (caddr field)
                (symbol-append 'set- name '|:| field)))))
    (if macro?
      `(begin
        ;; constructor macro
        (define-macro (,make-func-name . fields)
          `(vector ',',name ,@fields))
        
        (define-macro (,test-func-name obj)
          `(vector? ,obj))
        
        ;; accessors and mutators for each field
        ,@(apply append
            (map
          (lambda (field index)
            (let ((accessor-name (get-accessor-name field))
            (mutator-name (get-mutator-name field)))
          `((define-macro (,accessor-name o)
              `(let ((o ,o))
            (if (,',test-func-name o)
                (vector-ref-ur o ,,index)
                (DEAD-END "record type error"))))
            (define-macro (,mutator-name o v)
              `(let ((o ,o) (v ,v))
            (if (,',test-func-name o)
                (vector-set-ur! o ,,index v)
                (DEAD-END "record type error")))))))
          fields
          (iota (length fields) 1))))
        `(begin
          ;; constructor procedure
          (define (,make-func-name ,@field-names)
            (vector ',name ,@field-names))

          (define ,test-func-name vector?)

          ;; accessors and mutators for each field
          ,@(apply append
              (map
                (lambda (field index)
                  (let ((accessor-name (get-accessor-name field))
                        (mutator-name (get-mutator-name field)))
                  `((define (,accessor-name o)
                      (if (,test-func-name o)
                        (vector-ref-ur o ,index)
                        (DEAD-END "record type error")))
                    (define (,mutator-name o v)
                      (if (,test-func-name o)
                        (vector-set-ur! o ,index v)
                        (DEAD-END "record type error"))))))
                fields
                (iota (length fields) 1)))))))

(define-macro (define-keys signature . body)
  (define (replace lst)
    (cond
      ((null? lst) '())
      ((eq? (car lst) '!key) (cons '#!key (cdr lst)))
      (else (cons (car lst) (replace (cdr lst))))))
  `(define ,(replace signature) ,@body))

(define-macro (set-bbv-version-limit! . rest) `(begin))
(define-macro (set-custom-version-limits! . rest) `(begin))

