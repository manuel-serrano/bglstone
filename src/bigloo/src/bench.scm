(module slatex
   (export (class point (x (default 0)))
	   (class point2d::point (y (default 0)))
	   (class point3d::point2d (z (default 0)))
	   (class point4d::point3d (t (default 0)))
	   (class point5d::point4d (u (default 0)))
	   (class point6d::point5d (v (default 0)))
	   (class point7d::point6d (w (default 0)))
	   pp-read
	   (foo-match l))
   (main main))

(define *ignore-comment* #f)
(define *case* #f)

(define (rgc-buffer-subsymbol . l) l)

;slatex.scm file generated using config.scm                                  
;This file is compatible for the dialect OTHER                               
;(c) Dorai Sitaram, Rice U., 1991, 1994                                      
(define *op-sys* 'os2)

(define slatex.ormap
  (lambda (f l)
    (let loop ((l l))
      (if (null? l) #f (or (f (car l)) (loop (cdr l)))))))

(define slatex.ormapcdr
  (lambda (f l)
    (let loop ((l l))
      (if (null? l) #f (or (f l) (loop (cdr l)))))))

(define slatex.append!
  (lambda (l1 l2)
    (cond ((null? l1) l2)
          ((null? l2) l1)
          (else
           (let loop ((l1 l1))
             (if (null? (cdr l1))
               (set-cdr! l1 l2)
               (loop (cdr l1))))
           l1))))

(define slatex.append-map!
  (lambda (f l)
    (let loop ((l l))
      (if (null? l)
        '()
        (slatex.append! (f (car l)) (loop (cdr l)))))))

(define slatex.remove-if!
  (lambda (p s)
    (let loop ((s s))
      (cond ((null? s) '())
            ((p (car s)) (loop (cdr s)))
            (else
             (let ((r (loop (cdr s)))) (set-cdr! s r) s))))))


(define slatex.reverse!
  (lambda (s)
    (let loop ((s s) (r '()))
      (if (null? s)
        r
        (let ((d (cdr s))) (set-cdr! s r) (loop d s))))))

(define slatex.list-set!
  (lambda (l i v)
    (let loop ((l l) (i i))
      (cond ((null? l)
             (slatex.error 'slatex.list-set! 'list-too-small))
            ((=fx i 0) (set-car! l v))
            (else (loop (cdr l) (-fx i 1)))))))

(define slatex.list-prefix?
  (lambda (pfx l)
    (cond ((null? pfx) #t)
          ((null? l) #f)
          ((eqv? (car pfx) (car l))
           (slatex.list-prefix? (cdr pfx) (cdr l)))
          (else #f))))

(define slatex.string-prefix?
  (lambda (pfx s)
    (let ((pfx-len (string-length pfx))
          (s-len (string-length s)))
      (if (>fx pfx-len s-len)
        #f
        (let loop ((i 0))
          (if (>=fx i pfx-len)
            #t
            (and (char=? (string-ref pfx i) (string-ref s i))
                 (loop (+fx i 1)))))))))

(define slatex.string-suffix?
  (lambda (sfx s)
    (let ((sfx-len (string-length sfx))
          (s-len (string-length s)))
      (if (>fx sfx-len s-len)
        #f
        (let loop ((i (-fx sfx-len 1)) (j (-fx s-len 1)))
          (if (<fx i 0)
            #t
            (and (char=? (string-ref sfx i) (string-ref s j))
                 (loop (-fx i 1) (-fx j 1)))))))))

(define slatex.member-string member)
(define slatex.adjoin-string
  (lambda (s l)
    (if (slatex.member-string s l) l (cons s l))))

(define slatex.remove-string!
  (lambda (s l)
    (slatex.remove-if!
      (lambda (l_i) (string=? l_i s))
      l)))

(define slatex.adjoin-char
  (lambda (c l) (if (memv c l) l (cons c l))))

(define slatex.remove-char!
  (lambda (c l)
    (slatex.remove-if!
      (lambda (l_i) (char=? l_i c))
      l)))

(define slatex.sublist
  (lambda (l i f)
    (let loop ((l (list-tail l i)) (k i) (r '()))
      (cond ((>=fx k f) (slatex.reverse! r))
            ((null? l)
             (slatex.error 'slatex.sublist 'list-too-small))
            (else (loop (cdr l) (+fx k 1) (cons (car l) r)))))))

(define slatex.position-char
  (lambda (c l)
    (let loop ((l l) (i 0))
      (cond ((null? l) #f)
            ((char=? (car l) c) i)
            (else (loop (cdr l) (+fx i 1)))))))

(define slatex.string-position-right
  (lambda (c s)
    (let ((n (string-length s)))
      (let loop ((i (-fx n 1)))
        (cond ((<fx i 0) #f)
              ((char=? (string-ref s i) c) i)
              (else (loop (-fx i 1))))))))

(define slatex.assoc-token
  (lambda (x s)
    (slatex.ormap
      (lambda (s_i)
        (if (slatex.token=? (car s_i) x) s_i #f))
      s)))

(define slatex.member-token
  (lambda (x s)
    (slatex.ormapcdr
      (lambda (s_i..)
        (if (slatex.token=? (car s_i..) x) s_i.. #f))
      s)))

(define slatex.remove-token!
  (lambda (x s)
    (slatex.remove-if!
      (lambda (s_i) (slatex.token=? s_i x))
      s)))

(define slatex.file-exists? (lambda (f) (file-exists? f)))
(define slatex.delete-file
   (lambda (f) (delete-file f)))

(define slatex.force-output
   (lambda (p) (flush-output-port p)))

(define slatex.*return* (integer->char 13))
(define slatex.*tab* (integer->char 9))
(define slatex.error
  (lambda (error-type . error-values)
    (display "Error: ")
    (display error-type)
    (display ": ")
    (newline)
    (for-each
      (lambda (x) (write x) (newline))
      error-values)
    (exit -10)))

(define slatex.exit-scheme
  (lambda ()
    (display "Exit Scheme!")
    (newline)
    (exit -10)))

(define slatex.keyword-tokens
  (map symbol->string
       '(=> %
            abort
            and
            begin
            begin0
            case
            case-lambda
            cond
            define
            define!
            define-macro!
            define-syntax
            defmacro
            defrec!
            delay
            do
            else
            extend-syntax
            fluid-let
            if
            lambda
            let
            let*
            letrec
            let-syntax
            letrec-syntax
            or
            quasiquote
            quote
            rec
            record-case
            record-evcase
            recur
            set!
            sigma
            struct
            syntax
            syntax-rules
            trace
            trace-lambda
            trace-let
            trace-recur
            unless
            unquote
            unquote-splicing
            untrace
            when
            with)))

(define slatex.variable-tokens '())
(define slatex.constant-tokens '())
(define slatex.data-tokens '())
(define slatex.special-symbols
  '(("." . ".")
    ("..." . "{\\dots}")
    ("-" . "$-$")
    ("1-" . "\\va{1$-$}")
    ("-1+" . "\\va{$-$1$+$}")))

(define slatex.macro-definers
  '("define-syntax"
    "syntax-rules"
    "defmacro"
    "extend-syntax"
    "define-macro!"))

(define slatex.case-and-ilk
  '("case" "record-case"))

(define slatex.tex-analog
  (lambda (c)
    (cond ((memv c '(#\$ #\& #\% #\# #\_)) (string #\\ c))
          ((memv c '(#\{ #\})) (string #\$ #\\ c #\$))
          ((char=? c #\\) "$\\backslash$")
          ((char=? c #\+) "$+$")
          ((char=? c #\=) "$=$")
          ((char=? c #\<) "$\\lt$")
          ((char=? c #\>) "$\\gt$")
          ((char=? c #\^) "\\^{}")
          ((char=? c #\|) "$\\vert$")
          ((char=? c #\~) "\\~{}")
          ((char=? c #\@) "{\\atsign}")
          ((char=? c #\") "{\\tt\\dq}")
          ((char=? c #\?) "?\\null")
          ((char=? c #\!) "!\\null")
          (else (string c)))))

(define slatex.token=?
  (lambda (t1 t2)
    ((if slatex.*slatex-case-sensitive?*
       string=?
       string-ci=?)
     t1
     t2)))

(define slatex.*slatex-case-sensitive?* #t)
(define slatex.*slatex-enabled?* #t)
(define slatex.*slatex-reenabler* "UNDEFINED")
(define slatex.*intext-triggerers*
  (list "scheme"))

(define slatex.*resultintext-triggerers*
  (list "schemeresult"))

(define slatex.*display-triggerers*
  (list "schemedisplay"))

(define slatex.*response-triggerers*
  (list "schemeresponse"))

(define *respbox-triggerers*
  (list "schemeresponsebox"))

(define slatex.*box-triggerers*
  (list "schemebox"))

(define slatex.*input-triggerers*
  (list "schemeinput"))

(define slatex.*region-triggerers*
  (list "schemeregion"))

(define slatex.*math-triggerers* '())
(define slatex.*slatex-in-protected-region?* #f)
(define slatex.*protected-files* '())
(define slatex.*include-onlys* 'all)
(define slatex.*latex?* #t)
(define slatex.*slatex-separate-includes?* #f)
(define slatex.set-keyword
  (lambda (x)
    (if (slatex.member-token x slatex.keyword-tokens)
      'skip
      (begin
        (set! slatex.constant-tokens
          (slatex.remove-token! x slatex.constant-tokens))
        (set! slatex.variable-tokens
          (slatex.remove-token! x slatex.variable-tokens))
        (set! slatex.data-tokens
          (slatex.remove-token! x slatex.data-tokens))
        (set! slatex.keyword-tokens
          (cons x slatex.keyword-tokens))))))

(define slatex.set-constant
  (lambda (x)
    (if (slatex.member-token x slatex.constant-tokens)
      'skip
      (begin
        (set! slatex.keyword-tokens
          (slatex.remove-token! x slatex.keyword-tokens))
        (set! slatex.variable-tokens
          (slatex.remove-token! x slatex.variable-tokens))
        (set! slatex.data-tokens
          (slatex.remove-token! x slatex.data-tokens))
        (set! slatex.constant-tokens
          (cons x slatex.constant-tokens))))))

(define slatex.set-variable
  (lambda (x)
    (if (slatex.member-token x slatex.variable-tokens)
      'skip
      (begin
        (set! slatex.keyword-tokens
          (slatex.remove-token! x slatex.keyword-tokens))
        (set! slatex.constant-tokens
          (slatex.remove-token! x slatex.constant-tokens))
        (set! slatex.data-tokens
          (slatex.remove-token! x slatex.data-tokens))
        (set! slatex.variable-tokens
          (cons x slatex.variable-tokens))))))

(define slatex.set-data
  (lambda (x)
    (if (slatex.member-token x slatex.data-tokens)
      'skip
      (begin
        (set! slatex.keyword-tokens
          (slatex.remove-token! x slatex.keyword-tokens))
        (set! slatex.constant-tokens
          (slatex.remove-token! x slatex.constant-tokens))
        (set! slatex.variable-tokens
          (slatex.remove-token! x slatex.variable-tokens))
        (set! slatex.data-tokens
          (cons x slatex.data-tokens))))))

(define slatex.set-special-symbol
  (lambda (x transl)
    (let ((c (slatex.assoc-token x slatex.special-symbols)))
      (if c
        (set-cdr! c transl)
        (set! slatex.special-symbols
          (cons (cons x transl) slatex.special-symbols))))))

(define slatex.unset-special-symbol
  (lambda (x)
    (set! slatex.special-symbols
      (slatex.remove-if!
        (lambda (c) (slatex.token=? (car c) x))
        slatex.special-symbols))))

(define slatex.texify
  (lambda (s) (list->string (slatex.texify-aux s))))

(define slatex.texify-data
  (lambda (s)
    (let loop ((l (slatex.texify-aux s)) (r '()))
      (if (null? l)
        (list->string (slatex.reverse! r))
        (let ((c (car l)))
          (loop (cdr l)
                (if (char=? c #\-)
                  (slatex.append! (list #\$ c #\$) r)
                  (cons c r))))))))

(define slatex.texify-aux
  (let* ((arrow (string->list "-$>$"))
         (arrow-lh (length arrow)))
    (lambda (s)
      (let* ((sl (string->list s))
             (texified-sl
               (slatex.append-map!
                 (lambda (c) (string->list (slatex.tex-analog c)))
                 sl)))
        (slatex.ormapcdr
          (lambda (d)
            (if (slatex.list-prefix? arrow d)
              (let ((to (string->list "$\\to$")))
                (set-car! d (car to))
                (set-cdr!
                  d
                  (append (cdr to) (list-tail d arrow-lh)))))
            #f)
          texified-sl)
        texified-sl))))

(define slatex.display-begin-sequence
  (lambda (out)
    (if (or slatex.*intext?* (not slatex.*latex?*))
      (begin
        (display "\\" out)
        (display slatex.*code-env-spec* out)
        (newline out))
      (begin
        (display "\\begin{" out)
        (display slatex.*code-env-spec* out)
        (display "}" out)
        (newline out)))))

(define slatex.display-end-sequence
  (lambda (out)
    (cond (slatex.*intext?*
           (display "\\end" out)
           (display slatex.*code-env-spec* out)
           (newline out))
          (slatex.*latex?*
           (display "\\end{" out)
           (display slatex.*code-env-spec* out)
           (display "}" out)
           (newline out))
          (else
           (display "\\end" out)
           (display slatex.*code-env-spec* out)
           (newline out)))))

(define slatex.display-tex-char
  (lambda (c p)
    (display
      (if (char? c) (slatex.tex-analog c) c)
      p)))

(define slatex.display-token
  (lambda (s typ p)
    (cond ((eq? typ 'syntax)
           (display "\\sy{" p)
           (display (slatex.texify s) p)
           (display "}" p))
          ((eq? typ 'variable)
           (display "\\va{" p)
           (display (slatex.texify s) p)
           (display "}" p))
          ((eq? typ 'constant)
           (display "\\cn{" p)
           (display (slatex.texify s) p)
           (display "}" p))
          ((eq? typ 'data)
           (display "\\dt{" p)
           (display (slatex.texify-data s) p)
           (display "}" p))
          (else (slatex.error 'slatex.display-token typ)))))

(define slatex.*max-line-length* 200)
(begin
  (define slatex.&inner-space (integer->char 7))
  (define slatex.&quote-space (integer->char 6))
  (define slatex.&bracket-space (integer->char 5))
  (define slatex.&paren-space (integer->char 4))
  (define slatex.&init-plain-space
    (integer->char 3))
 (define slatex.&init-space (integer->char 2))
  (define slatex.&plain-space (integer->char 1))
  (define slatex.&void-space (integer->char 0)))
(begin
  (define slatex.&plain-crg-ret (integer->char 4))
  (define slatex.&tabbed-crg-ret (integer->char 3))
  (define slatex.&move-tab (integer->char 2))
  (define slatex.&set-tab (integer->char 1))
  (define slatex.&void-tab (integer->char 0)))
(begin
  (define slatex.&end-math (integer->char 8))
  (define slatex.&mid-math (integer->char 7))
  (define slatex.&begin-math (integer->char 6))
  (define slatex.&end-string (integer->char 5))
  (define slatex.&mid-string (integer->char 4))
  (define slatex.&begin-string (integer->char 3))
  (define slatex.&mid-comment (integer->char 2))
  (define slatex.&begin-comment (integer->char 1))
  (define slatex.&void-notab (integer->char 0)))
(begin
  (define slatex.make-raw-line
    (lambda () (make-vector 5)))
 (define slatex.=notab 4)
  (define slatex.=tab 3)
  (define slatex.=space 2)
  (define slatex.=char 1)
  (define slatex.=rtedge 0))
(define slatex.make-line
  (lambda ()
    (let ((l (slatex.make-raw-line)))
      (vector-set! l slatex.=rtedge 0)
      (vector-set!
        l
        slatex.=char
        (make-string slatex.*max-line-length* #\space))
      (vector-set!
        l
        slatex.=space
        (make-string
          slatex.*max-line-length*
          slatex.&void-space))
      (vector-set!
        l
        slatex.=tab
        (make-string
          slatex.*max-line-length*
          slatex.&void-tab))
      (vector-set!
        l
        slatex.=notab
        (make-string
          slatex.*max-line-length*
          slatex.&void-notab))
      l)))

(define slatex.*line1* (slatex.make-line))
(define slatex.*line2* (slatex.make-line))
(begin
  (define slatex.make-case-frame
    (lambda () (make-vector 3)))
 (define slatex.=in-case-exp 2)
  (define slatex.=in-bktd-ctag-exp 1)
  (define =in-ctag-tkn 0))
(begin
  (define slatex.make-bq-frame
    (lambda () (make-vector 3)))
 (define slatex.=in-bktd-bq-exp 2)
  (define slatex.=in-bq-tkn 1)
  (define slatex.=in-comma 0))
(define slatex.*latex-paragraph-mode?* 'fwd1)
(define slatex.*intext?* 'fwd2)
(define slatex.*code-env-spec* "UNDEFINED")
(define slatex.*in* 'fwd3)
(define slatex.*out* 'fwd4)
(define slatex.*in-qtd-tkn* 'fwd5)
(define slatex.*in-bktd-qtd-exp* 'fwd6)
(define slatex.*in-mac-tkn* 'fwd7)
(define slatex.*in-bktd-mac-exp* 'fwd8)
(define slatex.*case-stack* 'fwd9)
(define slatex.*bq-stack* 'fwd10)
(define slatex.display-space
  (lambda (s p)
    (cond ((eq? s slatex.&plain-space) (display #\space p))
          ((eq? s slatex.&init-plain-space)
           (display #\space p))
          ((eq? s slatex.&init-space) (display "\\HL " p))
          ((eq? s slatex.&paren-space)
           (display "\\PRN " p))
          ((eq? s slatex.&bracket-space)
           (display "\\BKT " p))
          ((eq? s slatex.&quote-space)
           (display "\\QUO " p))
          ((eq? s slatex.&inner-space) (display "\\ " p)))))

(define slatex.display-tab
  (lambda (tab p)
    (cond ((eq? tab slatex.&set-tab) (display "\\=" p))
          ((eq? tab slatex.&move-tab) (display "\\>" p)))))

(define slatex.display-notab
  (lambda (notab p)
    (cond ((eq? notab slatex.&begin-string)
           (display "\\dt{" p))
          ((eq? notab slatex.&end-string) (display "}" p)))))

(define slatex.get-line
  (let ((curr-notab slatex.&void-notab))
    (lambda (line)
      (let ((graphic-char-seen? #f))
        (let loop ((i 0))
          (let ((c (read-char slatex.*in*)))
            (cond (graphic-char-seen? 'already-seen)
                  ((or (eof-object? c)
                       (char=? c slatex.*return*)
                       (char=? c #\newline)
                       (char=? c #\space)
                       (char=? c slatex.*tab*))
                   'not-yet)
                  (else (set! graphic-char-seen? #t)))
            (cond ((eof-object? c)
                   (cond ((eq? curr-notab slatex.&mid-string)
                          (if (>fx i 0)
                            (string-set!
                              (vector-ref line slatex.=notab)
                              (-fx i 1)
                              slatex.&end-string)))
                         ((eq? curr-notab slatex.&mid-comment)
                          (set! curr-notab slatex.&void-notab))
                         ((eq? curr-notab slatex.&mid-math)
                          (slatex.error
                            'slatex.get-line
                            'runaway-math-subformula)))
                   (string-set!
                     (vector-ref line slatex.=char)
                     i
                     #\newline)
                   (string-set!
                     (vector-ref line slatex.=space)
                     i
                     slatex.&void-space)
                   (string-set!
                     (vector-ref line slatex.=tab)
                     i
                     slatex.&void-tab)
                   (string-set!
                     (vector-ref line slatex.=notab)
                     i
                     slatex.&void-notab)
                   (vector-set! line slatex.=rtedge i)
                   (if (eq? (string-ref (vector-ref line slatex.=notab) 0)
                            slatex.&mid-string)
                     (string-set!
                       (vector-ref line slatex.=notab)
                       0
                       slatex.&begin-string))
                   (if (=fx i 0) #f #t))
                  ((or (char=? c slatex.*return*)
                       (char=? c #\newline))
                   (if (and (eq? *op-sys* 'dos)
                            (char=? c slatex.*return*))
                     (if (char=? (peek-char slatex.*in*) #\newline)
                       (read-char slatex.*in*)))
                   (cond ((eq? curr-notab slatex.&mid-string)
                          (if (>fx i 0)
                            (string-set!
                              (vector-ref line slatex.=notab)
                              (-fx i 1)
                              slatex.&end-string)))
                         ((eq? curr-notab slatex.&mid-comment)
                          (set! curr-notab slatex.&void-notab))
                         ((eq? curr-notab slatex.&mid-math)
                          (slatex.error
                            'slatex.get-line
                            'runaway-math-subformula)))
                   (string-set!
                     (vector-ref line slatex.=char)
                     i
                     #\newline)
                   (string-set!
                     (vector-ref line slatex.=space)
                     i
                     slatex.&void-space)
                   (string-set!
                     (vector-ref line slatex.=tab)
                     i
                     (cond ((eof-object? (peek-char slatex.*in*))
                            slatex.&plain-crg-ret)
                           (slatex.*intext?* slatex.&plain-crg-ret)
                           (else slatex.&tabbed-crg-ret)))
                   (string-set!
                     (vector-ref line slatex.=notab)
                     i
                     slatex.&void-notab)
                   (vector-set! line slatex.=rtedge i)
                   (if (eq? (string-ref (vector-ref line slatex.=notab) 0)
                            slatex.&mid-string)
                     (string-set!
                       (vector-ref line slatex.=notab)
                       0
                       slatex.&begin-string))
                   #t)
                  ((eq? curr-notab slatex.&mid-comment)
                   (string-set! (vector-ref line slatex.=char) i c)
                   (string-set!
                     (vector-ref line slatex.=space)
                     i
                     (cond ((char=? c #\space) slatex.&plain-space)
                           ((char=? c slatex.*tab*) slatex.&plain-space)
                           (else slatex.&void-space)))
                   (string-set!
                     (vector-ref line slatex.=tab)
                     i
                     slatex.&void-tab)
                   (string-set!
                     (vector-ref line slatex.=notab)
                     i
                     slatex.&mid-comment)
                   (loop (+fx i 1)))
                  ((char=? c #\\)
                   (string-set! (vector-ref line slatex.=char) i c)
                   (string-set!
                     (vector-ref line slatex.=space)
                     i
                     slatex.&void-space)
                   (string-set!
                     (vector-ref line slatex.=tab)
                     i
                     slatex.&void-tab)
                   (string-set!
                     (vector-ref line slatex.=notab)
                     i
                     curr-notab)
                   (let ((i+1 (+fx i 1)) (c+1 (read-char slatex.*in*)))
                     (if (char=? c+1 slatex.*tab*) (set! c+1 #\space))
                     (string-set!
                       (vector-ref line slatex.=char)
                       i+1
                       c+1)
                     (string-set!
                       (vector-ref line slatex.=space)
                       i+1
                       (if (char=? c+1 #\space)
                         slatex.&plain-space
                         slatex.&void-space))
                     (string-set!
                       (vector-ref line slatex.=tab)
                       i+1
                       slatex.&void-tab)
                     (string-set!
                       (vector-ref line slatex.=notab)
                       i+1
                       curr-notab)
                     (loop (+fx i+1 1))))
                  ((eq? curr-notab slatex.&mid-math)
                   (if (char=? c slatex.*tab*) (set! c #\space))
                   (string-set!
                     (vector-ref line slatex.=space)
                     i
                     (if (char=? c #\space)
                       slatex.&plain-space
                       slatex.&void-space))
                   (string-set!
                     (vector-ref line slatex.=tab)
                     i
                     slatex.&void-tab)
                   (cond ((memv c slatex.*math-triggerers*)
                          (string-set!
                            (vector-ref line slatex.=char)
                            i
                            #\$)
                          (string-set!
                            (vector-ref line slatex.=notab)
                            i
                            slatex.&end-math)
                          (set! curr-notab slatex.&void-notab))
                         (else
                          (string-set! (vector-ref line slatex.=char) i c)
                          (string-set!
                            (vector-ref line slatex.=notab)
                            i
                            slatex.&mid-math)))
                   (loop (+fx i 1)))
                  ((eq? curr-notab slatex.&mid-string)
                   (if (char=? c slatex.*tab*) (set! c #\space))
                   (string-set! (vector-ref line slatex.=char) i c)
                   (string-set!
                     (vector-ref line slatex.=space)
                     i
                     (if (char=? c #\space)
                       slatex.&inner-space
                       slatex.&void-space))
                   (string-set!
                     (vector-ref line slatex.=tab)
                     i
                     slatex.&void-tab)
                   (string-set!
                     (vector-ref line slatex.=notab)
                     i
                     (cond ((char=? c #\")
                            (set! curr-notab slatex.&void-notab)
                            slatex.&end-string)
                           (else slatex.&mid-string)))
                   (loop (+fx i 1)))
                  ((char=? c #\space)
                   (string-set! (vector-ref line slatex.=char) i c)
                   (string-set!
                     (vector-ref line slatex.=space)
                     i
                     (cond (slatex.*intext?* slatex.&plain-space)
                           (graphic-char-seen? slatex.&inner-space)
                           (else slatex.&init-space)))
                   (string-set!
                     (vector-ref line slatex.=tab)
                     i
                     slatex.&void-tab)
                   (string-set!
                     (vector-ref line slatex.=notab)
                     i
                     slatex.&void-notab)
                   (loop (+fx i 1)))
                  ((char=? c slatex.*tab*)
                   (let loop2 ((i i) (j 0))
                     (if (<fx j 8)
                       (begin
                         (string-set!
                           (vector-ref line slatex.=char)
                           i
                           #\space)
                         (string-set!
                           (vector-ref line slatex.=space)
                           i
                           (cond (slatex.*intext?* slatex.&plain-space)
                                 (graphic-char-seen? slatex.&inner-space)
                                 (else slatex.&init-space)))
                         (string-set!
                           (vector-ref line slatex.=tab)
                           i
                           slatex.&void-tab)
                         (string-set!
                           (vector-ref line slatex.=notab)
                           i
                           slatex.&void-notab)
                         (loop2 (+fx i 1) (+fx j 1)))))
                   (loop (+fx i 8)))
                  ((char=? c #\")
                   (string-set! (vector-ref line slatex.=char) i c)
                   (string-set!
                     (vector-ref line slatex.=space)
                     i
                     slatex.&void-space)
                   (string-set!
                     (vector-ref line slatex.=tab)
                     i
                     slatex.&void-tab)
                   (string-set!
                     (vector-ref line slatex.=notab)
                     i
                     slatex.&begin-string)
                   (set! curr-notab slatex.&mid-string)
                   (loop (+fx i 1)))
                  ((char=? c #\;)
                   (string-set! (vector-ref line slatex.=char) i c)
                   (string-set!
                     (vector-ref line slatex.=space)
                     i
                     slatex.&void-space)
                   (string-set!
                     (vector-ref line slatex.=tab)
                     i
                     slatex.&void-tab)
                   (string-set!
                     (vector-ref line slatex.=notab)
                     i
                     slatex.&begin-comment)
                   (set! curr-notab slatex.&mid-comment)
                   (loop (+fx i 1)))
                  ((memv c slatex.*math-triggerers*)
                   (string-set!
                     (vector-ref line slatex.=char)
                     i
                     #\$)
                   (string-set!
                     (vector-ref line slatex.=space)
                     i
                     slatex.&void-space)
                   (string-set!
                     (vector-ref line slatex.=tab)
                     i
                     slatex.&void-tab)
                   (string-set!
                     (vector-ref line slatex.=notab)
                     i
                     slatex.&begin-math)
                   (set! curr-notab slatex.&mid-math)
                   (loop (+fx i 1)))
                  (else
                   (string-set! (vector-ref line slatex.=char) i c)
                   (string-set!
                     (vector-ref line slatex.=space)
                     i
                     slatex.&void-space)
                   (string-set!
                     (vector-ref line slatex.=tab)
                     i
                     slatex.&void-tab)
                   (string-set!
                     (vector-ref line slatex.=notab)
                     i
                     slatex.&void-notab)
                   (loop (+fx i 1))))))))))

(define slatex.peephole-adjust
  (lambda (curr prev)
    (if (or (slatex.blank-line? curr)
            (slatex.flush-comment-line? curr))
      (if slatex.*latex-paragraph-mode?*
        'skip
        (begin
          (set! slatex.*latex-paragraph-mode?* #t)
          (if slatex.*intext?*
            'skip
            (begin
              (slatex.remove-some-tabs prev 0)
              (let ((prev-rtedge (vector-ref prev slatex.=rtedge)))
                (if (eq? (string-ref
                           (vector-ref prev slatex.=tab)
                           prev-rtedge)
                         slatex.&tabbed-crg-ret)
                  (string-set!
                    (vector-ref prev slatex.=tab)
                    (vector-ref prev slatex.=rtedge)
                    slatex.&plain-crg-ret)))))))
      (begin
        (if slatex.*latex-paragraph-mode?*
          (set! slatex.*latex-paragraph-mode?* #f)
          (if slatex.*intext?*
            'skip
            (let ((remove-tabs-from #f))
              (let loop ((i 0))
                (cond ((char=?
                         (string-ref (vector-ref curr slatex.=char) i)
                         #\newline)
                       (set! remove-tabs-from i))
                      ((char=?
                         (string-ref (vector-ref prev slatex.=char) i)
                         #\newline)
                       (set! remove-tabs-from #f))
                      ((eq? (string-ref (vector-ref curr slatex.=space) i)
                            slatex.&init-space)
                       (if (eq? (string-ref (vector-ref prev slatex.=notab) i)
                                slatex.&void-notab)
                         (begin
                           (cond ((or (char=?
                                        (string-ref
                                          (vector-ref prev slatex.=char)
                                          i)
                                        #\()
                                      (eq? (string-ref
                                             (vector-ref prev slatex.=space)
                                             i)
                                           slatex.&paren-space))
                                  (string-set!
                                    (vector-ref curr slatex.=space)
                                    i
                                    slatex.&paren-space))
                                 ((or (char=?
                                        (string-ref
                                          (vector-ref prev slatex.=char)
                                          i)
                                        #\[)
                                      (eq? (string-ref
                                             (vector-ref prev slatex.=space)
                                             i)
                                           slatex.&bracket-space))
                                  (string-set!
                                    (vector-ref curr slatex.=space)
                                    i
                                    slatex.&bracket-space))
                                 ((or (memv (string-ref
                                              (vector-ref prev slatex.=char)
                                              i)
                                            '(#\' #\` #\,))
                                      (eq? (string-ref
                                             (vector-ref prev slatex.=space)
                                             i)
                                           slatex.&quote-space))
                                  (string-set!
                                    (vector-ref curr slatex.=space)
                                    i
                                    slatex.&quote-space)))
                           (if (memq (string-ref
                                       (vector-ref prev slatex.=tab)
                                       i)
                                     (list slatex.&set-tab slatex.&move-tab))
                             (string-set!
                               (vector-ref curr slatex.=tab)
                               i
                               slatex.&move-tab))))
                       (loop (+fx i 1)))
                      ((=fx i 0) (set! remove-tabs-from 0))
                      ((not (eq? (string-ref (vector-ref prev slatex.=tab) i)
                                 slatex.&void-tab))
                       (set! remove-tabs-from (+fx i 1))
                       (if (memq (string-ref (vector-ref prev slatex.=tab) i)
                                 (list slatex.&set-tab slatex.&move-tab))
                         (string-set!
                           (vector-ref curr slatex.=tab)
                           i
                           slatex.&move-tab)))
                      ((memq (string-ref (vector-ref prev slatex.=space) i)
                             (list slatex.&init-space
                                   slatex.&init-plain-space
                                   slatex.&paren-space
                                   slatex.&bracket-space
                                   slatex.&quote-space))
                       (set! remove-tabs-from (+fx i 1)))
                      ((and (char=?
                              (string-ref
                                (vector-ref prev slatex.=char)
                                (-fx i 1))
                              #\space)
                            (eq? (string-ref
                                   (vector-ref prev slatex.=notab)
                                   (-fx i 1))
                                 slatex.&void-notab))
                       (set! remove-tabs-from (+fx i 1))
                       (string-set!
                         (vector-ref prev slatex.=tab)
                         i
                         slatex.&set-tab)
                       (string-set!
                         (vector-ref curr slatex.=tab)
                         i
                         slatex.&move-tab))
                      (else
                       (set! remove-tabs-from (+fx i 1))
                       (let loop1 ((j (-fx i 1)))
                         (cond ((<=fx j 0) 'exit-loop1)
                               ((not (eq? (string-ref
                                            (vector-ref curr slatex.=tab)
                                            j)
                                          slatex.&void-tab))
                                'exit-loop1)
                               ((memq (string-ref
                                        (vector-ref curr slatex.=space)
                                        j)
                                      (list slatex.&paren-space
                                            slatex.&bracket-space
                                            slatex.&quote-space))
                                (loop1 (-fx j 1)))
                               ((or (not (eq? (string-ref
                                                (vector-ref prev slatex.=notab)
                                                j)
                                              slatex.&void-notab))
                                    (char=?
                                      (string-ref
                                        (vector-ref prev slatex.=char)
                                        j)
                                      #\space))
                                (let ((k (+fx j 1)))
                                  (if (memq (string-ref
                                              (vector-ref prev slatex.=notab)
                                              k)
                                            (list slatex.&mid-comment
                                                  slatex.&mid-math
                                                  slatex.&end-math
                                                  slatex.&mid-string
                                                  slatex.&end-string))
                                    'skip
                                    (begin
                                      (if (eq? (string-ref
                                                 (vector-ref prev slatex.=tab)
                                                 k)
                                               slatex.&void-tab)
                                        (string-set!
                                          (vector-ref prev slatex.=tab)
                                          k
                                          slatex.&set-tab))
                                      (string-set!
                                        (vector-ref curr slatex.=tab)
                                        k
                                        slatex.&move-tab)))))
                               (else 'anything-else?))))))
              (slatex.remove-some-tabs prev remove-tabs-from))))
        (if slatex.*intext?*
          'skip
          (slatex.add-some-tabs curr))
        (slatex.clean-init-spaces curr)
        (slatex.clean-inner-spaces curr)))))

(define slatex.add-some-tabs
  (lambda (line)
    (let loop ((i 1) (succ-parens? #f))
      (let ((c (string-ref (vector-ref line slatex.=char) i)))
        (cond ((char=? c #\newline) 'exit-loop)
              ((not (eq? (string-ref (vector-ref line slatex.=notab) i)
                         slatex.&void-notab))
               (loop (+fx i 1) #f))
              ((char=? c #\[)
               (if (eq? (string-ref (vector-ref line slatex.=tab) i)
                        slatex.&void-tab)
                 (string-set!
                   (vector-ref line slatex.=tab)
                   i
                   slatex.&set-tab))
               (loop (+fx i 1) #f))
              ((char=? c #\()
               (if (eq? (string-ref (vector-ref line slatex.=tab) i)
                        slatex.&void-tab)
                 (if succ-parens?
                   'skip
                   (string-set!
                     (vector-ref line slatex.=tab)
                     i
                     slatex.&set-tab)))
               (loop (+fx i 1) #t))
              (else (loop (+fx i 1) #f)))))))

(define slatex.remove-some-tabs
  (lambda (line i)
    (if i
      (let loop ((i i))
        (cond ((char=?
                 (string-ref (vector-ref line slatex.=char) i)
                 #\newline)
               'exit)
              ((eq? (string-ref (vector-ref line slatex.=tab) i)
                    slatex.&set-tab)
               (string-set!
                 (vector-ref line slatex.=tab)
                 i
                 slatex.&void-tab)
               (loop (+fx i 1)))
              (else (loop (+fx i 1))))))))

(define slatex.clean-init-spaces
  (lambda (line)
    (let loop ((i (vector-ref line slatex.=rtedge)))
      (cond ((<fx i 0) 'exit-loop)
            ((eq? (string-ref (vector-ref line slatex.=tab) i)
                  slatex.&move-tab)
             (let loop2 ((i (-fx i 1)))
               (cond ((<fx i 0) 'exit-loop2)
                     ((memq (string-ref (vector-ref line slatex.=space) i)
                            (list slatex.&init-space
                                  slatex.&paren-space
                                  slatex.&bracket-space
                                  slatex.&quote-space))
                      (string-set!
                        (vector-ref line slatex.=space)
                        i
                        slatex.&init-plain-space)
                      (loop2 (-fx i 1)))
                     (else (loop2 (-fx i 1))))))
            (else (loop (-fx i 1)))))))

(define slatex.clean-inner-spaces
  (lambda (line)
    (let loop ((i 0) (succ-inner-spaces? #f))
      (cond ((char=?
               (string-ref (vector-ref line slatex.=char) i)
               #\newline)
             'exit-loop)
            ((eq? (string-ref (vector-ref line slatex.=space) i)
                  slatex.&inner-space)
             (if succ-inner-spaces?
               'skip
               (string-set!
                 (vector-ref line slatex.=space)
                 i
                 slatex.&plain-space))
             (loop (+fx i 1) #t))
            (else (loop (+fx i 1) #f))))))

(define slatex.blank-line?
  (lambda (line)
    (let loop ((i 0))
      (let ((c (string-ref (vector-ref line slatex.=char) i)))
        (cond ((char=? c #\space)
               (if (eq? (string-ref (vector-ref line slatex.=notab) i)
                        slatex.&void-notab)
                 (loop (+fx i 1))
                 #f))
              ((char=? c #\newline)
               (let loop2 ((j (-fx i 1)))
                 (if (<=fx j 0)
                   'skip
                   (begin
                     (string-set!
                       (vector-ref line slatex.=space)
                       i
                       slatex.&void-space)
                     (loop2 (-fx j 1)))))
               #t)
              (else #f))))))

(define slatex.flush-comment-line?
  (lambda (line)
    (and (char=?
           (string-ref (vector-ref line slatex.=char) 0)
           #\;)
         (eq? (string-ref (vector-ref line slatex.=notab) 0)
              slatex.&begin-comment)
         (not (char=?
                (string-ref (vector-ref line slatex.=char) 1)
                #\;)))))

(define slatex.do-all-lines
  (lambda ()
    (let loop ((line1 slatex.*line1*) (line2 slatex.*line2*))
      (let* ((line2-paragraph? slatex.*latex-paragraph-mode?*)
             (more? (slatex.get-line line1)))
        (slatex.peephole-adjust line1 line2)
        ((if line2-paragraph?
           slatex.display-tex-line
           slatex.display-scm-line)
         line2)
        (if (eq? line2-paragraph?
                 slatex.*latex-paragraph-mode?*)
          'else
          ((if slatex.*latex-paragraph-mode?*
             slatex.display-end-sequence
             slatex.display-begin-sequence)
           slatex.*out*))
        (if more? (loop line2 line1))))))

(define scheme2tex
  (lambda (inport outport)
    (set! slatex.*in* inport)
    (set! slatex.*out* outport)
    (set! slatex.*latex-paragraph-mode?* #t)
    (set! slatex.*in-qtd-tkn* #f)
    (set! slatex.*in-bktd-qtd-exp* 0)
    (set! slatex.*in-mac-tkn* #f)
    (set! slatex.*in-bktd-mac-exp* 0)
    (set! slatex.*case-stack* '())
    (set! slatex.*bq-stack* '())
    (let ((flush-line
            (lambda (line)
              (vector-set! line slatex.=rtedge 0)
              (string-set!
                (vector-ref line slatex.=char)
                0
                #\newline)
              (string-set!
                (vector-ref line slatex.=space)
                0
                slatex.&void-space)
              (string-set!
                (vector-ref line slatex.=tab)
                0
                slatex.&void-tab)
              (string-set!
                (vector-ref line slatex.=notab)
                0
                slatex.&void-notab))))
      (flush-line slatex.*line1*)
      (flush-line slatex.*line2*))
    (slatex.do-all-lines)))

(define slatex.display-tex-line
  (lambda (line)
    (cond (else
           (let loop ((i (if (slatex.flush-comment-line? line) 1 0)))
             (let ((c (string-ref (vector-ref line slatex.=char) i)))
               (if (char=? c #\newline)
                 (if (eq? (string-ref (vector-ref line slatex.=tab) i)
                          slatex.&void-tab)
                   'skip
                   (newline slatex.*out*))
                 (begin (display c slatex.*out*) (loop (+fx i 1))))))))))

(define slatex.display-scm-line
  (lambda (line)
    (let loop ((i 0))
      (let ((c (string-ref (vector-ref line slatex.=char) i)))
        (cond ((char=? c #\newline)
               (let ((tab (string-ref (vector-ref line slatex.=tab) i)))
                 (cond ((eq? tab slatex.&tabbed-crg-ret)
                        (display "\\\\" slatex.*out*)
                        (newline slatex.*out*))
                       ((eq? tab slatex.&plain-crg-ret)
                        (newline slatex.*out*))
                       ((eq? tab slatex.&void-tab)
                        (display #\% slatex.*out*)
                        (newline slatex.*out*)))))
              ((eq? (string-ref (vector-ref line slatex.=notab) i)
                    slatex.&begin-comment)
               (slatex.display-tab
                 (string-ref (vector-ref line slatex.=tab) i)
                 slatex.*out*)
               (display c slatex.*out*)
               (loop (+fx i 1)))
              ((eq? (string-ref (vector-ref line slatex.=notab) i)
                    slatex.&mid-comment)
               (display c slatex.*out*)
               (loop (+fx i 1)))
              ((eq? (string-ref (vector-ref line slatex.=notab) i)
                    slatex.&begin-string)
               (slatex.display-tab
                 (string-ref (vector-ref line slatex.=tab) i)
                 slatex.*out*)
               (display "\\dt{" slatex.*out*)
               (if (char=? c #\space)
                 (slatex.display-space
                   (string-ref (vector-ref line slatex.=space) i)
                   slatex.*out*)
                 (slatex.display-tex-char c slatex.*out*))
               (loop (+fx i 1)))
              ((eq? (string-ref (vector-ref line slatex.=notab) i)
                    slatex.&mid-string)
               (if (char=? c #\space)
                 (slatex.display-space
                   (string-ref (vector-ref line slatex.=space) i)
                   slatex.*out*)
                 (slatex.display-tex-char c slatex.*out*))
               (loop (+fx i 1)))
              ((eq? (string-ref (vector-ref line slatex.=notab) i)
                    slatex.&end-string)
               (if (char=? c #\space)
                 (slatex.display-space
                   (string-ref (vector-ref line slatex.=space) i)
                   slatex.*out*)
                 (slatex.display-tex-char c slatex.*out*))
               (display "}" slatex.*out*)
               (if slatex.*in-qtd-tkn*
                 (set! slatex.*in-qtd-tkn* #f)
                 (if slatex.*in-mac-tkn*
                   (set! slatex.*in-mac-tkn* #f)))
               (loop (+fx i 1)))
              ((eq? (string-ref (vector-ref line slatex.=notab) i)
                    slatex.&begin-math)
               (slatex.display-tab
                 (string-ref (vector-ref line slatex.=tab) i)
                 slatex.*out*)
               (display c slatex.*out*)
               (loop (+fx i 1)))
              ((eq? (string-ref (vector-ref line slatex.=notab) i)
                    slatex.&mid-math)
               (display c slatex.*out*)
               (loop (+fx i 1)))
              ((eq? (string-ref (vector-ref line slatex.=notab) i)
                    slatex.&end-math)
               (display c slatex.*out*)
               (if slatex.*in-qtd-tkn*
                 (set! slatex.*in-qtd-tkn* #f)
                 (if slatex.*in-mac-tkn*
                   (set! slatex.*in-mac-tkn* #f)))
               (loop (+fx i 1)))
              ((char=? c #\space)
               (slatex.display-tab
                 (string-ref (vector-ref line slatex.=tab) i)
                 slatex.*out*)
               (slatex.display-space
                 (string-ref (vector-ref line slatex.=space) i)
                 slatex.*out*)
               (loop (+fx i 1)))
              ((char=? c #\')
               (slatex.display-tab
                 (string-ref (vector-ref line slatex.=tab) i)
                 slatex.*out*)
               (display c slatex.*out*)
               (if (or slatex.*in-qtd-tkn*
                       (>fx slatex.*in-bktd-qtd-exp* 0))
                 'skip
                 (set! slatex.*in-qtd-tkn* #t))
               (loop (+fx i 1)))
              ((char=? c #\`)
               (slatex.display-tab
                 (string-ref (vector-ref line slatex.=tab) i)
                 slatex.*out*)
               (display c slatex.*out*)
               (if (or (null? slatex.*bq-stack*)
                       (vector-ref
                         (car slatex.*bq-stack*)
                         slatex.=in-comma))
                 (set! slatex.*bq-stack*
                   (cons (let ((f (slatex.make-bq-frame)))
                           (vector-set! f slatex.=in-comma #f)
                           (vector-set! f slatex.=in-bq-tkn #t)
                           (vector-set! f slatex.=in-bktd-bq-exp 0)
                           f)
                         slatex.*bq-stack*)))
               (loop (+fx i 1)))
              ((char=? c #\,)
               (slatex.display-tab
                 (string-ref (vector-ref line slatex.=tab) i)
                 slatex.*out*)
               (display c slatex.*out*)
               (if (or (null? slatex.*bq-stack*)
                       (vector-ref
                         (car slatex.*bq-stack*)
                         slatex.=in-comma))
                 'skip
                 (set! slatex.*bq-stack*
                   (cons (let ((f (slatex.make-bq-frame)))
                           (vector-set! f slatex.=in-comma #t)
                           (vector-set! f slatex.=in-bq-tkn #t)
                           (vector-set! f slatex.=in-bktd-bq-exp 0)
                           f)
                         slatex.*bq-stack*)))
               (if (char=?
                     (string-ref
                       (vector-ref line slatex.=char)
                       (+fx i 1))
                     #\@)
                 (begin
                   (slatex.display-tex-char #\@ slatex.*out*)
                   (loop (+fx 2 i)))
                 (loop (+fx i 1))))
              ((memv c '(#\( #\[))
               (slatex.display-tab
                 (string-ref (vector-ref line slatex.=tab) i)
                 slatex.*out*)
               (display c slatex.*out*)
               (cond (slatex.*in-qtd-tkn*
                      (set! slatex.*in-qtd-tkn* #f)
                      (set! slatex.*in-bktd-qtd-exp* 1))
                     ((>fx slatex.*in-bktd-qtd-exp* 0)
                      (set! slatex.*in-bktd-qtd-exp*
                        (+fx slatex.*in-bktd-qtd-exp* 1))))
               (cond (slatex.*in-mac-tkn*
                      (set! slatex.*in-mac-tkn* #f)
                      (set! slatex.*in-bktd-mac-exp* 1))
                     ((>fx slatex.*in-bktd-mac-exp* 0)
                      (set! slatex.*in-bktd-mac-exp*
                        (+fx slatex.*in-bktd-mac-exp* 1))))
               (if (null? slatex.*bq-stack*)
                 'skip
                 (let ((top (car slatex.*bq-stack*)))
                   (cond ((vector-ref top slatex.=in-bq-tkn)
                          (vector-set! top slatex.=in-bq-tkn #f)
                          (vector-set! top slatex.=in-bktd-bq-exp 1))
                         ((>fx (vector-ref top slatex.=in-bktd-bq-exp) 0)
                          (vector-set!
                            top
                            slatex.=in-bktd-bq-exp
                            (+fx (vector-ref top slatex.=in-bktd-bq-exp) 1))))))
               (if (null? slatex.*case-stack*)
                 'skip
                 (let ((top (car slatex.*case-stack*)))
                   (cond ((vector-ref top =in-ctag-tkn)
                          (vector-set! top =in-ctag-tkn #f)
                          (vector-set! top slatex.=in-bktd-ctag-exp 1))
                         ((>fx (vector-ref top slatex.=in-bktd-ctag-exp) 0)
                          (vector-set!
                            top
                            slatex.=in-bktd-ctag-exp
                            (+fx (vector-ref top slatex.=in-bktd-ctag-exp) 1)))
                         ((>fx (vector-ref top slatex.=in-case-exp) 0)
                          (vector-set!
                            top
                            slatex.=in-case-exp
                            (+fx (vector-ref top slatex.=in-case-exp) 1))
                          (if (=fx (vector-ref top slatex.=in-case-exp) 2)
                            (set! slatex.*in-qtd-tkn* #t))))))
               (loop (+fx i 1)))
              ((memv c '(#\) #\]))
               (slatex.display-tab
                 (string-ref (vector-ref line slatex.=tab) i)
                 slatex.*out*)
               (display c slatex.*out*)
               (if (>fx slatex.*in-bktd-qtd-exp* 0)
                 (set! slatex.*in-bktd-qtd-exp*
                   (-fx slatex.*in-bktd-qtd-exp* 1)))
               (if (>fx slatex.*in-bktd-mac-exp* 0)
                 (set! slatex.*in-bktd-mac-exp*
                   (-fx slatex.*in-bktd-mac-exp* 1)))
               (if (null? slatex.*bq-stack*)
                 'skip
                 (let ((top (car slatex.*bq-stack*)))
                   (if (>fx (vector-ref top slatex.=in-bktd-bq-exp) 0)
                     (begin
                       (vector-set!
                         top
                         slatex.=in-bktd-bq-exp
                         (-fx (vector-ref top slatex.=in-bktd-bq-exp) 1))
                       (if (=fx (vector-ref top slatex.=in-bktd-bq-exp) 0)
                         (set! slatex.*bq-stack* (cdr slatex.*bq-stack*)))))))
               (let loop ()
                 (if (null? slatex.*case-stack*)
                   'skip
                   (let ((top (car slatex.*case-stack*)))
                     (cond ((>fx (vector-ref top slatex.=in-bktd-ctag-exp) 0)
                            (vector-set!
                              top
                              slatex.=in-bktd-ctag-exp
                              (-fx (vector-ref top slatex.=in-bktd-ctag-exp) 1))
                            (if (=fx (vector-ref top slatex.=in-bktd-ctag-exp) 0)
                              (vector-set! top slatex.=in-case-exp 1)))
                           ((>fx (vector-ref top slatex.=in-case-exp) 0)
                            (vector-set!
                              top
                              slatex.=in-case-exp
                              (-fx (vector-ref top slatex.=in-case-exp) 1))
                            (if (=fx (vector-ref top slatex.=in-case-exp) 0)
                              (begin
                                (set! slatex.*case-stack*
                                  (cdr slatex.*case-stack*))
                                (loop))))))))
               (loop (+fx i 1)))
              (else
               (slatex.display-tab
                 (string-ref (vector-ref line slatex.=tab) i)
                 slatex.*out*)
               (loop (slatex.do-token line i))))))))

(define slatex.do-token
  (let ((token-delims
          (list #\(
                #\)
                #\[
                #\]
                #\space
                slatex.*return*
                #\newline
                #\,
                #\;)))
    (lambda (line i)
      (let loop ((buf '()) (i i))
        (let ((c (string-ref (vector-ref line slatex.=char) i)))
          (cond ((char=? c #\\)
                 (loop (cons (string-ref
                               (vector-ref line slatex.=char)
                               (+fx i 1))
                             (cons c buf))
                       (+fx i 2)))
                ((or (memv c token-delims)
                     (memv c slatex.*math-triggerers*))
                 (slatex.output-token
                   (list->string (slatex.reverse! buf)))
                 i)
                ((char? c)
                 (loop (cons (string-ref (vector-ref line slatex.=char) i)
                             buf)
                       (+fx i 1)))
                (else (slatex.error 'slatex.do-token 1))))))))

(define slatex.output-token
  (lambda (token)
    (if (null? slatex.*case-stack*)
      'skip
      (let ((top (car slatex.*case-stack*)))
        (if (vector-ref top =in-ctag-tkn)
          (begin
            (vector-set! top =in-ctag-tkn #f)
            (vector-set! top slatex.=in-case-exp 1)))))
    (if (slatex.assoc-token token slatex.special-symbols)
      (begin
        (if slatex.*in-qtd-tkn*
          (set! slatex.*in-qtd-tkn* #f)
          (if slatex.*in-mac-tkn*
            (set! slatex.*in-mac-tkn* #f)))
        (display
          (cdr (slatex.assoc-token token slatex.special-symbols))
          slatex.*out*))
      (slatex.display-token
        token
        (cond (slatex.*in-qtd-tkn*
               (set! slatex.*in-qtd-tkn* #f)
               (cond ((equal? token "else") 'syntax)
                     ((slatex.member-token token slatex.data-tokens)
                      'data)
                     ((slatex.member-token
                        token
                        slatex.constant-tokens)
                      'constant)
                     ((slatex.member-token
                        token
                        slatex.variable-tokens)
                      'constant)
                     ((slatex.member-token token slatex.keyword-tokens)
                      'constant)
                     ((slatex.prim-data-token? token) 'data)
                     (else 'constant)))
              ((>fx slatex.*in-bktd-qtd-exp* 0) 'constant)
              ((and (not (null? slatex.*bq-stack*))
                    (not (vector-ref
                           (car slatex.*bq-stack*)
                           slatex.=in-comma)))
               'constant)
              (slatex.*in-mac-tkn*
               (set! slatex.*in-mac-tkn* #f)
               (slatex.set-keyword token)
               'syntax)
              ((>fx slatex.*in-bktd-mac-exp* 0)
               (slatex.set-keyword token)
               'syntax)
              ((slatex.member-token token slatex.data-tokens)
               'data)
              ((slatex.member-token
                 token
                 slatex.constant-tokens)
               'constant)
              ((slatex.member-token
                 token
                 slatex.variable-tokens)
               'variable)
              ((slatex.member-token token slatex.keyword-tokens)
               (cond ((slatex.token=? token "quote")
                      (set! slatex.*in-qtd-tkn* #t))
                     ((slatex.member-token token slatex.macro-definers)
                      (set! slatex.*in-mac-tkn* #t))
                     ((slatex.member-token token slatex.case-and-ilk)
                      (set! slatex.*case-stack*
                        (cons (let ((f (slatex.make-case-frame)))
                                (vector-set! f =in-ctag-tkn #t)
                                (vector-set! f slatex.=in-bktd-ctag-exp 0)
                                (vector-set! f slatex.=in-case-exp 0)
                                f)
                              slatex.*case-stack*))))
               'syntax)
              ((slatex.prim-data-token? token) 'data)
              (else 'variable))
        slatex.*out*))
    (if (and (not (null? slatex.*bq-stack*))
             (vector-ref
               (car slatex.*bq-stack*)
               slatex.=in-bq-tkn))
      (set! slatex.*bq-stack* (cdr slatex.*bq-stack*)))))

(define slatex.prim-data-token?
  (lambda (token)
    (or (char=? (string-ref token 0) #\#)
        (string->number token))))

(define slatex.*texinputs* "")
(define slatex.*texinputs-list* '())
(define slatex.*path-separator*
  (cond ((eq? *op-sys* 'unix) #\:)
        ((memq *op-sys* '(os2 dos os2fat)) #\;)
        (else
         (slatex.error
           'slatex.*path-separator*
           'cant-determine))))

(define slatex.*directory-mark*
  (cond ((eq? *op-sys* 'unix) "/")
        ((memq *op-sys* '(os2 dos os2fat)) "\\")
        (else
         (slatex.error
           'slatex.*directory-mark*
           'cant-determine))))

(define slatex.*file-hider*
  (cond ((memq *op-sys* '(os2 unix)) ".")
        ((memq *op-sys* '(dos os2fat)) "x")
        (else ".")))

(define slatex.path-to-list
  (lambda (p)
    (let loop ((p (string->list p)) (r (list "")))
      (let ((separator-pos
              (slatex.position-char slatex.*path-separator* p)))
        (if separator-pos
          (loop (list-tail p (+fx separator-pos 1))
                (cons (list->string (slatex.sublist p 0 separator-pos))
                      r))
          (slatex.reverse! (cons (list->string p) r)))))))

'(define slatex.path-to-list
   (lambda (p)
     (let loop ((p (string->list p)) (r (list "")))
       (let ((space-pos (slatex.position-char #\space p))
             (colon-pos (slatex.position-char #\: p)))
         (if (and (not space-pos) (not colon-pos))
           (slatex.reverse! (cons (list->string p) r))
           (let ((i (cond ((not space-pos) colon-pos)
                          ((not colon-pos) space-pos)
                          (else (min space-pos colon-pos)))))
             (loop (list-tail p (+fx i 1))
                   (cons (list->string (slatex.sublist p 0 i)) r))))))))

(define slatex.find-some-file
  (lambda (path . files)
    (let loop ((path path))
      (if (null? path)
        #f
        (let ((dir (car path)))
          (let loop2 ((files (if (or (string=? dir "") (string=? dir "."))
                               files
                               (map (lambda (file)
                                      (string-append
                                        dir
                                        slatex.*directory-mark*
                                        file))
                                    files))))
            (if (null? files)
              (loop (cdr path))
              (let ((file (car files)))
                (if (slatex.file-exists? file)
                  file
                  (loop2 (cdr files)))))))))))

(define slatex.file-extension
  (lambda (filename)
    (let ((i (slatex.string-position-right #\. filename)))
      (if i
        (substring filename i (string-length filename))
        #f))))

(define slatex.basename
  (lambda (filename ext)
    (let* ((filename-len (string-length filename))
           (ext-len (string-length ext))
           (len-diff (-fx filename-len ext-len)))
      (cond ((>fx ext-len filename-len) filename)
            ((equal?
               ext
               (substring filename len-diff filename-len))
             (substring filename 0 len-diff))
            (else filename)))))

(define slatex.full-texfile-name
  (lambda (filename)
    (let ((extn (slatex.file-extension filename)))
      (if (and extn
               (or (string=? extn ".sty")
                   (string=? extn ".tex")))
        (slatex.find-some-file
          slatex.*texinputs-list*
          filename)
        (slatex.find-some-file
          slatex.*texinputs-list*
          (string-append filename ".tex")
          filename)))))

(define full-styfile-name
  (lambda (filename)
    (slatex.find-some-file
      slatex.*texinputs-list*
      (string-append filename ".sty"))))

(define full-clsfile-name
  (lambda (filename)
    (slatex.find-some-file
      slatex.*texinputs-list*
      (string-append filename ".cls"))))

(define slatex.full-scmfile-name
  (lambda (filename)
    (apply slatex.find-some-file
           slatex.*texinputs-list*
           filename
           (map (lambda (extn) (string-append filename extn))
                '(".scm" ".ss" ".s")))))

(define slatex.new-aux-file
  (lambda e
    (apply (if slatex.*slatex-in-protected-region?*
             slatex.new-secondary-aux-file
             slatex.new-primary-aux-file)
           e)))

(define slatex.subjobname 'fwd)
(define primary-aux-file-count -1)
(define slatex.new-primary-aux-file
  (lambda e
    (set! primary-aux-file-count
      (+fx primary-aux-file-count 1))
    (apply string-append
           slatex.*file-hider*
           "Z"
           (number->string primary-aux-file-count)
           slatex.subjobname
           e)))

(define slatex.new-secondary-aux-file
  (let ((n -1))
    (lambda e
      (set! n (+fx n 1))
      (apply string-append
             slatex.*file-hider*
             "ZZ"
             (number->string n)
             slatex.subjobname
             e))))

(define slatex.eat-till-newline
  (lambda (in)
    (let loop ()
      (let ((c (read-char in)))
        (cond ((eof-object? c) 'done)
              ((char=? c #\newline) 'done)
              (else (loop)))))))

(define slatex.read-ctrl-seq
  (lambda (in)
    (let ((c (read-char in)))
      (if (eof-object? c)
        (slatex.error 'read-ctrl-exp 1))
      (if (char-alphabetic? c)
        (list->string
          (slatex.reverse!
            (let loop ((s (list c)))
              (let ((c (peek-char in)))
                (cond ((eof-object? c) s)
                      ((char-alphabetic? c)
                       (read-char in)
                       (loop (cons c s)))
                      ((char=? c #\%)
                       (slatex.eat-till-newline in)
                       (loop s))
                      (else s))))))
        (string c)))))

(define slatex.eat-tabspace
  (lambda (in)
    (let loop ()
      (let ((c (peek-char in)))
        (cond ((eof-object? c) 'done)
              ((or (char=? c #\space) (char=? c slatex.*tab*))
               (read-char in)
               (loop))
              (else 'done))))))

(define slatex.eat-whitespace
  (lambda (in)
    (let loop ()
      (let ((c (peek-char in)))
        (cond ((eof-object? c) 'done)
              ((char-whitespace? c) (read-char in) (loop))
              (else 'done))))))

(define slatex.eat-tex-whitespace
  (lambda (in)
    (let loop ()
      (let ((c (peek-char in)))
        (cond ((eof-object? c) 'done)
              ((char-whitespace? c) (read-char in) (loop))
              ((char=? c #\%) (slatex.eat-till-newline in))
              (else 'done))))))

(define slatex.chop-off-whitespace
  (lambda (l)
    (slatex.ormapcdr
      (lambda (d) (if (char-whitespace? (car d)) #f d))
      l)))

(define slatex.read-grouped-latexexp
  (lambda (in)
    (slatex.eat-tex-whitespace in)
    (let ((c (read-char in)))
      (if (eof-object? c)
        (slatex.error 'slatex.read-grouped-latexexp 1))
      (if (char=? c #\{)
        'ok
        (slatex.error 'slatex.read-grouped-latexexp 2))
      (slatex.eat-tex-whitespace in)
      (list->string
        (slatex.reverse!
          (slatex.chop-off-whitespace
            (let loop ((s '()) (nesting 0) (escape? #f))
              (let ((c (read-char in)))
                (if (eof-object? c)
                  (slatex.error 'slatex.read-grouped-latexexp 3))
                (cond (escape? (loop (cons c s) nesting #f))
                      ((char=? c #\\) (loop (cons c s) nesting #t))
                      ((char=? c #\%)
                       (slatex.eat-till-newline in)
                       (loop s nesting #f))
                      ((char=? c #\{)
                       (loop (cons c s) (+fx nesting 1) #f))
                      ((char=? c #\})
                       (if (=fx nesting 0)
                         s
                         (loop (cons c s) (-fx nesting 1) #f)))
                      (else (loop (cons c s) nesting #f)))))))))))

(define slatex.read-filename
  (let ((filename-delims
          (list #\{
                #\}
                #\[
                #\]
                #\(
                #\)
                #\#
                #\%
                #\\
                #\,
                #\space
                slatex.*return*
                #\newline
                slatex.*tab*
                #\\)))
    (lambda (in)
      (slatex.eat-tex-whitespace in)
      (let ((c (peek-char in)))
        (if (eof-object? c)
          (slatex.error 'slatex.read-filename 1))
        (if (char=? c #\{)
          (slatex.read-grouped-latexexp in)
          (list->string
            (slatex.reverse!
              (let loop ((s '()) (escape? #f))
                (let ((c (peek-char in)))
                  (cond ((eof-object? c)
                         (if escape?
                           (slatex.error 'slatex.read-filename 2)
                           s))
                        (escape? (read-char in) (loop (cons c s) #f))
                        ((char=? c #\\)
                         (read-char in)
                         (loop (cons c s) #t))
                        ((memv c filename-delims) s)
                        (else (read-char in) (loop (cons c s) #f))))))))))))

(define slatex.read-schemeid
  (let ((schemeid-delims
          (list #\{
                #\}
                #\[
                #\]
                #\(
                #\)
                #\space
                slatex.*return*
                #\newline
                slatex.*tab*)))
    (lambda (in)
      (slatex.eat-whitespace in)
      (list->string
        (slatex.reverse!
          (let loop ((s '()) (escape? #f))
            (let ((c (peek-char in)))
              (cond ((eof-object? c) s)
                    (escape? (read-char in) (loop (cons c s) #f))
                    ((char=? c #\\)
                     (read-char in)
                     (loop (cons c s) #t))
                    ((memv c schemeid-delims) s)
                    (else (read-char in) (loop (cons c s) #f))))))))))

(define slatex.read-delimed-commaed-filenames
  (lambda (in lft-delim rt-delim)
    (slatex.eat-tex-whitespace in)
    (let ((c (read-char in)))
      (if (eof-object? c)
        (slatex.error
          'slatex.read-delimed-commaed-filenames
          1))
      (if (char=? c lft-delim)
        'ok
        (slatex.error
          'slatex.read-delimed-commaed-filenames
          2))
      (let loop ((s '()))
        (slatex.eat-tex-whitespace in)
        (let ((c (peek-char in)))
          (if (eof-object? c)
            (slatex.error
              'slatex.read-delimed-commaed-filenames
              3))
          (if (char=? c rt-delim)
            (begin (read-char in) (slatex.reverse! s))
            (let ((s (cons (slatex.read-filename in) s)))
              (slatex.eat-tex-whitespace in)
              (let ((c (peek-char in)))
                (if (eof-object? c)
                  (slatex.error
                    'slatex.read-delimed-commaed-filenames
                    4))
                (cond ((char=? c #\,) (read-char in))
                      ((char=? c rt-delim) 'void)
                      (else
                       (slatex.error
                         'slatex.read-delimed-commaed-filenames
                         5)))
                (loop s)))))))))

(define slatex.read-grouped-commaed-filenames
  (lambda (in)
    (slatex.read-delimed-commaed-filenames
      in
      #\{
      #\})))

(define slatex.read-bktd-commaed-filenames
  (lambda (in)
    (slatex.read-delimed-commaed-filenames
      in
      #\[
      #\])))

(define slatex.read-grouped-schemeids
  (lambda (in)
    (slatex.eat-tex-whitespace in)
    (let ((c (read-char in)))
      (if (eof-object? c)
        (slatex.error 'slatex.read-grouped-schemeids 1))
      (if (char=? c #\{)
        'ok
        (slatex.error 'slatex.read-grouped-schemeids 2))
      (let loop ((s '()))
        (slatex.eat-whitespace in)
        (let ((c (peek-char in)))
          (if (eof-object? c)
            (slatex.error 'slatex.read-grouped-schemeids 3))
          (if (char=? c #\})
            (begin (read-char in) (slatex.reverse! s))
            (loop (cons (slatex.read-schemeid in) s))))))))

(define slatex.eat-delimed-text
  (lambda (in lft-delim rt-delim)
    (slatex.eat-tex-whitespace in)
    (let ((c (peek-char in)))
      (if (eof-object? c)
        'exit
        (if (char=? c lft-delim)
          (let loop ()
            (let ((c (read-char in)))
              (if (eof-object? c)
                'exit
                (if (char=? c rt-delim) 'exit (loop))))))))))

(define slatex.eat-bktd-text
  (lambda (in)
    (slatex.eat-delimed-text in #\[ #\])))

(define slatex.eat-grouped-text
  (lambda (in)
    (slatex.eat-delimed-text in #\{ #\})))

(define slatex.disable-slatex-temply
  (lambda (in)
    (set! slatex.*slatex-enabled?* #f)
    (set! slatex.*slatex-reenabler*
      (slatex.read-grouped-latexexp in))))

(define slatex.enable-slatex-again
  (lambda ()
    (set! slatex.*slatex-enabled?* #t)
    (set! slatex.*slatex-reenabler* "UNDEFINED")))

(define slatex.ignore2 (lambda (i ii) 'void))
(define slatex.add-to-slatex-db
  (lambda (in categ)
    (if (memq categ '(keyword constant variable))
      (slatex.add-to-slatex-db-basic in categ)
      (slatex.add-to-slatex-db-special in categ))))

(define slatex.add-to-slatex-db-basic
  (lambda (in categ)
    (let ((setter
            (cond ((eq? categ 'keyword) slatex.set-keyword)
                  ((eq? categ 'constant) slatex.set-constant)
                  ((eq? categ 'variable) slatex.set-variable)
                  (else
                   (slatex.error 'slatex.add-to-slatex-db-basic 1))))
          (ids (slatex.read-grouped-schemeids in)))
      (for-each setter ids))))

(define slatex.add-to-slatex-db-special
  (lambda (in what)
    (let ((ids (slatex.read-grouped-schemeids in)))
      (cond ((eq? what 'unsetspecialsymbol)
             (for-each slatex.unset-special-symbol ids))
            ((eq? what 'setspecialsymbol)
             (if (=fx (length ids) 1)
               'ok
               (slatex.error
                 'slatex.add-to-slatex-db-special
                 'setspecialsymbol-takes-one-arg-only))
             (let ((transl (slatex.read-grouped-latexexp in)))
               (slatex.set-special-symbol (car ids) transl)))
            (else
             (slatex.error 'slatex.add-to-slatex-db-special 2))))))

(define slatex.process-slatex-alias
  (lambda (in what which)
    (let ((triggerer (slatex.read-grouped-latexexp in)))
      (cond ((eq? which 'intext)
             (set! slatex.*intext-triggerers*
               (what triggerer slatex.*intext-triggerers*)))
            ((eq? which 'resultintext)
             (set! slatex.*resultintext-triggerers*
               (what triggerer slatex.*resultintext-triggerers*)))
            ((eq? which 'display)
             (set! slatex.*display-triggerers*
               (what triggerer slatex.*display-triggerers*)))
            ((eq? which 'response)
             (set! slatex.*response-triggerers*
               (what triggerer slatex.*response-triggerers*)))
            ((eq? which 'respbox)
             (set! *respbox-triggerers*
               (what triggerer *respbox-triggerers*)))
            ((eq? which 'box)
             (set! slatex.*box-triggerers*
               (what triggerer slatex.*box-triggerers*)))
            ((eq? which 'input)
             (set! slatex.*input-triggerers*
               (what triggerer slatex.*input-triggerers*)))
            ((eq? which 'region)
             (set! slatex.*region-triggerers*
               (what triggerer slatex.*region-triggerers*)))
            ((eq? which 'mathescape)
             (if (=fx (string-length triggerer) 1)
               'ok
               (slatex.error
                 'slatex.process-slatex-alias
                 'math-escape-should-be-character))
             (set! slatex.*math-triggerers*
               (what (string-ref triggerer 0)
                     slatex.*math-triggerers*)))
            (else
             (slatex.error 'slatex.process-slatex-alias 2))))))

(define slatex.decide-latex-or-tex
  (lambda (latex?)
    (set! slatex.*latex?* latex?)
    (let ((pltexchk.jnk "pltexchk.jnk"))
      (if (slatex.file-exists? pltexchk.jnk)
        (slatex.delete-file pltexchk.jnk))
      (if (not slatex.*latex?*)
        (call-with-output-file
          pltexchk.jnk
          (lambda (outp)
            (display 'junk outp)
            (newline outp)))))))

(define slatex.process-include-only
  (lambda (in)
    (set! slatex.*include-onlys* '())
    (for-each
      (lambda (filename)
        (let ((filename (slatex.full-texfile-name filename)))
          (if filename
            (set! slatex.*include-onlys*
              (slatex.adjoin-string
                filename
                slatex.*include-onlys*)))))
      (slatex.read-grouped-commaed-filenames in))))

(define slatex.process-documentstyle
  (lambda (in)
    (slatex.eat-tex-whitespace in)
    (if (char=? (peek-char in) #\[)
      (for-each
        (lambda (filename)
          (let ((%:g0% slatex.*slatex-in-protected-region?*))
            (set! slatex.*slatex-in-protected-region?* #f)
            (let ((%temp%
                    (begin
                      (slatex.process-tex-file
                        (string-append filename ".sty")))))
              (set! slatex.*slatex-in-protected-region?* %:g0%)
              %temp%)))
        (slatex.read-bktd-commaed-filenames in)))))

(define slatex.process-documentclass
  (lambda (in)
    (slatex.eat-bktd-text in)
    (slatex.eat-grouped-text in)))

(define slatex.process-case-info
  (lambda (in)
    (let ((bool (slatex.read-grouped-latexexp in)))
      (set! slatex.*slatex-case-sensitive?*
        (cond ((string-ci=? bool "true") #t)
              ((string-ci=? bool "false") #f)
              (else
               (slatex.error
                 'slatex.process-case-info
                 'bad-schemecasesensitive-arg)))))))

(define slatex.seen-first-command? #f)
(define slatex.process-main-tex-file
  (lambda (filename)
    (display "SLaTeX v. 2.4")
    (newline)
    (set! primary-aux-file-count -1)
    (set! slatex.*slatex-separate-includes?* #f)
    (if (null? slatex.*texinputs-list*)
      (set! slatex.*texinputs-list*
        (slatex.path-to-list slatex.*texinputs*)))
    (let ((file-hide-file "xZfilhid.tex"))
      (if (slatex.file-exists? file-hide-file)
        (slatex.delete-file file-hide-file))
      (if (memq *op-sys* '(dos os2fat))
        (call-with-output-file
          file-hide-file
          (lambda (out)
            (display "\\def\\filehider{x}" out)
            (newline out)))))
    (display "typesetting code")
    (set! slatex.subjobname
      (slatex.basename filename ".tex"))
    (set! slatex.seen-first-command? #f)
    (slatex.process-tex-file filename)
    (display 'done)
    (newline)))

(define slatex.dump-intext
  (lambda (in out)
    (let* ((display (if out display slatex.ignore2))
           (delim-char
             (begin (slatex.eat-whitespace in) (read-char in)))
           (delim-char
             (cond ((char=? delim-char #\{) #\})
                   (else delim-char))))
      (if (eof-object? delim-char)
        (slatex.error 'slatex.dump-intext 1))
      (let loop ()
        (let ((c (read-char in)))
          (if (eof-object? c)
            (slatex.error 'slatex.dump-intext 2))
          (if (char=? c delim-char)
            'done
            (begin (display c out) (loop))))))))

(define slatex.dump-display
  (lambda (in out ender)
    (slatex.eat-tabspace in)
    (let ((display (if out display slatex.ignore2))
          (ender-lh (string-length ender))
          (c (peek-char in)))
      (if (eof-object? c)
        (slatex.error 'slatex.dump-display 1))
      (if (char=? c #\newline) (read-char in))
      (let loop ((buf ""))
        (let ((c (read-char in)))
          (if (eof-object? c)
            (slatex.error 'slatex.dump-display 2))
          (let ((buf (string-append buf (string c))))
            (if (slatex.string-prefix? buf ender)
              (if (=fx (string-length buf) ender-lh)
                'done
                (loop buf))
              (begin (display buf out) (loop "")))))))))

(define slatex.debug? #f)
(define slatex.process-tex-file
  (lambda (raw-filename)
    (if slatex.debug?
      (begin
        (display "begin ")
        (display raw-filename)
        (newline)))
    (let ((filename
            (slatex.full-texfile-name raw-filename)))
      (if (not filename)
        (begin
          (display "[")
          (display raw-filename)
          (display "]")
          (slatex.force-output (current-output-port)))
        (call-with-input-file
          filename
          (lambda (in)
            (let ((done? #f))
              (let loop ()
                (if done?
                  'exit-loop
                  (begin
                    (let ((c (read-char in)))
                      (cond ((eof-object? c) (set! done? #t))
                            ((char=? c #\%) (slatex.eat-till-newline in))
                            ((char=? c #\\)
                             (let ((cs (slatex.read-ctrl-seq in)))
                               (if slatex.seen-first-command?
                                 'skip
                                 (begin
                                   (set! slatex.seen-first-command? #t)
                                   (slatex.decide-latex-or-tex
                                     (or (string=? cs "documentstyle")
                                         (string=? cs "documentclass")
                                         (string=? cs "NeedsTeXFormat")))))
                               (cond ((not slatex.*slatex-enabled?*)
                                      (if (string=?
                                            cs
                                            slatex.*slatex-reenabler*)
                                        (slatex.enable-slatex-again)))
                                     ((string=? cs "slatexignorecurrentfile")
                                      (set! done? #t))
                                     ((string=? cs "slatexseparateincludes")
                                      (if slatex.*latex?*
                                        (set! slatex.*slatex-separate-includes?*
                                          #t)))
                                     ((string=? cs "slatexdisable")
                                      (slatex.disable-slatex-temply in))
                                     ((string=? cs "begin")
                                      (slatex.eat-tex-whitespace in)
                                      (if (eqv? (peek-char in) #\{)
                                        (let ((cs (slatex.read-grouped-latexexp
                                                    in)))
                                          (cond ((member
                                                   cs
                                                   slatex.*display-triggerers*)
                                                 (slatex.trigger-scheme2tex
                                                   'envdisplay
                                                   in
                                                   cs))
                                                ((member
                                                   cs
                                                   slatex.*response-triggerers*)
                                                 (slatex.trigger-scheme2tex
                                                   'envresponse
                                                   in
                                                   cs))
                                                ((member
                                                   cs
                                                   *respbox-triggerers*)
                                                 (slatex.trigger-scheme2tex
                                                   'envrespbox
                                                   in
                                                   cs))
                                                ((member
                                                   cs
                                                   slatex.*box-triggerers*)
                                                 (slatex.trigger-scheme2tex
                                                   'envbox
                                                   in
                                                   cs))
                                                ((member
                                                   cs
                                                   slatex.*region-triggerers*)
                                                 (slatex.trigger-region
                                                   'envregion
                                                   in
                                                   cs))))))
                                     ((member cs slatex.*intext-triggerers*)
                                      (slatex.trigger-scheme2tex 'intext in #f))
                                     ((member
                                        cs
                                        slatex.*resultintext-triggerers*)
                                      (slatex.trigger-scheme2tex
                                        'resultintext
                                        in
                                        #f))
                                     ((member cs slatex.*display-triggerers*)
                                      (slatex.trigger-scheme2tex
                                        'plaindisplay
                                        in
                                        cs))
                                     ((member cs slatex.*response-triggerers*)
                                      (slatex.trigger-scheme2tex
                                        'plainresponse
                                        in
                                        cs))
                                     ((member cs *respbox-triggerers*)
                                      (slatex.trigger-scheme2tex
                                        'plainrespbox
                                        in
                                        cs))
                                     ((member cs slatex.*box-triggerers*)
                                      (slatex.trigger-scheme2tex
                                        'plainbox
                                        in
                                        cs))
                                     ((member cs slatex.*region-triggerers*)
                                      (slatex.trigger-region
                                        'plainregion
                                        in
                                        cs))
                                     ((member cs slatex.*input-triggerers*)
                                      (slatex.process-scheme-file
                                        (slatex.read-filename in)))
                                     ((string=? cs "input")
                                      (let ((f (slatex.read-filename in)))
                                        (if (not (string=? f ""))
                                          (let ((%:g1% slatex.*slatex-in-protected-region?*))
                                            (set! slatex.*slatex-in-protected-region?*
                                              #f)
                                            (let ((%temp%
                                                    (begin
                                                      (slatex.process-tex-file
                                                        f))))
                                              (set! slatex.*slatex-in-protected-region?*
                                                %:g1%)
                                              %temp%)))))
                                     ((string=? cs "usepackage")
                                      (let ((%:g2% slatex.*slatex-in-protected-region?*))
                                        (set! slatex.*slatex-in-protected-region?*
                                          #f)
                                        (let ((%temp%
                                                (begin
                                                  (slatex.process-tex-file
                                                    (string-append
                                                      (slatex.read-filename in)
                                                      ".sty")))))
                                          (set! slatex.*slatex-in-protected-region?*
                                            %:g2%)
                                          %temp%)))
                                     ((string=? cs "include")
                                      (if slatex.*latex?*
                                        (let ((f (slatex.full-texfile-name
                                                   (slatex.read-filename in))))
                                          (if (and f
                                                   (or (eq? slatex.*include-onlys*
                                                            'all)
                                                       (member
                                                         f
                                                         slatex.*include-onlys*)))
                                            (let ((%:g3% slatex.*slatex-in-protected-region?*))
                                              (set! slatex.*slatex-in-protected-region?*
                                                #f)
                                              (let ((%temp%
                                                      (begin
                                                        (if slatex.*slatex-separate-includes?*
                                                          (let ((%:g4% slatex.subjobname)
                                                                (%:g5% primary-aux-file-count))
                                                            (set! slatex.subjobname
                                                              (slatex.basename
                                                                f
                                                                ".tex"))
                                                            (set! primary-aux-file-count
                                                              -1)
                                                            (let ((%temp%
                                                                    (begin
                                                                      (slatex.process-tex-file
                                                                        f))))
                                                              (set! slatex.subjobname
                                                                %:g4%)
                                                              (set! primary-aux-file-count
                                                                %:g5%)
                                                              %temp%))
                                                          (slatex.process-tex-file
                                                            f)))))
                                                (set! slatex.*slatex-in-protected-region?*
                                                  %:g3%)
                                                %temp%))))))
                                     ((string=? cs "includeonly")
                                      (if slatex.*latex?*
                                        (slatex.process-include-only in)))
                                     ((string=? cs "documentstyle")
                                      (if slatex.*latex?*
                                        (slatex.process-documentstyle in)))
                                     ((string=? cs "documentclass")
                                      (if slatex.*latex?*
                                        (slatex.process-documentclass in)))
                                     ((string=? cs "schemecasesensitive")
                                      (slatex.process-case-info in))
                                     ((string=? cs "defschemetoken")
                                      (slatex.process-slatex-alias
                                        in
                                        slatex.adjoin-string
                                        'intext))
                                     ((string=? cs "undefschemetoken")
                                      (slatex.process-slatex-alias
                                        in
                                        slatex.remove-string!
                                        'intext))
                                     ((string=? cs "defschemeresulttoken")
                                      (slatex.process-slatex-alias
                                        in
                                        slatex.adjoin-string
                                        'resultintext))
                                     ((string=? cs "undefschemeresulttoken")
                                      (slatex.process-slatex-alias
                                        in
                                        slatex.remove-string!
                                        'resultintext))
                                     ((string=? cs "defschemeresponsetoken")
                                      (slatex.process-slatex-alias
                                        in
                                        slatex.adjoin-string
                                        'response))
                                     ((string=? cs "undefschemeresponsetoken")
                                      (slatex.process-slatex-alias
                                        in
                                        slatex.remove-string!
                                        'response))
                                     ((string=? cs "defschemeresponseboxtoken")
                                      (slatex.process-slatex-alias
                                        in
                                        slatex.adjoin-string
                                        'respbox))
                                     ((string=?
                                        cs
                                        "undefschemeresponseboxtoken")
                                      (slatex.process-slatex-alias
                                        in
                                        slatex.remove-string!
                                        'respbox))
                                     ((string=? cs "defschemedisplaytoken")
                                      (slatex.process-slatex-alias
                                        in
                                        slatex.adjoin-string
                                        'display))
                                     ((string=? cs "undefschemedisplaytoken")
                                      (slatex.process-slatex-alias
                                        in
                                        slatex.remove-string!
                                        'display))
                                     ((string=? cs "defschemeboxtoken")
                                      (slatex.process-slatex-alias
                                        in
                                        slatex.adjoin-string
                                        'box))
                                     ((string=? cs "undefschemeboxtoken")
                                      (slatex.process-slatex-alias
                                        in
                                        slatex.remove-string!
                                        'box))
                                     ((string=? cs "defschemeinputtoken")
                                      (slatex.process-slatex-alias
                                        in
                                        slatex.adjoin-string
                                        'input))
                                     ((string=? cs "undefschemeinputtoken")
                                      (slatex.process-slatex-alias
                                        in
                                        slatex.remove-string!
                                        'input))
                                     ((string=? cs "defschemeregiontoken")
                                      (slatex.process-slatex-alias
                                        in
                                        slatex.adjoin-string
                                        'region))
                                     ((string=? cs "undefschemeregiontoken")
                                      (slatex.process-slatex-alias
                                        in
                                        slatex.remove-string!
                                        'region))
                                     ((string=? cs "defschememathescape")
                                      (slatex.process-slatex-alias
                                        in
                                        slatex.adjoin-char
                                        'mathescape))
                                     ((string=? cs "undefschememathescape")
                                      (slatex.process-slatex-alias
                                        in
                                        slatex.remove-char!
                                        'mathescape))
                                     ((string=? cs "setkeyword")
                                      (slatex.add-to-slatex-db in 'keyword))
                                     ((string=? cs "setconstant")
                                      (slatex.add-to-slatex-db in 'constant))
                                     ((string=? cs "setvariable")
                                      (slatex.add-to-slatex-db in 'variable))
                                     ((string=? cs "setspecialsymbol")
                                      (slatex.add-to-slatex-db
                                        in
                                        'setspecialsymbol))
                                     ((string=? cs "unsetspecialsymbol")
                                      (slatex.add-to-slatex-db
                                        in
                                        'unsetspecialsymbol)))))))
                    (loop)))))))))
    (if slatex.debug?
      (begin
        (display "end ")
        (display raw-filename)
        (newline)))))

(define slatex.process-scheme-file
  (lambda (raw-filename)
    (let ((filename
            (slatex.full-scmfile-name raw-filename)))
      (if (not filename)
        (begin
          (display "process-scheme-file: ")
          (display raw-filename)
          (display " doesn't exist")
          (newline))
        (let ((aux.tex (slatex.new-aux-file ".tex")))
          (display ".")
          (slatex.force-output (current-output-port))
          (if (slatex.file-exists? aux.tex)
            (slatex.delete-file aux.tex))
          (call-with-input-file
            filename
            (lambda (in)
              (call-with-output-file
                aux.tex
                (lambda (out)
                  (let ((%:g6% slatex.*intext?*)
                        (%:g7% slatex.*code-env-spec*))
                    (set! slatex.*intext?* #f)
                    (set! slatex.*code-env-spec*
                      "ZZZZschemedisplay")
                    (let ((%temp% (begin (scheme2tex in out))))
                      (set! slatex.*intext?* %:g6%)
                      (set! slatex.*code-env-spec* %:g7%)
                      %temp%))))))
          (if slatex.*slatex-in-protected-region?*
            (set! slatex.*protected-files*
              (cons aux.tex slatex.*protected-files*)))
          (slatex.process-tex-file filename))))))

(define slatex.trigger-scheme2tex
  (lambda (typ in env)
    (let* ((aux (slatex.new-aux-file))
           (aux.scm (string-append aux ".scm"))
           (aux.tex (string-append aux ".tex")))
      (if (slatex.file-exists? aux.scm)
        (slatex.delete-file aux.scm))
      (if (slatex.file-exists? aux.tex)
        (slatex.delete-file aux.tex))
      (display ".")
      (slatex.force-output (current-output-port))
      (call-with-output-file
        aux.scm
        (lambda (out)
          (cond ((memq typ '(intext resultintext))
                 (slatex.dump-intext in out))
                ((memq typ
                       '(envdisplay envresponse envrespbox envbox))
                 (slatex.dump-display
                   in
                   out
                   (string-append "\\end{" env "}")))
                ((memq typ
                       '(plaindisplay
                          plainresponse
                          plainrespbox
                          plainbox))
                 (slatex.dump-display
                   in
                   out
                   (string-append "\\end" env)))
                (else
                 (slatex.error 'slatex.trigger-scheme2tex 1)))))
      (call-with-input-file
        aux.scm
        (lambda (in)
          (call-with-output-file
            aux.tex
            (lambda (out)
              (let ((%:g8% slatex.*intext?*)
                    (%:g9% slatex.*code-env-spec*))
                (set! slatex.*intext?*
                  (memq typ '(intext resultintext)))
                (set! slatex.*code-env-spec*
                  (cond ((eq? typ 'intext) "ZZZZschemecodeintext")
                        ((eq? typ 'resultintext)
                         "ZZZZschemeresultintext")
                        ((memq typ '(envdisplay plaindisplay))
                         "ZZZZschemedisplay")
                        ((memq typ '(envresponse plainresponse))
                         "ZZZZschemeresponse")
                        ((memq typ '(envrespbox plainrespbox))
                         "ZZZZschemeresponsebox")
                        ((memq typ '(envbox plainbox)) "ZZZZschemebox")
                        (else
                         (slatex.error 'slatex.trigger-scheme2tex 2))))
                (let ((%temp% (begin (scheme2tex in out))))
                  (set! slatex.*intext?* %:g8%)
                  (set! slatex.*code-env-spec* %:g9%)
                  %temp%))))))
      (if slatex.*slatex-in-protected-region?*
        (set! slatex.*protected-files*
          (cons aux.tex slatex.*protected-files*)))
      (if (memq typ
                '(envdisplay plaindisplay envbox plainbox))
        (slatex.process-tex-file aux.tex))
      (slatex.delete-file aux.scm))))

(define slatex.trigger-region
  (lambda (typ in env)
    (let ((aux.tex (slatex.new-primary-aux-file ".tex"))
          (aux2.tex
            (slatex.new-secondary-aux-file ".tex")))
      (if (slatex.file-exists? aux2.tex)
        (slatex.delete-file aux2.tex))
      (if (slatex.file-exists? aux.tex)
        (slatex.delete-file aux.tex))
      (display ".")
      (slatex.force-output (current-output-port))
      (let ((%:g10% slatex.*slatex-in-protected-region?*)
            (%:g11% slatex.*protected-files*))
        (set! slatex.*slatex-in-protected-region?* #t)
        (set! slatex.*protected-files* '())
        (let ((%temp%
                (begin
                  (call-with-output-file
                    aux2.tex
                    (lambda (out)
                      (cond ((eq? typ 'envregion)
                             (slatex.dump-display
                               in
                               out
                               (string-append "\\end{" env "}")))
                            ((eq? typ 'plainregion)
                             (slatex.dump-display
                               in
                               out
                               (string-append "\\end" env)))
                            (else (slatex.error 'slatex.trigger-region 1)))))
                  (slatex.process-tex-file aux2.tex)
                  (set! slatex.*protected-files*
                    (slatex.reverse! slatex.*protected-files*))
                  (call-with-input-file
                    aux2.tex
                    (lambda (in)
                      (call-with-output-file
                        aux.tex
                        (lambda (out)
                          (slatex.inline-protected-files in out)))))
                  (slatex.delete-file aux2.tex))))
          (set! slatex.*slatex-in-protected-region?*
            %:g10%)
          (set! slatex.*protected-files* %:g11%)
          %temp%)))))

(define slatex.inline-protected-files
  (lambda (in out)
    (let ((done? #f))
      (let loop ()
        (if done?
          'exit-loop
          (begin
            (let ((c (read-char in)))
              (cond ((eof-object? c) (set! done? #t))
                    ((or (char=? c slatex.*return*)
                         (char=? c #\newline))
                     (let ((c2 (peek-char in)))
                       (if (not (eof-object? c2)) (display c out))))
                    ((char=? c #\%) (slatex.eat-till-newline in))
                    ((char=? c #\\)
                     (let ((cs (slatex.read-ctrl-seq in)))
                       (cond ((string=? cs "begin")
                              (let ((cs (slatex.read-grouped-latexexp in)))
                                (cond ((member cs slatex.*display-triggerers*)
                                       (slatex.inline-protected
                                         'envdisplay
                                         in
                                         out
                                         cs))
                                      ((member cs slatex.*response-triggerers*)
                                       (slatex.inline-protected
                                         'envresponse
                                         in
                                         out
                                         cs))
                                      ((member cs *respbox-triggerers*)
                                       (slatex.inline-protected
                                         'envrespbox
                                         in
                                         out
                                         cs))
                                      ((member cs slatex.*box-triggerers*)
                                       (slatex.inline-protected
                                         'envbox
                                         in
                                         out
                                         cs))
                                      ((member cs slatex.*region-triggerers*)
                                       (slatex.inline-protected
                                         'envregion
                                         in
                                         out
                                         cs))
                                      (else
                                       (display "\\begin{" out)
                                       (display cs out)
                                       (display "}" out)))))
                             ((member cs slatex.*intext-triggerers*)
                              (slatex.inline-protected 'intext in out #f))
                             ((member cs slatex.*resultintext-triggerers*)
                              (slatex.inline-protected 'resultintext in out #f))
                             ((member cs slatex.*display-triggerers*)
                              (slatex.inline-protected 'plaindisplay in out cs))
                             ((member cs slatex.*response-triggerers*)
                              (slatex.inline-protected
                                'plainresponse
                                in
                                out
                                cs))
                             ((member cs *respbox-triggerers*)
                              (slatex.inline-protected 'plainrespbox in out cs))
                             ((member cs slatex.*box-triggerers*)
                              (slatex.inline-protected 'plainbox in out cs))
                             ((member cs slatex.*region-triggerers*)
                              (slatex.inline-protected 'plainregion in out cs))
                             ((member cs slatex.*input-triggerers*)
                              (slatex.inline-protected 'input in out cs))
                             (else (display "\\" out) (display cs out)))))
                    (else (display c out))))
            (loop)))))))

(define slatex.inline-protected
  (lambda (typ in out env)
    (cond ((eq? typ 'envregion)
           (display "\\begin{" out)
           (display env out)
           (display "}" out)
           (slatex.dump-display
             in
             out
             (string-append "\\end{" env "}"))
           (display "\\end{" out)
           (display env out)
           (display "}" out))
          ((eq? typ 'plainregion)
           (display "\\" out)
           (display env out)
           (slatex.dump-display
             in
             out
             (string-append "\\end" env))
           (display "\\end" out)
           (display env out))
          (else
           (let ((f (car slatex.*protected-files*)))
             (set! slatex.*protected-files*
               (cdr slatex.*protected-files*))
             (call-with-input-file
               f
               (lambda (in)
                 (slatex.inline-protected-files in out)))
             (slatex.delete-file f))
           (cond ((memq typ '(intext resultintext))
                  (display "{}" out)
                  (slatex.dump-intext in #f))
                 ((memq typ '(envrespbox envbox))
                  (if (not slatex.*latex?*) (display "{}" out))
                  (slatex.dump-display
                    in
                    #f
                    (string-append "\\end{" env "}")))
                 ((memq typ '(plainrespbox plainbox))
                  (display "{}" out)
                  (slatex.dump-display
                    in
                    #f
                    (string-append "\\end" env)))
                 ((memq typ '(envdisplay envresponse))
                  (slatex.dump-display
                    in
                    #f
                    (string-append "\\end{" env "}")))
                 ((memq typ '(plaindisplay plainresponse))
                  (slatex.dump-display
                    in
                    #f
                    (string-append "\\end" env)))
                 ((eq? typ 'input) (slatex.read-filename in))
                 (else (slatex.error 'slatex.inline-protected 1)))))))


(define (bench)
   (slatex.process-main-tex-file "test"))
  
(define (run num)
   (let ((res (repeat num bench)))
      (display res)
      (newline)))

(define (repeat num f)
   (if (<= num 0)
       (f)
       (begin
          (with-output-to-file "/dev/null" f)
          (repeat (- num 1) f))))

(define (main.1 argv)
   (run 5))


(define (bitwise-and x y)
   (bit-and x y))
(define (bitwise-or x y)
   (bit-or x y))
(define (bitwise-not x)
   (bit-not x))

(define-macro (receive% variables producer . body)
   (define gensym (let ((n 0))
		     (lambda ()
			(set! n (+ 1 n))
			(string->symbol (string-append "g" (number->string n))))))
   (let ((values (gensym)))
      `(let ((,values ,producer))
          ,(let loop ((variables variables)
                      (off       0)
                      (bindings  '()))
              (if (null? variables)
                  `(let ,bindings ,@body)
                  (loop (cdr variables)
                        (+ off 1)
                        (cons `(,(car variables) (vector-ref ,values ,off))
                              bindings)))))))

(define-macro (values . values)
   `(vector ,@values))

;*---------------------------------------------------------------------*/
;*    defrec.scm                                                       */
;*---------------------------------------------------------------------*/
;;; Copyright (c) 1993 by Olin Shivers.

;;; Syntax for defining record types.
;;; This implementation works with the Scheme48 system --
;;; or any Scheme that uses Clinger's "explicit renaming"
;;; macro system.
;;;
;;; (define-record name . field-specs)
;;;
;;; A field-spec is one of the following:
;;;     field		; Initialised field
;;;     (field [default])	; Defaulted field.
;;; An initialised field has its initial value passed as an argument to
;;; the the record maker procedure. A defaulted field takes its value from
;;; the the DEFAULT expression. If a DEFAULT expression is not given, then
;;; the defaulted field's initial value is undefined.
;;; 
;;; Example:
;;; (define-record employee
;;;     name
;;;     id
;;;     (salary 10000)
;;;     (department)	; Initial value undefined.
;;;     sex
;;;     married?)
;;; 
;;; Defines the following:
;;; - A maker procedure:
;;;   (make-employee "John Smith" 742931 'male #f)
;;;   MAKE-EMPLOYEE takes one argument for each initialised field.
;;; 
;;; - Accessor procedures:
;;;   (employee:name emp)
;;;   (employee:id-number emp)
;;;   (employee:salary emp)
;;;   (employee:department emp)
;;;   (employee:sex emp)
;;;   (employee:married? emp)
;;; 
;;; - Setter procedures:
;;;   (set-employee:name emp)
;;;   (set-employee:id-number emp)
;;;   (set-employee:salary emp 20000)
;;;   (set-employee:department emp "Vaporware")
;;;   (set-employee:sex emp 'female)
;;;   (set-employee:married? emp #t)
;;; 
;;; - A type predicate:
;;;   (employee? x)
;;; 
;;; - The record type descriptor:
;;;     type/employee

(define-macro (define-record name . field)
   (define (symbol-append . s)
      (string->symbol (apply string-append (map symbol->string s))))
   (define gensym (let ((n 0))
		     (lambda ()
			(set! n (+ 1 n))
			(string->symbol (string-append "g" (number->string n))))))
   (let* ((alloc-name (symbol-append 'alloc- name))
	  (pred-name  (symbol-append name '?))
	  (make-name  (symbol-append 'make- name))
	  (init-field (let loop ((field  field)
				 (ifield '()))
			 (cond
			    ((null? field)
			     (reverse ifield))
			    ((pair? (car field))
			     (loop (cdr field) ifield))
			    (else
			     (loop (cdr field)
				   (cons (car field) ifield))))))
	  (default-field (let loop ((field  field)
				    (dfield '()))
			    (cond
			       ((null? field)
				(reverse dfield))
			       ((not (pair? (car field)))
				(loop (cdr field) dfield))
			       (else
				(loop (cdr field)
				      (cons (car field) dfield))))))
	  (len        (length field))
	  (alloc      `(define (,alloc-name)
			  (make-vector ,(+ len 1) 'unspecified)))
	  (pred       `(define (,pred-name x)
			  (and (vector? x)
			       (=fx (vector-length x) ,(+ len 1))
			       (eq? (vector-ref x 0) ',name))))
	  (make       (let ((v (gensym)))
			 `(define (,make-name ,@init-field)
			     (let ((,v (,alloc-name)))
				(vector-set! ,v 0 ',name)
				,@(let loop ((field  field)
					     (ifield init-field)
					     (init   '())
					     (off    1))
				     (cond
					((null? field)
					 (reverse (cons v init)))
					((pair? (car field))
					 (loop (cdr field)
					       ifield
					       (cons `(vector-set!
						       ,v
						       ,off
						       ,(cadr (car field)))
						     init)
					       (+ off 1)))
					(else
					 (loop (cdr field)
					       (cdr ifield)
					       (cons `(vector-set!
						       ,v
						       ,off
						       ,(car field))
						     init)
					       (+ off 1)))))))))
	  (set-get   (let loop ((field field)
				(off   1)
				(set   '()))
			(if (null? field)
			    (reverse set)
			    (let* ((fname (if (pair? (car field))
					      (car (car field))
					      (car field)))
				   (set-name (symbol-append 'set-
							    name
							    ':
							    fname))
				   (ref-name (symbol-append name
							    ':
							    fname))
				   (a-set `(define-macro (,set-name o v)
					    `(vector-set! ,o ,,off ,v)))
				   (a-ref `(define-macro (,ref-name o)
					    `(vector-ref ,o ,,off))))
			       (loop (cdr field)
				     (+ off 1)
				     (cons a-set (cons a-ref set))))))))
      `(begin ,alloc ,pred ,make ,@set-get)))
   
(define-record harr
  nrows
  ncols
  elts)

(define-record wall
  owner		; Box that owns this wall.
  neighbor	; The other box bordering this wall.
  bit)		; Integer -- a bit identifying this wall in OWNER's box.

(define-record box
  reachable	; Union/find set -- all reachable boxs.
  id		; Identifying info (e.g., the coords of the box).
  (walls -1)	; A bitset telling which walls are still standing.
  (parent #f)	; For DFS spanning tree construction.
  (mark #f))    ; For marking the solution path.

;*---------------------------------------------------------------------*/
;*    harr.scm                                                         */
;*---------------------------------------------------------------------*/
;;; Hex arrays
;;; Copyright (c) 1995 by Olin Shivers.

;;; External dependencies:
;;; - define-record

;;;        ___       ___       ___
;;;       /   \     /   \     /   \
;;;   ___/  A  \___/  A  \___/  A  \___
;;;  /   \     /   \     /   \     /   \
;;; /  A  \___/  A  \___/  A  \___/  A  \
;;; \     /   \     /   \     /   \     /
;;;  \___/     \___/     \___/     \___/
;;;  /   \     /   \     /   \     /   \
;;; /     \___/     \___/     \___/     \
;;; \     /   \     /   \     /   \     /
;;;  \___/     \___/     \___/     \___/
;;;  /   \     /   \     /   \     /   \
;;; /     \___/     \___/     \___/     \
;;; \     /   \     /   \     /   \     /
;;;  \___/     \___/     \___/     \___/

;;; Hex arrays are indexed by the (x,y) coord of the center of the hexagonal
;;; element. Hexes are three wide and two high; e.g., to get from the center
;;; of an elt to its {NW, N, NE} neighbors, add {(-3,1), (0,2), (3,1)}
;;; respectively.
;;;
;;; Hex arrays are represented with a matrix, essentially made by shoving the
;;; odd columns down a half-box so things line up. The mapping is as follows:
;;;     Center coord      row/column
;;;     ------------      ----------
;;;     (x,  y)        -> (y/2, x/3)
;;;     (3c, 2r + c&1) <- (r,   c)




(define (harr r c)
  (make-harr r c (make-vector (*fx r c))))



(define (href ha x y)
  (let ((r (/fx y 2))
	(c (/fx x 3)))
    (vector-ref (harr:elts ha)
		(+fx (*fx (harr:ncols ha) r) c))))

(define (hset! ha x y val)
  (let ((r (/fx y 2))
	(c (/fx x 3)))
    (vector-set! (harr:elts ha)
		 (+fx (*fx (harr:ncols ha) r) c)
		 val)))

(define (href/rc ha r c)
    (vector-ref (harr:elts ha)
		(+fx (*fx (harr:ncols ha) r) c)))

;;; Create a nrows x ncols hex array. The elt centered on coord (x, y)
;;; is the value returned by (PROC x y).

(define (harr-tabulate nrows ncols proc)
  (let ((v (make-vector (*fx nrows ncols))))

    (do ((r (-fx nrows 1) (-fx r 1)))
	((<fx r 0))
      (do ((c 0 (+fx c 1))
	   (i (*fx r ncols) (+fx i 1)))
	  ((=fx c ncols))
	(vector-set! v i (proc (*fx 3 c) (+fx (*fx 2 r) (bitwise-and c 1))))))

    (make-harr nrows ncols v)))


(define (harr-for-each proc harr)
  (vector-for-each proc (harr:elts harr)))

;*---------------------------------------------------------------------*/
;*    hex.scm                                                          */
;*---------------------------------------------------------------------*/
;;; Hexagonal hackery for maze generation.
;;; Copyright (c) 1995 by Olin Shivers.

;;; Every elt of the hex array manages his SW, S, and SE wall.
;;; Terminology: - An even column is one whose column index is even. That
;;;                means the first, third, ... columns (indices 0, 2, ...).
;;;              - An odd column is one whose column index is odd. That
;;;                means the second, fourth... columns (indices 1, 3, ...).
;;;              The even/odd flip-flop is confusing; be careful to keep it
;;;              straight. The *even* columns are the low ones. The *odd*
;;;              columns are the high ones.
;;;    _   _
;;;  _/ \_/ \
;;; / \_/ \_/
;;; \_/ \_/ \
;;; / \_/ \_/
;;; \_/ \_/ \
;;; / \_/ \_/
;;; \_/ \_/ \
;;; / \_/ \_/
;;; \_/ \_/
;;;  0 1 2 3


(define south-west 1)
(define south      2)
(define south-east 4)

(define (gen-maze-array r c)
  (harr-tabulate r c (lambda (x y) (my-make-box (base-set 1) (cons x y)))))

;;; This could be made more efficient.
(define (make-wall-vec harr)
  (let* ((nrows (harr:nrows harr))
	 (ncols (harr:ncols harr))
	 (xmax (*fx 3 (-fx ncols 1)))

	 ;; Accumulate walls.
	 (walls '())
	 (add-wall (lambda (o n b) ; owner neighbor bit
		     (set! walls (cons (make-wall o n b) walls)))))
	
    ;; Do everything but the bottom row.
    (do ((x (*fx (-fx ncols 1) 3) (-fx x 3)))
	((<fx x 0))
      (do ((y (+fx (*fx (-fx nrows 1) 2) (bitwise-and x 1))
	      (-fx y 2)))
	  ((<=fx y 1))	; Don't do bottom row.
	  (let ((hex (href harr x y)))
	    (if (not (zerofx? x))
		(add-wall hex (href harr (-fx x 3) (-fx y 1)) south-west))
	    (add-wall hex (href harr x (-fx y 2)) south)
	    (if (<fx x xmax)
		(add-wall hex (href harr (+fx x 3) (-fx y 1)) south-east)))))

    ;; Do the SE and SW walls of the odd columns on the bottom row.
    ;; If the rightmost bottom hex lies in an odd column, however,
    ;; don't add it's SE wall -- it's a corner hex, and has no SE neighbor.
    (if (>fx ncols 1)
	(let ((rmoc-x (+fx 3 (*fx 6 (/fx (-fx ncols 2) 2)))))
	  ;; Do rightmost odd col.
	  (let ((rmoc-hex (href harr rmoc-x 1)))
	    (if (<fx rmoc-x xmax) ; Not  a corner -- do E wall.
		(add-wall rmoc-hex (href harr xmax 0) south-east))
	    (add-wall rmoc-hex (href harr (-fx rmoc-x 3) 0) south-west))

	  (do ((x (-fx rmoc-x 6) ; Do the rest of the bottom row's odd cols.
		  (-fx x 6)))
	      ((<fx x 3))	; 3 is X coord of leftmost odd column.
	    (add-wall (href harr x 1) (href harr (-fx x 3) 0) south-west)
	    (add-wall (href harr x 1) (href harr (+fx x 3) 0) south-east))))

    (list->vector walls)))


;;; Find the box ctop from the top row, and the box cbot from the bottom
;;; row such that cbot is furthest from ctop. 
;;; Return [ctop-x, ctop-y, cbot-x, cbot-y].

(define (pick-entrances harr)
  (dfs-maze harr (href/rc harr 0 0) for-each-hex-child)
  (let ((nrows (harr:nrows harr))
	(ncols (harr:ncols harr)))
    (let tp-lp ((max-len -1)
		(entrance #f)
		(exit #f)
		(tcol (-fx ncols 1)))
      (if (<fx tcol 0) (values entrance exit)
	  (let ((top-box (href/rc harr (-fx nrows 1) tcol)))
	    (reroot-maze top-box)
	    (receive% (max-len entrance exit)
		(let bt-lp ((max-len max-len)
			    (entrance entrance)
			    (exit exit)
			    (bcol (-fx ncols 1)))
		  (if (<fx bcol 0) (values max-len entrance exit)
		      (let ((this-len (path-length (href/rc harr 0 bcol))))
			(if (>fx this-len max-len)
			    (bt-lp this-len tcol bcol (-fx bcol 1))
			    (bt-lp max-len  entrance exit (-fx bcol 1))))))
	      (tp-lp max-len entrance exit (-fx tcol 1))))))))
		


;;; Apply PROC to each node reachable from BOX.
(define (for-each-hex-child proc harr box)
  (let* ((walls (box:walls box))
	 (id (box:id box))
	 (x (car id))
	 (y (cdr id))
	 (nr (harr:nrows harr))
	 (nc (harr:ncols harr))
	 (maxy (*fx 2 (-fx nr 1)))
	 (maxx (*fx 3 (-fx nc 1))))
    (if (not (bit-test walls south-west)) (proc (href harr (-fx x 3) (-fx y 1))))
    (if (not (bit-test walls south))      (proc (href harr x       (-fx y 2))))
    (if (not (bit-test walls south-east)) (proc (href harr (+fx x 3) (-fx y 1))))

    ;; NW neighbor, if there is one (we may be in col 1, or top row/odd col)
    (if (and (>fx x 0)	; Not in first column.
	     (or (<=fx y maxy)		; Not on top row or
		 (zerofx? (modulo x 6))))	; not in an odd column.
	(let ((nw (href harr (-fx x 3) (+fx y 1))))
	  (if (not (bit-test (box:walls nw) south-east)) (proc nw))))

    ;; N neighbor, if there is one (we may be on top row).
    (if (<fx y maxy)		; Not on top row
	(let ((n (href harr x (+fx y 2))))
	  (if (not (bit-test (box:walls n) south)) (proc n))))

    ;; NE neighbor, if there is one (we may be in last col, or top row/odd col)
    (if (and (<fx x maxx)	; Not in last column.
	     (or (<=fx y maxy)		; Not on top row or
		 (zerofx? (modulo x 6))))	; not in an odd column.
	(let ((ne (href harr (+fx x 3) (+fx y 1))))
	  (if (not (bit-test (box:walls ne) south-west)) (proc ne))))))



;;; The top-level
(define (make-maze nrows ncols)
   (let* ((boxs (gen-maze-array nrows ncols))
	  (walls (permute-vec! (make-wall-vec boxs) (random-state 20))))
      (dig-maze walls (*fx nrows ncols))
      (receive% (entrance exit) (pick-entrances boxs)
	       (let* ((exit-box (href/rc boxs 0 exit))
		      (walls (box:walls exit-box)))
		  (reroot-maze (href/rc boxs (-fx nrows 1) entrance))
		  (mark-path exit-box)
		  (set-box:walls exit-box (bitwise-and walls (bitwise-not south)))
		  (values boxs entrance exit)))))


(define (pmaze nrows ncols)
  (receive% (boxs entrance exit) (make-maze nrows ncols)
    (print-hexmaze boxs entrance)))


;*---------------------------------------------------------------------*/
;*    hexprint.scm                                                     */
;*---------------------------------------------------------------------*/
;;; Print out a hex array with characters.
;;; Copyright (c) 1995 by Olin Shivers.

;;; External dependencies:
;;; - hex array code
;;; - hex box code

;;;    _   _
;;;  _/ \_/ \
;;; / \_/ \_/
;;; \_/ \_/ \
;;; / \_/ \_/
;;; \_/ \_/ \
;;; / \_/ \_/
;;; \_/ \_/ \
;;; / \_/ \_/
;;; \_/ \_/ 

;;; Top part of top row looks like this:
;;;    _   _  _   _
;;;  _/ \_/ \/ \_/ \
;;; /        

(define (print-hexmaze harr entrance)
  (let* ((nrows  (harr:nrows harr))
	 (ncols  (harr:ncols harr))
	 (ncols2 (*fx 2 (/fx ncols 2))))

    ;; Print out the flat tops for the top row's odd cols.
    (do ((c 1 (+fx c 2)))
	((>=fx c ncols))
      (display "   ")
      (write-char (if (=fx c entrance) #\space #\_)))
    (newline)

    ;; Print out the slanted tops for the top row's odd cols
    ;; and the flat tops for the top row's even cols.
    (write-char #\space)
    (do ((c 0 (+fx c 2)))
	((>=fx c ncols2))
      (display* (if (=fx c entrance) #\space #\_)
	      "/"
	      (dot/space harr (-fx nrows 1) (+fx c 1))
	      "\\"))
    (if (odd? ncols)
	(write-char (if (=fx entrance (-fx ncols 1)) #\space #\_)))
    (newline)

    (do ((r (-fx nrows 1) (-fx r 1)))
	((<fx r 0))

      ;; Do the bottoms for row r's odd cols.
      (write-char #\/)
      (do ((c 1 (+fx c 2)))
	  ((>=fx c ncols2))
	;; The dot/space for the even col just behind c.
	(write-char (dot/space harr r (-fx c 1)))
	(display-hexbottom (box:walls (href/rc harr r c))))	

      (cond ((odd? ncols)
	     (write-char (dot/space harr r (-fx ncols 1)))
	     (write-char #\\)))
      (newline)

      ;; Do the bottoms for row r's even cols.
      (do ((c 0 (+fx c 2)))
	  ((>=fx c ncols2))
	(display-hexbottom (box:walls (href/rc harr r c)))
	;; The dot/space is for the odd col just after c, on row below.
	(write-char (dot/space harr (-fx r 1) (+fx c 1))))
      
      (cond ((odd? ncols)
	     (display-hexbottom (box:walls (href/rc harr r (-fx ncols 1)))))
	    ((not (zerofx? r)) (write-char #\\)))
      (newline))))

(define (bit-test j bit)
  (not (zerofx? (bitwise-and j bit))))

;;; Return a . if harr[r,c] is marked, otherwise a space.
;;; We use the dot to mark the solution path.
(define (dot/space harr r c)
  (if (and (>=fx r 0) (box:mark (href/rc harr r c))) #\. #\space))

;;; Print a \_/ hex bottom.
(define (display-hexbottom hexwalls)
  (write-char (if (bit-test hexwalls south-west) #\\ #\space))
  (write-char (if (bit-test hexwalls south     ) #\_ #\space))
  (write-char (if (bit-test hexwalls south-east) #\/ #\space)))

;;;    _   _
;;;  _/ \_/ \
;;; / \_/ \_/
;;; \_/ \_/ \_/
;;; / \_/ \_/
;;; \_/ \_/ \
;;; / \_/ \_/
;;; \_/ \_/ \
;;; / \_/ \_/
;;; \_/ \_/ \_/


;*---------------------------------------------------------------------*/
;*    maze.scm                                                         */
;*---------------------------------------------------------------------*/
;;; Building mazes with union/find disjoint sets.
;;; Copyright (c) 1995 by Olin Shivers.

;;; This is the algorithmic core of the maze constructor.
;;; External dependencies:
;;; - RANDOM-INT
;;; - Union/find code
;;; - bitwise logical functions

(define (my-make-box r i )
   (let ((x (make-box r i)))
      (if (not (eq? (box:parent x) #f))
	  (error "my-make-box" "Not #f parent" x)
	  x)))

;;; Iterates in reverse order.

(define (vector-for-each proc v)
  (let lp ((i (-fx (vector-length v) 1)))
    (cond ((>=fx i 0)
	   (proc (vector-ref v i))
	   (lp (-fx i 1))))))


;;; Randomly permute a vector.

(define (permute-vec! v random-state)
  (let lp ((i (-fx (vector-length v) 1)))
    (cond ((>fx i 1)
	   (let ((elt-i (vector-ref v i))
		 (j (random-int i random-state)))	; j in [0,i)
	     (vector-set! v i (vector-ref v j))
	     (vector-set! v j elt-i))
	   (lp (-fx i 1)))))
  v)


;;; This is the core of the algorithm.

(define (dig-maze walls nboxs)
  (bind-exit (quit)
    (begin
      (vector-for-each
       (lambda (wall)			; For each wall,
	 (let* ((c1   (wall:owner wall)) ; find the boxs on
		(set1 (box:reachable c1))

		(c2   (wall:neighbor wall)) ; each side of the wall
		(set2 (box:reachable c2)))

	   ;; If there is no path from c1 to c2, knock down the
	   ;; wall and union the two sets of reachable boxs.
	   ;; If the new set of reachable boxs is the whole set
	   ;; of boxs, quit.
	   (if (not (set-equal? set1 set2))
	       (let ((walls (box:walls c1))	
		     (wall-mask (bitwise-not (wall:bit wall))))
		 (union! set1 set2)
		 (set-box:walls c1 (bitwise-and walls wall-mask))
		 (if (=fx (set-size set1) nboxs) (quit #f))))))
       walls))))


;;; Some simple DFS routines useful for determining path length 
;;; through the maze.

;;; Build a DFS tree from ROOT. 
;;; (DO-CHILDREN proc maze node) applies PROC to each of NODE's children.
;;; We assume there are no loops in the maze; if this is incorrect, the
;;; algorithm will diverge.

(define (dfs-maze maze root do-children)
   (let search ((node root) (parent #f))
      (set-box:parent node parent)
      (do-children (lambda (child)
		      (if (not (eq? child parent))
			  (search child node)))
		   maze node)))

;;; Move the root to NEW-ROOT.

(define (reroot-maze new-root)
   0 1 2 3 4 5 65 67
   0 1 2 3 4 5 65 67
   0 1 2 3 4 5 65 67
   0 1 2 3 4 5 65 67
   0 1 2 3 4 5 65 67
   0 1 2 3 4 5 65 67
   0 1 2 3 4 5 65 67
   0 1 2 3 4 5 65 67
   (let lp ((node new-root) (new-parent #f))
      (let ((old-parent (box:parent node)))
	 (set-box:parent node new-parent)
	 (if old-parent (lp old-parent node)))))

;;; How far from BOX to the root?

(define (path-length box)
  (do ((len 0 (+fx len 1))
       (node (box:parent box) (box:parent node)))
      ((not node) len)))

;;; Mark the nodes from NODE back to root. Used to mark the winning path.

(define (mark-path node)
  (let lp ((node node))
    (set-box:mark node #t)
    (cond ((box:parent node) => lp))))

;*---------------------------------------------------------------------*/
;*    rand.scm                                                         */
;*---------------------------------------------------------------------*/
; Minimal Standard Random Number Generator
; Park & Miller, CACM 31(10), Oct 1988, 32 bit integer version.
; better constants, as proposed by Park.
; By Ozan Yigit

;;; Rehacked by Olin 4/1995.

(define (random-state n)
  (cons n #f))

(define (rand state)
  (let ((seed (car state))
	(A 48271)
	(M 268435455) ;; 2147483647
	(Q 44488)
	(R 3399))
    (let* ((hi (/fx seed Q))
	   (lo (modulo seed Q))
	   (test (-fx (*fx A lo) (*fx R hi)))
	   (val (if (>fx test 0) test (+fx test M))))
      (set-car! state val)
      val)))

(define (random-int n state)
  (modulo (rand state) n))

; poker test
; seed 1
; cards 0-9 inclusive (random 10)
; five cards per hand
; 10000 hands
;
; Poker Hand     Example    Probability  Calculated
; 5 of a kind    (aaaaa)      0.0001      0
; 4 of a kind    (aaaab)      0.0045      0.0053
; Full house     (aaabb)      0.009       0.0093
; 3 of a kind    (aaabc)      0.072       0.0682
; two pairs      (aabbc)      0.108       0.1104
; Pair           (aabcd)      0.504       0.501
; Bust           (abcde)      0.3024      0.3058

; (define (random n)
;   (let* ((M 2147483647)
; 	 (slop (modulo M n)))
;     (let loop ((r (rand)))
;       (if (>fx r slop)
; 	  (modulo r n)	
; 	  (loop (rand))))))
; 
; (define (rngtest)
;   (display "implementation ")
;   (srand 1)
;   (let loop ((n 0))
;     (if (<fx n 10000)
;         (begin
;          (rand)
;          (loop (1+ n)))))
;   (if (=fx *seed* 399268537)
;       (display "looks correct.")
;       (begin
;        (display "failed.")
;        (newline)
;        (display "   current seed ") (display *seed*)
;        (newline)
;        (display "   correct seed 399268537")))
;   (newline))

;*---------------------------------------------------------------------*/
;*    uf.scm                                                           */
;*---------------------------------------------------------------------*/
;;; Tarjan's amortised union-find data structure.
;;; Copyright (c) 1995 by Olin Shivers.

;;; This data structure implements disjoint sets of elements.
;;; Four operations are supported. The implementation is extremely
;;; fast -- any sequence of N operations can be performed in time
;;; so close to linear it's laughable how close it is. See your
;;; intro data structures book for more. The operations are:
;;;
;;; - (base-set nelts) -> set
;;;   Returns a new set, of size NELTS.
;;;
;;; - (set-size s) -> integer
;;;   Returns the number of elements in set S.
;;;
;;; - (union! set1 set2)
;;;   Unions the two sets -- SET1 and SET2 are now considered the same set
;;;   by SET-EQUAL?.
;;;
;;; - (set-equal? set1 set2)
;;;   Returns true <==> the two sets are the same.

;;; Representation: a set is a cons box. Every set has a "representative"
;;; cons box, reached by chasing cdr links until we find the cons with
;;; cdr = (). Set equality is determined by comparing representatives using
;;; EQ?. A representative's car contains the number of elements in the set.

;;; The speed of the algorithm comes because when we chase links to find 
;;; representatives, we collapse links by changing all the boxs in the path
;;; we followed to point directly to the representative, so that next time
;;; we walk the cdr-chain, we'll go directly to the representative in one hop.


(define (base-set nelts) (cons nelts '()))

;;; Sets are chained together through cdr links. Last guy in the chain
;;; is the root of the set.

(define (get-set-root s)
  (let lp ((r s))			; Find the last pair
    (let ((next (cdr r)))		; in the list. That's
      (cond ((pair? next) (lp next))	; the root r.

	    (else
	     (if (not (eq? r s))	; Now zip down the list again,
		 (let lp ((x s))	; changing everyone's cdr to r.
		   (let ((next (cdr x)))	
		     (cond ((not (eq? r next))
			    (set-cdr! x r)
			    (lp next))))))
	     r)))))			; Then return r.

(define (set-equal? s1 s2) (eq? (get-set-root s1) (get-set-root s2)))

(define (set-size s) (car (get-set-root s)))

(define (union! s1 s2)
  (let* ((r1 (get-set-root s1))
	 (r2 (get-set-root s2))
	 (n1 (set-size r1))
	 (n2 (set-size r2))
	 (n  (+fx n1 n2)))

    (cond ((>fx n1 n2)
	   (set-cdr! r2 r1)
	   (set-car! r1 n))
	  (else
	   (set-cdr! r1 r2)
	   (set-car! r2 n)))))

(define (run.2 num)
   (with-output-to-file "/dev/null"
      (lambda () (pmaze 1500 35))))

(define (do-bench.2 num)
   (if (>fx num 0)
       (run.2 num)))

(define (main.2 argv)
   (let ((num (if (pair? (cdr argv))
		  (string->integer (cadr argv))
		  1)))
      (do-bench.2 num)))


; (make-parser grammar lexer) is used to create a parser from the grammar
; description `grammar' and the lexer function `lexer'.
;
; A grammar is a list of definitions.  Each definition defines a non-terminal
; by a set of rules.  Thus a definition has the form: (nt rule1 rule2...).
; A given non-terminal can only be defined once.  The first non-terminal
; defined is the grammar's goal.  Each rule is a possibly empty list of
; non-terminals.  Thus a rule has the form: (nt1 nt2...).  A non-terminal
; can be any scheme value.  Note that all grammar symbols are treated as
; non-terminals.  This is fine though because the lexer will be outputing
; non-terminals.
;
; The lexer defines what a token is and the mapping between tokens and
; the grammar's non-terminals.  It is a function of one argument, the input,
; that returns the list of tokens corresponding to the input.  Each token is
; represented by a list.  The first element is some `user-defined' information
; associated with the token and the rest represents the token's class(es) (as a
; list of non-terminals that this token corresponds to).
;
; The result of `make-parser' is a function that parses the single input it
; is given into the grammar's goal.  The result is a `parse' which can be
; manipulated with the procedures: `parse->parsed?', `parse->trees'
; and `parse->nb-trees' (see below).
;
; Let's assume that we want a parser for the grammar
;
;  S -> x = E
;  E -> E + E | V
;  V -> V y |
;
; and that the input to the parser is a string of characters.  Also, assume we
; would like to map the characters `x', `y', `+' and `=' into the corresponding
; non-terminals in the grammar.  Such a parser could be created with
;
; (make-parser
;   '(
;      (s (x = e))
;      (e (e + e) (v))
;      (v (v y) ())
;    )
;   (lambda (str)
;     (map (lambda (char)
;            (list char ; user-info = the character itself
;                  (case char
;                    ((#\x) 'x)
;                    ((#\y) 'y)
;                    ((#\+) '+)
;                    ((#\=) '=)
;                    (else (error "lexer error")))))
;          (string->list str)))
; )
;
; An alternative definition (that does not check for lexical errors) is
;
; (make-parser
;   '(
;      (s (#\x #\= e))
;      (e (e #\+ e) (v))
;      (v (v #\y) ())
;    )
;   (lambda (str) (map (lambda (char) (list char char)) (string->list str)))
; )
;
; To help with the rest of the discussion, here are a few definitions:
;
; An input pointer (for an input of `n' tokens) is a value between 0 and `n'.
; It indicates a point between two input tokens (0 = beginning, `n' = end).
; For example, if `n' = 4, there are 5 input pointers:
;
;   input                   token1     token2     token3     token4
;   input pointers       0          1          2          3          4
;
; A configuration indicates the extent to which a given rule is parsed (this
; is the common `dot notation').  For simplicity, a configuration is
; represented as an integer, with successive configurations in the same
; rule associated with successive integers.  It is assumed that the grammar
; has been extended with rules to aid scanning.  These rules are of the
; form `nt ->', and there is one such rule for every non-terminal.  Note
; that these rules are special because they only apply when the corresponding
; non-terminal is returned by the lexer.
;
; A configuration set is a configuration grouped with the set of input pointers
; representing where the head non-terminal of the configuration was predicted.
;
; Here are the rules and configurations for the grammar given above:
;
;  S -> .         \
;       0          |
;  x -> .          |
;       1          |
;  = -> .          |
;       2          |
;  E -> .          |
;       3           > special rules (for scanning)
;  + -> .          |
;       4          |
;  V -> .          |
;       5          |
;  y -> .          |
;       6         /
;  S -> .  x  .  =  .  E  .
;       7     8     9     10
;  E -> .  E  .  +  .  E  .
;       11    12    13    14
;  E -> .  V  .
;       15    16
;  V -> .  V  .  y  .
;       17    18    19
;  V -> .
;       20
;
; Starters of the non-terminal `nt' are configurations that are leftmost
; in a non-special rule for `nt'.  Enders of the non-terminal `nt' are
; configurations that are rightmost in any rule for `nt'.  Predictors of the
; non-terminal `nt' are configurations that are directly to the left of `nt'
; in any rule.
;
; For the grammar given above,
;
;   Starters of V   = (17 20)
;   Enders of V     = (5 19 20)
;   Predictors of V = (15 17)

(define (make-parser grammar lexer)

  (define (non-terminals grammar) ; return vector of non-terminals in grammar

    (define (add-nt nt nts)
      (if (member nt nts) nts (cons nt nts))) ; use equal? for equality tests

    (let def-loop ((defs grammar) (nts '()))
      (if (pair? defs)
        (let* ((def (car defs))
               (head (car def)))
          (let rule-loop ((rules (cdr def))
                          (nts (add-nt head nts)))
            (if (pair? rules)
              (let ((rule (car rules)))
                (let loop ((l rule) (nts nts))
                  (if (pair? l)
                    (let ((nt (car l)))
                      (loop (cdr l) (add-nt nt nts)))
                    (rule-loop (cdr rules) nts))))
              (def-loop (cdr defs) nts))))
        (list->vector (reverse nts))))) ; goal non-terminal must be at index 0

  (define (index nt nts) ; return index of non-terminal `nt' in `nts'
    (let loop ((i (- (vector-length nts) 1)))
      (if (>= i 0)
        (if (equal? (vector-ref nts i) nt) i (loop (- i 1)))
        #f)))

  (define (nb-configurations grammar) ; return nb of configurations in grammar
    (let def-loop ((defs grammar) (nb-confs 0))
      (if (pair? defs)
        (let ((def (car defs)))
          (let rule-loop ((rules (cdr def)) (nb-confs nb-confs))
            (if (pair? rules)
              (let ((rule (car rules)))
                (let loop ((l rule) (nb-confs nb-confs))
                  (if (pair? l)
                    (loop (cdr l) (+ nb-confs 1))
                    (rule-loop (cdr rules) (+ nb-confs 1)))))
              (def-loop (cdr defs) nb-confs))))
      nb-confs)))

; First, associate a numeric identifier to every non-terminal in the
; grammar (with the goal non-terminal associated with 0).
;
; So, for the grammar given above we get:
;
; s -> 0   x -> 1   = -> 4   e ->3    + -> 4   v -> 5   y -> 6

  (let* ((nts (non-terminals grammar))          ; id map = list of non-terms
         (nb-nts (vector-length nts))           ; the number of non-terms
         (nb-confs (+ (nb-configurations grammar) nb-nts)) ; the nb of confs
         (starters (make-vector nb-nts '()))    ; starters for every non-term
         (enders (make-vector nb-nts '()))      ; enders for every non-term
         (predictors (make-vector nb-nts '()))  ; predictors for every non-term
         (steps (make-vector nb-confs #f))      ; what to do in a given conf
         (names (make-vector nb-confs #f)))     ; name of rules

    (define (setup-tables grammar nts starters enders predictors steps names)

      (define (add-conf conf nt nts class)
        (let ((i (index nt nts)))
          (vector-set! class i (cons conf (vector-ref class i)))))

      (let ((nb-nts (vector-length nts)))

        (let nt-loop ((i (- nb-nts 1)))
          (if (>= i 0)
            (begin
              (vector-set! steps i (- i nb-nts))
              (vector-set! names i (list (vector-ref nts i) 0))
              (vector-set! enders i (list i))
              (nt-loop (- i 1)))))

        (let def-loop ((defs grammar) (conf (vector-length nts)))
          (if (pair? defs)
            (let* ((def (car defs))
                   (head (car def)))
              (let rule-loop ((rules (cdr def)) (conf conf) (rule-num 1))
                (if (pair? rules)
                  (let ((rule (car rules)))
                    (vector-set! names conf (list head rule-num))
                    (add-conf conf head nts starters)
                    (let loop ((l rule) (conf conf))
                      (if (pair? l)
                        (let ((nt (car l)))
                          (vector-set! steps conf (index nt nts))
                          (add-conf conf nt nts predictors)
                          (loop (cdr l) (+ conf 1)))
                        (begin
                          (vector-set! steps conf (- (index head nts) nb-nts))
                          (add-conf conf head nts enders)
                          (rule-loop (cdr rules) (+ conf 1) (+ rule-num 1))))))
                  (def-loop (cdr defs) conf))))))))

; Now, for each non-terminal, compute the starters, enders and predictors and
; the names and steps tables.

    (setup-tables grammar nts starters enders predictors steps names)

; Build the parser description

    (let ((parser-descr (vector lexer
                                nts
                                starters
                                enders
                                predictors
                                steps
                                names)))
      (lambda (input)

        (define (index nt nts) ; return index of non-terminal `nt' in `nts'
          (let loop ((i (- (vector-length nts) 1)))
            (if (>= i 0)
              (if (equal? (vector-ref nts i) nt) i (loop (- i 1)))
              #f)))

        (define (comp-tok tok nts) ; transform token to parsing format
          (let loop ((l1 (cdr tok)) (l2 '()))
            (if (pair? l1)
              (let ((i (index (car l1) nts)))
                (if i
                  (loop (cdr l1) (cons i l2))
                  (loop (cdr l1) l2)))
              (cons (car tok) (reverse l2)))))

        (define (input->tokens input lexer nts)
          (list->vector (map (lambda (tok) (comp-tok tok nts)) (lexer input))))

        (define (make-states nb-toks nb-confs)
          (let ((states (make-vector (+ nb-toks 1) #f)))
            (let loop ((i nb-toks))
              (if (>= i 0)
                (let ((v (make-vector (+ nb-confs 1) #f)))
                  (vector-set! v 0 -1)
                  (vector-set! states i v)
                  (loop (- i 1)))
                states))))

        (define (conf-set-get state conf)
          (vector-ref state (+ conf 1)))

        (define (conf-set-get* state state-num conf)
          (let ((conf-set (conf-set-get state conf)))
            (if conf-set
              conf-set
              (let ((conf-set (make-vector (+ state-num 6) #f)))
                (vector-set! conf-set 1 -3) ; old elems tail (points to head)
                (vector-set! conf-set 2 -1) ; old elems head
                (vector-set! conf-set 3 -1) ; new elems tail (points to head)
                (vector-set! conf-set 4 -1) ; new elems head
                (vector-set! state (+ conf 1) conf-set)
                conf-set))))

        (define (conf-set-merge-new! conf-set)
          (vector-set! conf-set
            (+ (vector-ref conf-set 1) 5)
            (vector-ref conf-set 4))
          (vector-set! conf-set 1 (vector-ref conf-set 3))
          (vector-set! conf-set 3 -1)
          (vector-set! conf-set 4 -1))

        (define (conf-set-head conf-set)
          (vector-ref conf-set 2))

        (define (conf-set-next conf-set i)
          (vector-ref conf-set (+ i 5)))

        (define (conf-set-member? state conf i)
          (let ((conf-set (vector-ref state (+ conf 1))))
            (if conf-set
              (conf-set-next conf-set i)
              #f)))

        (define (conf-set-adjoin state conf-set conf i)
          (let ((tail (vector-ref conf-set 3))) ; put new element at tail
            (vector-set! conf-set (+ i 5) -1)
            (vector-set! conf-set (+ tail 5) i)
            (vector-set! conf-set 3 i)
            (if (< tail 0)
              (begin
                (vector-set! conf-set 0 (vector-ref state 0))
                (vector-set! state 0 conf)))))

        (define (conf-set-adjoin* states state-num l i)
          (let ((state (vector-ref states state-num)))
            (let loop ((l1 l))
              (if (pair? l1)
                (let* ((conf (car l1))
                       (conf-set (conf-set-get* state state-num conf)))
                  (if (not (conf-set-next conf-set i))
                    (begin
                      (conf-set-adjoin state conf-set conf i)
                      (loop (cdr l1)))
                    (loop (cdr l1))))))))

        (define (conf-set-adjoin** states states* state-num conf i)
          (let ((state (vector-ref states state-num)))
            (if (conf-set-member? state conf i)
              (let* ((state* (vector-ref states* state-num))
                     (conf-set* (conf-set-get* state* state-num conf)))
                (if (not (conf-set-next conf-set* i))
                  (conf-set-adjoin state* conf-set* conf i))
                #t)
              #f)))

        (define (conf-set-union state conf-set conf other-set)
          (let loop ((i (conf-set-head other-set)))
            (if (>= i 0)
              (if (not (conf-set-next conf-set i))
                (begin
                  (conf-set-adjoin state conf-set conf i)
                  (loop (conf-set-next other-set i)))
                (loop (conf-set-next other-set i))))))

        (define (forw states state-num starters enders predictors steps nts)

          (define (predict state state-num conf-set conf nt starters enders)

            ; add configurations which start the non-terminal `nt' to the
            ; right of the dot

            (let loop1 ((l (vector-ref starters nt)))
              (if (pair? l)
                (let* ((starter (car l))
                       (starter-set (conf-set-get* state state-num starter)))
                  (if (not (conf-set-next starter-set state-num))
                    (begin
                      (conf-set-adjoin state starter-set starter state-num)
                      (loop1 (cdr l)))
                    (loop1 (cdr l))))))

            ; check for possible completion of the non-terminal `nt' to the
            ; right of the dot

            (let loop2 ((l (vector-ref enders nt)))
              (if (pair? l)
                (let ((ender (car l)))
                  (if (conf-set-member? state ender state-num)
                    (let* ((next (+ conf 1))
                           (next-set (conf-set-get* state state-num next)))
                      (conf-set-union state next-set next conf-set)
                      (loop2 (cdr l)))
                    (loop2 (cdr l)))))))

          (define (reduce states state state-num conf-set head preds)

            ; a non-terminal is now completed so check for reductions that
            ; are now possible at the configurations `preds'

            (let loop1 ((l preds))
              (if (pair? l)
                (let ((pred (car l)))
                  (let loop2 ((i head))
                    (if (>= i 0)
                      (let ((pred-set (conf-set-get (vector-ref states i)
						    pred)))
                        (if pred-set
                          (let* ((next (+ pred 1))
                                 (next-set (conf-set-get* state state-num
							  next)))
                            (conf-set-union state next-set next pred-set)))
                        (loop2 (conf-set-next conf-set i)))
                      (loop1 (cdr l))))))))

          (let ((state (vector-ref states state-num))
                (nb-nts (vector-length nts)))
            (let loop ()
              (let ((conf (vector-ref state 0)))
                (if (>= conf 0)
                  (let* ((step (vector-ref steps conf))
                         (conf-set (vector-ref state (+ conf 1)))
                         (head (vector-ref conf-set 4)))
                    (vector-set! state 0 (vector-ref conf-set 0))
                    (conf-set-merge-new! conf-set)
                    (if (>= step 0)
                      (predict state
			       state-num
			       conf-set
			       conf
			       step
			       starters
			       enders)
                      (let ((preds (vector-ref predictors (+ step nb-nts))))
                        (reduce states state state-num conf-set head preds)))
                    (loop)))))))

        (define (forward starters enders predictors steps nts toks)
          (let* ((nb-toks (vector-length toks))
                 (nb-confs (vector-length steps))
                 (states (make-states nb-toks nb-confs))
                 (goal-starters (vector-ref starters 0)))
            (conf-set-adjoin* states 0 goal-starters 0) ; predict goal
            (forw states 0 starters enders predictors steps nts)
            (let loop ((i 0))
              (if (< i nb-toks)
                (let ((tok-nts (cdr (vector-ref toks i))))
                  (conf-set-adjoin* states (+ i 1) tok-nts i) ; scan token
                  (forw states (+ i 1) starters enders predictors steps nts)
                  (loop (+ i 1)))))
            states))

        (define (produce conf i j enders steps toks states states* nb-nts)
          (let ((prev (- conf 1)))
            (if (and (>= conf nb-nts) (>= (vector-ref steps prev) 0))
              (let loop1 ((l (vector-ref enders (vector-ref steps prev))))
                (if (pair? l)
                  (let* ((ender (car l))
                         (ender-set (conf-set-get (vector-ref states j)
                                                  ender)))
                    (if ender-set
                      (let loop2 ((k (conf-set-head ender-set)))
                        (if (>= k 0)
                          (begin
                            (and (>= k i)
                                 (conf-set-adjoin** states states* k prev i)
                                 (conf-set-adjoin** states states* j ender k))
                            (loop2 (conf-set-next ender-set k)))
                          (loop1 (cdr l))))
                      (loop1 (cdr l)))))))))

        (define (back states states* state-num enders steps nb-nts toks)
          (let ((state* (vector-ref states* state-num)))
            (let loop1 ()
              (let ((conf (vector-ref state* 0)))
                (if (>= conf 0)
                  (let* ((conf-set (vector-ref state* (+ conf 1)))
                         (head (vector-ref conf-set 4)))
                    (vector-set! state* 0 (vector-ref conf-set 0))
                    (conf-set-merge-new! conf-set)
                    (let loop2 ((i head))
                      (if (>= i 0)
                        (begin
                          (produce conf i state-num enders steps
                                   toks states states* nb-nts)
                          (loop2 (conf-set-next conf-set i)))
                        (loop1)))))))))

        (define (backward states enders steps nts toks)
          (let* ((nb-toks (vector-length toks))
                 (nb-confs (vector-length steps))
                 (nb-nts (vector-length nts))
                 (states* (make-states nb-toks nb-confs))
                 (goal-enders (vector-ref enders 0)))
            (let loop1 ((l goal-enders))
              (if (pair? l)
                (let ((conf (car l)))
                  (conf-set-adjoin** states states* nb-toks conf 0)
                  (loop1 (cdr l)))))
            (let loop2 ((i nb-toks))
              (if (>= i 0)
                (begin
                  (back states states* i enders steps nb-nts toks)
                  (loop2 (- i 1)))))
            states*))

        (define (parsed? nt i j nts enders states)
          (let ((nt* (index nt nts)))
            (if nt*
              (let ((nb-nts (vector-length nts)))
                (let loop ((l (vector-ref enders nt*)))
                  (if (pair? l)
                    (let ((conf (car l)))
                      (if (conf-set-member? (vector-ref states j) conf i)
                        #t
                        (loop (cdr l))))
                    #f)))
              #f)))

        (define (deriv-trees conf i j enders steps names toks states nb-nts)
          (let ((name (vector-ref names conf)))

            (if name ; `conf' is at the start of a rule (either special or not)
              (if (< conf nb-nts)
                (list (list name (car (vector-ref toks i))))
                (list (list name)))

              (let ((prev (- conf 1)))
                (let loop1 ((l1 (vector-ref enders (vector-ref steps prev)))
                            (l2 '()))
                  (if (pair? l1)
                    (let* ((ender (car l1))
                           (ender-set (conf-set-get (vector-ref states j)
                                                    ender)))
                      (if ender-set
                        (let loop2 ((k (conf-set-head ender-set)) (l2 l2))
                          (if (>= k 0)
                            (if (and (>= k i)
                                     (conf-set-member? (vector-ref states k)
                                                       prev i))
                              (let ((prev-trees
                                      (deriv-trees prev i k enders steps names
                                                   toks states nb-nts))
                                    (ender-trees
                                      (deriv-trees ender k j enders steps names
                                                   toks states nb-nts)))
                                (let loop3 ((l3 ender-trees) (l2 l2))
                                  (if (pair? l3)
                                    (let ((ender-tree (list (car l3))))
                                      (let loop4 ((l4 prev-trees) (l2 l2))
                                        (if (pair? l4)
                                          (loop4 (cdr l4)
                                                 (cons (append (car l4)
                                                               ender-tree)
                                                       l2))
                                          (loop3 (cdr l3) l2))))
                                    (loop2 (conf-set-next ender-set k) l2))))
                              (loop2 (conf-set-next ender-set k) l2))
                            (loop1 (cdr l1) l2)))
                        (loop1 (cdr l1) l2)))
                    l2))))))

        (define (deriv-trees* nt i j nts enders steps names toks states)
          (let ((nt* (index nt nts)))
            (if nt*
              (let ((nb-nts (vector-length nts)))
                (let loop ((l (vector-ref enders nt*)) (trees '()))
                  (if (pair? l)
                    (let ((conf (car l)))
                      (if (conf-set-member? (vector-ref states j) conf i)
                        (loop (cdr l)
                              (append (deriv-trees conf i j enders steps names
                                                   toks states nb-nts)
                                      trees))
                        (loop (cdr l) trees)))
                    trees)))
              #f)))

        (define (nb-deriv-trees conf i j enders steps toks states nb-nts)
          (let ((prev (- conf 1)))
            (if (or (< conf nb-nts) (< (vector-ref steps prev) 0))
              1
              (let loop1 ((l (vector-ref enders (vector-ref steps prev)))
                          (n 0))
                (if (pair? l)
                  (let* ((ender (car l))
                         (ender-set (conf-set-get (vector-ref states j)
                                                  ender)))
                    (if ender-set
                      (let loopII ((k (conf-set-head ender-set)) (n n))
                        (if (>= k 0)
                          (if (and (>= k i)
                                   (conf-set-member? (vector-ref states k)
                                                     prev i))
                            (let ((nb-prev-trees
                                    (nb-deriv-trees prev i k enders steps
                                                    toks states nb-nts))
                                  (nb-ender-trees
                                    (nb-deriv-trees ender k j enders steps
                                                    toks states nb-nts)))
                              (loopII (conf-set-next ender-set k)
                                     (+ n (* nb-prev-trees nb-ender-trees))))
                            (loopII (conf-set-next ender-set k) n))
                          (loop1 (cdr l) n)))
                      (loop1 (cdr l) n)))
                  n)))))

        (define (nb-deriv-trees* nt i j nts enders steps toks states)
          (let ((nt* (index nt nts)))
            (if nt*
              (let ((nb-nts (vector-length nts)))
                (let loop ((l (vector-ref enders nt*)) (nb-trees 0))
                  (if (pair? l)
                    (let ((conf (car l)))
                      (if (conf-set-member? (vector-ref states j) conf i)
                        (loop (cdr l)
                              (+ (nb-deriv-trees conf i j enders steps
                                                 toks states nb-nts)
                                 nb-trees))
                        (loop (cdr l) nb-trees)))
                    nb-trees)))
              #f)))

        (let* ((lexer      (vector-ref parser-descr 0))
               (nts        (vector-ref parser-descr 1))
               (starters   (vector-ref parser-descr 2))
               (enders     (vector-ref parser-descr 3))
               (predictors (vector-ref parser-descr 4))
               (steps      (vector-ref parser-descr 5))
               (names      (vector-ref parser-descr 6))
               (toks       (input->tokens input lexer nts)))

          (vector nts
                  starters
                  enders
                  predictors
                  steps
                  names
                  toks
                  (backward (forward starters enders predictors steps nts toks)
                            enders steps nts toks)
                  parsed?
                  deriv-trees*
                  nb-deriv-trees*))))))

(define (parse->parsed? parse nt i j)
  (let* ((nts     (vector-ref parse 0))
         (enders  (vector-ref parse 2))
         (states  (vector-ref parse 7))
         (parsed? (vector-ref parse 8)))
    (parsed? nt i j nts enders states)))

(define (parse->trees parse nt i j)
  (let* ((nts          (vector-ref parse 0))
         (enders       (vector-ref parse 2))
         (steps        (vector-ref parse 4))
         (names        (vector-ref parse 5))
         (toks         (vector-ref parse 6))
         (states       (vector-ref parse 7))
         (deriv-trees* (vector-ref parse 9)))
    (deriv-trees* nt i j nts enders steps names toks states)))

(define (parse->nb-trees parse nt i j)
  (let* ((nts             (vector-ref parse 0))
         (enders          (vector-ref parse 2))
         (steps           (vector-ref parse 4))
         (toks            (vector-ref parse 6))
         (states          (vector-ref parse 7))
         (nb-deriv-trees* (vector-ref parse 10)))
    (nb-deriv-trees* nt i j nts enders steps toks states)))

(define (test.3)
  (let ((p (make-parser '( (s (a) (s s)) )
                        (lambda (l) (map (lambda (x) (list x x)) l)))))
    (let ((x (p '(a a a a a a a a a))))
      (length (parse->trees x 's 0 9)))))

(define (main.3 argv)
   (let loop ((i (if (pair? (cdr argv))
		     (string->number (cadr argv))
		     50)))
      (if (= i 1)
	  (begin
	     (display "EARLY")
	     (display (test.3))
	     (newline))
	  (begin
	     (test.3)
	     (loop (- i 1))))))

(define (rgc-buffer-integer x) x)

(define pp-read
   (let* ((par-open      0)
	  (grammar
	   (regular-grammar ((float    (or (: (* digit) "." (+ digit))
					   (: (+ digit) "." (* digit))))
			     (letter   (in ("azAZ") (#a128 #a255)))
			     (special  (in "!@#~$%^&*></-_+\\=?.:"))
			     (quote    (in "\",'`"))
			     (paren    (in "()[]{}"))
			     (blank    (in #\Space #\Tab #a012 #a013 #\Newline))
			     (id       (: (or letter digit special)
					  (* (or letter digit special quote)))))
	      ((+ blank)                    ;; on oublie les separateurs
	       (ignore))
	      
	      ((: #\# #\\ (or letter        ;; Les caracteres normaux
			      digit
			      special
			      quote
			      paren
			      (in "|;")))
	       (string-ref (the-string) 2))
	      
	      ((: (* #\space) ";" (* all))  ;; les commentaires
	       (if *ignore-comment*
		   (ignore)
		   (let* ((string (the-string))
			  (indice (let loop ((i 0))
				     (if (char=? (string-ref string i) #\;)
					 i
					 (loop (+fx i 1))))))
		      (list 'COMMENT
			    indice
			    (substring string
				       indice
				       (string-length string))))))
	      ((: #\# #\\ (uncase "newline")) ;; retour charriot
	       #\newline)
	      ((: #\# #\\ (uncase "tab"))     ;; tabulation
	       #\tab)
	      ((or (: #\# #\\ (uncase "space"));; espace
		   (: #\# #\\ #\space))
	       #\space)
	      ((: #\# #\\ (uncase "return"))  ;; carriage return
	       (integer->char 13))
	      ((: #\" (* (or (out #\\ #\")     ;; Les chaines de caracteres
			     (: #\\ all))) #\")
	       (escape-scheme-string (the-substring 1 (-fx (the-length) 1))))
	      ((: #\# #\"                  ;; Les chaines de caracteres foreign
		      (* (or (out #\\ #\")  
			     (: #\\ all))) #\")
	       (escape-C-string (the-substring 1 (-fx (the-length) 1))))
	      ((: (or #\" (: #\# #\"))     ;; Les bouts de chaines non termines
		  (* (or (out #\\ #\")  
			 (: #\\ all))))
	       (error "read" "Unexpected end-of-file" (the-string)))
	      ((or (+ digit)             ;; Les entiers
		   (: #\- (+ digit))
		   (: #\+ (+ digit)))
	       (string->integer (the-string) 10))
	      ((: "#o" (or (+ (in (#\0 #\7))) ;; Les entiers en base 8
			   (: (in #\+ #\-)
			      (in (#\0 #\7)))))
	       (string->integer (substring (the-string) 2 (the-length)) 8))
	      ((: "#d" (or (+ digit)       ;; Les entiers en base 10
			   (: (in #\+ #\-)
			      digit)))
	       (string->integer (substring (the-string) 2 (the-length)) 10))
	      ((: "#x" (or (+ (or digit     ;; Les entiers en base 10
				  (in (#\a #\f))
				  (in (#\A #\F))))
			   (: (in #\+ #\-)
			      (+ (or digit
				     (in (#\a #\f))
				     (in (#\A #\F)))))))
	       (string->integer (substring (the-string) 2 (the-length)) 16))
	      ((or float                   ;; Les reels
		   (: (in #\+ #\-) float)
		   (: (or float (+ digit))
		      (in #\e #\E) (+ digit)) 
		   (: (in #\+ #\-) (or float (+ digit))
				   (in #\e #\E) (+ digit))
		   (: (or float (+ digit)) (in #\e #\E) (in #\+ #\-)
					   (+ digit))
		   (: (in #\+ #\-) (or float (+ digit))
				   (in #\e #\E) (in #\+ #\-) (+ digit)))
	       (string->real (the-string)))
	      ((context pair (: #\. (* blank) #\)))
	       (error "read" "Illegal pair" (the-string)))
	      ((context pair #\.)        ;; Le point des pairs pointees
	       '__dot__)
	      (#\.
	       (error "read" "Illegal token" #\.))
	      ((uncase "#t")             ;; true
	       #t)
	      ((uncase "#f")             ;; false
	       #f)
	      ("#unspecified" 
	       (unspecified))
	      ((or id (: #\. (+ #\.)))      ;; Les identificateurs
	       (if (eq? *case* 'respect)
		   (string->symbol (the-string)) 
		   (the-symbol)))
	      ((: "|" (+ (or (out #a000 #\\ #\|) (: #\\ all))) "|")
	       (let ((str (the-substring 0 (-fx (the-length) 1))))
		  (string->symbol (escape-C-string str))))
	      (#\'                     ;; Les simples quotations
	       (cons 'quote (cons (ignore) '())))
	      (#\`                     ;; Les quasiquotes
	       (cons 'quasiquote (cons (ignore) '())))
	      (#\,                     ;; Les unquotations
	       (cons 'unquote (cons (ignore) '())))
	      ((: #\, #\@)                 ;; Les unquote-splicing
	       (cons 'unquote-splicing (cons (ignore) '())))
	      ((in #\( #\[)              ;; Les parentheses ouvrantes
	       (let ((open-key par-open))
		  (set! par-open (+fx 1 par-open))
		  (rgc-context 'pair)
		  (let loop-pair ((walk (ignore))) 
		     (cond
			((eq? walk '__dot__) ;; une pair pointee
			 (rgc-context)
			 (let ((cdr (ignore)))
			    (ignore)
			    (if (=fx open-key par-open)
				(begin
				   (rgc-context 'pair)
				   cdr)
				(error "read" "Illegal pair" cdr))))
			((=fx open-key par-open)
			 (if (=fx open-key 0)
			     (rgc-context))
			 '())
			(else
			 (cons walk (loop-pair (ignore))))))))
	      ((in #\) #\])               ;; Les parentheses fermantes
	       (set! par-open (-fx par-open 1))
	       (if (<fx par-open 0)
		   (begin
		      (set! par-open 0)
		      (ignore))
		   #f))
	      ((: #\# #\()                 ;; Les debuts de vecteur
	       (let ((open-key par-open))
		  (set! par-open (+fx 1 par-open))
		  (list->vector (let loop-vector ((walk (ignore)))
				   (cond
				      ((=fx open-key par-open)
				       '())
				      (else
				       (cons walk (loop-vector (ignore)))))))))
	      (else
	       (let ((char (the-failure)))
		  (if (eof-object? char)
		      (if (>fx par-open 0)
			  (error "read" "Unexpected end-of-file" char)
			  (begin
			     (reset-eof input-port)
			     char))
		      (error "read"
			     "Illegal char"
			     (illegal-char-rep char))))))))
      (lambda input-port
	 (if (null? input-port)
	     (read/rp grammar (current-input-port))
	     (read/rp grammar (car input-port))))))

(define (foo-match l)
   (match-case l
      ((toto . ?-)
       1)
      ((tutu (?x ?- ?x))
       4)
      ((tutu (?x ?- ?x) tutu)
       5)
      ((tutu (?x ?- ?x ?x ?x) tutu)
       6)
      ((tata (and ?y ((?x ?- ?x ?x ?x) tutu)) ?y)
       7)
      ((tutu . ?-)
       2)
      (else
       3)))
      
      
(define (main argv)
   (main.1 argv)
   (main.2 argv)
   (main.3 argv)
   (let ((t (instantiate::point5d)))
      (print (point? t))
      (print (point2d? t))
      (print (point3d? t))
      (print (point4d? t))
      (print (point5d? t))
      (print (point6d? t))
      (print (point7d? t))))




		      
		 

