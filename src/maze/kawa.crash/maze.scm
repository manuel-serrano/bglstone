;; unit: TOPLEVEL
(begin
  (define (alloc-harr)
    (make-vector 4 'unspecified))
 (define (harr? x)
    (let ((_andtest_1002 (vector? x)))
      (if _andtest_1002
        (let ((_andtest_1003 (= (vector-length x) 4)))
          (if _andtest_1003
            (eq? (vector-ref x 0) 'harr)
            #f))
        #f)))
 (define (make-harr nrows ncols elts)
    (let ((g1 (alloc-harr)))
      (begin
        (vector-set! g1 0 'harr)
        (vector-set! g1 1 nrows)
        (vector-set! g1 2 ncols)
        (vector-set! g1 3 elts)
        g1)))
 #f
  #f
  #f
  #f
  #f
  #f)
(begin
  (define (alloc-wall)
    (make-vector 4 'unspecified))
 (define (wall? x)
    (let ((_andtest_1011 (vector? x)))
      (if _andtest_1011
        (let ((_andtest_1012 (= (vector-length x) 4)))
          (if _andtest_1012
            (eq? (vector-ref x 0) 'wall)
            #f))
        #f)))
 (define (make-wall owner neighbor bit)
    (let ((g1 (alloc-wall)))
      (begin
        (vector-set! g1 0 'wall)
        (vector-set! g1 1 owner)
        (vector-set! g1 2 neighbor)
        (vector-set! g1 3 bit)
        g1)))
 #f
  #f
  #f
  #f
  #f
  #f)
(begin
  (define (alloc-box) (make-vector 6 'unspecified))
  (define (box? x)
    (let ((_andtest_1019 (vector? x)))
      (if _andtest_1019
        (let ((_andtest_1020 (= (vector-length x) 6)))
          (if _andtest_1020
            (eq? (vector-ref x 0) 'box)
            #f))
        #f)))
 (define (make-box reachable id)
    (let ((g1 (alloc-box)))
      (begin
        (vector-set! g1 0 'box)
        (vector-set! g1 1 reachable)
        (vector-set! g1 2 id)
        (vector-set! g1 3 -1)
        (vector-set! g1 4 #f)
        (vector-set! g1 5 #f)
        g1)))
 #f
  #f
  #f
  #f
  #f
  #f
  #f
  #f
  #f
  #f)
;; HARR
(define (harr r c)
  (make-harr r c (make-vector (* r c))))

;; HREF
(define (href ha x y)
  (let ((r (/ y 2)) (c (/ x 3)))
    (vector-ref
      (vector-ref ha 3)
      (+ (* (vector-ref ha 2) r) c))))

;; HSET!
(define (hset! ha x y val)
  (let ((r (/ y 2)) (c (/ x 3)))
    (vector-set!
      (vector-ref ha 3)
      (+ (* (vector-ref ha 2) r) c)
      val)))

;; HREF/RC
(define (href/rc ha r c)
  (vector-ref
    (vector-ref ha 3)
    (+ (* (vector-ref ha 2) r) c)))

;; HARR-TABULATE
(define (harr-tabulate nrows ncols proc)
  (let ((v (make-vector (* nrows ncols))))
    (begin
      (letrec ((do-loop--1031
                 (lambda (r)
                   (if (< r 0)
                     (begin #f)
                     (begin
                       (letrec ((do-loop--1032
                                  (lambda (c i)
                                    (if (= c ncols)
                                      (begin #f)
                                      (begin
                                        (vector-set!
                                          v
                                          i
                                          (proc (* 3 c)
                                                (+ (* 2 r) (logand c 1))))
                                        (do-loop--1032
                                          (+ c 1)
                                          (+ i 1)))))))
                         (do-loop--1032 0 (* r ncols)))
                       (do-loop--1031 (- r 1)))))))
        (do-loop--1031 (- nrows 1)))
      (make-harr nrows ncols v))))

;; HARR-FOR-EACH
(define (harr-for-each proc harr)
  (vector-for-each proc (vector-ref harr 3)))

;; SOUTH-WEST
(define south-west 1)
;; SOUTH
(define south 2)
;; SOUTH-EAST
(define south-east 4)
;; GEN-MAZE-ARRAY
(define (gen-maze-array r c)
  (harr-tabulate
    r
    c
    (lambda (x y)
      (my-make-box (base-set 1) (cons x y)))))

;; MAKE-WALL-VEC
(define (make-wall-vec harr)
  (let ((nrows (vector-ref harr 1)))
    (let ((ncols (vector-ref harr 2)))
      (let ((xmax (* 3 (- ncols 1))))
        (let ((walls '()))
          (let ((add-wall
                  (lambda (o n b)
                    (set! walls (cons (make-wall o n b) walls)))))
            (let ()
              (begin
                (letrec ((do-loop--1033
                           (lambda (x)
                             (if (< x 0)
                               (begin #f)
                               (begin
                                 (letrec ((do-loop--1034
                                            (lambda (y)
                                              (if (<= y 1)
                                                (begin #f)
                                                (begin
                                                  (let ((hex (href harr x y)))
                                                    (begin
                                                      (if (if (zero? x) #f #t)
                                                        (add-wall
                                                          hex
                                                          (href harr
                                                                (- x 3)
                                                                (- y 1))
                                                          south-west)
                                                        #f)
                                                      (add-wall
                                                        hex
                                                        (href harr x (- y 2))
                                                        south)
                                                      (if (< x xmax)
                                                        (add-wall
                                                          hex
                                                          (href harr
                                                                (+ x 3)
                                                                (- y 1))
                                                          south-east)
                                                        #f)))
                                                  (do-loop--1034
                                                    (- y 2)))))))
                                   (do-loop--1034
                                     (+ (* (- nrows 1) 2) (logand x 1))))
                                 (do-loop--1033 (- x 3)))))))
                  (do-loop--1033 (* (- ncols 1) 3)))
                (if (> ncols 1)
                  (let ((rmoc-x (+ 3 (* 6 (/ (- ncols 2) 2)))))
                    (begin
                      (let ((rmoc-hex (href harr rmoc-x 1)))
                        (begin
                          (if (< rmoc-x xmax)
                            (add-wall rmoc-hex (href harr xmax 0) south-east)
                            #f)
                          (add-wall
                            rmoc-hex
                            (href harr (- rmoc-x 3) 0)
                            south-west)))
                      (letrec ((do-loop--1035
                                 (lambda (x)
                                   (if (< x 3)
                                     (begin #f)
                                     (begin
                                       (add-wall
                                         (href harr x 1)
                                         (href harr (- x 3) 0)
                                         south-west)
                                       (add-wall
                                         (href harr x 1)
                                         (href harr (+ x 3) 0)
                                         south-east)
                                       (do-loop--1035 (- x 6)))))))
                        (do-loop--1035 (- rmoc-x 6)))))
                  #f)
                (list->vector walls)))))))))

;; PICK-ENTRANCES
(define (pick-entrances harr)
  (begin
    (dfs-maze
      harr
      (href/rc harr 0 0)
      for-each-hex-child)
    (let ((nrows (vector-ref harr 1))
          (ncols (vector-ref harr 2)))
      (labels
        ((tp-lp (max-len entrance exit tcol)
                (if (< tcol 0)
                  (values entrance exit)
                  (let ((top-box (href/rc harr (- nrows 1) tcol)))
                    (begin
                      (reroot-maze top-box)
                      (call-with-values
                        (lambda ()
                          (labels
                            ((bt-lp (max-len entrance exit bcol)
                                    (if (< bcol 0)
                                      (values max-len entrance exit)
                                      (let ((this-len
                                              (path-length
                                                (href/rc harr 0 bcol))))
                                        (if (> this-len max-len)
                                          (bt-lp this-len
                                                 tcol
                                                 bcol
                                                 (- bcol 1))
                                          (bt-lp max-len
                                                 entrance
                                                 exit
                                                 (- bcol 1)))))))
                            (bt-lp max-len entrance exit (- ncols 1))))
                        (lambda (max-len entrance exit)
                          (tp-lp max-len entrance exit (- tcol 1)))))))))
        (tp-lp -1 #f #f (- ncols 1))))))

;; FOR-EACH-HEX-CHILD
(define (for-each-hex-child proc harr box)
  (let ((walls (vector-ref box 3)))
    (let ((id (vector-ref box 2)))
      (let ((x (car id)))
        (let ((y (cdr id)))
          (let ((nr (vector-ref harr 1)))
            (let ((nc (vector-ref harr 2)))
              (let ((maxy (* 2 (- nr 1))))
                (let ((maxx (* 3 (- nc 1))))
                  (let ()
                    (begin
                      (if (if (bit-test walls south-west) #f #t)
                        (proc (href harr (- x 3) (- y 1)))
                        #f)
                      (if (if (bit-test walls south) #f #t)
                        (proc (href harr x (- y 2)))
                        #f)
                      (if (if (bit-test walls south-east) #f #t)
                        (proc (href harr (+ x 3) (- y 1)))
                        #f)
                      (if (if (> x 0)
                            (let ((_ortest_1036 (<= y maxy)))
                              (if _ortest_1036
                                _ortest_1036
                                (zero? (modulo x 6))))
                            #f)
                        (let ((nw (href harr (- x 3) (+ y 1))))
                          (if (if (bit-test (vector-ref nw 3) south-east)
                                #f
                                #t)
                            (proc nw)
                            #f))
                        #f)
                      (if (< y maxy)
                        (let ((n (href harr x (+ y 2))))
                          (if (if (bit-test (vector-ref n 3) south) #f #t)
                            (proc n)
                            #f))
                        #f)
                      (if (if (< x maxx)
                            (let ((_ortest_1037 (<= y maxy)))
                              (if _ortest_1037
                                _ortest_1037
                                (zero? (modulo x 6))))
                            #f)
                        (let ((ne (href harr (+ x 3) (+ y 1))))
                          (if (if (bit-test (vector-ref ne 3) south-west)
                                #f
                                #t)
                            (proc ne)
                            #f))
                        #f))))))))))))

;; MAKE-MAZE
(define (make-maze nrows ncols)
  (let ((boxs (gen-maze-array nrows ncols)))
    (let ((walls (permute-vec!
                   (make-wall-vec boxs)
                   (random-state 20))))
      (let ()
        (begin
          (dig-maze walls (* nrows ncols))
          (call-with-values
            (lambda () (pick-entrances boxs))
            (lambda (entrance exit)
              (let ((exit-box (href/rc boxs 0 exit)))
                (let ((walls (vector-ref exit-box 3)))
                  (let ()
                    (begin
                      (reroot-maze
                        (href/rc boxs (- nrows 1) entrance))
                      (mark-path exit-box)
                      (vector-set!
                        exit-box
                        3
                        (logand walls (lognot south)))
                      (values boxs entrance exit))))))))))))

;; PMAZE
(define (pmaze nrows ncols)
  (call-with-values
    (lambda () (make-maze nrows ncols))
    (lambda (boxs entrance exit)
      (print-hexmaze boxs entrance))))

;; PRINT-HEXMAZE
(define (print-hexmaze harr entrance)
  (let ((nrows (vector-ref harr 1)))
    (let ((ncols (vector-ref harr 2)))
      (let ((ncols2 (* 2 (/ ncols 2))))
        (let ()
          (begin
            (letrec ((do-loop--1038
                       (lambda (c)
                         (if (>= c ncols)
                           (begin #f)
                           (begin
                             (display "   ")
                             (write-char (if (= c entrance) #\space #\_))
                             (do-loop--1038 (+ c 2)))))))
              (do-loop--1038 1))
            (newline)
            (write-char #\space)
            (letrec ((do-loop--1039
                       (lambda (c)
                         (if (>= c ncols2)
                           (begin #f)
                           (begin
                             (display*
                               (if (= c entrance) #\space #\_)
                               "/"
                               (dot/space harr (- nrows 1) (+ c 1))
                               "\\")
                             (do-loop--1039 (+ c 2)))))))
              (do-loop--1039 0))
            (if (odd? ncols)
              (write-char
                (if (= entrance (- ncols 1)) #\space #\_))
              #f)
            (newline)
            (letrec ((do-loop--1040
                       (lambda (r)
                         (if (< r 0)
                           (begin #f)
                           (begin
                             (write-char #\/)
                             (letrec ((do-loop--1041
                                        (lambda (c)
                                          (if (>= c ncols2)
                                            (begin #f)
                                            (begin
                                              (write-char
                                                (dot/space harr r (- c 1)))
                                              (display-hexbottom
                                                (vector-ref
                                                  (href/rc harr r c)
                                                  3))
                                              (do-loop--1041 (+ c 2)))))))
                               (do-loop--1041 1))
                             (if (odd? ncols)
                               (begin
                                 (write-char (dot/space harr r (- ncols 1)))
                                 (write-char #\\))
                               #f)
                             (newline)
                             (letrec ((do-loop--1042
                                        (lambda (c)
                                          (if (>= c ncols2)
                                            (begin #f)
                                            (begin
                                              (display-hexbottom
                                                (vector-ref
                                                  (href/rc harr r c)
                                                  3))
                                              (write-char
                                                (dot/space
                                                  harr
                                                  (- r 1)
                                                  (+ c 1)))
                                              (do-loop--1042 (+ c 2)))))))
                               (do-loop--1042 0))
                             (if (odd? ncols)
                               (begin
                                 (display-hexbottom
                                   (vector-ref
                                     (href/rc harr r (- ncols 1))
                                     3)))
                               (if (if (zero? r) #f #t)
                                 (begin (write-char #\\))
                                 #f))
                             (newline)
                             (do-loop--1040 (- r 1)))))))
              (do-loop--1040 (- nrows 1)))))))))

;; BIT-TEST
(define (bit-test j bit)
  (if (zero? (logand j bit)) #f #t))

;; DOT/SPACE
(define (dot/space harr r c)
  (if (if (>= r 0)
        (vector-ref (href/rc harr r c) 5)
        #f)
    #\.
    #\space))

;; DISPLAY-HEXBOTTOM
(define (display-hexbottom hexwalls)
  (begin
    (write-char
      (if (bit-test hexwalls south-west) #\\ #\space))
    (write-char
      (if (bit-test hexwalls south) #\_ #\space))
    (write-char
      (if (bit-test hexwalls south-east) #\/ #\space))))

;; MY-MAKE-BOX
(define (my-make-box r i)
  (let ((x (make-box r i)))
    (if (if (eq? (vector-ref x 4) #f) #f #t)
      (error "my-make-box" "Not #f parent" x)
      x)))

;; VECTOR-FOR-EACH
(define (vector-for-each proc v)
  (labels
    ((lp (i)
         (if (>= i 0)
           (begin (proc (vector-ref v i)) (lp (- i 1)))
           #f)))
    (lp (- (vector-length v) 1))))

;; PERMUTE-VEC!
(define (permute-vec! v random-state)
  (begin
    (labels
      ((lp (i)
           (if (> i 1)
             (begin
               (let ((elt-i (vector-ref v i))
                     (j (random-int i random-state)))
                 (begin
                   (vector-set! v i (vector-ref v j))
                   (vector-set! v j elt-i)))
               (lp (- i 1)))
             #f)))
      (lp (- (vector-length v) 1)))
    v))

;; DIG-MAZE
(define (dig-maze walls nboxs)
  (set-exit
    (an_exit1043)
    (let ()
      (begin
        (push-exit! an_exit1043 #t)
        (let ((an_exitd1044 (%get-exitd-top)))
          (labels
            ((quit (val1045)
                   (unwind-until! an_exitd1044 val1045)))
            (let ((res1046
                    (vector-for-each
                      (lambda (wall)
                        (let ((c1 (vector-ref wall 1)))
                          (let ((set1 (vector-ref c1 1)))
                            (let ((c2 (vector-ref wall 2)))
                              (let ((set2 (vector-ref c2 1)))
                                (let ()
                                  (if (if (set-equal? set1 set2) #f #t)
                                    (let ((walls (vector-ref c1 3))
                                          (wall-mask
                                            (lognot (vector-ref wall 3))))
                                      (begin
                                        (union! set1 set2)
                                        (vector-set!
                                          c1
                                          3
                                          (logand walls wall-mask))
                                        (if (= (set-size set1) nboxs)
                                          (quit #f)
                                          #f)))
                                    #f)))))))
                      walls)))
              (begin (pop-exit!) res1046))))))))

;; DFS-MAZE
(define (dfs-maze maze root do-children)
  (labels
    ((search
       (node parent)
       (begin
         (vector-set! node 4 parent)
         (do-children
           (lambda (child)
             (if (if (eq? child parent) #f #t)
               (search child node)
               #f))
           maze
           node))))
    (search root #f)))

;; REROOT-MAZE
(define (reroot-maze new-root)
  (begin
    0
    1
    2
    3
    4
    5
    65
    67
    0
    1
    2
    3
    4
    5
    65
    67
    0
    1
    2
    3
    4
    5
    65
    67
    0
    1
    2
    3
    4
    5
    65
    67
    0
    1
    2
    3
    4
    5
    65
    67
    0
    1
    2
    3
    4
    5
    65
    67
    0
    1
    2
    3
    4
    5
    65
    67
    0
    1
    2
    3
    4
    5
    65
    67
    (labels
      ((lp (node new-parent)
           (let ((old-parent (vector-ref node 4)))
             (begin
               (vector-set! node 4 new-parent)
               (if old-parent (lp old-parent node) #f)))))
      (lp new-root #f))))

;; PATH-LENGTH
(define (path-length box)
  (letrec ((do-loop--1047
             (lambda (len node)
               (if (if node #f #t)
                 (begin len)
                 (begin
                   (do-loop--1047 (+ len 1) (vector-ref node 4)))))))
    (do-loop--1047 0 (vector-ref box 4))))

;; MARK-PATH
(define (mark-path node)
  (labels
    ((lp (node)
         (begin
           (vector-set! node 5 #t)
           (let ((test-result (vector-ref node 4))
                 (thunk2
                   (lambda (_cdtest_1048) (lp _cdtest_1048)))
                 (thunk3 (lambda () #f)))
             (if test-result (thunk2 test-result) (thunk3))))))
    (lp node)))

;; RANDOM-STATE
(define (random-state n) (cons n #f))
;; RAND
(define (rand state)
  (let ((seed (car state))
        (a 48271)
        (m 268435455)
        (q 44488)
        (r 3399))
    (let ((hi (/ seed q)))
      (let ((lo (modulo seed q)))
        (let ((test (- (* a lo) (* r hi))))
          (let ((val (if (> test 0) test (+ test m))))
            (let () (begin (set-car! state val) val))))))))

;; RANDOM-INT
(define (random-int n state)
  (modulo (rand state) n))

;; BASE-SET
(define (base-set nelts) (cons nelts '()))
;; GET-SET-ROOT
(define (get-set-root s)
  (labels
    ((lp (r)
         (let ((next (cdr r)))
           (if (pair? next)
             (begin (lp next))
             (begin
               (if (if (eq? r s) #f #t)
                 (labels
                   ((lp (x)
                        (let ((next (cdr x)))
                          (if (if (eq? r next) #f #t)
                            (begin (set-cdr! x r) (lp next))
                            #f))))
                   (lp s))
                 #f)
               r)))))
    (lp s)))

;; SET-EQUAL?
(define (set-equal? s1 s2)
  (eq? (get-set-root s1) (get-set-root s2)))

;; SET-SIZE
(define (set-size s) (car (get-set-root s)))
;; UNION!
(define (union! s1 s2)
  (let ((r1 (get-set-root s1)))
    (let ((r2 (get-set-root s2)))
      (let ((n1 (set-size r1)))
        (let ((n2 (set-size r2)))
          (let ((n (+ n1 n2)))
            (let ()
              (if (> n1 n2)
                (begin (set-cdr! r2 r1) (set-car! r1 n))
                (begin (set-cdr! r1 r2) (set-car! r2 n))))))))))

;; RUN
(define (run num)
  (with-output-to-file
    "/dev/null"
    (lambda () (pmaze 500 35))))

;; DO-BENCH
(define (do-bench num)
  (if (> num 0)
    (begin (run num) (do-bench (- num 1)))
    #f))

;; MAIN
(define (main argv)
  (let ((num (if (pair? (cdr argv))
               (string->integer (cadr argv))
               1)))
    (do-bench num)))

(main '("a.kawa"))
