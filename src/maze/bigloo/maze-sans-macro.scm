;; =========================================================
;; The expanded module
;; Bigloo (3.6b)
;; Inria -- Sophia Antipolis        Tue Jun 7 21:07:22 CEST 2011 
;; (bigloo -expand maze.scm)
;; =========================================================

;; ---------------------------------------------------------
;; !!! WARNING !!!      !!! WARNING !!!      !!! WARNING !!!
;; ---------------------------------------------------------
;; This expanded file cannot be compiled "as is". In order to
;; compile it:
;;    - the explicit call to the MODULE-INITIALIZATION 
;;      must be removed.
;;    - If the source module was INCLUDING files,
;;      you must select manually which files still have to
;;      be included in the expanded forms.
;; ---------------------------------------------------------
;; The module clause
(module maze (main main))

;; unit: toplevel
;; bitand
(define (bitand x y) (bit-and x y))
;; bitor
(define (bitor x y) (bit-or x y))
;; bitnot
(define (bitnot x) (bit-not x))
(define (alloc-harr)
   (make-vector 4 'unspecified))
(define (harr? x)
   (let ((_andtest_1518 (vector? x)))
      (if _andtest_1518
	  (let ((_andtest_1519 (=fx (vector-length x) 4)))
	     (if _andtest_1519
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

(define (alloc-wall)
   (make-vector 4 'unspecified))
(define (wall? x)
   (let ((_andtest_1538 (vector? x)))
      (if _andtest_1538
	  (let ((_andtest_1539 (=fx (vector-length x) 4)))
	     (if _andtest_1539
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

(define (alloc-box) (make-vector 6 'unspecified))
(define (box? x)
   (let ((_andtest_1558 (vector? x)))
      (if _andtest_1558
	  (let ((_andtest_1559 (=fx (vector-length x) 6)))
	     (if _andtest_1559 (eq? (vector-ref x 0) 'box) #f))
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
;; harr
(define (harr r c)
  (make-harr r c (make-vector (*fx r c))))

;; href
(define (href ha x y)
  (let ((r (/fx y 2)) (c (/fx x 3)))
    (vector-ref
      (vector-ref ha 3)
      (+fx (*fx (vector-ref ha 2) r) c))))

;; hset!
(define (hset! ha x y val)
  (let ((r (/fx y 2)) (c (/fx x 3)))
    (vector-set!
      (vector-ref ha 3)
      (+fx (*fx (vector-ref ha 2) r) c)
      val)))

;; href/rc
(define (href/rc ha r c)
  (vector-ref
    (vector-ref ha 3)
    (+fx (*fx (vector-ref ha 2) r) c)))

;; harr-tabulate
(define (harr-tabulate nrows ncols proc)
  (let ((v (make-vector (*fx nrows ncols))))
    (begin
      (letrec ((do-loop--1590
                 (lambda (r)
                    (if (<fx r 0)
                      #f
                      (begin
                        (letrec ((do-loop--1591
                                   (lambda (c i)
                                      (if (=fx c ncols)
                                        #f
                                        (begin
                                          (vector-set!
                                            v
                                            i
                                            (proc (*fx 3 c)
                                                  (+fx (*fx 2 r) (bitand c 1))))
                                          (do-loop--1591
                                            (+fx c 1)
                                            (+fx i 1)))))))
                          (do-loop--1591 0 (*fx r ncols)))
                        (do-loop--1590 (-fx r 1)))))))
        (do-loop--1590 (-fx nrows 1)))
      (make-harr nrows ncols v))))

;; harr-for-each
(define (harr-for-each proc harr)
  (vector-for-each proc (vector-ref harr 3)))

;; south-west
(define south-west 1)
;; south
(define south 2)
;; south-east
(define south-east 4)
;; gen-maze-array
(define (gen-maze-array r c)
  (harr-tabulate
    r
    c
    (lambda (x y)
       (my-make-box (base-set 1) (cons x y)))))

;; make-wall-vec
(define (make-wall-vec harr)
  (let ((nrows (vector-ref harr 1)))
    (let ((ncols (vector-ref harr 2)))
      (let ((xmax (*fx 3 (-fx ncols 1))))
        (let ((walls '()))
          (let ((add-wall
                  (lambda (o n b)
                     (set! walls (cons (make-wall o n b) walls)))))
            (let ()
              (begin
                (letrec ((do-loop--1592
                           (lambda (x)
                              (if (<fx x 0)
                                #f
                                (begin
                                  (letrec ((do-loop--1593
                                             (lambda (y)
                                                (if (<=fx y 1)
                                                  #f
                                                  (begin
                                                    (let ((hex (href harr x y)))
                                                      (begin
                                                        (if (if (zerofx? x)
                                                              #f
                                                              #t)
                                                          (add-wall
                                                            hex
                                                            (href harr
                                                                  (-fx x 3)
                                                                  (-fx y 1))
                                                            south-west)
                                                          #f)
                                                        (add-wall
                                                          hex
                                                          (href harr
                                                                x
                                                                (-fx y 2))
                                                          south)
                                                        (if (<fx x xmax)
                                                          (add-wall
                                                            hex
                                                            (href harr
                                                                  (+fx x 3)
                                                                  (-fx y 1))
                                                            south-east)
                                                          #f)))
                                                    (do-loop--1593
                                                      (-fx y 2)))))))
                                    (do-loop--1593
                                      (+fx (*fx (-fx nrows 1) 2) (bitand x 1))))
                                  (do-loop--1592 (-fx x 3)))))))
                  (do-loop--1592 (*fx (-fx ncols 1) 3)))
                (if (>fx ncols 1)
                  (let ((rmoc-x (+fx 3 (*fx 6 (/fx (-fx ncols 2) 2)))))
                    (begin
                      (let ((rmoc-hex (href harr rmoc-x 1)))
                        (begin
                          (if (<fx rmoc-x xmax)
                            (add-wall rmoc-hex (href harr xmax 0) south-east)
                            #f)
                          (add-wall
                            rmoc-hex
                            (href harr (-fx rmoc-x 3) 0)
                            south-west)))
                      (letrec ((do-loop--1594
                                 (lambda (x)
                                    (if (<fx x 3)
                                      #f
                                      (begin
                                        (add-wall
                                          (href harr x 1)
                                          (href harr (-fx x 3) 0)
                                          south-west)
                                        (add-wall
                                          (href harr x 1)
                                          (href harr (+fx x 3) 0)
                                          south-east)
                                        (do-loop--1594 (-fx x 6)))))))
                        (do-loop--1594 (-fx rmoc-x 6)))))
                  #f)
                (list->vector walls)))))))))

;; pick-entrances
(define (pick-entrances harr)
  (begin
    (dfs-maze
      harr
      (href/rc harr 0 0)
      for-each-hex-child)
    (let ((nrows (vector-ref harr 1))
          (ncols (vector-ref harr 2)))
      (let ((g1595 (-fx ncols 1)))
        (letrec ((tp-lp (lambda (max-len entrance exit tcol)
                           (if (<fx tcol 0)
                             (vector entrance exit)
                             (let ((top-box (href/rc harr (-fx nrows 1) tcol)))
                               (begin
                                 (reroot-maze top-box)
                                 (let ((g1 (let ((g1596 (-fx ncols 1)))
                                             (letrec ((bt-lp (lambda (max-len entrance exit bcol)
                                                                (if (<fx bcol 0)
                                                                  (vector
                                                                    max-len
                                                                    entrance
                                                                    exit)
                                                                  (let ((this-len
                                                                          (path-length
                                                                            (href/rc
                                                                              harr
                                                                              0
                                                                              bcol))))
                                                                    (if (>fx this-len
                                                                             max-len)
                                                                      (bt-lp this-len
                                                                             tcol
                                                                             bcol
                                                                             (-fx bcol
                                                                                  1))
                                                                      (bt-lp max-len
                                                                             entrance
                                                                             exit
                                                                             (-fx bcol
                                                                                  1))))))))
                                               (bt-lp max-len
                                                      entrance
                                                      exit
                                                      g1596)))))
                                   (let ((exit (vector-ref g1 2))
                                         (entrance (vector-ref g1 1))
                                         (max-len (vector-ref g1 0)))
                                     (tp-lp max-len
                                            entrance
                                            exit
                                            (-fx tcol 1))))))))))
          (tp-lp -1 #f #f g1595))))))

;; for-each-hex-child
(define (for-each-hex-child proc harr box)
  (let ((walls (vector-ref box 3)))
    (let ((id (vector-ref box 2)))
      (let ((x (car id)))
        (let ((y (cdr id)))
          (let ((nr (vector-ref harr 1)))
            (let ((nc (vector-ref harr 2)))
              (let ((maxy (*fx 2 (-fx nr 1))))
                (let ((maxx (*fx 3 (-fx nc 1))))
                  (let ()
                    (begin
                      (if (if (bit-test walls south-west) #f #t)
                        (proc (href harr (-fx x 3) (-fx y 1)))
                        #f)
                      (if (if (bit-test walls south) #f #t)
                        (proc (href harr x (-fx y 2)))
                        #f)
                      (if (if (bit-test walls south-east) #f #t)
                        (proc (href harr (+fx x 3) (-fx y 1)))
                        #f)
                      (if (if (>fx x 0)
                            (let ((_ortest_1597 (<=fx y maxy)))
                              (if _ortest_1597
                                _ortest_1597
                                (zerofx? (modulo x 6))))
                            #f)
                        (let ((nw (href harr (-fx x 3) (+fx y 1))))
                          (if (if (bit-test (vector-ref nw 3) south-east)
                                #f
                                #t)
                            (proc nw)
                            #f))
                        #f)
                      (if (<fx y maxy)
                        (let ((n (href harr x (+fx y 2))))
                          (if (if (bit-test (vector-ref n 3) south) #f #t)
                            (proc n)
                            #f))
                        #f)
                      (if (if (<fx x maxx)
                            (let ((_ortest_1598 (<=fx y maxy)))
                              (if _ortest_1598
                                _ortest_1598
                                (zerofx? (modulo x 6))))
                            #f)
                        (let ((ne (href harr (+fx x 3) (+fx y 1))))
                          (if (if (bit-test (vector-ref ne 3) south-west)
                                #f
                                #t)
                            (proc ne)
                            #f))
                        #f))))))))))))

;; make-maze
(define (make-maze nrows ncols)
  (let ((boxs (gen-maze-array nrows ncols)))
    (let ((walls (permute-vec!
                   (make-wall-vec boxs)
                   (random-state 20))))
      (let ()
        (begin
          (dig-maze walls (*fx nrows ncols))
          (let ((g1 (pick-entrances boxs)))
            (let ((exit (vector-ref g1 1))
                  (entrance (vector-ref g1 0)))
              (let ((exit-box (href/rc boxs 0 exit)))
                (let ((walls (vector-ref exit-box 3)))
                  (let ()
                    (begin
                      (reroot-maze
                        (href/rc boxs (-fx nrows 1) entrance))
                      (mark-path exit-box)
                      (vector-set!
                        exit-box
                        3
                        (bitand walls (bitnot south)))
                      (vector boxs entrance exit))))))))))))

;; pmaze
(define (pmaze nrows ncols)
  (let ((g1 (make-maze nrows ncols)))
    (let ((exit (vector-ref g1 2))
          (entrance (vector-ref g1 1))
          (boxs (vector-ref g1 0)))
      (print-hexmaze boxs entrance))))

;; print-hexmaze
(define (print-hexmaze harr entrance)
  (let ((nrows (vector-ref harr 1)))
    (let ((ncols (vector-ref harr 2)))
      (let ((ncols2 (*fx 2 (/fx ncols 2))))
        (let ()
          (begin
            (letrec ((do-loop--1599
                       (lambda (c)
                          (if (>=fx c ncols)
                            #f
                            (begin
                              (display "   ")
                              (write-char (if (=fx c entrance) #\space #\_))
                              (do-loop--1599 (+fx c 2)))))))
              (do-loop--1599 1))
            (newline)
            (write-char #\space)
            (letrec ((do-loop--1600
                       (lambda (c)
                          (if (>=fx c ncols2)
                            #f
                            (begin
                              (display*
                                (if (=fx c entrance) #\space #\_)
                                "/"
                                (dot/space harr (-fx nrows 1) (+fx c 1))
                                "\\")
                              (do-loop--1600 (+fx c 2)))))))
              (do-loop--1600 0))
            (if (odd? ncols)
              (write-char
                (if (=fx entrance (-fx ncols 1)) #\space #\_))
              #f)
            (newline)
            (letrec ((do-loop--1601
                       (lambda (r)
                          (if (<fx r 0)
                            #f
                            (begin
                              (write-char #\/)
                              (letrec ((do-loop--1602
                                         (lambda (c)
                                            (if (>=fx c ncols2)
                                              #f
                                              (begin
                                                (write-char
                                                  (dot/space harr r (-fx c 1)))
                                                (display-hexbottom
                                                  (vector-ref
                                                    (href/rc harr r c)
                                                    3))
                                                (do-loop--1602 (+fx c 2)))))))
                                (do-loop--1602 1))
                              (if (odd? ncols)
                                (begin
                                  (write-char (dot/space harr r (-fx ncols 1)))
                                  (write-char #\\))
                                #f)
                              (newline)
                              (letrec ((do-loop--1603
                                         (lambda (c)
                                            (if (>=fx c ncols2)
                                              #f
                                              (begin
                                                (display-hexbottom
                                                  (vector-ref
                                                    (href/rc harr r c)
                                                    3))
                                                (write-char
                                                  (dot/space
                                                    harr
                                                    (-fx r 1)
                                                    (+fx c 1)))
                                                (do-loop--1603 (+fx c 2)))))))
                                (do-loop--1603 0))
                              (if (odd? ncols)
                                (display-hexbottom
                                  (vector-ref (href/rc harr r (-fx ncols 1)) 3))
                                (if (if (zerofx? r) #f #t) (write-char #\\) #f))
                              (newline)
                              (do-loop--1601 (-fx r 1)))))))
              (do-loop--1601 (-fx nrows 1)))))))))

;; bit-test
(define (bit-test j bit)
  (if (zerofx? (bitand j bit)) #f #t))

;; dot/space
(define (dot/space harr r c)
  (if (if (>=fx r 0)
        (vector-ref (href/rc harr r c) 5)
        #f)
    #\.
    #\space))

;; display-hexbottom
(define (display-hexbottom hexwalls)
  (begin
    (write-char
      (if (bit-test hexwalls south-west) #\\ #\space))
    (write-char
      (if (bit-test hexwalls south) #\_ #\space))
    (write-char
      (if (bit-test hexwalls south-east) #\/ #\space))))

;; my-make-box
(define (my-make-box r i)
  (let ((x (make-box r i)))
    (if (if (eq? (vector-ref x 4) #f) #f #t)
      (error "my-make-box" "Not #f parent" x)
      x)))

;; permute-vec!
(define (permute-vec! v random-state)
  (begin
    (let ((g1605 (-fx (vector-length v) 1)))
      (letrec ((lp (lambda (i)
                      (if (>fx i 1)
                        (begin
                          (let ((elt-i (vector-ref v i))
                                (j (random-int i random-state)))
                            (begin
                              (vector-set! v i (vector-ref v j))
                              (vector-set! v j elt-i)))
                          (lp (-fx i 1)))
                        #f))))
        (lp g1605)))
    v))

;; dig-maze
(define (dig-maze walls nboxs)
   (bind-exit (quit)
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
					(bitnot (vector-ref wall 3))))
				  (begin
				     (union! set1 set2)
				     (vector-set!
					c1
					3
					(bitand walls wall-mask))
				     (if (=fx (set-size set1) nboxs)
					 (quit #f)
					 #f)))
			       #f)))))))
	 walls)))

;; dfs-maze
(define (dfs-maze maze root do-children)
  (letrec ((search
             (lambda (node parent)
                (begin
                  (vector-set! node 4 parent)
                  (do-children
                    (lambda (child)
                       (if (if (eq? child parent) #f #t)
                         (search child node)
                         #f))
                    maze
                    node)))))
    (search root #f)))

;; reroot-maze
(define (reroot-maze new-root)
  (letrec ((lp (lambda (node new-parent)
                  (let ((old-parent (vector-ref node 4)))
                    (begin
                      (vector-set! node 4 new-parent)
                      (if old-parent (lp old-parent node) #f))))))
    (lp new-root #f)))

;; path-length
(define (path-length box)
  (letrec ((do-loop--1610
             (lambda (len node)
                (if (if node #f #t)
                  len
                  (do-loop--1610 (+fx len 1) (vector-ref node 4))))))
    (do-loop--1610 0 (vector-ref box 4))))

;; mark-path
(define (mark-path node)
  (letrec ((lp (lambda (node)
                  (begin
                    (vector-set! node 5 #t)
                    (let ((g1611 (vector-ref node 4)))
                      (if g1611 (let ((g1612 g1611)) (lp g1612)) #f))))))
    (lp node)))

(define (fx28 a)
   (bit-and a (-fx (bit-lsh 1 28) 1)))

;; random-state
(define (random-state n) (cons n #f))
;; rand
(define (rand state)
  (let ((seed (car state))
        (A 48271)
        (M 268435455)
        (Q 44488)
        (R 3399))
    (let ((hi (fx28 (/fx seed Q))))
      (let ((lo (fx28 (modulofx seed Q))))
        (let ((test (fx28 (-fx (fx28 (*fx A lo)) (fx28 (*fx R hi))))))
          (let ((val (if (>fx test 0) test (fx28 (+fx test M)))))
            (let () (begin (set-car! state val) val))))))))

;; random-int
(define (random-int n state)
  (fx28 (modulofx (rand state) n)))

;; base-set
(define (base-set nelts) (cons nelts '()))
;; get-set-root
(define (get-set-root s)
  (letrec ((lp (lambda (r)
                  (let ((next (cdr r)))
                    (if (pair? next)
                      (lp next)
                      (begin
                        (if (if (eq? r s) #f #t)
                          (letrec ((lp (lambda (x)
                                          (let ((next (cdr x)))
                                            (if (if (eq? r next) #f #t)
                                              (begin (set-cdr! x r) (lp next))
                                              #f)))))
                            (lp s))
                          #f)
                        r))))))
    (lp s)))

;; set-equal?
(define (set-equal? s1 s2)
  (eq? (get-set-root s1) (get-set-root s2)))

;; set-size
(define (set-size s) (car (get-set-root s)))
;; union!
(define (union! s1 s2)
  (let ((r1 (get-set-root s1)))
    (let ((r2 (get-set-root s2)))
      (let ((n1 (set-size r1)))
        (let ((n2 (set-size r2)))
          (let ((n (+fx n1 n2)))
            (let ()
              (if (>fx n1 n2)
                (begin (set-cdr! r2 r1) (set-car! r1 n))
                (begin (set-cdr! r1 r2) (set-car! r2 n))))))))))

;; run
(define (run num)
  (with-output-to-file
    "/dev/null"
    (lambda () (pmaze 500 35))))

;; do-bench
(define (do-bench num)
  (if (>fx num 0)
    (begin (run num) (do-bench (-fx num 1)))
    #f))

;; main
(define (main argv)
   (let ((num (if (pair? (cdr argv))
		  (string->integer (cadr argv))
		  100)))
      (when (=fx num 1)
	 (newline)
	 (pmaze 5 35))
      (do-bench num)
      (exit 0)))


