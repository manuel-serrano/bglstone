(module puzzle (main main))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; file:         puzzle.sch
; description:  puzzle benchmark
; author:       richard gabriel, after forrest baskett
; created:      12-apr-85
; modified:     12-apr-85 14:20:23 (bob shaw)
;               11-aug-87 (will clinger)
;               22-jan-88 (will clinger)
; language:     scheme
; status:       public domain
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (piota n)
  (do ((n n (-fx n 1))
       (list '() (cons (-fx n 1) list)))
      ((zerofx? n) list)))

;;; puzzle -- forest baskett's puzzle benchmark, originally written in pascal.

(define (call-cc thunk)
   (bind-exit (exit)
      (thunk exit)))

(define size 1022)
(define classmax 6)
(define typemax 13)

(define *iii* 0)
(define *kount* 0)
(define *d* 8)

(define *piececount* (make-vector (+fx classmax 1) 0))
(define *class* (make-vector (+fx typemax 1) 0))
(define *piecemax* (make-vector (+fx typemax 1) 0))
(define *puzzle* (make-vector (+fx size 1)))
(define *p* (make-vector (+fx typemax 1)))

(define (fit i j)
  (let ((end (vector-ref *piecemax* i)))
    (do ((k 0 (+fx k 1)))
        ((or (>fx k end)
             (and (vector-ref (vector-ref *p* i) k)
                  (vector-ref *puzzle* (+fx j k))))
         (if (>fx k end) #t #f)))))

(define (place i j)
  (let ((end (vector-ref *piecemax* i)))
    (do ((k 0 (+fx k 1)))
        ((>fx k end))
        (cond ((vector-ref (vector-ref *p* i) k)
               (vector-set! *puzzle* (+fx j k) #t)
               #t)))
    (vector-set! *piececount*
                 (vector-ref *class* i)
                 (-fx (vector-ref *piececount* (vector-ref *class* i)) 1))
    (do ((k j (+fx k 1)))
        ((or (>fx k size) (not (vector-ref *puzzle* k)))
         ;        (newline)
         ;        (display "*puzzle* filled")
         (if (>fx k size) 0 k)))))

(define (puzzle-remove i j)
  (let ((end (vector-ref *piecemax* i)))
    (do ((k 0 (+fx k 1)))
        ((>fx k end))
        (cond ((vector-ref (vector-ref *p* i) k)
               (vector-set! *puzzle* (+fx j k) #f)
               #f)))
    (vector-set! *piececount*
                 (vector-ref *class* i)
                 (+fx (vector-ref *piececount* (vector-ref *class* i)) 1))))

(define (trial j)
   (let ((k 0))
      (let ((return #f))
	 (do ((i 0 (+fx i 1)))
	       ((or return (>fx i typemax))
		(if return
		    return
		    (begin
		       (set! *kount* (+fx *kount* 1)) #f)))
	    (cond
	       ((not
		 (zerofx?
		  (vector-ref *piececount* (vector-ref *class* i))))
		(cond
		   ((fit i j)
		    (set! k (place i j))
		    (cond
		       ((or (trial k) (zerofx? k))
			;; (trial-output (+fx i 1) (+fx k 1))
			(set! *kount* (+fx *kount* 1))
			(set! return #t))
		       (else (puzzle-remove i j)))))))))))

(define (trial-bis j)
  (let ((k 0))
    (call-cc
     (lambda (return)
       (do ((i 0 (+fx i 1)))
           ((>fx i typemax) (set! *kount* (+fx *kount* 1)) #f)
           (cond
            ((not
              (zerofx?
               (vector-ref *piececount* (vector-ref *class* i))))
             (cond
              ((fit i j)
               (set! k (place i j))
               (cond
                ((or (trial k) (zerofx? k))
                 ;(trial-output (+fx i 1) (+fx k 1))
                 (set! *kount* (+fx *kount* 1))
                 (return #t))
                (else (puzzle-remove i j))))))))))))

(define (trial-output x y)
  (newline)
  (display (string-append "piece "
                          (number->string x)
                          " at "
                          (number->string y)
                          ".")))

(define (definepiece iclass ii jj kk)
  (let ((index 0))
    (do ((i 0 (+fx i 1)))
        ((>fx i ii))
        (do ((j 0 (+fx j 1)))
            ((>fx j jj))
            (do ((k 0 (+fx k 1)))
                ((>fx k kk))
                (set! index (+fx i (*fx *d* (+fx j (*fx *d* k)))))
                (vector-set! (vector-ref *p* *iii*) index  #t))))
    (vector-set! *class* *iii* iclass)
    (vector-set! *piecemax* *iii* index)
    (cond ((not (=fx *iii* typemax))
           (set! *iii* (+fx *iii* 1))))))

(define (start)
  (do ((m 0 (+fx m 1)))
      ((>fx m size))
      (vector-set! *puzzle* m #t))
  (do ((i 1 (+fx i 1)))
      ((>fx i 5))
      (do ((j 1 (+fx j 1)))
          ((>fx j 5))
          (do ((k 1 (+fx k 1)))
              ((>fx k 5))
              (vector-set! *puzzle* (+fx i (*fx *d* (+fx j (*fx *d* k)))) #f))))
  (do ((i 0 (+fx i 1)))
      ((>fx i typemax))
      (do ((m 0 (+fx m 1)))
          ((>fx m size))
          (vector-set! (vector-ref *p* i) m #f)))
  (set! *iii* 0)
  (definepiece 0 3 1 0)
  (definepiece 0 1 0 3)
  (definepiece 0 0 3 1)
  (definepiece 0 1 3 0)
  (definepiece 0 3 0 1)
  (definepiece 0 0 1 3)
  
  (definepiece 1 2 0 0)
  (definepiece 1 0 2 0)
  (definepiece 1 0 0 2)
  
  (definepiece 2 1 1 0)
  (definepiece 2 1 0 1)
  (definepiece 2 0 1 1)
  
  (definepiece 3 1 1 1)
  
  (vector-set! *piececount* 0 13)
  (vector-set! *piececount* 1 3)
  (vector-set! *piececount* 2 1)
  (vector-set! *piececount* 3 1)
  (let ((m (+fx (*fx *d* (+fx *d* 1)) 1))
        (n 0))
    (cond ((fit 0 m) (set! n (place 0 m)))
          (else (begin (newline) (display "error."))))
    (cond ((trial n)
;           (begin (newline)
;                  (display "success in ")
;                  (write *kount*)
;                  (display " trials."))
           *kount*)
          (else (begin (newline) (display "failure."))))))

(for-each (lambda (i) (vector-set! *p* i (make-vector (+fx size 1))))
          (piota (+fx typemax 1)))

;;; call:  (start)

(define (repeat num thunk)
   (if (<=fx num 1)
       (thunk)
       (begin
	  (thunk)
	  (repeat (-fx num 1) thunk))))

(define (main argv)
   (let ((n (if (pair? (cdr argv)) (string->integer (cadr argv)) 700)))
      (let ((r (repeat n start)))
	 (exit (if (=fx (/fx r n) 101805) 0 1)))))
