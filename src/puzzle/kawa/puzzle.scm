;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; File:         puzzle.sch
; Description:  PUZZLE benchmark
; Author:       Richard Gabriel, after Forrest Baskett
; Created:      12-Apr-85
; Modified:     12-Apr-85 14:20:23 (Bob Shaw)
;               11-Aug-87 (Will Clinger)
;               22-Jan-88 (Will Clinger)
; Language:     Scheme
; Status:       Public Domain
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (iota n)
  (do ((n n (- n 1))
       (list '() (cons (- n 1) list)))
      ((zero? n) list)))

;;; PUZZLE -- Forest Baskett's Puzzle benchmark, originally written in Pascal.

(define size 1022)
(define classmax 6)
(define typemax 13)

(define *iii* 0)
(define *kount* 0)
(define *d* 8)

(define *piececount* (make-vector (+ classmax 1) 0))
(define *class* (make-vector (+ typemax 1) 0))
(define *piecemax* (make-vector (+ typemax 1) 0))
(define *puzzle* (make-vector (+ size 1)))
(define *p* (make-vector (+ typemax 1)))

(define (fit i j)
  (let ((end (vector-ref *piecemax* i)))
    (do ((k 0 (+ k 1)))
        ((or (> k end)
             (and (vector-ref (vector-ref *p* i) k)
                  (vector-ref *puzzle* (+ j k))))
         (if (> k end) #t #f)))))

(define (place i j)
  (let ((end (vector-ref *piecemax* i)))
    (do ((k 0 (+ k 1)))
        ((> k end))
        (cond ((vector-ref (vector-ref *p* i) k)
               (vector-set! *puzzle* (+ j k) #t)
               #t)))
    (vector-set! *piececount*
                 (vector-ref *class* i)
                 (- (vector-ref *piececount* (vector-ref *class* i)) 1))
    (do ((k j (+ k 1)))
        ((or (> k size) (not (vector-ref *puzzle* k)))
         ;        (newline)
         ;        (display "*Puzzle* filled")
         (if (> k size) 0 k)))))

(define (puzzle-remove i j)
  (let ((end (vector-ref *piecemax* i)))
    (do ((k 0 (+ k 1)))
        ((> k end))
        (cond ((vector-ref (vector-ref *p* i) k)
               (vector-set! *puzzle* (+ j k) #f)
               #f)))
    (vector-set! *piececount*
                 (vector-ref *class* i)
                 (+ (vector-ref *piececount* (vector-ref *class* i)) 1))))

(define (trial j)
   (let ((k 0))
      (let ((return #f))
	 (do ((i 0 (+ i 1)))
	       ((or return (> i typemax))
		(if return
		    return
		    (begin
		       (set! *kount* (+ *kount* 1)) #f)))
	    (cond
	       ((not
		 (zero?
		  (vector-ref *piececount* (vector-ref *class* i))))
		(cond
		   ((fit i j)
		    (set! k (place i j))
		    (cond
		       ((or (trial k) (zero? k))
			;; (trial-output (+ i 1) (+ k 1))
			(set! *kount* (+ *kount* 1))
			(set! return #t))
		       (else (puzzle-remove i j)))))))))))

(define (trial-bis j)
  (let ((k 0))
    (call-with-current-continuation
     (lambda (return)
       (do ((i 0 (+ i 1)))
           ((> i typemax) (set! *kount* (+ *kount* 1)) #f)
           (cond
            ((not
              (zero?
               (vector-ref *piececount* (vector-ref *class* i))))
             (cond
              ((fit i j)
               (set! k (place i j))
               (cond
                ((or (trial k) (zero? k))
                 ;(trial-output (+ i 1) (+ k 1))
                 (set! *kount* (+ *kount* 1))
                 (return #t))
                (else (puzzle-remove i j))))))))))))

(define (trial-output x y)
  (newline)
  (display (string-append "Piece "
                          (number->string x)
                          " at "
                          (number->string y)
                          ".")))

(define (definepiece iclass ii jj kk)
  (let ((index 0))
    (do ((i 0 (+ i 1)))
        ((> i ii))
        (do ((j 0 (+ j 1)))
            ((> j jj))
            (do ((k 0 (+ k 1)))
                ((> k kk))
                (set! index (+ i (* *d* (+ j (* *d* k)))))
                (vector-set! (vector-ref *p* *iii*) index  #t))))
    (vector-set! *class* *iii* iclass)
    (vector-set! *piecemax* *iii* index)
    (cond ((not (= *iii* typemax))
           (set! *iii* (+ *iii* 1))))))

(define (start)
  (do ((m 0 (+ m 1)))
      ((> m size))
      (vector-set! *puzzle* m #t))
  (do ((i 1 (+ i 1)))
      ((> i 5))
      (do ((j 1 (+ j 1)))
          ((> j 5))
          (do ((k 1 (+ k 1)))
              ((> k 5))
              (vector-set! *puzzle* (+ i (* *d* (+ j (* *d* k)))) #f))))
  (do ((i 0 (+ i 1)))
      ((> i typemax))
      (do ((m 0 (+ m 1)))
          ((> m size))
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
  (let ((m (+ (* *d* (+ *d* 1)) 1))
        (n 0))
    (cond ((fit 0 m) (set! n (place 0 m)))
          (else (begin (newline) (display "Error."))))
    (cond ((trial n)
;           (begin (newline)
;                  (display "Success in ")
;                  (write *kount*)
;                  (display " trials."))
           *kount*)
          (else (begin (newline) (display "Failure."))))))

(for-each (lambda (i) (vector-set! *p* i (make-vector (+ size 1))))
          (iota (+ typemax 1)))

;;; call:  (start)

(define (repeat num thunk)
   (if (<= num 1)
       (thunk)
       (begin
	  (thunk)
	  (repeat (- num 1) thunk))))

(define (run num)
   (display (repeat num start))
   (display "...101805 expected")
   (newline))

(define (main argv)
   (run 50))

(main '("titi"))
