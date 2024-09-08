;*=====================================================================*/
;*    serrano/trashcan/cgc/Tools/set.scm                               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Feb 23 17:02:23 1995                          */
;*    Last change :  Wed Dec 27 16:19:59 2000 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The `set' package.                                               */
;*=====================================================================*/
  
;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module tools_set
   (export  (declare-set!   ::vector ::procedure ::procedure)
            (make-set!      <set>)
            (set?::bool     <obj>)
	    (set-member?    <set> <obj>)
            (set-extend!    <set> <obj>)
	    (set-remove!    <set> <obj>)
            (set-union!     <set> <set>)
	    (set-minus!     <set> <set>)
            (set-for-each   ::procedure <set>)
            (set-length     <set>)
            (set->list      <set>)
            (set->vector    <set>)))

;*---------------------------------------------------------------------*/
;*    The `set' and `meta-set' structures                              */
;*---------------------------------------------------------------------*/
(define-struct meta-set getkey table compacted-size)
(define-struct large-set the-set meta getkey)
(define-struct small-set the-set meta getkey)

;*---------------------------------------------------------------------*/
;*    max sizes                                                        */
;*---------------------------------------------------------------------*/
(define max-small-set-size 30)
(define max-large-size     10000)

;*---------------------------------------------------------------------*/
;*    declare-set! ...                                                 */
;*---------------------------------------------------------------------*/
(define (declare-set! table::vector getkey setkey)
   (let* ((cardinal  (vector-length table))
          (quotient  (quotient  cardinal 8))
          (remainder (remainder cardinal 8))
          (size      (cond
                        ((<fx cardinal max-small-set-size)
                         0)
                        ((=fx remainder 0)
                         (+fx quotient 1))
                        (else
                         (+fx quotient 2)))))
      (cond
         ((>=fx cardinal max-large-size)
          (error "declare-set!" "Too many elements in set" table))
         ((=fx 0 size)
          (let loop ((i   0)
                     (pow 1))
             (cond
                ((=fx i cardinal)
                 (meta-set getkey table size))
                (else
                 (setkey (vector-ref table i) pow)
                 (loop (+fx i 1)
                       (*fx 2 pow))))))
         (else
          (let loop ((i         0)
                     (quotient  0)
                     (mask      1))
             (cond
                ((=fx i cardinal)
                 (meta-set getkey table size))
                ((=fx mask 256)
                 (loop i (+fx quotient 1) 1))
                (else
                 (setkey (vector-ref table i) (cons quotient mask))
                 (loop (+fx i 1) quotient (*fx mask 2)))))))))

;*---------------------------------------------------------------------*/
;*    make-set! ...                                                    */
;*---------------------------------------------------------------------*/
(define (make-set! meta-set)
   (cond
      ((not (meta-set? meta-set))
       (error "make-set" "Not a meta-set" meta-set))
      ((=fx (meta-set-compacted-size meta-set) 0)
       (small-set 0 meta-set (meta-set-getkey meta-set)))
      (else
       (large-set (make-string (meta-set-compacted-size meta-set) #a000)
                  meta-set
                  (meta-set-getkey meta-set))))) 

;*---------------------------------------------------------------------*/
;*    set? ...                                                         */
;*---------------------------------------------------------------------*/
(define (set? obj)
   (or (small-set? obj) (large-set? obj)))

;*---------------------------------------------------------------------*/
;*    set-extend! ...                                                  */
;*---------------------------------------------------------------------*/
(define (set-extend! set obj)
   (define (small-set-extend!)
      (small-set-the-set-set! set
                              (bit-or (small-set-the-set set)
                                      ((small-set-getkey set) obj)))
      #unspecified)
   (define (large-set-extend!)
      (let* ((key       ((large-set-getkey set) obj))
             (the-set   (large-set-the-set set))
             (quotient  (car key))
             (mask      (cdr key)))
         (string-set! the-set
                      quotient
                      (char-or (integer->char mask)
                               (string-ref the-set quotient)))
         #unspecified))
   (cond
      ((small-set? set)
       (small-set-extend!))
      ((large-set? set)
       (large-set-extend!)) 
      (else
       (error "set-extend!" "Not a set" set))))
       
;*---------------------------------------------------------------------*/
;*    set-remove! ...                                                  */
;*---------------------------------------------------------------------*/
(define (set-remove! set obj)
   (define (small-set-remove!)
      (small-set-the-set-set! set
                              (bit-and (small-set-the-set set)
				       (bit-not ((small-set-getkey set) obj))))
      #unspecified)
   (define (large-set-remove!)
      (let* ((key       ((large-set-getkey set) obj))
             (the-set   (large-set-the-set set))
             (quotient  (car key))
             (mask      (cdr key)))
         (string-set! the-set
                      quotient
                      (char-and (string-ref the-set quotient)
				(char-not (integer->char mask))))
         #unspecified))
   (cond
      ((small-set? set)
       (small-set-remove!))
      ((large-set? set)
       (large-set-remove!))
      (else
       (error "set-remove!" "Not a set" set))))
       
;*---------------------------------------------------------------------*/
;*    set-member? ...                                                  */
;*---------------------------------------------------------------------*/
(define (set-member? set obj)
   (define (small-set-member?)
      (>fx (bit-and (small-set-the-set set) ((small-set-getkey set) obj)) 0))
   (define (large-set-member?)
      (let* ((key       ((large-set-getkey set) obj))
             (the-set   (large-set-the-set set))
             (quotient  (car key))
             (mask      (cdr key)))
         (>fx (bit-and mask (char->integer (string-ref the-set quotient)))
              0)))
   (cond
      ((small-set? set)
       (small-set-member?))
      ((large-set? set)
       (large-set-member?))
      (else
       (error "set-member?" "Not a set" set))))
       
;*---------------------------------------------------------------------*/
;*    set-union! ...                                                   */
;*    -------------------------------------------------------------    */
;*    This function returns #t if nothing as been added. Otherwise,    */
;*    it returns #f.                                                   */
;*---------------------------------------------------------------------*/
(define (set-union! dst src)
   (define (small-set-union!)
      (let ((old (small-set-the-set dst)))
         (small-set-the-set-set! dst
                                 (bit-or (small-set-the-set dst)
                                         (small-set-the-set src)))
         (not (eq? old (small-set-the-set dst)))))
   (define (large-set-union!)
      (let ((the-dst (large-set-the-set dst))
            (the-src (large-set-the-set src)))
         (let loop ((i   (-fx (meta-set-compacted-size (large-set-meta dst))
                              1))
                    (res #f))
            (if (=fx i -1)
                res
                (let ((old (string-ref the-dst i))
                      (new (char-or (string-ref the-dst i)
                                    (string-ref the-src i))))
                   (if (char=? new old)
                       (loop (-fx i 1) res)
                       (begin
                          (string-set! the-dst i new)
                          (loop (-fx i 1) #t))))))))
   (cond
      ((small-set? dst)
       (if (not (small-set? src))
           (error "set-union!" "Incompatible sets" src)
           (small-set-union!)))
      ((large-set? dst)
       (if (not (large-set? src))
           (error "set-union!" "Incompatible sets" src)
           (if (not (=fx (string-length (large-set-the-set dst))
                         (string-length (large-set-the-set src))))
               (error "set-union!" "Incompatible sets" src)
               (large-set-union!))))
      (else
       (error "set-union!" "Not a set" dst))))

;*---------------------------------------------------------------------*/
;*    set-minus! ...                                                   */
;*    -------------------------------------------------------------    */
;*    This function returns #t if nothing as been removed. Otherwise,  */
;*    it returns #f.                                                   */
;*---------------------------------------------------------------------*/
(define (set-minus! dst src)
   (define (small-set-minus!)
      (let ((old (small-set-the-set dst)))
         (small-set-the-set-set! dst
                                 (bit-and (small-set-the-set dst)
					  (bit-not (small-set-the-set src))))
         (not (eq? old (small-set-the-set dst)))))
   (define (large-set-minus!)
      (let ((the-dst (large-set-the-set dst))
            (the-src (large-set-the-set src)))
         (let loop ((i   (-fx (meta-set-compacted-size (large-set-meta dst))
                              1))
                    (res #f))
            (if (=fx i -1)
                res
                (let ((old (string-ref the-dst i))
                      (new (char-and (string-ref the-dst i)
                                     (char-not (string-ref the-src i)))))
                   (if (char=? new old)
                       (loop (-fx i 1) res)
                       (begin
                          (string-set! the-dst i new)
                          (loop (-fx i 1) #t))))))))
   (cond
      ((small-set? dst)
       (if (not (small-set? src))
           (error "set-minus!" "Incompatible sets" src)
           (small-set-minus!)))
      ((large-set? dst)
       (if (not (large-set? src))
           (error "set-minus!" "Incompatible sets" src)
           (if (not (=fx (string-length (large-set-the-set dst))
                         (string-length (large-set-the-set src))))
               (error "set-minus!" "Incompatible sets" src)
               (large-set-minus!))))
      (else
       (error "set-minus!" "Not a set" dst))))
                    
;*---------------------------------------------------------------------*/
;*    set-for-each ...                                                 */
;*---------------------------------------------------------------------*/
(define (set-for-each proc set)
   (define (small-set-member? set obj)
      (>fx (bit-and (small-set-the-set set) ((small-set-getkey set) obj)) 0))
   (define (small-set-for-each)
      (if (=fx (small-set-the-set set) 0)
          #unspecified
          (let* ((meta  (small-set-meta set))
                 (table (meta-set-table meta)))
             [assert (table)
                     (<fx (small-set-the-set set)
			  (bit-lsh 1 (vector-length table)))]
             (let loop ((i (-fx (vector-length table) 1)))
                (cond
                   ((=fx i -1)
                    #unspecified)
                   ((>fx (bit-and (small-set-the-set set) (bit-lsh 1 i)) 0)
                    [assert (i) (set-member? set (vector-ref table i))]
                    [assert (i)
                            (let* ((obj (vector-ref table i))
                                   (key ((small-set-getkey set) obj)))
                               (=fx key (bit-lsh 1 i)))]
                    (proc (vector-ref table i))
                    (loop (-fx i 1)))
                   (else
                    [assert (i) (not (set-member? set (vector-ref table i)))]
                    [assert (i)
                            (let* ((obj (vector-ref table i))
                                   (key ((small-set-getkey set) obj)))
                               (=fx key (bit-lsh 1 i)))]
                    (loop (-fx i 1))))))))
   (define (large-set-for-each)
      (let* ((meta  (large-set-meta set))
             (table (meta-set-table meta)))
         (let loop ((i (-fx (vector-length table) 1)))
            (cond
               ((=fx i -1)
                #unspecified)
               ((set-member? set (vector-ref table i))
                (proc (vector-ref table i))
                (loop (-fx i 1)))
               (else
                (loop (-fx i 1)))))))
   (cond
      ((small-set? set)
       (small-set-for-each))
      ((large-set? set)
       (large-set-for-each))
      (else
       (error "set-for-each" "Not a set" set))))

;*---------------------------------------------------------------------*/
;*    set-length ...                                                   */
;*---------------------------------------------------------------------*/
(define (set-length set)
   (define (small-set-length)
      (let loop ((the-set (small-set-the-set set))
                 (num     0))
         (cond
            ((=fx the-set 0)
             num)
            (else
             (loop (bit-rsh the-set 1) (+fx num (bit-and the-set 1)))))))
   (define (large-set-length)
      (let* ((the-set (large-set-the-set set))
             (the-len (string-length the-set)))
         (let loop ((offset 0)
                    (num    0))
            (if (=fx offset the-len)
                num
                (let liip ((char (char->integer (string-ref the-set offset)))
                           (num  num))
                   (cond
                      ((=fx char 0)
                       (loop (+fx 1 offset) num))
                      (else
                       (liip (bit-rsh char 1)
                             (+fx num (bit-and char 1))))))))))
   (cond
      ((small-set? set)
       (small-set-length))
      ((large-set? set)
       (large-set-length))
      (else
       (error "set-length" "Not a set" set))))
    
;*---------------------------------------------------------------------*/
;*    set->list ...                                                    */
;*---------------------------------------------------------------------*/
(define (set->list set)
   (let ((meta (cond
                  ((small-set? set)
                   (small-set-meta set))
                  ((large-set? set)
                   (large-set-meta set))
                  (else
                   (error "set-for-each"
			  "argument not a set"
			  set)))))
      (let* ((table (meta-set-table meta))
             (size  (vector-length table)))
         (let loop ((i 0)
                    (l '()))
            (cond
               ((=fx i size)
                l)
               ((set-member? set (vector-ref table i))
                (loop (+fx i 1) (cons (vector-ref table i) l)))
               (else
                (loop (+fx i 1) l)))))))

;*---------------------------------------------------------------------*/
;*    set->vector ...                                                  */
;*---------------------------------------------------------------------*/
(define (set->vector set)
   (list->vector (set->list set)))
