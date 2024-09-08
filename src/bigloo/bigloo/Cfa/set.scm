;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Cfa/set.scm                 */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Feb 23 17:02:23 1995                          */
;*    Last change :  Mon May 15 07:34:52 2000 (serrano)                */
;*    Copyright   :  1995-2000 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The `set' package.                                               */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module cfa_set
   (import  type_type
	    ast_var
	    ast_node
	    cfa_info
	    cfa_info2
	    tools_shape
	    tools_error
	    (node-key cfa_approx)
	    (node-key-set! cfa_approx))
   (export  (declare-set! ::vector)
	    (make-set!    <set>)
	    (set?::bool   <obj>)
	    (set-extend!  <set>    <obj>)
	    (set-union!   <set>  . <set>*)
	    (set-for-each ::procedure <set>)
	    (set-length   <set>)
	    (set->list    <set>)
	    (set->vector  <set>)))

;*---------------------------------------------------------------------*/
;*    The `set' and `meta-set' structures                              */
;*---------------------------------------------------------------------*/
(define-struct meta-set  table compacted-size)
(define-struct large-set the-set meta)

;*---------------------------------------------------------------------*/
;*    max sizes                                                        */
;*---------------------------------------------------------------------*/
(define max-large-size     10000)

;*---------------------------------------------------------------------*/
;*    declare-set! ...                                                 */
;*---------------------------------------------------------------------*/
(define (declare-set! table)
   (let* ((cardinal  (vector-length table))
	  (quotient  (quotient  cardinal 8))
	  (remainder (remainder cardinal 8))
	  (size      (cond
			((=fx remainder 0)
			 (+fx quotient 1))
			(else
			 (+fx quotient 2)))))
      (cond
	 ((>=fx cardinal max-large-size)
	  (internal-error "define-set!"
			  "Too many element in set"
			  (shape table)))
	 (else
	  (let loop ((i         0)
		     (quotient  0)
		     (mask      1))
	     (cond
		((=fx i cardinal)
		 (meta-set table size))
		((=fx mask 256)
		 (loop i (+fx quotient 1) 1))
		(else
		 (node-key-set! (vector-ref table i) (cons quotient mask))
		 (loop (+fx i 1) quotient (*fx mask 2)))))))))

;*---------------------------------------------------------------------*/
;*    make-set! ...                                                    */
;*---------------------------------------------------------------------*/
(define (make-set! meta-set)
   (cond
      ((not (meta-set? meta-set))
       (internal-error "make-set" "Not a meta-set" (shape meta-set)))
      (else
       (large-set (make-string (meta-set-compacted-size meta-set) #a000)
		  meta-set))))

;*---------------------------------------------------------------------*/
;*    set? ...                                                         */
;*---------------------------------------------------------------------*/
(define (set? obj)
   (large-set? obj))

;*---------------------------------------------------------------------*/
;*    set-extend! ...                                                  */
;*---------------------------------------------------------------------*/
(define (set-extend! set obj)
   (let* ((key       (node-key obj))
	  (the-set   (large-set-the-set set))
	  (quotient  (car key))
	  (mask      (cdr key)))
      (string-set! the-set
		   quotient
		   (char-or (integer->char mask)
			    (string-ref the-set quotient)))
      #unspecified))

;*---------------------------------------------------------------------*/
;*    set-member? ...                                                  */
;*---------------------------------------------------------------------*/
(define-inline (set-member? set obj)
   (let* ((key       (node-key obj))
	  (the-set   (large-set-the-set set))
	  (quotient  (car key))
	  (mask      (cdr key)))
      (>fx (bit-and mask (char->integer (string-ref the-set quotient))) 0)))
       
;*---------------------------------------------------------------------*/
;*    set-union-2! ...                                                 */
;*    -------------------------------------------------------------    */
;*    This function returns #t if nothing as been added. Otherwise,    */
;*    it returns #f.                                                   */
;*---------------------------------------------------------------------*/
(define (set-union-2! dst src)
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
   (if (not (large-set? src))
       (internal-error "set-union!" "Incompatible sets" (shape src))
       (if (not (=fx (string-length (large-set-the-set dst))
		     (string-length (large-set-the-set src))))
	   (internal-error "set-union!" "Incompatible sets" (shape src))
	   (large-set-union!))))

;*---------------------------------------------------------------------*/
;*    set-union! ...                                                   */
;*    -------------------------------------------------------------    */
;*    This function returns #t if nothing as been added. Otherwise,    */
;*    it returns #f.                                                   */
;*---------------------------------------------------------------------*/
(define (set-union! dst . src*)
   (cond
      ((null? src*)
       dst)
      ((null? (cdr src*))
       (set-union-2! dst (car src*)))
      (else
       (let loop ((src* src*)
		  (res  #f))
	  (if (null? src*)
	      res
	      (loop (cdr src*)
		    (or (set-union-2! dst (car src*)) res)))))))
		    
;*---------------------------------------------------------------------*/
;*    set-for-each ...                                                 */
;*---------------------------------------------------------------------*/
(define (set-for-each proc set)
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
   (large-set-for-each))

;*---------------------------------------------------------------------*/
;*    set-length ...                                                   */
;*---------------------------------------------------------------------*/
(define (set-length set)
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
   (large-set-length))
    
;*---------------------------------------------------------------------*/
;*    set->list ...                                                    */
;*---------------------------------------------------------------------*/
(define (set->list set)
   (let ((meta (large-set-meta set)))
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
		
		
