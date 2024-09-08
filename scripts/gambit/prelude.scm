;*=====================================================================*/
;*    serrano/prgm/project/bglstone/scheme/gambit/prelude.scm          */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Jun  7 13:29:28 2011                          */
;*    Last change :  Wed Jun  8 10:54:59 2011 (serrano)                */
;*    Copyright   :  2011 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Gambit prelude (Bigloo compatibility kit)                        */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    module                                                           */
;*---------------------------------------------------------------------*/
(define-macro (module . l)
   #f)

;*---------------------------------------------------------------------*/
;*    Fixnum arithmetic                                                */
;*---------------------------------------------------------------------*/
(define-macro (<fx x y)
   `(##fixnum.< ,x ,y))

(define-macro (<=fx x y)
   `(##fixnum.<= ,x ,y))

(define-macro (-fx x y)
   `(##fixnum.- ,x ,y))

(define-macro (+fx x y)
   `(##fixnum.+ ,x ,y))

(define-macro (*fx x y)
   `(##fixnum.* ,x ,y))

(define-macro (/fx x y)
   `(##fixnum.quotient ,x ,y))

(define-macro (>fx x y)
   `(##fixnum.> ,x ,y))

(define-macro (>=fx x y)
   `(##fixnum.>= ,x ,y))

(define-macro (=fx x y)
   `(##fixnum.= ,x ,y))

(define-macro (modulo x y)
   `(##fixnum.modulo ,x ,y))

(define-macro (quotient x y)
   `(##fixnum.quotient ,x ,y))

(define-macro (quotientfx x y)
   `(##fixnum.quotient ,x ,y))

(define-macro (modulofx x y)
   `(##fixnum.modulo ,x ,y))

(define-macro (zerofx? x)
   `(##fixnum.zero? ,x))

;*---------------------------------------------------------------------*/
;*    Flonum arithmetic                                                */
;*---------------------------------------------------------------------*/
(define-macro (<fl x y)
   `(##flonum.< ,x ,y))

(define-macro (<=fl x y)
   `(##flonum.<= ,x ,y))

(define-macro (>fl x y)
   `(##flonum.> ,x ,y))

(define-macro (>=fl x y)
   `(##flonum.>= ,x ,y))

(define-macro (=fl x y)
   `(##flonum.= ,x ,y))

(define-macro (negfl x)
   `(##flonum.- ,x))

(define-macro (-fl x y)
   `(##flonum.- ,x ,y))

(define-macro (*fl x y)
   `(##flonum.* ,x ,y))

(define-macro (+fl x y)
   `(##flonum.+ ,x ,y))

(define-macro (/fl x y)
   `(##flonum./ ,x ,y))

(define-macro (cosfl x)
   `(##flonum.cos ,x))

(define-macro (sinfl x)
   `(##flonum.sin ,x))

(define-macro (atanfl x)
   `(##flonum.atan ,x))

(define-macro (atan-2fl-ur x y)
   `(##flonum.atan ,x ,y))

(define-macro (sqrtfl x)
   `(##flonum.sqrt ,x))

(define-macro (sqrtfl-ur x)
   `(##flonum.sqrt ,x))

(define-macro (>fl x y)
   `(##flonum.> ,x ,y))

(define-macro (>fl x y)
   `(##flonum.>= ,x ,y))

(define-macro (=fl x y)
   `(##flonum.= ,x ,y))

(define-macro (remainderfl x y)
   (let ((gx (gensym))
	 (gy (gensym)))
      `(let* ((,gx ,x)
	      (,gy ,y)
	      (n (round (/ ,gx ,gy))))
	  (- ,gx (* n ,gy)))))

;*---------------------------------------------------------------------*/
;*    Logical operations                                               */
;*---------------------------------------------------------------------*/
(define-macro (bit-or x y)
   `(bitwise-ior ,x ,y))

(define-macro (bit-not x)
   `(bitwise-not ,x))

(define-macro (bit-and x y)
   `(bitwise-and ,x ,y))

(define-macro (bit-xor x y)
   `(bitwise-xor ,x ,y))

(define-macro (bit-lsh x y)
   `(arithmetic-shift ,x ,y))

;*---------------------------------------------------------------------*/
;*    IO                                                               */
;*---------------------------------------------------------------------*/
(define (print . l)
   (for-each display l) (newline))

(define (display* . l)
   (for-each display l))

;*---------------------------------------------------------------------*/
;*    string                                                           */
;*---------------------------------------------------------------------*/
(define-macro (string->integer s . n)
   `(inexact->exact (round (string->number ,s ,@n))))

;*---------------------------------------------------------------------*/
;*    Control                                                          */
;*---------------------------------------------------------------------*/
(define-macro (bind-exit exit . body)
   `(call/cc (lambda ,exit ,@body)))

(define-macro (labels bindings . body)
   `(letrec ,(map (lambda (b)
		     `(,(car b) (lambda ,(cadr b) ,@(cddr b))))
		bindings)
       ,@body))

(define-macro (when test . body)
   `(if ,test (begin ,@body)))

(define-macro (unless test . body)
   `(if (not ,test) (begin ,@body)))


