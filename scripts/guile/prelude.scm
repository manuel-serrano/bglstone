;*=====================================================================*/
;*    serrano/prgm/project/bglstone/scheme/guile/prelude.scm           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Jun  7 14:08:34 2011                          */
;*    Last change :  Wed Jun  8 11:00:09 2011 (serrano)                */
;*    Copyright   :  2011 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Guile prelude                                                    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    Misc guile configuration                                         */
;*---------------------------------------------------------------------*/
(debug-set! stack 640000)

;*---------------------------------------------------------------------*/
;*    module                                                           */
;*---------------------------------------------------------------------*/
(define-macro (module . l)
   #f)

;*---------------------------------------------------------------------*/
;*    Fixnum arithmetic                                                */
;*---------------------------------------------------------------------*/
(define-macro (<fx x y)
   `(< ,x ,y))

(define-macro (<=fx x y)
   `(<= ,x ,y))

(define-macro (-fx x y)
   `(- ,x ,y))

(define-macro (+fx x y)
   `(+ ,x ,y))

(define-macro (*fx x y)
   `(* ,x ,y))

(define-macro (/fx x y)
   `(quotient ,x ,y))

(define-macro (>fx x y)
   `(> ,x ,y))

(define-macro (>=fx x y)
   `(>= ,x ,y))

(define-macro (=fx x y)
   `(= ,x ,y))

(define-macro (zerofx? x)
   `(= ,x 0))

(define-macro (modulofx x y)
   `(modulo ,x ,y))

;*---------------------------------------------------------------------*/
;*    Flonum arithmetic                                                */
;*---------------------------------------------------------------------*/
(define-macro (<fl x y)
   `(< ,x ,y))

(define-macro (<=fl x y)
   `(<= ,x ,y))

(define-macro (-fl x y)
   `(- ,x ,y))

(define-macro (+fl x y)
   `(+ ,x ,y))

(define-macro (*fl x y)
   `(* ,x ,y))

(define-macro (/fl x y)
   `(/ ,x ,y))

(define-macro (negfl x)
   `(- ,x))

(define-macro (>fl x y)
   `(> ,x ,y))

(define-macro (>=fl x y)
   `(>= ,x ,y))

(define-macro (=fl x y)
   `(= ,x ,y))

(define-macro (zerofl? x)
   `(zero? ,x))

(define-macro (sqrtfl-ur x)
   `(sqrt ,x))

(define-macro (sqrtfl x)
   `(sqrt ,x))

(define-macro (atan-2fl-ur x y)
   `(atan ,x ,y))

(define-macro (atanfl x)
   `(atan ,x))

(define-macro (cosfl x)
   `(cos ,x))

(define-macro (sinfl x)
   `(sin ,x))

(define-macro (remainderfl x y)
   (let ((gx (gensym))
	 (gy (gensym)))
      `(let* ((,gx ,x)
	      (,gy ,y)
	      (n (round (/ ,gx ,gy))))
	  (- ,gx (* n ,gy)))))

;*---------------------------------------------------------------------*/
;*    logical operations                                               */
;*---------------------------------------------------------------------*/
(define-macro (bit-and x y)
   `(logand ,x ,y))

(define-macro (bit-or x y)
   `(logor ,x ,y))

(define-macro (bit-not x)
   `(lognot ,x))

(define-macro (bit-lsh x y)
   `(ash ,x ,y))

;*---------------------------------------------------------------------*/
;*    IO                                                               */
;*---------------------------------------------------------------------*/
(define (print . l)
   (for-each display l) (newline))

(define (display* . l)
   (for-each display l))

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

;*---------------------------------------------------------------------*/
;*    string                                                           */
;*---------------------------------------------------------------------*/
(define-macro (string->integer s . n)
   `(inexact->exact (round (string->number ,s ,@n))))
				  
