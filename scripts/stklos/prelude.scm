;*=====================================================================*/
;*    serrano/prgm/project/bglstone/scripts/stklos/prelude.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Jun  7 13:56:58 2011                          */
;*    Last change :  Wed Jul 20 09:30:11 2011 (serrano)                */
;*    Copyright   :  2011 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    STklos prelude                                                   */
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
   `(fx< ,x ,y))

(define-macro (<=fx x y)
   `(fx<= ,x ,y))

(define-macro (-fx x y)
   `(fx- ,x ,y))

(define-macro (+fx x y)
   `(fx+ ,x ,y))

(define-macro (/fx x y)
   `(fxdiv ,x ,y))

(define-macro (*fx x y)
   `(fx* ,x ,y))
;*    ;; don't use fx* otherwise we get                                */
;*    ;; Reason: *** PANIC *** non existent opcode |INT-SINT-FXMUL2|   */
;*    `(bit-and (* ,x ,y) ,(fx- (bit-shift 1 28) 1)))                  */

(define-macro (/fx x y)
   `(quotient ,x ,y))

(define-macro (>fx x y)
   `(fx> ,x ,y))

(define-macro (>=fx x y)
   `(fx>= ,x ,y))

(define-macro (=fx x y)
   `(fx= ,x ,y))

(define-macro (quotientfx x y)
   `(quotient ,x ,y))

(define-macro (modulofx x y)
   `(fxmod ,x ,y))

(define-macro (remainderfx x y)
   `(fxrem ,x ,y))

(define-macro (zerofx? x)
   `(zero? ,x))

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

(define-macro (>fl x y)
   `(> ,x ,y))

(define-macro (>=fl x y)
   `(>= ,x ,y))

(define-macro (=fl x y)
   `(= ,x ,y))

(define-macro (negfl x)
   `(- ,x))

(define-macro (atanfl x)
   `(atan ,x))

(define-macro (cosfl x)
   `(cos ,x))

(define-macro (sinfl x)
   `(sin ,x))

(define-macro (zerofl? x)
   `(zero? ,x))

(define-macro (sqrtfl-ur x)
   `(sqrt ,x))

(define-macro (sqrtfl x)
   `(sqrt ,x))

(define-macro (atan-2fl-ur x y)
   `(atan ,x ,y))

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
(define-macro (bit-lsh x y)
   `(bit-shift ,x ,y))

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
;*    control                                                          */
;*---------------------------------------------------------------------*/
(define-macro (bind-exit exit . body)
   `(call/cc (lambda ,exit ,@body)))
 
(define-macro (labels bindings . body)
   `(letrec ,(map (lambda (b)
		     `(,(car b) (lambda ,(cadr b) ,@(cddr b))))
		bindings)
       ,@body))

