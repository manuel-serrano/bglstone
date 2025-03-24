;*=====================================================================*/
;*    .../project/bglstone/src/bigloo/bigloo/Expand/garith.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Aug 26 09:16:36 1994                          */
;*    Last change :  Thu Mar  6 16:40:13 2025 (serrano)                */
;*    Copyright   :  1994-2025 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    Les expandeurs arithmetiques (generiques)                        */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    Le module                                                        */
;*---------------------------------------------------------------------*/
(module expand_garithmetique
   (include "Ast/node.sch" "Type/type.sch")
   (export (expand-g+  ::obj ::procedure)
	   (expand-g-  ::obj ::procedure)
	   (expand-g*  ::obj ::procedure)
	   (expand-g/  ::obj ::procedure)
	   (expand-g=  ::obj ::procedure)
	   (expand-g<  ::obj ::procedure)
	   (expand-g>  ::obj ::procedure)
	   (expand-g<= ::obj ::procedure)
	   (expand-g>= ::obj ::procedure)))

;*---------------------------------------------------------------------*/
;*    expand-g-number? ...                                             */
;*---------------------------------------------------------------------*/
(define (expand-g-number? x)
   (or (fixnum? x) (flonum? x)))

;*---------------------------------------------------------------------*/
;*    expand-g+ ...                                                    */
;*---------------------------------------------------------------------*/
(define (expand-g+ x e)
   (match-case x
      ((?-)
       0)
      ((?- . (?x . ()))
       (e x e))
      ((?- ?x . (?y . ()))
       (cond
	  ((and (expand-g-number? x) (expand-g-number? y))
	   (+ x y))
	  (else
	   (e `(2+ ,x ,y) e))))
      ((?- ?x . ?y)
       (e `(2+ ,x (+ ,@y)) e)))) 
      
;*---------------------------------------------------------------------*/
;*    expand-g- ...                                                    */
;*---------------------------------------------------------------------*/
(define (expand-g- x e)
   (match-case x
      ((?- . (?x . ()))
       (cond
	  ((fixnum? x)
	   (negfx x))
	  ((real? x)
	   (negfl x))
	  (else
	   `(- ,(e x e)))))
      ((?- ?x . (?y . ()))
       (cond
	  ((and (expand-g-number? x) (expand-g-number? y))
	   (- x y))
	  (else
	   (e `(2- ,x ,y) e))))
      ((?- ?x . ?y)
       (e `(2- ,x (+ ,@y)) e))))
       
;*---------------------------------------------------------------------*/
;*    expand-g* ...                                                    */
;*---------------------------------------------------------------------*/
(define (expand-g* x e)
   (match-case x
      ((?-)
       1)
      ((?- . (?x . ()))
       (e x e))
      ((?- ?x . (?y . ()))
       (cond
	  ((and (expand-g-number? x) (expand-g-number? y))
	   (* x y))
	  (else
	   (e `(2* ,x ,y) e))))
      ((?- ?x . ?y)
       (e `(2* ,x (* ,@y)) e)))) 
      
;*---------------------------------------------------------------------*/
;*    expand-g/ ...                                                    */
;*---------------------------------------------------------------------*/
(define (expand-g/ x e)
   (match-case x
      ((?- . (?x . ()))
       `(2/ 1 ,(e x e)))
      ((?- ?x . (?y . ()))
       (cond
	  ((and (expand-g-number? x) (expand-g-number? y))
	   (if (= y 0)
	       (e `(2/ ,x ,y) e)
	       (/ x y)))
	  (else
	   (e `(2/ ,x ,y) e))))
      ((?- ?x . ?y)
       (e `(2/ ,x (* ,@y)) e))))
      
;*---------------------------------------------------------------------*/
;*    expand-g= ...                                                    */
;*---------------------------------------------------------------------*/
(define (expand-g= x e)
   (match-case x
      ((?- ?x . (?y . ()))
       (cond
	  ((and (expand-g-number? x) (expand-g-number? y))
	   (= x y))
	  (else
	   (e `(2= ,x ,y) e))))
      ((?- ?-)
       (error "=" "Illegal form" x))
      ((?- ?x . ?y)
       (e `(and (2= ,x ,(car y)) (= ,@y)) e))))

;*---------------------------------------------------------------------*/
;*    expand-g< ...                                                    */
;*---------------------------------------------------------------------*/
(define (expand-g< x e)
   (match-case x
      ((?- ?x . (?y . ()))
       (cond
	  ((and (expand-g-number? x) (expand-g-number? y))
	   (< x y))
	  (else
	   (e `(2< ,x ,y) e))))
      ((?- ?-)
       (error "<" "Illegal form" x))
      ((?- ?x . ?y)
       (e `(and (2< ,x ,(car y)) (< ,@y)) e))))

;*---------------------------------------------------------------------*/
;*    expand-g> ...                                                    */
;*---------------------------------------------------------------------*/
(define (expand-g> x e)
    (match-case x
      ((?- ?x . (?y . ()))
       (cond
	  ((and (expand-g-number? x) (expand-g-number? y))
	   (> x y))
	  (else
	   (e `(2> ,x ,y) e))))
      ((?- ?-)
       (error ">" "Illegal form" x))
      ((?- ?x . ?y)
       (e `(and (2> ,x ,(car y)) (> ,@y)) e))))
     
;*---------------------------------------------------------------------*/
;*    expand-g<= ...                                                   */
;*---------------------------------------------------------------------*/
(define (expand-g<= x e)
   (match-case x
      ((?- ?x . (?y . ()))
       (cond
	  ((and (expand-g-number? x) (expand-g-number? y))
	   (<= x y))
	  (else
	   (e `(2<= ,x ,y) e))))
      ((?- ?-)
       (error "<=" "Illegal form" x))
      ((?- ?x . ?y)
       (e `(and (2<= ,x ,(car y)) (<= ,@y)) e))))
      
;*---------------------------------------------------------------------*/
;*    expand-g>= ...                                                   */
;*---------------------------------------------------------------------*/
(define (expand-g>= x e)
    (match-case x
      ((?- ?x . (?y . ()))
       (cond
	  ((and (expand-g-number? x) (expand-g-number? y))
	   (>= x y))
	  (else
	   (e `(2>= ,x ,y) e))))
      ((?- ?-)
       (error ">=" "Illegal form" x))
      ((?- ?x . ?y)
       (e `(and (2>= ,x ,(car y)) (>= ,@y)) e))))

