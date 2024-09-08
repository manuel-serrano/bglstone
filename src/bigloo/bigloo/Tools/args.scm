;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Tools/args.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Dec 27 09:49:59 1994                          */
;*    Last change :  Thu Apr  3 14:11:12 2003 (serrano)                */
;*    Copyright   :  1994-2003 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    Tools function for managing parameters.                          */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    Le module                                                        */
;*---------------------------------------------------------------------*/
(module tools_args
   (import tools_dsssl)
   (export (arity::long exp)
	   (make-args-list args nil cons)
	   (args*->args-list exp)
	   (args-list->args* list arity)
	   (sound-arity?::bool arity args)
	   (make-n-proto n)
	   (+-arity arity <integer>)))

;*---------------------------------------------------------------------*/
;*    arity ...                                                        */
;*    args-list --> int                                                */
;*    -------------------------------------------------------------    */
;*    (lambda n ..)               --> -1                               */
;*    (lambda () ..)              -->  0                               */
;*    (lambda (a1 a2 .. an) ..)   -->  n                               */
;*    (lambda (a1 a2 .. . an) ..) --> -n                               */
;*    -------------------------------------------------------------    */
;*    Any dsssl named constant is equivalent to a `.' expression.      */
;*    Thus:                                                            */
;*    (lambda (a1 a2 .. an-1 #!optional ...#!rest ...) ..) --> -n      */
;*---------------------------------------------------------------------*/
(define (arity args)
   (let loop ((i    0)
	      (args args))
      (cond
	 ((null? args)
	  i)
	 ((pair? args)
	  (if (dsssl-named-constant? (car args))
	      (negfx (+fx i 1))
	      (loop (+fx i 1) (cdr args))))
	 (else
	  (negfx (+fx i 1))))))

;*---------------------------------------------------------------------*/
;*    args*->args-list ...                                             */
;*    cons* --> list                                                   */
;*---------------------------------------------------------------------*/
(define (args*->args-list exp)
   (cond
      ((null? exp)
       '())
      ((not (pair? exp))
       (list exp))
      (else
       (cons (car exp)
             (args*->args-list (cdr exp))))))

;*---------------------------------------------------------------------*/
;*    args-list->args* ...                                             */
;*---------------------------------------------------------------------*/
(define (args-list->args* list arity)
   (cond
      ((>=fx arity 0)
       list)
      ((=fx arity -1)
       (car list))
      (else
       (let loop ((list  list)
		  (arity arity))
	  (if (=fx arity -1)
	      (car list)
	      (cons (car list) (loop (cdr list) (+fx arity 1))))))))
   
;*---------------------------------------------------------------------*/
;*    sound-arity? ...                                                 */
;*---------------------------------------------------------------------*/
(define (sound-arity? arity args)
   (let ((len (length args)))
      (if (>=fx arity 0)
	  (=fx arity len)
	  (<=fx (negfx arity) (+fx len 1)))))

;*---------------------------------------------------------------------*/
;*    make-args-list ...                                               */
;*---------------------------------------------------------------------*/
(define (make-args-list args nil cons)
   (let loop ((args args))
      (if (null? args)
	  nil
	  `(,cons ,(car args) ,(loop (cdr args))))))

;*---------------------------------------------------------------------*/
;*    make-n-proto ...                                                 */
;*    -------------------------------------------------------------    */
;*    On construit un prototype a partir de l'arite.                   */
;*---------------------------------------------------------------------*/
(define (make-n-proto n)
   (define (make-args-name n)
      (string->symbol (string-append "A" (integer->string n))))
   (define (make-va-proto n count)
      (if (=fx n -1)
	  (make-args-name count)
	  (cons (make-args-name count)
		(make-va-proto (+fx n 1) (+fx count 1)))))
   (define (make-fx-proto n count)
      (if (=fx n 0)
	  '()
	  (cons (make-args-name count)
		(make-fx-proto (-fx n 1) (+fx count 1)))))
   (if (<fx n 0)
       (make-va-proto n 0)
       (make-fx-proto n 0)))


;*---------------------------------------------------------------------*/
;*    +-arity ...                                                      */
;*---------------------------------------------------------------------*/
(define (+-arity arity add)
   (if (>=fx arity 0)
       (+fx add arity)
       (-fx arity add)))

       
