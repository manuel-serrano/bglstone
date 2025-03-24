;*=====================================================================*/
;*    .../prgm/project/bglstone/src/bigloo/bigloo/Tools/dsssl.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  SERRANO Manuel                                    */
;*    Creation    :  Thu Apr  3 14:42:11 1997                          */
;*    Last change :  Fri Mar  7 08:37:38 2025 (serrano)                */
;*    Copyright   :  1997-2025 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    Some dsssl goodies.                                              */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module tools_dsssl
   (include "Ast/node.sch" "Type/type.sch")
   (import tools_error)
   (export (dsssl-named-constant? obj)
	   (dsssl-defaulted-formal? obj)
	   (dsssl-default-formal obj)
	   (dsssl-find-first-formal obj)
	   (dsssl-formals-skeleton obj)
	   (dsssl-formals-encoding obj)
	   (dsssl-args*->args-list obj)))

;*---------------------------------------------------------------------*/
;*    dsssl-named-constant? ...                                        */
;*    -------------------------------------------------------------    */
;*    Is an object a dsssl named constant (#!optional, #!key or        */
;*    #!rest) ?                                                        */
;*---------------------------------------------------------------------*/
(define (dsssl-named-constant? obj)
   ;; this has to be changed after the bootstrap
   (and (cnst? obj)
	(case (cnst->integer obj)
	   ((#x102 #x103 #x106)
	    #t)
	   (else
	    #f))))

;*---------------------------------------------------------------------*/
;*    dsssl-defaulted-formal? ...                                      */
;*    -------------------------------------------------------------    */
;*    Is an expression a defaulted dsssl argument ?                    */
;*---------------------------------------------------------------------*/
(define (dsssl-defaulted-formal? obj)
   (match-case obj
      ((?- ?-)
       #t)
      (else
       #f)))

;*---------------------------------------------------------------------*/
;*    dsssl-default-formal ...                                         */
;*    -------------------------------------------------------------    */
;*    The formal of a dsssl-defaulted-formal expression                */
;*---------------------------------------------------------------------*/
(define (dsssl-default-formal obj)
   (car obj))

;*---------------------------------------------------------------------*/
;*    dsssl-find-first-formal ...                                      */
;*---------------------------------------------------------------------*/
(define (dsssl-find-first-formal args)
   (cond
      ((null? args)
       #f)
      ((not (pair? args))
       (internal-error "dsssl-find-first-formal"
		       "Illegal dsssl formal list"
		       args))
      ((dsssl-named-constant? (car args))
       (dsssl-find-first-formal (cdr args)))
      ((dsssl-defaulted-formal? (car args))
       (dsssl-default-formal (car args)))
      ((not (symbol? (car args)))
       (internal-error "dsssl-find-first-formal"
		       "Illegal dsssl formal list"
		       args))
      (else
       (car args))))
       
;*---------------------------------------------------------------------*/
;*    dsssl-formals-skeleton ...                                       */
;*---------------------------------------------------------------------*/
(define (dsssl-formals-skeleton args)
   (let loop ((args args)
	      (skip #f))
      (cond
	 ((null? args)
	  '())
	 ((not (pair? args))
	  args)
	 ((dsssl-named-constant? (car args))
	  (cons (car args) (loop (cdr args) #t)))
	 (skip
	  (loop (cdr args) skip))
	 (else
	  (cons (car args) (loop (cdr args) skip))))))
	 
;*---------------------------------------------------------------------*/
;*    dsssl-formals-encoding ...                                       */
;*---------------------------------------------------------------------*/
(define (dsssl-formals-encoding args)
   (let loop ((args args)
	      (res  '()))
      (cond
	 ((null? args)
	  res)
	 ((not (pair? args))
	  res)
	 ((dsssl-named-constant? (car args))
	  (loop (cdr args) (cons (car args) res)))
	 (else
	  (loop (cdr args) res)))))
	       
;*---------------------------------------------------------------------*/
;*    dsssl-args*->args-list ...                                       */
;*    cons* --> list                                                   */
;*---------------------------------------------------------------------*/
(define (dsssl-args*->args-list exp)
   (cond
      ((null? exp)
       '())
      ((not (pair? exp))
       (list exp))
      ((dsssl-named-constant? (car exp))
       (let ((arg (dsssl-find-first-formal (cdr exp))))
	  (if arg
	      (list arg)
	      '())))
      ((dsssl-defaulted-formal? (car exp))
       (dsssl-args*->args-list (cdr exp)))
      (else
       (cons (car exp) (dsssl-args*->args-list (cdr exp))))))

