#lang scheme/base
;*=====================================================================*/
;*    serrano/prgm/project/bglstone/scheme/mzscheme/prelude.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Jun  7 13:56:58 2011                          */
;*    Last change :  Wed Jun  8 11:01:56 2011 (serrano)                */
;*    Copyright   :  2011 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    MzScheme prelude                                                 */
;*=====================================================================*/

(require (except-in r5rs with-output-to-file)
         (only-in r5rs (with-output-to-file %with-output-to-file))
         scheme/cmdline
         racket/fixnum
         racket/flonum
	 racket/unsafe/ops)

;*---------------------------------------------------------------------*/
;*    module                                                           */
;*---------------------------------------------------------------------*/
(define-syntax module
   (syntax-rules ()
      ((module ...)
       (define __ 0))))

;*---------------------------------------------------------------------*/
;*    Fixnum arithmetic                                                */
;*---------------------------------------------------------------------*/
(define-syntax <fx
   (syntax-rules ()
      ((<fx x y)
       (fx< x y))))

(define-syntax <=fx
   (syntax-rules ()
      ((<=fx x y)
       (fx<= x y))))

(define-syntax >fx
   (syntax-rules ()
      ((>fx x y)
       (fx> x y))))

(define-syntax >=fx
   (syntax-rules ()
      ((>=fx x y)
       (fx>= x y))))

(define-syntax =fx
   (syntax-rules ()
      ((=fx x y)
       (fx= x y))))

(define-syntax +fx
   (syntax-rules ()
      ((+fx x y)
       (fx+ x y))))

(define-syntax *fx
   (syntax-rules ()
      ((*fx x y)
       (unsafe-fx* x y))))

(define-syntax -fx
   (syntax-rules ()
      ((-fx x y)
       (fx- x y))))

(define-syntax /fx
   (syntax-rules ()
      ((/fx x y)
       (fxquotient x y))))

(define-syntax modulofx
   (syntax-rules ()
      ((modulofx x y)
       (fxmodulo x y))))

(define-syntax zerofx?
   (syntax-rules ()
      ((_ x)
       (fx= x 0))))

;*---------------------------------------------------------------------*/
;*    Flonum arithmetic                                                */
;*---------------------------------------------------------------------*/
(define-syntax <fl
   (syntax-rules ()
      ((<fl x y)
       (fl< x y))))

(define-syntax <=fl
   (syntax-rules ()
      ((<=fl x y)
       (fl<= x y))))

(define-syntax >fl
   (syntax-rules ()
      ((>fl x y)
       (fl> x y))))

(define-syntax >=fl
   (syntax-rules ()
      ((>=fl x y)
       (fl>= x y))))

(define-syntax =fl
   (syntax-rules ()
      ((=fl x y)
       (fl= x y))))

(define-syntax +fl
   (syntax-rules ()
      ((+fl x y)
       (fl+ x y))))

(define-syntax *fl
   (syntax-rules ()
      ((*fl x y)
       (fl* x y))))

(define-syntax -fl
   (syntax-rules ()
      ((-fl x y)
       (fl- x y))))

(define-syntax /fl
   (syntax-rules ()
      ((/fl x y)
       (fl/ x y))))

(define-syntax negfl
   (syntax-rules ()
      ((negfl x)
       (fl* -1. x))))

(define-syntax remainderfl
   (syntax-rules ()
      ((remainderfl x y)
       (let ((n (round (unsafe-fl/ x y))))
	  (unsafe-fl- x (unsafe-fl* n y))))))

(define-syntax atan-2fl-ur
   (syntax-rules ()
      ((atan-2fl-ur x y)
       (atan x y))))

(define-syntax atanfl
   (syntax-rules ()
      ((atanfl x)
       (flatan x))))

(define-syntax cosfl
   (syntax-rules ()
      ((_ x)
       (flcos x))))

(define-syntax sinfl
   (syntax-rules ()
      ((_ x)
       (flsin x))))

(define-syntax sqrtfl-ur
   (syntax-rules ()
      ((sqrtl-ur x)
       (flsqrt x))))

(define-syntax sqrtfl
   (syntax-rules ()
      ((_-ur x)
       (flsqrt x))))

;*---------------------------------------------------------------------*/
;*    logical operations                                               */
;*---------------------------------------------------------------------*/
(define-syntax bit-and
   (syntax-rules ()
      ((_ x y)
       (fxand x y))))
       
(define-syntax bit-or
   (syntax-rules ()
      ((_ x y)
       (fxior x y))))

(define-syntax bit-not
   (syntax-rules ()
      ((_ x)
       (fxnot x))))
       
(define-syntax bit-lsh
   (syntax-rules ()
      ((_ x y)
       (fxlshift x y))))
       
;*---------------------------------------------------------------------*/
;*    IO                                                               */
;*---------------------------------------------------------------------*/
(define (print . l)
   (for-each display l) (newline))

(define (display* . l)
   (for-each display l))

(define-syntax with-output-to-file
   (syntax-rules ()
      ((_ file thunk)
       (%with-output-to-file file thunk #:exists 'truncate))))

;*---------------------------------------------------------------------*/
;*    string                                                           */
;*---------------------------------------------------------------------*/
(define-syntax string->integer
   (syntax-rules ()
      ((string->integer s n1 ...)
       (inexact->exact (round (string->number s n1 ...))))))

;*---------------------------------------------------------------------*/
;*    bind-exit ...                                                    */
;*---------------------------------------------------------------------*/
(define-syntax bind-exit
   (syntax-rules ()
      ((bind-exit (exit) exp ...)
       (call/cc (lambda (exit) exp ...)))))

;*---------------------------------------------------------------------*/
;*    labels                                                           */
;*---------------------------------------------------------------------*/
(define-syntax labels
   (syntax-rules ()
      ((_ ((id (a ...) e ...) ...) exp ...)
       (letrec ((id (lambda (a ...) e ...)) ...) exp ...))))

;*---------------------------------------------------------------------*/
;*    macros                                                           */
;*---------------------------------------------------------------------*/
(define-syntax define-macro
   (syntax-rules ()
      ((_ binding body ...)
       #f)))

;*---------------------------------------------------------------------*/
;*    if                                                               */
;*---------------------------------------------------------------------*/
;* (define-syntax if                                                   */
;*    (syntax-rules ()                                                 */
;*       ((_ test then)                                                */
;*        (when test then))                                            */
;*       ((_ test x-then x-else)                                       */
;*        (cond (test x-then) (else x-else)))))                        */
