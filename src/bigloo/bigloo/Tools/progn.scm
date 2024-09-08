;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Tools/progn.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Dec 29 11:08:04 1994                          */
;*    Last change :  Wed Apr 17 20:53:53 2002 (serrano)                */
;*    Copyright   :  1994-2002 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The code sequence normalization.                                 */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module tools_progn
   (include "Tools/location.sch")
   (import  tools_misc
	    tools_error
	    tools_location)
   (export  (normalize-progn <expression>)
	    (normalize-progn/loc <expression> <value>)
	    (emap f l)))

;*---------------------------------------------------------------------*/
;*    normalize-progn ...                                              */
;*    sexp --> sexp                                                    */
;*---------------------------------------------------------------------*/
(define (normalize-progn body*)
   (cond
      ((not (pair? body*))
       (internal-error "normalize-progn" "Illegal expression" body*))
      ((null? (cdr body*))
       (match-case (car body*)
	  ((begin)
	   #unspecified)
	  ((begin ?exps)
	   (car body*))
	  ((begin . ?exps)
	   (normalize-progn exps))
	  (else
	   (car body*))))
      (else
       (let ((sub (let loop ((body* (if (eq? (car body*) 'begin)
					(cdr body*)
					body*)))
		     (if (null? body*)
			 '()
			 (let ((expr (car body*)))
			    (if (and (pair? expr)
				     (eq? (car expr) 'begin))
				(append (cdr expr)
					(loop (cdr body*)))
				(cond
				   ((epair? expr)
				    (econs expr
					   (loop (cdr body*))
					   (cer expr)))
				   ((epair? body*)
				    (econs expr
					   (loop (cdr body*))
					   (cer body*)))
				   (else
				    (cons expr
					  (loop (cdr body*)))))))))))
	  (if (epair? body*)
	      (econs 'begin sub (cer body*))
	      (cons 'begin sub))))))

;*---------------------------------------------------------------------*/
;*    normalize-progn/loc ...                                          */
;*---------------------------------------------------------------------*/
(define (normalize-progn/loc body* loc)
   (let ((nbody (normalize-progn body*)))
      (cond
	 ((not loc)
	  nbody)
	 ((epair? nbody)
	  nbody)
	 ((pair? nbody)
	  (econs (car nbody) (cdr nbody) loc))
	 (else
	  (econs 'begin (list nbody) loc)))))

;*---------------------------------------------------------------------*/
;*    emap ...                                                         */
;*---------------------------------------------------------------------*/
(define (emap f l)
   (let loop ((l l))
      (cond
	 ((null? l)
	  '())
	 ((epair? l)
	  (econs (f (car l)) (loop (cdr l)) (cer l)))
	 (else
	  (cons (f (car l)) (loop (cdr l)))))))
