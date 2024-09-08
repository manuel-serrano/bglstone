;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Expand/exit.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Apr 21 15:03:35 1995                          */
;*    Last change :  Tue Mar 23 22:07:21 2004 (serrano)                */
;*    Copyright   :  1995-2004 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The macro expansion of the `exit' machinery.                     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module expand_exit
   (include "Expand/expander.sch"
	    "Tools/trace.sch")
   (import  tools_progn
	    tools_args
	    tools_speek
	    tools_misc
	    expand_expander
	    expand_eps
	    expand_lambda
	    engine_param
	    type_type
	    ast_ident)
   (export  (expand-jump-exit      ::obj ::procedure)
	    (expand-set-exit       ::obj ::procedure)
	    (expand-bind-exit      ::obj ::procedure)
	    (expand-unwind-protect ::obj ::procedure)))

;*---------------------------------------------------------------------*/
;*    expand-jump-exit ...                                             */
;*---------------------------------------------------------------------*/
(define (expand-jump-exit x e)
   (match-case x
      ((?- ?exit . ?value)
       (let ((new `(jump-exit ,(e exit e) ,(e (normalize-progn value) e))))
	  (replace! x new)))
      (else
       (error #f "Illegal 'jump-exit' form" x))))

;*---------------------------------------------------------------------*/
;*    expand-set-exit ...                                              */
;*---------------------------------------------------------------------*/
(define (expand-set-exit x e)
   (match-case x
      ((?- (?exit) . ?body)
       (let ((new `(set-exit (,exit) ,(e (normalize-progn body) e))))
	  (replace! x new)))
      (else
       (error #f "Illegal `set-exit' form" x))))
	  
;*---------------------------------------------------------------------*/
;*    expand-bind-exit ...                                             */
;*---------------------------------------------------------------------*/
(define (expand-bind-exit x e)
   (match-case x
      ((?- (?exit) . ?body)
       (let ((an-exit  (mark-symbol-non-user! (gensym 'an_exit)))
	     (an-exitd (mark-symbol-non-user! (gensym 'an_exitd)))
	     (val      (mark-symbol-non-user! (gensym 'val)))
	     (res      (mark-symbol-non-user! (gensym 'res))))
	  (let ((new (e `(set-exit (,an-exit)
				   (let ()
				      (push-exit! ,an-exit 1)
				      (let ((,an-exitd (%get-exitd-top)))
					 (labels ((,exit (,val)
							 (unwind-until!
							  ,an-exitd
							  ,val)))
					    (let ((,res (begin ,@body)))
					       (pop-exit!)
					       ,res)))))
			e)))
	     (replace! x new))))
      (else
       (error #f "Illegal `bind-exit' form" x))))

;*---------------------------------------------------------------------*/
;*    expand-unwind-protect ...                                        */
;*---------------------------------------------------------------------*/
(define (expand-unwind-protect x e)
   (match-case x
      ((?- ?exp . (and (? pair?) ?cleanup))
       (let* ((val     (mark-symbol-non-user! (gensym 'val)))
	      (an-exit (mark-symbol-non-user! (gensym 'an_exit)))
	      (valbis  (mark-symbol-non-user! (gensym 'val)))
	      (eexp    (e exp e))
	      (aux     `(let ((,valbis ,eexp))
			   (pop-exit!)
			   ,valbis))
	      (eaux    (if (epair? eexp)
			   (econs (car aux) (cdr aux) (cer eexp))
			   aux)))
	  (let ((new `(let ((,val (set-exit (,an-exit)
					    (let ()
					       (push-exit! ,an-exit 0)
					       ,aux))))
			 ,(e (normalize-progn cleanup) e)
			 (if (val-from-exit? ,val)
			     (unwind-until! (car ,val) (cdr ,val))
			     ,val))))
	     (replace! x new))))
      (else
       (error #f "Illegal `unwind-protect' form" x))))
			  
     
  
