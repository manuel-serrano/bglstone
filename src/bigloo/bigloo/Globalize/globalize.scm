;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Globalize/globalize.scm     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jan 26 14:45:58 1995                          */
;*    Last change :  Tue Feb 24 08:51:11 2004 (serrano)                */
;*    Copyright   :  1995-2004 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The `globalization' process                                      */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module globalize_globalize
   (include "Tools/trace.sch"
	    "Tools/verbose.sch")
   (import  tools_shape
	    tools_speek
	    type_type
	    ast_var
	    ast_node
	    globalize_ginfo
	    globalize_node
	    globalize_free
	    globalize_kapture
	    globalize_gn
	    globalize_integration
	    globalize_new-body
	    globalize_local->global
	    globalize_global-closure)
   (export  (globalize! global::global)
	    *E*
	    *G0*
	    *G1*))

;*---------------------------------------------------------------------*/
;*    The global pass registers                                        */
;*---------------------------------------------------------------------*/
(define *E*  '())
(define *G0* '())
(define *G1* '())

;*---------------------------------------------------------------------*/
;*    globalize! ...                                                   */
;*---------------------------------------------------------------------*/
(define (globalize! global)
   (trace globalize
	  "========================================" #\newline
	  (shape global) " "
	  (if (global/Ginfo-escape? global) "[escaping]" "[non escaping]")
	  #\Newline
	  "----------------------------------------" #\newline)
   (verbose 3 "        " (shape global) ": " #\Newline)
   (let ((fun (global-value global)))
      (set! *E*  '())
      (set! *G0* '())
      (set! *G1* '())
      (Gn! (sfun-args fun) (sfun-body fun) global '())
      (trace globalize
	     "   E  : " (shape *E*)
	     #\Newline
	     "   G0 : " (shape *G0*)
	     #\Newline
	     "   G1 : " (shape *G1*)
	     #\Newline)
      ;; we compute the integration property
      (set-integration!)
      ;; we computed the really globalised functions.
      (let ((G (let loop ((G1 *G1*)
			  (G  *E*))
		  (cond
		     ((null? G1)
		      (trace globalize "   G  : " (shape G) #\Newline)
		      G)
		     ((local? (sfun/Ginfo-integrator (local-value (car G1))))
		      (loop (cdr G1) G))
		     (else
		      (loop (cdr G1) (cons (car G1) G)))))))
	 ;; we print the globalization result
	 (verb-globalization)
	 ;; for each globalized function, we computed its new body
	 (set-globalized-new-bodies! global G)
	 ;; for each globalized function, we computes its set of
	 ;; kaptured variables.
	 (set-kaptured! G)
	 ;; then, we compute new global definitions
	 (let loop ((G     G)
		    (new-G (if (global/Ginfo-escape? global)
			       (list (global-closure global
						     (node-loc
						      (sfun-body fun)))
				     global)
			       (list global))))
	    (if (null? G)
		;; we still have, to globalize the global function
		(let ((body (node-globalize! (sfun-body fun) global '())))
		   (sfun-body-set! fun body)
		   (trace globalize #a012 #\Newline)
		   new-G)
		(loop (cdr G)
		      (cons (local->global (car G)) new-G)))))))

;*---------------------------------------------------------------------*/
;*    verb-globalization ...                                           */
;*---------------------------------------------------------------------*/
(define (verb-globalization)
   (for-each (lambda (local)
		(verbose 3 "           " (shape local) " ==>" #\Newline))
	     *E*)
   (for-each (lambda (local)
		(let ((sfun/Ginfo (local-value local)))
		   (if (local? (sfun/Ginfo-integrator sfun/Ginfo))
		       (verbose 3 "           "
				(shape local)
				" --> "
				(shape (sfun/Ginfo-integrator sfun/Ginfo))
				#\Newline)
		       (verbose 3 "           " (shape local) " -->"
				#\Newline))))
	     *G1*))
 
