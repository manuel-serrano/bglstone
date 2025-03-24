;*=====================================================================*/
;*    serrano/prgm/project/bglstone/src/bigloo/bigloo/Cfa/show.scm     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Mar  8 18:51:37 1995                          */
;*    Last change :  Fri Mar  7 07:45:17 2025 (serrano)                */
;*    Copyright   :  1995-2025 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    We show the result of the cfa.                                   */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module cfa_show
   (include "Ast/node.sch" "Type/type.sch" "Cfa/cinfo.sch" "Cfa/cinfo2.sch" "Cfa/cinfo3.sch")
   (include "Tools/trace.sch")
   (import  tools_shape
	    tools_trace
	    tools_speek
	    write_scheme
	    type_type
	    ast_var
	    ast_node
	    cfa_info
	    cfa_info2
	    cfa_info3
	    cfa_approx
	    cfa_collect
	    cfa_iterate)
   (export  (show-cfa-nb-iterations)
	    (show-cfa-results globals)))

;*---------------------------------------------------------------------*/
;*    show-cfa-nb-iterations ...                                       */
;*---------------------------------------------------------------------*/
(define (show-cfa-nb-iterations)
   (verbose 2 "      (" (+fx 1 *cfa-stamp*) " Iterations)" #\Newline))

;*---------------------------------------------------------------------*/
;*    show-cfa-results ...                                             */
;*---------------------------------------------------------------------*/
(define (show-cfa-results globals)
   (trace cfa
	  #\Newline
	  "============================================" #\Newline
	  "cfa-results:" #\Newline)
   (if (pair? (get-allocs)) (trace cfa #\Newline))
   (for-each (lambda (a)
		(trace cfa (node-key a) ": " (shape a) " """ #\Newline))
	     (get-allocs))
   (trace cfa #\Newline)
   (trace cfa
	  (let ((old-case *pp-case*))
	     (set! *pp-case* 'lower)
	     (for-each (lambda (g)
			  (let ((fun (global-value g)))
			     (write-scheme-comment *trace-port* (shape g))
			     (pp `(,(case (sfun-class fun)
				       ((sgfun)
					'define-generic)
				       ((sifun)
					'define-inline)
				       ((smfun)
					'define-method)
				       (else
					'define))
				   (,(shape g)
				    ,@(map shape (sfun-args fun)))
				   ,(shape (sfun-body fun)))
				 *trace-port*)))
		       globals)
	     (set! *pp-case* old-case)
	     #\Newline #\Newline)))

;*---------------------------------------------------------------------*/
;*    shape ...                                                        */
;*---------------------------------------------------------------------*/
(define-method (shape local::reshaped-local)
   (let ((port (open-output-string)))
      (display (call-next-method) port)
      (cfa-variable-shape local port)))

;*---------------------------------------------------------------------*/
;*    shape ...                                                        */
;*---------------------------------------------------------------------*/
(define-method (shape global::reshaped-global)
   (let ((port (open-output-string)))
      (display (call-next-method) port)
      (cfa-variable-shape global port)))
   
;*---------------------------------------------------------------------*/
;*    cfa-variable-shape ...                                           */
;*---------------------------------------------------------------------*/
(define (cfa-variable-shape variable port)
   (let ((value (variable-value variable)))
      (cond 
	 ((svar/Cinfo? value)
	  (display " " port)
	  (display (shape (svar/Cinfo-approx value)) port))
	 ((cvar/Cinfo? value)
	  (display " " port)
	  (display (shape (cvar/Cinfo-approx value)) port))
	 ((scnst/Cinfo? value)
	  (display " " port)
	  (display (shape (scnst/Cinfo-approx value)) port))
	 ((intern-sfun/Cinfo? value)
	  (display " " port)
	  (display (shape (intern-sfun/Cinfo-approx value)) port))
	 ((extern-sfun/Cinfo? value)
	  (display " " port)
	  (display (shape (extern-sfun/Cinfo-approx value)) port))
	 ((cfun/Cinfo? value)
	  (display " " port)
	  (display (shape (cfun/Cinfo-approx value)) port)))
      (string->symbol (close-output-port port))))
