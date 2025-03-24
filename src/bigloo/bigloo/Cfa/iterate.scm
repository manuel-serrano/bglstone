;*=====================================================================*/
;*    .../prgm/project/bglstone/src/bigloo/bigloo/Cfa/iterate.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Feb 22 18:11:52 1995                          */
;*    Last change :  Fri Mar  7 07:42:54 2025 (serrano)                */
;*    Copyright   :  1995-2025 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    THE control flow analysis engine                                 */
;*=====================================================================*/
 
;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module cfa_iterate
   (include "Ast/node.sch" "Type/type.sch" "Cfa/cinfo.sch" "Cfa/cinfo.sch")
   (include "Tools/trace.sch")
   (import  tools_shape
	    type_type
	    ast_var
	    ast_node
	    ast_unit
	    cfa_cfa
	    cfa_info
	    cfa_info2
	    cfa_loose
	    cfa_approx)
   (export  (cfa-iterate-to-fixpoint! globals)
	    (cfa-intern-sfun!::approx ::intern-sfun/Cinfo ::obj)
	    (generic cfa-export-var! ::value ::obj)
	    (continue-cfa!)
	    (cfa-iterate! globals)
	    *cfa-stamp*))

;*---------------------------------------------------------------------*/
;*    cfa-iterate-to-fixpoint! ...                                     */
;*---------------------------------------------------------------------*/
(define (cfa-iterate-to-fixpoint! globals)
   ;; we reset the global stamp
   (set! *cfa-stamp* -1)
   ;; we collect all the exported variables (both functions and
   ;; variables). They are the root of the iteration process.
   (let ((glodefs '()))
      (for-each (lambda (g)
		   (if (eq? (global-import g) 'export)
		       (set! glodefs (cons g glodefs))))
		globals)
      ;; we add the top level forms
      (set! glodefs (append (unit-initializers) glodefs))
      ;; and we start iterations
      (continue-cfa!)
      ;; and we do it
      (let loop ()
	 (if (continue-cfa?)
	     (begin
		(cfa-iterate! glodefs)
		(trace cfa "<======= Cfa iteration!: " *cfa-stamp* #\Newline)
		(loop))
	     glodefs))))

;*---------------------------------------------------------------------*/
;*    cfa-iterate! ...                                                 */
;*---------------------------------------------------------------------*/
(define (cfa-iterate! globals)
   (stop-cfa!)
   (set! *cfa-stamp* (+fx 1 *cfa-stamp*))
   (trace cfa #\Newline "=======> Cfa iteration!: " *cfa-stamp* #\Newline)
   (for-each (lambda (g)
		(trace (cfa 2) "Exporting " (shape g) #\: #\Newline)
		(cfa-export-var! (global-value g) g)
		(trace (cfa 2) #\Newline)) 
	     globals)
   (trace cfa #\Newline))

;*---------------------------------------------------------------------*/
;*    cfa-export-var! ...                                              */
;*---------------------------------------------------------------------*/
(define-generic (cfa-export-var! value::value owner))

;*---------------------------------------------------------------------*/
;*    cfa-export-var! ::svar ...                                       */
;*---------------------------------------------------------------------*/
(define-method (cfa-export-var! value::svar/Cinfo owner)
   (with-access::intern-sfun/Cinfo sfun (stamp)
      (trace (cfa 3) "~~~ cfa-export/var!::svar/Cinfo[stamp: " stamp
	     " *cfa-stamp*: " *cfa-stamp*
	     "]" #\Newline)
      (if (=fx stamp *cfa-stamp*)
	  (cfa-variable-value-approx value)
	  (begin
	     (set! stamp *cfa-stamp*)
	     (loose! (cfa-variable-value-approx value) 'all)))))

;*---------------------------------------------------------------------*/
;*    cfa-export-var! ::intern-sfun/Cinfo ...                          */
;*---------------------------------------------------------------------*/
(define-method (cfa-export-var! value::intern-sfun/Cinfo owner)
   (with-access::intern-sfun/Cinfo value (stamp args approx)
      (trace (cfa 3) "  ~~~ cfa-export-var!::intern-sfun/Cinfo[stamp: " stamp 
	     " *cfa-stamp*: " *cfa-stamp*
	     "]" #\Newline)
      (if (=fx stamp *cfa-stamp*)
	  (begin
	     (set! stamp *cfa-stamp*)
	     approx)
	  (begin
	     ;; for each iteration, we re-loose the approximation of the
	     ;; formal parameters. Doing this, we don't have to take care
	     ;; when we add an approximation of a previous set if this
	     ;; set contains `top' or not.
	     (for-each (lambda (local)
			  (let ((val (local-value local)))
			     (trace (cfa 3) " ~~~ formal " (shape local)
				    " clo-env?: " (svar/Cinfo-clo-env? val)
				    #\Newline)
			     (if (not (svar/Cinfo-clo-env? val))
				 (approx-set-top! (svar/Cinfo-approx val)))))
		       args)
	     ;; after the formals, we loose the result.
	     (loose! (cfa-intern-sfun! value owner) 'all)))))
 
;*---------------------------------------------------------------------*/
;*    cfa-intern-sfun! ::intern-sfun/Cinfo ...                         */
;*---------------------------------------------------------------------*/
(define (cfa-intern-sfun!::approx sfun::intern-sfun/Cinfo owner)
   (with-access::intern-sfun/Cinfo sfun (stamp body approx args)
      (trace (cfa 3) "~~~ cfa-intern-sfun!: " (shape body) #\Newline)
      (if (=fx stamp *cfa-stamp*)
	  approx
	  (begin
	     (set! stamp *cfa-stamp*)
	     (union-approx! approx (cfa! body))
	     approx))))

;*---------------------------------------------------------------------*/
;*    The iteration process control                                    */
;*---------------------------------------------------------------------*/
(define *cfa-continue?* #unspecified)
(define *cfa-stamp*     -1)

;*---------------------------------------------------------------------*/
;*    continue-cfa! ...                                                */
;*---------------------------------------------------------------------*/
(define (continue-cfa!)
   (if (not *cfa-continue?*)
       (trace (cfa 2) "--> continue-cfa!" #\Newline))
   (set! *cfa-continue?* #t))

;*---------------------------------------------------------------------*/
;*    continue-cfa? ...                                                */
;*---------------------------------------------------------------------*/
(define (continue-cfa?)
   *cfa-continue?*)

;*---------------------------------------------------------------------*/
;*    stop-cfa! ...                                                    */
;*---------------------------------------------------------------------*/
(define (stop-cfa!)
   (set! *cfa-continue?* #f))


   

