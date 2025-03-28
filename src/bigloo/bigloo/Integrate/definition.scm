;*=====================================================================*/
;*    .../bglstone/src/bigloo/bigloo/Integrate/definition.scm          */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Mar 13 16:16:29 1995                          */
;*    Last change :  Fri Mar  7 07:33:47 2025 (serrano)                */
;*    Copyright   :  1995-2025 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The integration of one global definition.                        */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module integrate_definition
   (include "Ast/node.sch" "Type/type.sch" "Integrate/iinfo.sch")
   (include "Tools/trace.sch"
	    "Tools/verbose.sch")
   (import  type_type
	    ast_var
	    ast_node
	    tools_shape
	    tools_speek
	    integrate_info
	    integrate_a
	    integrate_kk
	    integrate_u
	    integrate_ctn
	    integrate_g
	    integrate_kaptured
	    integrate_let-fun
	    integrate_node
	    integrate_local->global)
   (export  (integrate-definition! <global>)))
 
;*---------------------------------------------------------------------*/
;*    integrate-definition! ...                                        */
;*    -------------------------------------------------------------    */
;*    This pass is another globalization pass. This pass is            */
;*    mandatory for the C back-end because this language does not      */
;*    possesses local [recursive] functions. This theoretical          */
;*    fundations of this pass can be found in the Nitsan Seniak        */
;*    thesis about Sqil (page 100). Here are shortly described         */
;*    this pass' organization:                                         */
;*        i) we compute the Phi set (the set of used functions).       */
;*           The Phi we are computing is a subset of the real          */
;*           Phi. Here its formal definition:                          */
;*                  Phi = { f in PHI ^ !G( f ) }                       */
;*           ie. Phi = PHI minus all the Globalized function           */
;*           by the `Globalize' pass.                                  */
;*       ii) Compute A                                                 */
;*      iii) Compute K                                                 */
;*       iv) Compute K*                                                */
;*        v) Compute U                                                 */
;*       vi) Compute Cn, Ct and G                                      */
;*---------------------------------------------------------------------*/
(define (integrate-definition! global)
   (trace integrate
	  "========================================" #\newline
	  (shape global) #\Newline
	  "----------------------------------------" #\newline)
   (let* ((fun  (global-value global))
	  (body (sfun-body fun))
	  (A    (A global body)))
      (K*! (K! A global))
      (U!) 
      (let ((G (G! (Cn&Ct! A))))
	 ;; for each globalized function, we set the integrated
	 ;; functions in order to be able to build the new
	 ;; global functions
	 (if (null? G)
	     ;; an optimization to avoid useless long compilations.
	     (list global)
	     (begin
		;; we print the globalization result
		(verb-globalization G)
		(for-each
		 (lambda (f)
		    (if (and (local? f)
			     (not (sfun/Iinfo-G? (local-value f))))
			(let* ((g   (sfun/Iinfo-L (local-value f)))
			       (ifu (variable-value g)))
			   (sfun/Iinfo-Led-set! ifu
						(cons f
						      (sfun/Iinfo-Led ifu))))))
		 *phi*)
		;; for each function (local and global), we add/remove
		;; the integrated local functions.
		(for-each displace-let-fun! G)
		(displace-let-fun! global)
		;; we have computed for all the global functions (including
		;; the root global one) the new bodies. Now we compute
		;; the set of kaptyred variables (on for the local functions,
		;; of course).
		(set-kaptured! G)
		;; now for each function, we allocate a new
		;; global definition
		(let ((new-G (map local->global G)))
		   (sfun-body-set! fun (globalize! body global '()))
		   (trace integrate #a012 #\Newline #\Newline)
		   (cons global new-G)))))))

;*---------------------------------------------------------------------*/
;*    verb-globalization ...                                           */
;*---------------------------------------------------------------------*/
(define (verb-globalization G)
   (for-each (lambda (local)
		(verbose 3 "           " (shape local) " -->" #\Newline))
	     G))
 
