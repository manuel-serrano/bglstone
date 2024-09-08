;*=====================================================================*/
;*    .../97-98/maitrise/compil/cgc/Btensioning/btensioning.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Feb 14 18:40:17 1998                          */
;*    Last change :  Tue Mar  3 20:27:25 1998 (serrano)                */
;*    -------------------------------------------------------------    */
;*    This module implement a very straightforward branch tensioning   */
;*    optimization.                                                    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module btensioning_btensioning
   (import tools_speek
	   ast_node
	   ir_node
	   iselect_iselect
	   iselect_asm)
   (export (branch-tensioning asms)))

;*---------------------------------------------------------------------*/
;*    branch-tensioning ...                                            */
;*---------------------------------------------------------------------*/
(define (branch-tensioning srcs)
   (verbose 1 #"  - branch tensioning\n")
   [assert (*nop*) (not (eq? *nop* #unspecified))]
   (define (same-asm-label? lb1 lb2)
      (or (eq? lb1 lb2)
	  (ident=? (label-ident lb1) (label-ident lb2))))
   (if (null? (cdr srcs))
       srcs
       ;; In order to avoid inifite loops, we maintain an `old' variable
       ;; called unloop.
       (let loop ((asms   srcs)
		  (unloop #unspecified))
	  (cond
	     ((null? (cdr asms))
	      (set-succ! srcs))
	     ((eq? (car asms) unloop)
	      (loop (cdr asms) (car asms)))
	     ((asm-jump? (car asms))
	      (with-access::asm-jump (car asms) (asm-label succ def use)
		 (if (and (asm-label? asm-label) (asm-label? (cadr asms)))
		     (if (same-asm-label? asm-label (cadr asms))
			 (let ((new (duplicate::asm-oper *nop*
				       (succ succ))))
			    (set-car! asms new))
			 (let ((next (asm-label-succ asm-label)))
			    (if (asm-jump? next)
				(let ((old-car (car asms)))
				   (set-car! asms next)
				   (loop asms old-car))))))
		 (loop (cdr asms) (car asms))))
	     (else
	      (loop (cdr asms) (car asms)))))))
