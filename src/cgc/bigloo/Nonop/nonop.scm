;*=====================================================================*/
;*    serrano/uni/97-98/maitrise/compil/cgc/Nonop/nonop.scm            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Feb 14 18:40:17 1998                          */
;*    Last change :  Tue Mar  3 20:27:04 1998 (serrano)                */
;*    -------------------------------------------------------------    */
;*    We remove useless nop instructions.                              */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module nonop_nonop
   (import tools_speek
	   ast_node
	   ir_node
	   iselect_iselect
	   iselect_asm)
   (export (nonop asms)))

;*---------------------------------------------------------------------*/
;*    nonop ...                                                        */
;*---------------------------------------------------------------------*/
(define (nonop srcs)
   (verbose 1 #"  - nonop\n")
   [assert (*nop?*) (procedure? *nop?*)]
   (let loop ((srcs srcs))
      (cond
	 ((null? srcs)
	  srcs)
	 ((*nop?* (car srcs))
	  (loop (cdr srcs)))
	 (else
	  (if (null? (cdr srcs))
	      srcs
	      (let loop ((asms (cdr srcs))
			 (prev srcs))
		 (cond
		    ((null? asms)
		     (set-succ! srcs))
		    ((*nop?* (car asms))
		     (set-cdr! prev (cdr asms))
		     (loop (cdr asms) prev))
		    (else
		     (loop (cdr asms) asms)))))))))
