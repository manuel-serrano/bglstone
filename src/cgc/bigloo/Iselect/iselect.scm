;*=====================================================================*/
;*    serrano/uni/99-00/compilation/cgc0.1/Iselect/iselect.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Feb 14 14:38:12 1998                          */
;*    Last change :  Tue Jan 18 15:15:20 2000 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The instructions selection                                       */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module iselect_iselect
   (import arch_arch
	   ast_node
	   ir_node
	   iselect_asm)
   (export (instruction-selection ::ir)
	   (set-succ! l)
	   *nop?*
	   *nop*))

;*---------------------------------------------------------------------*/
;*    instruction-selection ...                                        */
;*---------------------------------------------------------------------*/
(define (instruction-selection ir)
   (let ((il (arch-iselect ir)))
      ;; we now have to setup succ fields
      (set-succ! il)
      il))

;*---------------------------------------------------------------------*/
;*    set-succ! ...                                                    */
;*    -------------------------------------------------------------    */
;*    This function sets up the succ fields. It has to be invoked      */
;*    several times. For instance, after the no-nop and branch         */
;*    tensioning stages.                                               */
;*---------------------------------------------------------------------*/
(define (set-succ! il)      
   (let loop ((l il))
      (if (pair? l)
	  (let ((next (if (pair? (cdr l))
			  (cadr l)
			  '())))
	     (cond
		((asm-jump? (car l))
		 (with-access::asm-jump (car l) (succ asm-label)
		    (set! succ (list asm-label))))
		((asm-cjump? (car l))
		 (with-access::asm-cjump (car l) (succ asm-label)
		    (set! succ (list asm-label next))))
		((asm-oper? (car l))
		 (with-access::asm-oper (car l) (succ)
		    (set! succ (list next))))
		((asm-label? (car l))
		 (with-access::asm-label (car l) (succ)
		    (set! succ (list next))))
		((label? (car l))
		 (widen!::asm-label (car l)
		    (succ '()))))
	     (loop (cdr l)))
	  il)))

;*---------------------------------------------------------------------*/
;*    *nop* ...                                                        */
;*---------------------------------------------------------------------*/
(define *nop* #unspecified)

;*---------------------------------------------------------------------*/
;*    *nop?* ...                                                       */
;*---------------------------------------------------------------------*/
(define *nop?* #unspecified)







