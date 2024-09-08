;*=====================================================================*/
;*    serrano/uni/97-98/maitrise/compil/cgc/Arecord/arecord.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Mar  2 14:40:47 1998                          */
;*    Last change :  Thu Mar 26 09:19:20 1998 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The activation record allocation. This pass is runned after the  */
;*    the register allocation. Thus, it cannot use the classical       */
;*    instructions selection algorithm for saving and restoring fp     */
;*    and ra or pushing, poping the stack. We have to use machine      */
;*    dependent constructions. This is tedious to write because we     */
;*    have to put specific code in the arch_arch module for these      */
;*    constructions but, at least, it is not difficult to write.       */
;*=====================================================================*/
 
;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module arecord_arecord
   
   (import ast_node
	   ir_node
	   tools_speek
	   tools_set
	   engine_param
	   arch_arch
	   ast_node
	   iselect_asm
	   iselect_iselect)
   
   (export (arecord! il)))

;*---------------------------------------------------------------------*/
;*    arecord! ...                                                     */
;*    -------------------------------------------------------------    */
;*    This stage allocates activation record. That is activation       */
;*    record sizes have been computed by previous stages (instruction  */
;*    selection + register allocation). Now we have to allocate rooms  */
;*    inside stack for spill variables. This stage turn pseudo         */
;*    instructions proc-entry and proc-exit to pushes and pops.        */
;*    -------------------------------------------------------------    */
;*    This function scans all the instruction (il is the list of all   */
;*    the instructions) until it finds a pseudo-instruction. This      */
;*    instruction is replaced.                                         */
;*    -------------------------------------------------------------    */
;*    Here, we implement the following stack frame layout:             */
;*                                                                     */
;*                   |             |                                   */
;*                   +-------------+                                   */
;*           fp ->   |    old fp   |                                   */
;*                   +-------------+                                   */
;*                   |    escape   |                                   */
;*                   |  .......... |                                   */
;*                   |  variables  |                                   */
;*                   +-------------+                                   */
;*                   | return addr |                                   */
;*                   |      +      |                                   */
;*                   |   callee    |                                   */
;*                   |    saved    |                                   */
;*                   |  registers  |                                   */
;*                   +-------------+                                   */
;*                   |    saved    |                                   */
;*                   |  variables  |                                   */
;*                   | (because of |                                   */
;*                   |  spills).   |                                   */
;*                   +-------------+                                   */
;*           sp ->   |             |                                   */
;*                                                                     */
;*     At procedure entry we allocate stack room and we exchange sp    */
;*     and fp. At procedure exit, we restore sp with former fp.        */
;*---------------------------------------------------------------------*/
(define (arecord! il)
   (define (insert! list bundle)
      (let ((res (cdr list)))
	 (set-car! list (car bundle))
	 (set-cdr! list (cdr bundle))
	 (set-cdr! (last-pair bundle) res)
	 res))
   (verbose 1 #"  - Activation record allocation\n")
   (let loop ((old il))
      (cond
	 ((null? old)
	  il)
	 ((proc-entry? (car old))
	  ;; on a proc-entry we have to allocate room in the stack, push
	  ;; the old fp and old ra.
	  (with-access::framedecl (proc-entry-framedecl (car old))
		(id frame-pointer-saved? local-num saved-registers spill-num)
	     ;; the frame size (in word size) is the sum of:
	     ;;      the number of escaping variables
	     ;;    + the number of registers that may be pushed
	     ;;         (ra + callee saved registers)
	     ;;    + the number of spills
	     (let* ((save-num   (+fx (length saved-registers) spill-num))
		    (frame-size (+ 1 local-num save-num))
		    ;; the frame pointer stack offset
		    (fp-offset (*fx (arch-word-size) frame-size))
		    ;; the instruction that save the frame pointer
		    (save-fp (if frame-pointer-saved?
				 (arch-save-register (arch-fp-expr)
						     fp-offset)
				 (arch-nop-expr)))
		    ;; the instructions that save the registers
		    (save-regs (let loop ((regs   saved-registers)
					  (offset (*fx (-fx frame-size
							    (+fx 1 local-num))
						       (arch-word-size)))
					  (asms   '()))
				  (if (null? regs)
				      asms
				      (loop (cdr regs)
					    (-fx offset (arch-word-size))
					    (cons
					     (arch-save-register (car regs)
								 offset)
					     asms)))))
		    ;; the instruction that enlarge the stack
		    (enlarge-stack (arch-enlarge-stack (*fx (arch-word-size)
							    frame-size)))
		    ;; the instruction that loads the new frame pointer
		    (load-fp (if frame-pointer-saved? 
				 (arch-add-registers (arch-fp-expr)
						     (arch-sp-expr)
						     (*fx (arch-word-size)
							  frame-size))
				 (arch-nop-expr)))
		    ;; the instructions that make the new frame
		    (extra-instr `(,enlarge-stack
				   ,save-fp
				   ,@save-regs
				   ,load-fp))
		    ;; when in main we have to prepare gp
		    (extra+gp    (if (ident-is? id "main")
				     (cons (arch-set-gp) extra-instr)
				     extra-instr)))
		;; we have to prepare the execution here because we
		;; have no control over _main with spim
		(loop (insert! old extra+gp)))))
	 ((proc-exit? (car old))
	  ;; here, we have to restore former sp, fp and ra values callee
	  ;; registers and pop the stack.
	  (with-access::framedecl (proc-exit-framedecl (car old))
		(frame-pointer-saved? local-num saved-registers spill-num)
	     ;; all restorations must use fp relative indexes
	     (let* ((save-num   (+fx (length saved-registers) spill-num))
		    (frame-size (+ 1 local-num save-num))
		    ;; the frame pointer stack offset
		    (fp-offset (*fx (arch-word-size) frame-size))
		    ;; the instruction that restore the frame pointer
		    (load-fp (if frame-pointer-saved?
				 (arch-restore-register (arch-fp-expr)
							0
							(arch-fp-expr))
				 (arch-nop-expr)))
		    ;; the instructions that restore the registers
		    (load-regs (let loop ((regs   saved-registers)
					  (offset (*fx (+fx 1 local-num)
						       (arch-word-size)))
					  (asms   '()))
				  (if (null? regs)
				      asms
				      (loop (cdr regs)
					    (+fx offset (arch-word-size))
					    (cons
					     (arch-restore-register
					      (car regs)
					      (negfx offset)
					      (arch-fp-expr))
					     asms)))))
		    ;; the instruction that enlarge the stack
		    (shrink-stack (arch-shrink-stack (*fx (arch-word-size)
							  frame-size)))
		    ;; the old sp restore
		    (load-sp (arch-move-registers (arch-sp-expr)
						  (arch-fp-expr))))
		(loop (insert! old `(,@load-regs ,load-sp ,load-fp))))))
	 (else
	  (loop (cdr old))))))
