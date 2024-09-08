;*=====================================================================*/
;*    serrano/trashcan/cgc/Regalloc/push.scm                           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Mar 22 11:23:34 1998                          */
;*    Last change :  Wed Dec 27 20:27:37 2000 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The `push' register allocator. This allocator does not make use  */
;*    of callee save registers.                                        */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module regalloc_push
   
   (import ast_node
	   ir_node
	   tools_speek
	   tools_set
	   engine_param
	   arch_arch
	   ast_node
	   iselect_asm
	   iselect_iselect)
   
   (export (push! asms)))

(define (check-instr i)
   (define (check i)
      (if (asm-oper? i)
	  (with-access::asm-oper i (def)
	     (for-each (lambda (t)
			  (if (not (temp? t))
			      (error "Not a temp" i t)))
		       def))))
   (if (pair? i)
       (for-each check i)
       (check i)))

;*---------------------------------------------------------------------*/
;*    push! ...                                                        */
;*---------------------------------------------------------------------*/
(define (push! asms)
   (verbose 2 " (`push' allocator)")
   (verbose 1 #"\n")
   (let loop ((asms      asms)
	      (framedecl #unspecified)
	      (res       '()))
      (if (pair? asms)
	  (let* ((asm       (car asms))
		 (framedecl (if (label? asm)
				(label-framedecl asm)
				framedecl)))
	     (if (and (asm-oper? asm) (framedecl? framedecl))
		 (multiple-value-bind (before after)
		    (allocate-registers-at asm framedecl)
		    (check-instr before)
		    (check-instr after)
		    (check-instr asm)
		    (loop (cdr asms)
			  framedecl
			  (append after
				  (cons asm (append (reverse! before) res)))))
		 (loop (cdr asms) framedecl (cons (car asms) res))))
	  (reverse! res))))

;*---------------------------------------------------------------------*/
;*    allocate-registers-at ...                                        */
;*    -------------------------------------------------------------    */
;*    This function makes the register allocation for one instruction. */
;*    It makes side effects to temporaries and it returns an           */
;*    instruction list that corresponds to spills and restores.        */
;*---------------------------------------------------------------------*/
(define (allocate-registers-at asm::asm-oper fd)
   (with-access::asm-oper asm (use def live-out trash)
      ;; first we compute the next free registers and next
      ;; used register
      (multiple-value-bind (nex-free nex-used)
	 (find-next-free-and-used-registers asm)
	 ;; the we compute the current free and used registers
	 (multiple-value-bind (cur-free cur-used)
	    (find-current-free-and-used-registers asm)
	    ;; the following step is to allocate registers
	    ;; for all used temporaries	 then, to allocate registers
	    ;; for defined temporaries. And last, to emit spill code
	    ;; for trashed registers and live-out registers
	    (let* ((iuse (allocate-registers-at/use fd use cur-free cur-used))
		   (idef (allocate-registers-at/def fd def nex-free nex-used)))
	       (multiple-value-bind (itrash irestore)
		  (spill-trashed-registers-at fd use trash live-out)
		  (if (>= *verbose* 4)
		      (begin
			 (if (pair? iuse)
			     (begin
				(print #"spill for use: \n"
				       "---------------")
				(print iuse "(" asm ")")))
			 (if (pair? idef)
			     (begin
				(print #"spill for def: \n"
				       "---------------")
				(print idef "(" asm ")")))
			 (if (pair? itrash)
			     (begin
				(print #"spill for trash: \n"
				       "---------------")
				(print itrash "(" asm ")")))))
		  ;; now we are done, we return
		  (values (append itrash idef iuse) irestore)))))))

;*---------------------------------------------------------------------*/
;*    allocate-registers-at/use ...                                    */
;*    -------------------------------------------------------------    */
;*    We compile the used temporaried. That is, for each used          */
;*    temporary, we make the following:                                */
;*      a - if the temporary is currently in a register, we            */
;*          simply use that register.                                  */
;*      b - if the temporary is currently spilled we find a free       */
;*          free register (this may cause a spill) for the tempoary    */
;*      c - else we allocate a free register (this is subject to       */
;*          another spill cause).                                      */
;*      d - the register is an hard register.                          */
;*    -------------------------------------------------------------    */
;*    This function returns a list of instructions.                    */
;*---------------------------------------------------------------------*/
(define (allocate-registers-at/use framedecl use free-regs used-regs)
   (let loop ((temps use)
	      (asms '())
	      (free  free-regs)
	      (used  used-regs))
      (if (null? temps)
	  asms
	  (if (ireg? (car temps))
	      ;; this is situation d
	      (loop (cdr temps) asms free used)
	      (with-access::temp (car temps) (hardware)
		 (cond
		    ((temp? hardware)
		     ;; This is situation a
		     (loop (cdr temps) asms free used))
		    ((integer? hardware)
		     ;; This is situation b
		     (let ((offset hardware))
			(multiple-value-bind (spill free used)
			   (assign-register! framedecl (car temps) free used)
			   (let ((load (arch-restore-register
					(temp-hardware (car temps))
					offset
					(arch-fp-expr))))
			      (loop (cdr temps)
				    (if spill
					(cons spill (cons load asms))
					(cons load asms))
				    free
				    used)))))
		    (else
		     ;; This is situation c
		     (multiple-value-bind (spill free used)
			(assign-register! framedecl (car temps) free used)
			(loop (cdr temps)
			      (if spill
				  (cons spill asms)
				  asms)
 			      free
			      used)))))))))
				     
;*---------------------------------------------------------------------*/
;*    allocate-registers-at/def ...                                    */
;*    -------------------------------------------------------------    */
;*    The process here is simplier than for the used registers because */
;*    we don't have to take care about previous value of def registers.*/
;*    -------------------------------------------------------------    */
;*    However, we must not allocate a fresh register on each           */
;*    definition site. It could be that the temporary is already       */
;*    allocated. This situation may arise because of basic block       */
;*    optimization. The register allocator could see a temporary       */
;*    usage before its definition.                                     */
;*    -------------------------------------------------------------    */
;*    This function returns another list of instructions.              */
;*---------------------------------------------------------------------*/
(define (allocate-registers-at/def framedecl def free-regs used-regs)
   (let loop ((temps def)
	      (asms '())
	      (free  free-regs)
	      (used  used-regs))
      (if (null? temps)
	  asms
	  (if (ireg? (car temps))
	      (loop (cdr temps) asms free used)
	      (with-access::temp (car temps) (hardware)
		 (cond
		    ((temp? hardware)
		     (loop (cdr temps) asms free used))
		    (else
		     (multiple-value-bind (spill free used)
			(assign-register! framedecl (car temps) free used)
			(loop (cdr temps)
			      (if spill
				  (cons spill asms)
				  asms)
			      free
			      used)))))))))

;*---------------------------------------------------------------------*/
;*    spill-trashed-registers-at ...                                   */
;*    -------------------------------------------------------------    */
;*    This functions looks for each trashed registers the one that     */
;*    are also live after the instruction and that have to be saved    */
;*    (that is spilled).                                               */
;*---------------------------------------------------------------------*/
(define (spill-trashed-registers-at framedecl use trash live-out)
   (let ((fp (arch-fp-expr)))
      (let loop ((temps   trash)
		 (spill   '())
		 (restore '()))
	 (cond
	    ((null? temps)
	     ;; we are done with these trashes
	     (values spill restore))
	    ((and (temp? (ireg-temp (car temps)))
		  (not (ireg? (ireg-temp (car temps))))
		  (set-member? live-out (ireg-temp (car temps))))
	     ;; this temporary has to be spilled
	     (let ((offset (framedecl-get-new-spill-offset framedecl))
		   (reg    (car temps)))
		(loop (cdr temps)
		      (cons (arch-save-register reg offset fp) spill)
		      (cons (arch-restore-register reg offset fp) restore))))
	    (else
	     ;; this register is trashed but dead after the instruction
	     (loop (cdr temps) spill restore))))))

;*---------------------------------------------------------------------*/
;*    assign-register! ...                                             */
;*    -------------------------------------------------------------    */
;*    This function assigns a register to a temporay. When it is       */
;*    required, this function may produce a spill. 3 values are        */
;*    returned:                                                        */
;*       - a possible spill code                                       */
;*       - a new free list                                             */
;*       - a new used list                                             */
;*---------------------------------------------------------------------*/
(define (assign-register! framedecl temp::temp free used)
   (define (assign-internal! reg spill free used)
      (with-access::temp temp (hardware)
	 (set! hardware reg)
	 (ireg-temp-set! reg temp)
	 (values spill free used)))
   (cond
      ((pair? free)
       ;; this is easy we have enough register
       (assign-internal! (car free) #f (cdr free) used))
      (else
       [assert (used) (pair? used)]
       ;; before assigning a register we have to free one (to spill one)
       (let* ((reg    (car used))
	      (offset (framedecl-get-new-spill-offset framedecl))
	      (spill  (arch-save-register reg offset (arch-fp-expr))))
	  (if (temp? (ireg-temp reg))
	      (temp-hardware-set! (ireg-temp reg) offset))
	  (assign-internal! reg spill free used)))))
	  
;*---------------------------------------------------------------------*/
;*    framedecl-get-new-spill-offset ...                               */
;*---------------------------------------------------------------------*/
(define (framedecl-get-new-spill-offset framedecl)
   (with-access::framedecl framedecl (spill-num local-num saved-registers)
	 (set! spill-num (+fx 1 spill-num))
	 (negfx (*fx (arch-word-size) (+ spill-num
					 local-num
					 (length saved-registers))))))

;*---------------------------------------------------------------------*/
;*    find-next-free-and-used-registers ...                            */
;*    -------------------------------------------------------------    */
;*    This function returns the two lists of free and used             */
;*    registers that characterize the registers status after the       */
;*    asm instruction (that is, when asm will have been evaluated).    */
;*---------------------------------------------------------------------*/
(define (find-next-free-and-used-registers asm)
   (with-access::asm-oper asm (live-out)
      (find-free-and-used-registers asm live-out)))

;*---------------------------------------------------------------------*/
;*    find-current-free-and-used-registers ...                         */
;*    -------------------------------------------------------------    */
;*    This function computes the lists of currently (that is when      */
;*    evaluating the current instruction) which register are free      */
;*    and which are used.                                              */
;*---------------------------------------------------------------------*/
(define (find-current-free-and-used-registers asm)
   (with-access::asm-oper asm (live-in)
      (find-free-and-used-registers asm live-in)))

;*---------------------------------------------------------------------*/
;*    find-free-and-used-registers ...                                 */
;*---------------------------------------------------------------------*/
(define (find-free-and-used-registers asm live)
   (let ((free (arch-caller-save-registers))
	 (used '()))
      (set-for-each (lambda (live)
		       (with-access::temp live (hardware)
			  (if (temp? hardware)
			      (begin
				 (set! free (remq hardware free))
				 (set! used (cons hardware used))))))
		    live)
      (values free used)))
	 
