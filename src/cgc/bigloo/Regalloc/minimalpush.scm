;*=====================================================================*/
;*    serrano/trashcan/cgc/Regalloc/minimalpush.scm                    */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Mar 22 11:23:34 1998                          */
;*    Last change :  Wed Dec 27 18:45:06 2000 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The `push' register allocator. This allocator does not make use  */
;*    of callee save registers but it tries to minimize the frame size */
;*    by re-using previously allocated room.                           */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module regalloc_minimal-push
   (import ast_node
	   ir_node
	   tools_speek
	   tools_set
	   engine_param
	   arch_arch
	   ast_node
	   iselect_asm
	   iselect_iselect)
   (export (minimal-push! asms)))

;*---------------------------------------------------------------------*/
;*    minimal-push! ...                                                */
;*---------------------------------------------------------------------*/
(define (minimal-push! asms)
   (verbose 2 " (`minimal push' allocator)")
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
(define (allocate-registers-at asm::asm-oper frame-decl::framedecl)
   (with-access::asm-oper asm (use def live-in live-out trash)
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
	    (let* ((iuse (allocate-registers-at/use frame-decl
						    use
						    cur-free
						    cur-used
						    live-in
						    live-out))
		   (idef (allocate-registers-at/def frame-decl
						    def
						    nex-free
						    nex-used
						    live-in
						    live-out)))
	       (multiple-value-bind (itrash irestore)
		  (spill-trashed-registers-at frame-decl
					      use
					      trash
					      live-in
					      live-out)
		  (if (>= *verbose* 3)
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
(define (allocate-registers-at/use framedecl use free-regs used-regs lin lout)
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
			   (assign-register! framedecl
					     (car temps)
					     free
					     used
					     lin
					     lout)
			   (let ((load (arch-restore-register (temp-hardware
							       (car temps))
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
			(assign-register! framedecl
					  (car temps)
					  free
					  used
					  lin
					  lout)
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
(define (allocate-registers-at/def framedecl def free-regs used-regs lin lout)
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
			(assign-register! framedecl (car temps)
					  free used lin lout)
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
(define (spill-trashed-registers-at fd::framedecl use trash lin lout)
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
		  (set-member? lout (ireg-temp (car temps))))
	     ;; this temporary has to be spilled
	     (let* ((reg    (car temps))
		    (temp   (ireg-temp (car temps)))
		    (offset (framedecl-get-new-spill-offset fd temp lin lout)))
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
(define (assign-register! framedecl temp::temp free used lin lout)
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
       [assert (temp) (temp? temp)]
       ;; before assigning a register we have to free one (to spill one)
       (let* ((reg    (car used))
	      (old-tmp (ireg-temp reg))
	      (offset (framedecl-get-new-spill-offset framedecl temp lin lout))
	      (spill  (arch-save-register reg offset (arch-fp-expr))))
	  (temp-hardware-set! old-tmp offset)
	  (assign-internal! reg spill free used)))))
	  
;*---------------------------------------------------------------------*/
;*    framedecl-get-new-spill-offset ...                               */
;*    -------------------------------------------------------------    */
;*    This function scans the list of spilled variable in the hope     */
;*    to find one that is neither live-in nor live-out.                */
;*---------------------------------------------------------------------*/
(define (framedecl-get-new-spill-offset fd::framedecl temp live-in live-out)
   (define (vector-realloc vector)
      (let* ((old-len (vector-length vector))
	     (len     (*fx 2 old-len))
	     (res     (make-vector len '())))
	 (let loop ((i 0))
	    (if (=fx i old-len)
		res
		(begin
		   (vector-set! res i (vector-ref vector i))
		   (loop (+fx i 1)))))))
   (define (spill-temporary! spill-num)
      (with-access::framedecl fd (local-num saved-registers spilled)
	 (let ((delta (+fx local-num (length saved-registers))))
	    ;; if the spilled vector is not large enough
	    ;; we have to enlarge it.
	    (cond
	       ((not (vector? spilled))
		(set! spilled (make-vector 10)))
	       ((>=fx spill-num (vector-length spilled))
		(set! spilled (vector-realloc spilled))))
	    ;; we remember the spill
	    (vector-set! spilled (-fx spill-num 1) temp)
	    (negfx (*fx (arch-word-size) (+fx spill-num delta))))))
   [assert (temp) (temp? temp)]
   (with-access::framedecl fd (spill-num spilled)
      (let loop ((i (-fx spill-num 1)))
	 (if (=fx i -1)
	     (begin
		(set! spill-num (+fx 1 spill-num))
		(spill-temporary! spill-num))
	     ;; we look if this spilled variable is still live (that is:
	     ;; is this temporary in live-in or live-out ?)
	     (let ((tp (vector-ref spilled i)))
		(if (and (not (eq? tp temp))
			 (or (set-member? live-in tp)
			     (set-member? live-out tp)))
		    (loop (-fx i 1))
		    (spill-temporary! (+fx i 1))))))))

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
	 
