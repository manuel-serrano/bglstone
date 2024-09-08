;*=====================================================================*/
;*    serrano/uni/99-00/compilation/cgc0.1/Regalloc/calleepush.scm     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Mar 22 11:23:34 1998                          */
;*    Last change :  Wed Apr 26 19:09:06 2000 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The `push' register allocator. This allocator is quite similar   */
;*    to the minimal-push. The difference comes from that this         */
;*    allocator makes use of callee save register.                     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module regalloc_callee-push
   (import ast_node
	   ir_node
	   tools_speek
	   tools_set
	   engine_param
	   arch_arch
	   ast_node
	   iselect_asm
	   iselect_iselect)
   (export (callee-push! asms)))

;*---------------------------------------------------------------------*/
;*    callee-push! ...                                                 */
;*---------------------------------------------------------------------*/
(define (callee-push! asms)
   (verbose 2 " (`callee push' allocator)")
   (verbose 1 #"\n")
   (let loop ((asms      asms)
	      (framedecl #unspecified)
	      (res       '()))
      (if (pair? asms)
	  (let* ((asm           (car asms))
		 (new-framedecl (if (label? asm)
				    (label-framedecl asm)
				    framedecl)))
	     ;; When entering a new function, we must clear
	     ;; the callee registers mark (otherwise we will be
	     ;; thinking that they are already pushed). If basic block
	     ;; ordering breaks the function order, this callee save
	     ;; registers can be implementated in a different way. When
	     ;; using a callee save register (in the function that produces
	     ;; the spill) we can simply check if the callee register is
	     ;; in the framedecl saved register list.
	     (for-each (lambda (ireg)
			  (ireg-temp-set! ireg #unspecified))
		       (arch-callee-save-registers))
	     (if (and (asm-oper? asm) (framedecl? new-framedecl))
		 (multiple-value-bind (before lasm after)
		    (allocate-registers-at asm new-framedecl)
		    (loop (cdr asms)
			  new-framedecl
			  (append after lasm (reverse! before) res)))
		 (loop (cdr asms) new-framedecl (cons (car asms) res))))
	  (begin
	     (if (and (>=fx *optim* 1) (>=fx *verbose* 2))
		 (begin
		    (print "      - move removed: "*move-removed*)))
	     (reverse! res)))))

;*---------------------------------------------------------------------*/
;*    *move-removed*                                                   */
;*---------------------------------------------------------------------*/
(define *move-removed* 0)

;*---------------------------------------------------------------------*/
;*    allocate-registers-at ...                                        */
;*---------------------------------------------------------------------*/
(define-generic (allocate-registers-at asm::asm-oper frame-decl))

;*---------------------------------------------------------------------*/
;*    allocate-registers-at ...                                        */
;*    -------------------------------------------------------------    */
;*    This function makes the register allocation for one instruction. */
;*    It makes side effects to temporaries and it returns an           */
;*    instruction list that corresponds to spills and restores.        */
;*---------------------------------------------------------------------*/
(define-method (allocate-registers-at asm::asm-oper frame-decl)
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
		  (values (append itrash idef iuse) (list asm) irestore)))))))

;*---------------------------------------------------------------------*/
;*    allocate-registers-at ::asm-move ...                             */
;*---------------------------------------------------------------------*/
(define-method (allocate-registers-at asm::asm-move frame-decl)
   (if (<fx *optim* 1)
       (call-next-method)
       (with-access::asm-move asm (use def live-in live-out)
	  (let ((src (car use))
		(dst (car def)))
	     (if (and (not (ireg? dst))
		      (not (ireg? src))
		      (not (set-member? live-out src)))
		 ;; we may optimize this move construction
		 ;; first we compute the next free registers and next
		 ;; used register
		 (multiple-value-bind (nex-free nex-used)
		    (find-next-free-and-used-registers asm)
		    ;; the we compute the current free and used registers
		    (multiple-value-bind (cur-free cur-used)
		       (find-current-free-and-used-registers asm)
		       ;; the following step is to allocate registers
		       ;; for all used temporaries then, to allocate registers
		       ;; for defined temporaries. And last, to emit spill code
		       ;; for trashed registers and live-out registers
		       (let ((iuse (allocate-registers-at/use frame-decl
							      use
							      cur-free
							      cur-used
							      live-in
							      live-out)))
			  (multiple-value-bind (spill free used)
			     (assign-register! frame-decl
					       dst
					       (list (temp-hardware src))
					       '()
					       live-in
					       live-out)
			     (set! *move-removed* (+fx 1 *move-removed*))
			     (values iuse '() '())))))
		 (call-next-method))))))
   
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
			(multiple-value-bind (save free used)
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
				    (if save
					(cons save (cons load asms))
					(cons load asms))
				    free
				    used)))))
		    (else
		     ;; This is situation c
		     (multiple-value-bind (save free used)
			(assign-register! framedecl (car temps)
					  free used lin lout)
			(loop (cdr temps)
			      (if save
				  (cons save asms)
				  asms)
			      free
			      used)))))))))
				     
;*---------------------------------------------------------------------*/
;*    allocate-registers-at/def ...                                    */
;*    -------------------------------------------------------------    */
;*    The process here is simplier than for the used registers because */
;*    we don't have to take care about previous value of def registers.*/
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
		     (multiple-value-bind (save free used)
			(assign-register! framedecl (car temps)
					  free used lin lout)
			(loop (cdr temps)
			      (if save
				  (cons save asms)
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
(define (spill-trashed-registers-at fd use trash lin lout)
   (let loop ((temps   trash)
	      (save   '())
	      (restore '()))
      (cond
	 ((null? temps)
	  ;; we are done with these trashes
	  (values save restore))
	 ((and (temp? (ireg-temp (car temps)))
	       (not (ireg? (ireg-temp (car temps))))
	       (set-member? lout (ireg-temp (car temps))))
	  ;; this temporary has to be spilled
	  (let* ((reg    (car temps))
		 (temp   (ireg-temp (car temps))))
	     (multiple-value-bind (new-save new-restore)
		(framedecl-get-new-spill fd temp reg lin lout)
		(loop (cdr temps)
		      (cons new-save save)
		      (cons new-restore restore)))))
	 (else
	  ;; this register is trashed but dead after the instruction so
	  ;; we don't have to spill it.
	  (loop (cdr temps) save restore)))))

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
(define (assign-register! framedecl temp::temp free used::pair-nil lin lout)
   (define (assign-internal! reg save free used)
      (with-access::temp temp (hardware)
	 (set! hardware reg)
	 (ireg-temp-set! reg temp)
	 (values save free used)))
   (cond
      ((pair? free)
       ;; this is easy we have enough register
       (assign-internal! (car free) #f (cdr free) used))
      (else
       [assert (used) (pair? used)]
       [assert (temp) (temp? temp)]
       ;; before assigning a register we have to free one (to spill one)
       (let ((reg (car used)))
	  (multiple-value-bind (save _)
	     (framedecl-get-new-spill framedecl temp reg lin lout)
	     (assign-internal! reg save free used))))))
	  
;*---------------------------------------------------------------------*/
;*    framedecl-get-new-spill ...                                      */
;*    -------------------------------------------------------------    */
;*    This function scans the list of spilled variable in the hope     */
;*    to find one that is neither live-in nor live-out.                */
;*    -------------------------------------------------------------    */
;*    Prior to find a stack room, this function looks in the           */
;*    callee-save list to find a free register for spilling.           */
;*    -------------------------------------------------------------    */
;*    This function returns a pair. The instruction for saving and     */
;*    the one for restoring.                                           */
;*---------------------------------------------------------------------*/
(define (framedecl-get-new-spill fd::framedecl temp reg live-in live-out)
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
	    (let* ((off  (negfx (*fx (arch-word-size) (+fx spill-num delta))))
		   (fp   (arch-fp-expr))
		   (save (arch-save-register reg off fp))
		   (load (arch-restore-register reg off fp)))
	       (values save load)))))
   [assert (temp) (temp? temp)]
   ;; we first look in the callee list to find a free register
   (let loop ((callee (arch-callee-save-registers)))
      (if (null? callee)
	  ;; We have not find a free callee save register. In consequence,
	  ;; we use the stack.
	  (with-access::framedecl fd (spill-num spilled)
	     (let loop ((i (-fx spill-num 1)))
		(if (=fx i -1)
		    (begin
		       (set! spill-num (+fx 1 spill-num))
		       (spill-temporary! spill-num))
		    ;; we look if this spilled variable is still live (that is:
		    ;; is this temporary in live-in or live-out ?)
		    (let ((tp (vector-ref spilled i)))
		       (cond
			  ((and (not (eq? tp temp))
				(or (set-member? live-in tp)
				    (set-member? live-out tp)))
			   (loop (-fx i 1)))
			  (else
			   (spill-temporary! (+fx i 1))))))))
	  (let* ((reg (car callee))
		 (tp  (ireg-temp reg)))
	     (cond
		((or (not (temp? tp))
		     (or (eq? tp temp))
		     (not (and (set-member? live-in tp)
			       (set-member? live-out tp))))
		 (with-access::framedecl fd (saved-registers)
		    (if (not (temp? tp))
			;; this callee register has never been used
			(set! saved-registers (cons reg saved-registers)))
		    (ireg-temp-set! reg temp)
		    (let ((save    (arch-move-registers reg temp))
			  (restore (arch-move-registers temp reg)))
		       (values save restore))))
		(else
		 ;; we cannot use that register, we examine the next one
		 (loop (cdr callee))))))))

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
	 
