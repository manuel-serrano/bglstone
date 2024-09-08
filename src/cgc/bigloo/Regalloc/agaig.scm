;*=====================================================================*/
;*    serrano/uni/97-98/maitrise/compil/cgc/Regalloc/agaig.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Mar 22 11:23:34 1998                          */
;*    Last change :  Wed Mar 25 11:04:02 1998 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The `as good as it gets' allocator. This stupid allocator        */
;*    attempts to allocate a new register for each new temp it         */
;*    founds. If this allocation is not possible, this allocator       */
;*    stops with an error message.                                     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module regalloc_agaig
   
   (import ast_node
	   ir_node
	   tools_speek
	   tools_set
	   engine_param
	   arch_arch
	   ast_node
	   iselect_asm
	   iselect_iselect)
   
   (export (agaig! asms)))

;*---------------------------------------------------------------------*/
;*    agaig! ...                                                       */
;*---------------------------------------------------------------------*/
(define (agaig! asms)
   (verbose 2 " (`as good as it gets' allocator)")
   (verbose 1 #"\n")
   (let loop ((asms           asms)
	      (free-registers (arch-caller-save-registers)))
      (if (pair? asms)
	  (let ((asm (car asms)))
	     (if (asm-oper? asm)
		 (with-access::asm-oper asm (def)
		    (let laap ((free-registers free-registers)
			       (temporaries    def))
		       (if (null? temporaries)
			   (loop (cdr asms) free-registers)
			   (laap (allocate-one-register (car temporaries)
							free-registers)
				 (cdr temporaries)))))
		 (loop (cdr asms) free-registers)))
	  (verbose 2 "    . "
		   (-fx (length (arch-caller-save-registers))
			(length free-registers))
		   #" registers used\n")))
   asms)

;*---------------------------------------------------------------------*/
;*    allocate-one-register ...                                        */
;*    ------------------------------------------------------------     */
;*    This function allocate a fresh register for a temporary.         */
;*    This function makes a side effect to the temp structure and      */
;*    returns the new register free list.                              */
;*---------------------------------------------------------------------*/
(define (allocate-one-register temp register-list)
   (cond
      ((ireg? temp)
       ;; this temporary is allocated to a specific register. Nothing to do
       register-list)
      ((temp? (temp-hardware temp))
       ;; this temporary has already been allocated a register
       register-list)
      ((null? register-list)
       ;; we cannot allocate anymore
       (error "cgc" "Can't allocate register" temp))
      (else
       ;; ok, lets allocate a fresh register
       (temp-hardware-set! temp (car register-list))
       (cdr register-list))))

