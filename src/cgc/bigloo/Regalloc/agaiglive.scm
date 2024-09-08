;*=====================================================================*/
;*    serrano/uni/97-98/maitrise/compil/cgc/Regalloc/agaiglive.scm     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Mar 22 11:23:34 1998                          */
;*    Last change :  Wed Mar 25 11:03:53 1998 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The `as good as it gets but live' allocator. This stupid         */
;*    allocator attempts to allocate a new register for each new temp  */
;*    it founds. If this allocation is not possible, this allocator    */
;*    stops with an error message. The only smart thing here is that   */
;*    when a temporary is dead, its register is re-used.               */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module regalloc_agaig/live
   
   (import ast_node
	   ir_node
	   tools_speek
	   tools_set
	   engine_param
	   arch_arch
	   ast_node
	   iselect_asm
	   iselect_iselect)
   
   (export (agaig/liveness! asms)))

;*---------------------------------------------------------------------*/
;*    agaig/liveness! ...                                              */
;*    -------------------------------------------------------------    */
;*    This implementation is likely to fail on true program. At        */
;*    each iteration (each time, we process a new instruction), we     */
;*    mark the live temporary register as used. The complexity of      */
;*    this strategy is much too high.                                  */
;*---------------------------------------------------------------------*/
(define (agaig/liveness! asms)
   (verbose 2 " (`as good as it gets but live' allocator)")
   (verbose 1 #"\n")
   (let loop ((asms           asms)
	      (free-registers (arch-caller-save-registers)))
      (if (pair? asms)
	  (let ((asm (car asms)))
	     (if (asm-oper? asm)
		 (with-access::asm-oper asm (def live-out)
		    ;; first we compute the next free registers
		    (let ((free (arch-caller-save-registers)))
		       (for-each (lambda (live)
				    (with-access::temp live (hardware)
				       (if (temp? hardware)
					   (set! free (remq hardware free)))))
				 (set->list live-out))
		       ;; now, we allocate the temporaries for the defined ones
		       (let laap ((free-registers free)
				  (temporaries    def))
			  (if (null? temporaries)
			      (loop (cdr asms) free)
			      (laap (allocate-one-register (car temporaries)
							   free)
				    (cdr temporaries))))))
		 (loop (cdr asms) free-registers)))))
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

