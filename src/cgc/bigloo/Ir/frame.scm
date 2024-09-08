;*=====================================================================*/
;*    serrano/uni/00-01/compilation/cgc/Ir/frame.scm                   */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Feb  5 14:40:31 1998                          */
;*    Last change :  Wed Nov 29 14:29:58 2000 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The frame (architecture independent) management.                 */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module ...                                                   */
;*---------------------------------------------------------------------*/
(module ir_frame
   (import tools_speek
	   engine_param
	   ast_node
	   ir_node
	   ir_label
	   ir_translate
	   arch_arch)
   (export (make-new-framedecl::framedecl ::fundecl ::obj)))
   
;*---------------------------------------------------------------------*/
;*    make-new-framedecl ...                                           */
;*    -------------------------------------------------------------    */
;*    When we enter this function, the static link has already been    */
;*    added (if it was necessary to, of course).                       */
;*    It is important to proceed that way because we want ir to be     */
;*    source language independant. Thus, there is no reason, in the    */
;*    frame allocation to add static link because the source language  */
;*    may not be needing it.                                           */
;*---------------------------------------------------------------------*/
(define (make-new-framedecl::framedecl fundecl::fundecl link)
   (with-access::fundecl fundecl (id params)
      ;; A little bit of verbosity. We print the framedecl identifier
      ;; with a pretty print. We use the link field to know the margin
      ;; value of the print.
      (if (verbose-level? 3)
	  (begin
	     (verbose 3 "       " (if (framedecl? link) "  " ". "))
	     (let loop ((link link))
		(if (not (framedecl? link))
		    (verbose 3 id #\Newline)
		    (begin
		       (verbose 3 "  ")
		       (loop (framedecl-link link)))))))
      ;; we make the selection
      (let loop ((params     params)
		 ;; the registers used by function calls
		 (registers  (arch-arg-registers))
		 ;; the offset outside the frame (that is for stack args).
		 (out-offset 0)
		 ;; the offset inside the frame (that is for local vars).
		 (in-offset  0)
		 ;; the shift of view inside the frame
		 (view-shift (if (null? params)
				 ;; when there is no parameters (which is
				 ;; not possible with cgc because of the
				 ;; static link) we use nop as view shift.
				 (list (instantiate::estmt
					  (>expr (no-op))))
				 '())))
	 (cond
	    ((null? params)
	     ;; we are done with the parameters
	     (let* ((str      (ident-name id))
		    (prolog   (get-label str fundecl))
		    (epilogue (get-label (string-append str "_ret") fundecl))
		    (vs       (instantiate::seq
				 (stmts (if (list? view-shift)
					    view-shift
					    (list (instantiate::estmt
						     (>expr (no-op)))))))))
		(widen!::framedecl fundecl
		   (link       link)
		   (prolog     prolog)
		   (epilogue   epilogue)
		   (local-num  in-offset)
		   ;; we set a dummy ir-stmt value that will be overwritten
		   ;; as soon as the frame will be completed.
		   (ir-stmt    (instantiate::seq
				  (stmts '()))))
		(with-access::framedecl fundecl (ir-stmt body)
		   (set! ir-stmt
			 (instantiate::seq
			    (stmts (list vs
					 (anode->inode body
						       fundecl
						       in-offset))))))
		fundecl))
	    ((local-escape? (car params))
	     ;; this parameter goes in memory
	     (let* ((ws        (arch-word-size))
		    (fp        (arch-fp-expr))
		    (offset    (instantiate::ir-const
				  (value (if (arch-stack-grows-down?)
					     (negfx (*fx (+fx 1 in-offset) ws))
					     (*fx (+fx 1 in-offset) ws)))))
		    (arg-addr  (plus fp offset))
		    (arg-fetch (instantiate::mem
				  (addr arg-addr))))
		(with-access::local (car params) (fetch)
		   (set! fetch offset))
		;; the shift of view instruction
		(if (null? registers)
		    ;; this parameter has been transmitted using the stack
		    (let* ((offset   (instantiate::ir-const
					(value (*fx out-offset ws))))
			   (get-expr (instantiate::mem
					(addr (plus fp offset))))
			   (vs-expr  (instantiate::move-mem
					(addr arg-addr)
					(k    ws)
					(expr get-expr))))
		       (loop (cdr params)
			     '()
			     (+fx 1 out-offset)
			     (+fx 1 in-offset)
			     (cons vs-expr view-shift)))
		    ;; this parameter has been send thru a register
		    (let* ((get-expr (car registers))
			   (vs-expr  (instantiate::move-mem
					(addr arg-addr)
					(k    ws)
					(expr get-expr))))
		       (loop (cdr params)
			     (cdr registers)
			     out-offset
			     (+fx 1 in-offset)
			     (cons vs-expr view-shift))))))
	    (else
	     ;; this parameter goes inside a temporary
	     (let* ((temp      (make-new-temp))
		    (arg-fetch temp))
		(with-access::local (car params) (fetch)
		   (set! fetch arg-fetch))
		;; the shift of view instruction
		(if (null? registers)
		    ;; this parameter has been transmitted using the stack
		    (let* ((ws       (arch-word-size))
			   (fp       (arch-fp-expr))
			   (offset   (instantiate::ir-const
					(value (*fx out-offset ws))))
			   (get-expr (instantiate::mem
					(addr (plus fp offset))))
			   (vs-expr  (instantiate::move-temp
					(temp temp)
					(expr get-expr))))
		       (loop (cdr params)
			     '()
			     (+fx 1 out-offset)
			     in-offset
			     (cons vs-expr view-shift)))
		    ;; this parameter has been sent thru a register
		    (let* ((get-expr (car registers))
			   (vs-expr  (instantiate::move-temp
					(temp temp)
					(expr get-expr))))
		       (loop (cdr params)
			     (cdr registers)
			     out-offset
			     in-offset
			     (cons vs-expr view-shift))))))))))
