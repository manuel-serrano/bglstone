;*=====================================================================*/
;*    serrano/trashcan/cgc/Canonical/trace.scm                         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Feb 14 19:03:13 1998                          */
;*    Last change :  Wed Dec 27 15:24:53 2000 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The module that implements the trace computation.                */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module canonical_trace
   (import tools_speek
	   ast_node
	   ir_node
	   ir_label)
   (export (trace::ir ::ir)))

;*---------------------------------------------------------------------*/
;*    trace ...                                                        */
;*---------------------------------------------------------------------*/
(define (trace::ir ir::ir)
   (verbose 1 #"  - trace\n")
   ;; we start spliting the basic blocks into two sets. The basic blocks
   ;; that are entered by a jump and the others. We mark the called ones
   (with-access::ir ir (text-segment)
      (for-each (lambda (block)
		   (with-access::basic-block block (epilogue)
		      (let ((next (cond
				     ((jump? epilogue)
				      (jump-addr epilogue))
				     ((cjump? epilogue)
				      (cjump-false epilogue)))))
			 (if (label-bb? next)
			     (let ((next-block (label-bb-basic-block next)))
				(with-access::basic-block next-block (mark)
				   (set! mark 'callee)))))))
		text-segment)
      ;; We scan all the basic blocks searching for unmarked ones
      (let loop ((bb  text-segment)
		 (new '()))
	 (cond
	    ((null? bb)
	     (duplicate::ir ir
		(text-segment (reverse! new))))
	    ((basic-block-mark (car bb))
	     ;; We skip that one, it is a callee one.
	     (loop (cdr bb) new))
	    (else
	     ;; This one is a caller...
	     (let laap ((b    (car bb))
			(nnew (cons (car bb) new)))
		(let* ((epilogue (basic-block-epilogue b))
		       (next     (cond
				    ((jump? epilogue)
				     (jump-addr epilogue))
				    ((cjump? epilogue)
				     (cjump-false epilogue)))))
		   (if (not (label-bb? next))
		       (loop (cdr bb) nnew)
		       (let ((next-block (label-bb-basic-block next)))
			  (if (eq? (basic-block-mark next-block) 'callee)
			      ;; the next is free to be added
			      (begin
				 (basic-block-mark-set! next-block 'called)
				 (laap next-block (cons next-block nnew)))
			      (loop (cdr bb) nnew)))))))))))
					  
		    
	     
