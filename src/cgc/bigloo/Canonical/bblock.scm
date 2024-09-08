;*=====================================================================*/
;*    serrano/trashcan/cgc/Canonical/bblock.scm                        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Feb 12 16:49:49 1998                          */
;*    Last change :  Wed Dec 27 15:25:28 2000 (serrano)                */
;*    -------------------------------------------------------------    */
;*    This module implements a basic blocks construction.              */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module canonical_basic-block
   (import tools_speek
	   ast_node
	   ir_node
	   ir_label)
   (export (basic-blocks::ir ir::ir)))

;*---------------------------------------------------------------------*/
;*    basic-blocks ...                                                 */
;*---------------------------------------------------------------------*/
(define (basic-blocks::ir ir::ir)
   (verbose 1 #"  - basic-blocks\n")
   (with-access::ir ir (text-segment)
      (set! text-segment (stmt-list->basic-block-list text-segment
						      (get-label "main"
								 *main-decl*)))
      ir))

;*---------------------------------------------------------------------*/
;*    stmt-list->basic-block-list ...                                  */
;*    -------------------------------------------------------------    */
;*    This function convert a list of statements into a list of        */
;*    basic blocks.                                                    */
;*---------------------------------------------------------------------*/
(define (stmt-list->basic-block-list stmts label)
   (define (frame-decl->basic-block-list framedecl::framedecl)
      (with-access::framedecl framedecl (prolog ir-stmt id epilogue)
	 ;; we close up the current basic block before starting a new one
	 (stmt-list->basic-block-list (cons
				       (instantiate::pseudo-fundef
					  (framedecl framedecl)
					  (name (ident-name id)))
				       (append
					(seq-stmts ir-stmt)
					(list epilogue
					      (instantiate::pseudo-return
						 (framedecl framedecl)))))
				      prolog)))
   (let loop ((stmt-list stmts)
	      (label     label)
	      (stmts     '())
	      (res       '())
	      (framedecl #unspecified))
      (cond
	 ((null? stmt-list)
	  ;; this is the end of the program
	  (if (null? stmts)
	      (reverse! res)
	      (let ((bblock (instantiate::basic-block
			       (prolog   label)
			       (body     (reverse! stmts))
			       (epilogue (instantiate::jump
					    (addr (get-label "_exit"
							     framedecl)))))))
		 (widen!::label-bb label
		    (basic-block bblock))
		 (cons bblock (reverse! res)))))
	 ((seq? (car stmt-list))
	  (loop (append (seq-stmts (car stmt-list)) (cdr stmt-list))
		label
		stmts
		res
		framedecl))
	 ((framedecl? (car stmt-list))
	  (loop (cdr stmt-list)
		label
		stmts
		(append (frame-decl->basic-block-list (car stmt-list))
			res)
		framedecl))
	 ((basic-block-ender? (car stmt-list))
	  (let ((bblock    (instantiate::basic-block
			      (prolog   label)
			      (body     (reverse! stmts))
			      (epilogue (car stmt-list))))
		(new-label (get-new-label framedecl)))
	     (if (and (not (framedecl? framedecl))
		      (not (eq? framedecl #unspecified)))
		 (error "Illegal frame declaration"
			new-label
			framedecl))
	     (widen!::label-bb label
		(basic-block bblock))
	     (loop (cdr stmt-list)
		   new-label
		   '()
		   (cons bblock res)
		   framedecl)))
	 ((basic-block-starter? (car stmt-list))
;* 	  (if (null? stmts)                                            */
;* 	      (loop (cdr stmt-list)                                    */
;* 		    (car stmt-list)                                    */
;* 		    '()                                                */
;* 		    res                                                */
;* 		    framedecl)                                         */
	      (let ((bblock (instantiate::basic-block
			       (prolog   label)
			       (body     (reverse! stmts))
			       (epilogue (instantiate::jump
					    (addr (car stmt-list)))))))
		 (widen!::label-bb label
		    (basic-block bblock))
		 (if (not (fundecl? (label-framedecl (car stmt-list))))
		     (error "Illegal label framedecl"
			    (car stmt-list)
			    (label-framedecl (car stmt-list)))
		     (loop (cdr stmt-list)
			   (car stmt-list)
			   '()
			   (cons bblock res)
			   (label-framedecl (car stmt-list))))))
	 ((pseudo-return? (car stmt-list))
	  (let ((bblock (instantiate::basic-block
			   (prolog   label)
			   (body     (if (null? stmts)
					 (list (instantiate::ir-const
						  (value 0)))
					 (reverse! stmts)))
			   (epilogue (car stmt-list)))))
	     (widen!::label-bb label
		(basic-block bblock))
	     (cons bblock (reverse! res))))
	 (else
	  (loop (cdr stmt-list)
		label
		(cons (car stmt-list) stmts)
		res
		framedecl)))))

;*---------------------------------------------------------------------*/
;*    basic-block-ender? ::ir-stmt ...                                 */
;*    -------------------------------------------------------------    */
;*    Does a stmt close a basic block (i.e. is it a jump or a cjump).  */
;*---------------------------------------------------------------------*/
(define (basic-block-ender? stmt::ir-stmt)
   (cond
      ((jump? stmt)
       #t)
      ((cjump? stmt)
       #t)
      (else
       #f)))
   
;*---------------------------------------------------------------------*/
;*    basic-block-starter? ::ir-stmt ...                               */
;*    -------------------------------------------------------------    */
;*    Does a stmt start a basic block (i.e. is it a label).            */
;*---------------------------------------------------------------------*/
(define (basic-block-starter? stmt::ir-stmt)
   (cond
      ((label? stmt)
       #t)
      (else
       #f)))
   
