;*=====================================================================*/
;*    .../bglstone/src/cgc/bigloo/Iselect/mips-r3000-munch.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Feb 13 16:32:39 1998                          */
;*    Last change :  Sun Apr  3 06:51:10 2011 (serrano)                */
;*    -------------------------------------------------------------    */
;*    Maximal munch implementation for the Mips architecture.          */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module iselect_mips-r3000-munch
   (import tools_speek
	   engine_param
	   arch_arch
	   ast_node
	   ir_node
	   ir_label
	   iselect_asm
	   iselect_iselect)
   (export (mips-r3000-munch ::ir)))

;*---------------------------------------------------------------------*/
;*    mips-r3000-munch ...                                             */
;*---------------------------------------------------------------------*/
(define (mips-r3000-munch ir::ir)
   (verbose 1 #"  - mips-r3000 maximal munch\n")
   (set! *instr-list* '())
   ;; the global variable heap segment
   (with-access::ir ir (bss-segment)
      (add-instr! (arch-bss-segment bss-segment)))
   ;; the data compilation
   (with-access::ir ir (data-segment)
      (if (pair? data-segment)
	  (add-instr! (arch-compile-data data-segment))))
   ;; the beginning of the text section
   (add-instr! (arch-text-section))
   (with-access::ir ir (text-segment)
      (for-each (lambda (basic-block)
		   (with-access::basic-block basic-block (prolog body epilogue)
		      (munch prolog)
		      (for-each munch body)
		      (munch epilogue)))
		text-segment))
   (let ((l (reverse! *instr-list*)))
      (if (>fx *optim* 0)
	  ;; We prepare here the branch tensioning optimization. For each
	  ;; label node, we set the field cdr that points to the label
	  ;; following instruction.
	  (begin
	     (set! *instr-list* '())
	     (set! *nop* (instantiate::asm-oper
			    (shape "addu %d0, %s0, %s1")
			    (use   (list (arch-zero-expr) (arch-zero-expr)))
			    (def   (list (arch-zero-expr)))))
 	     (set! *nop?* (lambda (obj)
			     (and (asm-oper? obj)
				  (with-access::asm-oper obj (shape use def)
				     (and (string=? shape
						    (asm-oper-shape *nop*))
					  (equal? use
						  (asm-oper-use *nop*))
					  (equal? def
						  (asm-oper-def *nop*)))))))))
      (set! *instr-list* '())
      l))

;*---------------------------------------------------------------------*/
;*    *instr-list* ...                                                 */
;*---------------------------------------------------------------------*/
(define *instr-list* '())

;*---------------------------------------------------------------------*/
;*    add-instr! ...                                                   */
;*---------------------------------------------------------------------*/
(define (add-instr! instr)
   (set! *instr-list* (cons instr *instr-list*)))

;*---------------------------------------------------------------------*/
;*    plus? ...                                                        */
;*    -------------------------------------------------------------    */
;*    A simple tools function that checks if a ir-expr node is         */
;*    a plus node (opfx '+ ...).                                       */
;*---------------------------------------------------------------------*/
(define (plus? addr::ir-expr)
   (and (opfx? addr) (eq? (opfx-op addr) '+)))

;*---------------------------------------------------------------------*/
;*    munch :: ...                                                     */
;*    -------------------------------------------------------------    */
;*    This generic function implements the maximal munch for the       */
;*    Mips processor. For each method, we show the term rewriting      */
;*    that can be proceed. In fact this function implements a          */
;*    hand-coded pattern-matching :-C                                  */
;*---------------------------------------------------------------------*/
(define-generic (munch::temp inode))

;*---------------------------------------------------------------------*/
;*    munch ::ir-const ...                                             */
;*    -------------------------------------------------------------    */
;*    addui %zero, %zero, %zero: _                                     */
;*    -------------------------------------------------------------    */
;*    li %d0, imm:               (const imm)                           */
;*---------------------------------------------------------------------*/
(define-method (munch::temp inode::ir-const)
   (with-access::ir-const inode (value)
      (if (=fx value 0)
	  (arch-zero-expr)
	  (let* ((temp  (make-new-temp))
		 (instr (instantiate::asm-oper
			   (shape (string-append "li %d0, "
						 (number->string value)))
			   (def   (list temp)))))
	     (add-instr! instr)
	     temp))))

;*---------------------------------------------------------------------*/
;*    munch::temp ::name ...                                           */
;*    -------------------------------------------------------------    */
;*    la %d0, lbl:                name                                 */
;*---------------------------------------------------------------------*/
(define-method (munch::temp inode::name)
   (with-access::name inode (label)
      (let* ((temp  (make-new-temp))
	     (ident (label-ident label))
	     (shape (string-append "la %d0, "  (ident-name ident)))
	     (instr (instantiate::asm-oper
		       (shape shape)
		       (def   (list temp)))))
	 (add-instr! instr)
	 temp)))

;*---------------------------------------------------------------------*/
;*    munch::temp ::temp ...                                           */
;*---------------------------------------------------------------------*/
(define-method (munch::temp inode::temp)
   inode)

;*---------------------------------------------------------------------*/
;*    munch ::opfx ...                                                 */
;*    -------------------------------------------------------------    */
;*    addu %d0, %s0, %s1:          opfx                                */
;*    	                          / |  \                               */
;*                            temp  +   temp                           */
;*    -------------------------------------------------------------    */
;*    addiu %d0, %s0, imm:         opfx                                */
;*    	                          / |  \                               */
;*                            temp  +   imm                            */
;*---------------------------------------------------------------------*/
(define-method (munch::temp inode::opfx)
   (with-access::opfx inode (op left right)
      (define (op->codeop op)
	 (case op
	    ((+) "addu")
	    ((-) "subu")
	    ((*) "mulou")
	    ((/) "divu")
	    ((==) "seq")
	    ((!=) "sne")
	    ((>) "sgt")
	    ((>=) "sge")
	    ((<) "slt")
	    ((<=) "sle")
	    (else
	     (error "maximal-munch" "Unknown operator" op))))
      (define (default)
	 (let ((left  (munch left))
	       (right (munch right))
	       (temp  (make-new-temp)))
	    ;; addu %d0, %s0, %s1
	    (let ((inst (instantiate::asm-oper
			   (shape (string-append (op->codeop op)
						 " %d0, %s0, %s1"))
			   (use   (list left right))
			   (def   (list temp)))))
	       (add-instr! inst)
	       temp)))
      (define (max-immediat-value)
	 (bit-lsh 1 16))
      (if (temp? left)
	  (cond
	     ((temp? right)
	      ;; addu %d0, %s0, %s1
	      (let* ((temp (make-new-temp))
		     (inst (instantiate::asm-oper
			      (shape (string-append (op->codeop op)
						    " %d0, %s0, %s1"))
			      (use   (list left right))
			      (def   (list temp)))))
		 (add-instr! inst)
		 temp))
	     ((ir-const? right)
	      (if (<fx (ir-const-value right) (max-immediat-value))
		  ;; addu %d0, %s0, imm
		  (let* ((shape (string-append (op->codeop op)
					       " %d0, %s0, "
					       (integer->string
						(ir-const-value right))))
			 (temp (make-new-temp))
			 (inst (instantiate::asm-oper
				  (shape shape)
				  (use   (list left))
				  (def   (list temp)))))
		     (add-instr! inst)
		     temp)
		  (default)))
	     (else
	      (default)))
	  (default))))
		      
;*---------------------------------------------------------------------*/
;*    munch ::mem ...                                                  */
;*    -------------------------------------------------------------    */
;*    move %d0, 0(%s0):           mem                                  */
;*                                 |                                   */
;*                                temp                                 */
;*    -------------------------------------------------------------    */
;*    lw %d0, n(%s0):             mem                                  */
;*                                 |                                   */
;*                               opfx                                  */
;*                               / | \                                 */
;*                          const  +  temp                             */
;*    -------------------------------------------------------------    */
;*    lw %d0, n(%s0):             mem                                  */
;*                                 |                                   */
;*                               opfx                                  */
;*                               / | \                                 */
;*                           temp  +  const                            */
;*---------------------------------------------------------------------*/
(define-method (munch::temp inode::mem)
   (with-access::mem inode (addr)
      (define (load dest::temp src::temp offset::int)
	 ;; Here, we should test that the immediat value is small enought.
	 ;; For now, I don't how many can be devoted to this constant (16 ?)
	 ;; thus, I don't implement the test.
	  (let ((instr (instantiate::asm-oper
			  (shape (string-append "lw %d0, "
						(number->string offset)
						"(%s0)"))
			  (use   (list src))
			  (def   (list dest)))))
	     (add-instr! instr)
	     dest))
      (cond
	 ((temp? addr)
	  (load (make-new-temp) addr 0))
	 ((and (plus? addr)
	       (temp? (opfx-left addr))
	       (ir-const? (opfx-right addr)))
	  ;; load %d0, n(%s0)
	  (load (make-new-temp)
		(opfx-left addr)
		(ir-const-value (opfx-right addr))))
	 ((and (plus? addr)
	       (temp? (opfx-right addr))
	       (ir-const? (opfx-left addr)))
	  ;; load %d0, n(%s0)
	  (load (make-new-temp)
		(opfx-right addr)
		(ir-const-value (opfx-left addr))))
	 (else
	  (load (make-new-temp) (munch addr) 0)))))

;*---------------------------------------------------------------------*/
;*    munch::temp ::call ...                                           */
;*---------------------------------------------------------------------*/
(define-method (munch::temp inode::call)
   (with-access::call inode (framedecl args)
      (let loop ((args  args)
		 (aregs (arch-arg-registers))
		 (depth 0)
		 (use   '()))
	 (cond
	    ((null? args)
	     ;; arguments are prepared, we can emit the call.
	     (with-access::framedecl framedecl (prolog)
		(with-access::label prolog (ident)
		   (let* ((shape (string-append "jal " (ident-name ident)))
			  (inst (instantiate::asm-oper
				   (shape shape)
				   (use   use)
				   (trash (arch-caller-save-registers))
				   (def   (arch-calldef-registers)))))
		      (add-instr! inst)
		      (arch-retv-expr)))))
	    ((null? aregs)
	     (let ((addr (instantiate::opfx
			    (op '+)
			    (left  (arch-sp-expr))
			    (right (instantiate::ir-const
				      (value (*fx depth (arch-word-size))))))))
		(munch (instantiate::move-mem
			  (addr addr)
			  (k (arch-word-size))
			  (expr (car args))))
		(loop (cdr args) '() (+fx depth 1) use)))
	    (else
	     (munch (instantiate::move-temp
		       (temp (car aregs))
		       (expr (car args))))
	     (loop (cdr args)
		   (cdr aregs)
		   0
		   (cons (car aregs) use)))))))

;*---------------------------------------------------------------------*/
;*    munch ::move-temp ...                                            */
;*    -------------------------------------------------------------    */
;*    move %d0, %s0:             move-temp                             */
;*    	                          /    \                               */
;*                             temp    temp                            */
;*    -------------------------------------------------------------    */
;*    lw %d0, 0(%s0):           move-temp                              */
;*                               /    \                                */
;*                             temp  mem                               */
;*                                    |                                */
;*                                   temp                              */
;*    -------------------------------------------------------------    */
;*    lw %d0, n(%s0):           move-temp                              */
;*                               /    \                                */
;*                             temp   mem                              */
;*                                     |                               */
;*                                    opfx                             */
;*                                   / | \                             */
;*                               temp  +  const                        */
;*    -------------------------------------------------------------    */
;*    lw %d0, n(%s0):           move-temp                              */
;*                               /    \                                */
;*                             temp   mem                              */
;*                                     |                               */
;*                                    opfx                             */
;*                                   / | \                             */
;*                              const  +  temp                         */
;*    -------------------------------------------------------------    */
;*    addiu %d0, %s0, n:        move-temp                              */
;*                               /    \                                */
;*                             temp   opfx                             */
;*                                   / | \                             */
;*                               temp  +  const                        */
;*    -------------------------------------------------------------    */
;*    addiu %d0, %s0, n:        move-temp                              */
;*                               /    \                                */
;*                             temp   opfx                             */
;*                                   / | \                             */
;*                              const  +  temp                         */
;*    -------------------------------------------------------------    */
;*    addu %d0, %s0, %s1:       move-temp                              */
;*                               /    \                                */
;*                             temp   opfx                             */
;*                                   / | \                             */
;*                               temp  +  temp                         */
;*---------------------------------------------------------------------*/
(define-method (munch::temp inode::move-temp)
   (with-access::move-temp inode (temp expr)
      (define (move dest::temp src::temp)
	  (let ((instr (instantiate::asm-move
			  (shape "move %d0, %s0")
			  (use   (list src))
			  (def   (list dest)))))
	     (add-instr! instr)
	     dest))
      (define (load dest::temp src::temp offset::int)
	 ;; Here, we should test that the immediat value is small enought.
	 ;; For now, I don't how many can be devoted to this constant (16 ?)
	 ;; thus, I don't implement the test.
	  (let ((instr (instantiate::asm-oper
			  (shape (string-append "lw %d0, "
						(number->string offset)
						"(%s0)"))
			  (use   (list src))
			  (def   (list dest)))))
	     (add-instr! instr)
	     dest))
      (define (op->codeop op)
	 (case op
	    ((+) "addu")
	    ((-) "subu")
	    ((*) "mulou")
	    ((/) "divu")
	    (else
	     (error "maximal-munch" "Unknown operator" op))))
      (define (binop? expr::ir-expr)
	 (and (opfx? expr) (memq (opfx-op expr) '(+ - / *)))) 
      (define (max-immediat-value)
	 (bit-lsh 1 16))
      (define (default)
	 (move temp (munch expr)))
      (cond
	 ((temp? expr)
	  ;; move %d0, %s0
	  (move temp expr))
	 ((mem? expr)
	  (with-access::mem expr (addr)
	     (cond
		((temp? addr)
		 ;; lw %d0, 0(%s0)
		 (load temp addr 0))
		((and (plus? addr)
		      (temp? (opfx-left addr))
		      (ir-const? (opfx-right addr))
		      (<fx (ir-const-value (opfx-right addr))
			   (max-immediat-value)))
		 ;; lw %d0, n(%s0)
		 (load temp
		       (opfx-left addr)
		       (ir-const-value (opfx-right addr))))
		((and (plus? addr)
		      (temp? (opfx-right addr))
		      (ir-const? (opfx-left addr))
		      (<fx (ir-const-value (opfx-left addr))
			   (max-immediat-value)))
		 ;; lw %d0, n(%s0)
		 (load temp
		       (opfx-right addr)
		       (ir-const-value (opfx-left addr))))
		(else
		 ;; move %d0, %s0
		 (default)))))
	 ((ir-const? expr)
	  (cond
	     ((=fx (ir-const-value expr) 0)
	      ;; addu %d0, %zero, %zero
	      (let ((instr (instantiate::asm-oper
			      (shape "addu %d0, %s0, %s1")
			      (use   (list (arch-zero-expr) (arch-zero-expr)))
			      (def   (list temp)))))
		 (add-instr! instr)
		 temp))
	     ((<fx (ir-const-value expr) (max-immediat-value))
	      ;; addu %d0, %zero, imm
	      (let ((instr (instantiate::asm-oper
			      (shape (string-append "addu %d0, %s0, "
						    (number->string
						     (ir-const-value expr))))
			      (use   (list (arch-zero-expr)))
			      (def   (list temp)))))
		 (add-instr! instr)
		 temp))
	     (else
	      ;; lw %d0, n
	      (let ((instr (instantiate::asm-oper
			      (shape (string-append "lw %d0, "
						    (integer->string
						     (ir-const-value expr))))
			      (def   (list temp)))))
		 (add-instr! instr)
		 temp))))
	 ((binop? expr)
	  (let ((op     (opfx-op expr))
		(eleft  (opfx-left expr))
		(eright (opfx-right expr)))
	     (cond
		((and (temp? eleft)
		      (ir-const? eright)
		      (<fx (ir-const-value eright) (max-immediat-value))
		      (eq? op '+))
		 ;; addiu %d0, %s0, imm
		 (let ((inst (instantiate::asm-oper
				(shape (string-append "addiu %d0, %s0, "
						      (number->string
						       (ir-const-value
							eright))))
				(use   (list eleft))
				(def   (list temp)))))
		    (add-instr! inst)
		    temp))
		((and (temp? eright)
		      (ir-const? eleft)
		      (<fx (ir-const-value eleft) (max-immediat-value))
		      (eq? op '+))
		 ;; addiu %d0, %s0, imm
		 (let ((inst (instantiate::asm-oper
				(shape (string-append "addiu %d0, %s0, "
						      (number->string
						       (ir-const-value
							eleft))))
				(use   (list eright))
				(def   (list temp)))))
		    (add-instr! inst)
		    temp))
		((and (temp? eleft)
		      (ir-const? eright)
		      (<fx (ir-const-value eright) (max-immediat-value))
		      (eq? op '-))
		 ;; addiu %d0, %s0, imm
		 (let ((inst (instantiate::asm-oper
				(shape (string-append "addiu %d0, %s0, -"
						      (number->string
						       (ir-const-value
							eright))))
				(use   (list eleft))
				(def   (list temp)))))
		    (add-instr! inst)
		    temp))
		((and (temp? eright)
		      (ir-const? eleft)
		      (<fx (ir-const-value eleft) (max-immediat-value))
		      (eq? op '-))
		 ;; addiu %d0, %s0, imm
		 (let ((inst (instantiate::asm-oper
				(shape (string-append "addiu %d0, %s0, -"
						      (number->string
						       (ir-const-value
							eleft))))
				(use   (list eright))
				(def   (list temp)))))
		    (add-instr! inst)
		    temp))
		((and (temp? eright) (temp? eleft))
		 ;; addu %d0, %s0, %s1
		 (let ((inst (instantiate::asm-oper
				(shape (string-append (op->codeop op)
						      " %d0, %s0, %s1"))
				(use   (list eleft eright))
				(def   (list temp)))))
		    (add-instr! inst)
		    temp))
		(else
		 (default)))))
	 (else
	  ;; move %d0, %s0
	  (default)))))

;*---------------------------------------------------------------------*/
;*    munch::temp ::move-mem ...                                       */
;*---------------------------------------------------------------------*/
(define-method (munch::temp inode::move-mem)
   (with-access::move-mem inode (addr k expr)
      (let ((addr (if (mem? addr)
		      (mem-addr addr)
		      addr)))
	 (define (store src::temp dest::temp offset::int)
	    (let ((instr (instantiate::asm-oper
			    (shape (if (=fx offset 0)
				       "sw %s0, (%s1)"
				       (string-append "sw %s0, "
						      (integer->string offset)
						      "(%s1)")))
			    (use   (list src dest))
			    (def   '()))))
	       (add-instr! instr)
	       dest))
	 (define (default-move-mem)
	    (if (not (ir-const? addr))
		(let* ((addr  (munch (move-mem-addr inode)))
		       (src   (munch expr))
		       (instr (instantiate::asm-oper
				 (shape "sw %s0, (%s1)")
				 (use   (list src addr))
				 (def   '()))))
		   (add-instr! instr)
		   (arch-zero-expr))
		(let* ((temp (make-new-temp))
		       (shape (string-append "la %d0, "
					     (number->string
					      (ir-const-value addr))))
		       (la (instantiate::asm-oper
			      (shape shape)
			      (def   (list temp)))))
		   (add-instr! la)
		   (let ((src (munch expr)))
		      (store src temp 0)))))
	 (define (max-immediat-value)
	    (bit-lsh 1 16))
	 (cond
	    ((not (=fx k (arch-word-size)))
	     (error "munch" "Can't move that size" k))
	    ((temp? addr)
	     ;; store %s0, (%d0)
	     (let ((texpr (if (temp? expr)
			      expr
			      (munch expr))))
		(store texpr addr 0)))
	    ((and (plus? addr)
		  (temp? (opfx-left addr))
		  (ir-const? (opfx-right addr))
		  (<fx (ir-const-value (opfx-right addr))
		       (max-immediat-value)))
	     (let ((texpr (if (temp? expr)
			      expr
			      (munch expr))))
		;; store %s0, O(%d0)
		(store texpr
		       (opfx-left addr)
		       (ir-const-value (opfx-right addr)))))
	    ((and (plus? addr)
		  (temp? (opfx-right addr))
		  (ir-const? (opfx-left addr))
		  (<fx (ir-const-value (opfx-left addr))
		       (max-immediat-value)))
	     (let ((texpr (if (temp? expr)
			      expr
			      (munch expr))))
		;; store %s0, O(%d0)
		(store texpr
		       (opfx-right addr)
		       (ir-const-value (opfx-left addr)))))
	    (else
	     ;; store %d0, %s0
	     (default-move-mem))))))

;*---------------------------------------------------------------------*/
;*    munch::temp ::estmt ...                                          */
;*---------------------------------------------------------------------*/
(define-method (munch::temp inode::estmt)
   (with-access::estmt inode (>expr)
      (munch >expr)))

;*---------------------------------------------------------------------*/
;*    munch::temp ::jump ...                                           */
;*---------------------------------------------------------------------*/
(define-method (munch::temp inode::jump)
   (with-access::jump inode (addr)
      (with-access::label addr (ident)
	 (let ((instr (instantiate::asm-jump
			 (shape     (string-append "b " (ident-name ident)))
			 (asm-label addr))))
	    (add-instr! instr)
	    (arch-zero-expr)))))
 
;*---------------------------------------------------------------------*/
;*    munch::temp ::cjump ...                                          */
;*    -------------------------------------------------------------    */
;*    The term rewriting for these nodes are mosltly the same as the   */
;*    opfx ones (if less numerous). The only important variation comes */
;*    from the instruction names.                                      */
;*    -------------------------------------------------------------    */
;*    beq %s0, %s1, true:         cjump -----------+                   */
;*    b false                     / |  \          / \                  */
;*                            temp ==   temp  true   false             */
;*    -------------------------------------------------------------    */
;*    beq %s0, imm, true:         cjump -----------+                   */
;*    b false                     / |  \          / \                  */
;*                            temp ==   imm  true    false             */
;*---------------------------------------------------------------------*/
(define-method (munch::temp inode::cjump)
   (with-access::cjump inode (op left right true false)
      (define (op->codeop op)
	 (case op
	    ((==) "beq")
	    ((!=) "bne")
	    ((>)  "bgt")
	    ((>=) "bge")
	    ((<)  "blt")
	    ((<=) "ble")
	    (else
	     (error "maximal-munch" "Unknown operator" op))))
      (define (default)
	 (let ((left  (munch left))
	       (right (munch right)))
	    ;; brz %s0, %s1, label
	    (let ((inst (instantiate::asm-cjump
			   (shape (string-append (op->codeop op)
						 " %s0, %s1, "
						 (ident-name
						  (label-ident true))))
			   (asm-label true)
			   (use (list left right)))))
	       (add-instr! inst)
	       (munch (instantiate::jump
			 (addr false)))
	       (arch-zero-expr))))
      (define (max-immediat-value)
	 (bit-lsh 1 16))
      (if (temp? left)
	  (cond
	     ((temp? right)
	      ;; brz %s0, %s1, label
	      (default))
	     ((ir-const? right)
	      (if (<fx (ir-const-value right) (max-immediat-value))
		  ;; brz %s0, %s1, label
		  (let* ((shape (string-append (op->codeop op)
					       " %s0, "
					       (integer->string
						(ir-const-value right))
					       ", "
					       (ident-name
						(label-ident true))))
			 (inst (instantiate::asm-cjump
				  (shape     shape)
				  (asm-label true)
				  (use       (list left)))))
		     (add-instr! inst)
		     (munch (instantiate::jump
			       (addr false)))
		     (arch-zero-expr))
		  (default)))
	     (else
	      (default)))
	  (default))))

;*---------------------------------------------------------------------*/
;*    munch::temp ::label ...                                          */
;*---------------------------------------------------------------------*/
(define-method (munch::temp inode::label)
   (add-instr! (widen!::asm-label (if (wide-object? inode)
				      (shrink! inode)
				      inode)))
   (arch-zero-expr))
	 
;*---------------------------------------------------------------------*/
;*    munch::temp ::pseudo-fundef ...                                  */
;*---------------------------------------------------------------------*/
(define-method (munch::temp inode::pseudo-fundef)
   (with-access::pseudo-fundef inode (name framedecl)
      (let ((inst (instantiate::asm-instr
		     (shape (string-append ".globl " name))))
	    (entry (instantiate::proc-entry
		      (shape   (string-append ".ent "
					      (ident-name
					       (framedecl-id framedecl))))
		      (framedecl framedecl))))
	 (add-instr! inst)
	 (add-instr! entry)
	 (arch-zero-expr))))
      
;*---------------------------------------------------------------------*/
;*    munch::temp ::pseudo-return ...                                  */
;*---------------------------------------------------------------------*/
(define-method (munch::temp inode::pseudo-return)
   (with-access::pseudo-return inode (framedecl)
      (let ((inst      (instantiate::asm-oper
			  (shape "j %s0")
			  (use   (list (arch-reta-expr)))))
	    (delayslot (instantiate::asm-oper
			  (shape "addu %d0, %s0, 0")
			  (def   (list (arch-zero-expr)))
			  (use   (list (arch-zero-expr)))))
	    (exit      (instantiate::proc-exit
			  (shape   (string-append ".end "
						  (ident-name
						   (framedecl-id framedecl))))
			  (use   (list (arch-sp-expr)))
			  (def   (list (arch-fp-expr) (arch-reta-expr)))
			  (framedecl framedecl))))
	 ;; the delayslot looks like a nop but it is not and thus, it cannot
	 ;; be removed by optimization pass.
	 (add-instr! exit)
	 (add-instr! inst)
	 (add-instr! delayslot)
	 (arch-zero-expr))))



