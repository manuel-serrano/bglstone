;*=====================================================================*/
;*    .../prgm/project/bglstone/src/cgc/bigloo/Ir/translate.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Feb  4 09:58:44 1998                          */
;*    Last change :  Fri Dec 13 14:31:18 2002 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The translation `ast' -> `ir'.                                   */
;*=====================================================================*/
 
;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module ir_translate
   (import engine_param
	   tools_speek
	   type_type
	   type_bind
	   ast_location
	   ast_node
	   arch_arch
	   ir_node
	   ir_label
	   ir_frame
	   runtime_runtime)
   (export (ast->ir::ir ::ast)
	   (generic anode->inode ::obj ::obj ::int)))

;*---------------------------------------------------------------------*/
;*    ast->ir ...                                                      */
;*---------------------------------------------------------------------*/
(define (ast->ir::ir ast::ast)
   (verbose 1 #"  - ast->ir\n")
   (with-access::ast ast (decl-list)
      (let loop ((decl-list  decl-list)
		 (global-num 0))
	 (cond
	    ((null? decl-list)
	     (let ((text *text-segment*))
		;; before ending this pass we have to prepare all
		;; runtime functions
		(for-each (lambda (decl)
			     (framedecl<-fundecl decl #unspecified))
			  (runtime-fundecls))
		;; and now we are done
		(instantiate::ir
		   (bss-segment  global-num)
		   (text-segment text)
		   (data-segment *string-constants*))))
	    ((fundecl? (car decl-list))
	     (let ((text (framedecl<-fundecl (car decl-list) #unspecified)))
		(add-to-text-segment! text)
		(loop (cdr decl-list) global-num)))
	    ((not (vardecl? (car decl-list)))
	     (loop (cdr decl-list) global-num))
	    (else
	     (loop (cdr decl-list) (+fx global-num 1)))))))

;*---------------------------------------------------------------------*/
;*    *text-segment* ...                                               */
;*---------------------------------------------------------------------*/
(define *text-segment* '())

;*---------------------------------------------------------------------*/
;*    add-to-text-segment! ...                                         */
;*---------------------------------------------------------------------*/
(define (add-to-text-segment! framedecl::framedecl)
   (set! *text-segment* (cons framedecl *text-segment*)))

;*---------------------------------------------------------------------*/
;*    framedecl<-fundecl ...                                           */
;*    -------------------------------------------------------------    */
;*    This function is just a wrapper. It will all                     */
;*    `make-new-framedecl' after adding the extra parameter            */
;*    implementing the static link.                                    */
;*---------------------------------------------------------------------*/
(define (framedecl<-fundecl::framedecl fundecl::fundecl link)
   (with-access::fundecl fundecl (params)
      (if *inner-functions*
	  ;; when inner functions are enabled we have to add the
	  ;; extra parameter that is the static link.
	  (let ((static-link (instantiate::local
				(escape? #t))))
	     (set! params (cons static-link params))))
      (make-new-framedecl fundecl link)))

;*---------------------------------------------------------------------*/
;*    anode->inode :: ...                                              */
;*    -------------------------------------------------------------    */
;*    The formal argument framedecl can be #unspecified when compiling */
;*    the top-level statement expression. That's why we do not declare */
;*    it (in the prototype) as a ::framedecl.                          */
;*---------------------------------------------------------------------*/
(define-generic (anode->inode obj framedecl::obj local-num::int))

;*---------------------------------------------------------------------*/
;*    anode->inode ::stmt ...                                          */
;*    -------------------------------------------------------------    */
;*    This method is given only for runtime library functions that     */
;*    have a dummy body. This function body is just an instance        */
;*    of the stmt class (that can be still considered as an abstract   */
;*    class).                                                          */
;*---------------------------------------------------------------------*/
(define-method (anode->inode body::stmt frame local-num)
   body)
		       
;*---------------------------------------------------------------------*/
;*    anode->inode ::block ...                                         */
;*---------------------------------------------------------------------*/
(define-method (anode->inode body::block frame local-num)
   (with-access::block body (decl-list stmt-list)
      ;; We dispatch between local variables that go into the stack
      ;; and the ones that go into temporaries
      (let loop ((decl-list decl-list)
		 (local-num local-num)
		 (stmts     stmt-list))
	 (cond
	    ((null? decl-list)
	     (instantiate::seq
		(stmts (map (lambda (stmt)
			       (anode->inode stmt frame local-num))
			    stmts))))
	    ((fundecl? (car decl-list))
	     (add-to-text-segment! (framedecl<-fundecl (car decl-list) frame))
	     (loop (cdr decl-list) local-num stmts))
	    ((local-escape? (car decl-list))
	     ;; this local goes in memory at position local-num
	     (let* ((ws            (arch-word-size))
		    (local-addr    (instantiate::ir-const
				      (value (negfx (*fx (+fx 1 local-num)
							 ws)))))
		    (new-local-num (+fx local-num 1)))
		;; we test if we are needing more room in the frame
		(with-access::framedecl frame (local-num)
		   (if (>fx new-local-num local-num)
		       (set! local-num new-local-num)))
		(with-access::local (car decl-list) (fetch)
		   (set! fetch local-addr))
		(loop (cdr decl-list) new-local-num stmts)))
	    (else
	     ;; this local goes in a temporary
	     (let* ((temp        (make-new-temp))
		    (local-fetch temp))
		(with-access::local (car decl-list) (fetch)
		   (set! fetch local-fetch))
		(loop (cdr decl-list) local-num stmts)))))))
	 
;*---------------------------------------------------------------------*/
;*    anode->inode ::if-then ...                                       */
;*    -------------------------------------------------------------    */
;*    This compilation is just a simplified version of the             */
;*    if-then-else compilation.                                        */
;*---------------------------------------------------------------------*/
(define-method (anode->inode body::if-then frame local-num)
   (with-access::if-then body (test then)
      (let* ((true  (get-new-label frame "true"))
	     (false (get-new-label frame "false"))
	     (itest (anode->inode test frame local-num))
	     (cjump (if (and (opfx? itest)
			     (memq (opfx-op itest) '(< > <= >= == !=)))
			(with-access::opfx itest (op left right)
			   (instantiate::cjump
			      (op op)
			      (left left)
			      (right right)
			      (true true)
			      (false false)))
			(instantiate::cjump
			   (op '!=)
			   (left itest)
			   (right (instantiate::ir-const
				     (value 0)))
			   (true true)
			   (false false))))
	     (ithen (anode->inode then frame local-num)))
	 (instantiate::seq
	    (stmts (list cjump true ithen false))))))

;*---------------------------------------------------------------------*/
;*    anode->inode ::if-then-else ...                                  */
;*    -------------------------------------------------------------    */
;*    This statement is compiled into a cjump inode. We have to take   */
;*    care here if the test is already a opfx or not. If it is not,    */
;*    we create the test expr != 0.                                    */
;*---------------------------------------------------------------------*/
(define-method (anode->inode body::if-then-else frame local-num)
   (with-access::if-then-else body (test then otherwise)
      (let* ((true  (get-new-label frame "true"))
	     (false (get-new-label frame "false"))
	     (endif (get-new-label frame "endif"))
	     (itest (anode->inode test frame local-num))
	     (cjump (if (and (opfx? itest)
			     (memq (opfx-op itest) '(< > <= >= == !=)))
			(with-access::opfx itest (op left right)
			   (instantiate::cjump
			      (op op)
			      (left left)
			      (right right)
			      (true true)
			      (false false)))
			(instantiate::cjump
			   (op '!=)
			   (left itest)
			   (right (instantiate::ir-const
				     (value 0)))
			   (true true)
			   (false false))))
	     (ithen (anode->inode then frame local-num))
	     (jump-endif (instantiate::jump
			    (addr endif)))
	     (ielse (anode->inode otherwise frame local-num)))
	 (instantiate::seq
	    (stmts (list cjump true ithen jump-endif false ielse endif))))))
	 
;*---------------------------------------------------------------------*/
;*    anode->inode ::setq ...                                          */
;*    -------------------------------------------------------------    */
;*    Here we use the same split as in varref compilation. We have     */
;*    to split between non escaping variables (holded in register)     */
;*    and escaping ones, holded in memory.                             */
;*---------------------------------------------------------------------*/
(define-method (anode->inode body::setq frame local-num)
   (with-access::setq body (varref value)
      (let ((ir-varref (anode->inode varref frame local-num))
	    (ir-value  (anode->inode value frame local-num)))
	 (cond
	    ((mem? ir-varref)
	     (instantiate::move-mem
		(addr (mem-addr ir-varref))
		(k    (arch-word-size))
		(expr ir-value)))
	    ((temp? ir-varref)
	     (instantiate::move-temp
		(temp ir-varref)
		(expr ir-value)))
	    (else
	     (error "anode->inode"
		    "Illegal varref"
		    (find-runtime-type ir-varref)))))))

;*---------------------------------------------------------------------*/
;*    anode->inode ::aset ...                                          */
;*---------------------------------------------------------------------*/
(define-method (anode->inode body::aset frame local-num)
   (with-access::aset body (aref value)
      (instantiate::move-mem
	 (addr (mem-addr (anode->inode aref frame local-num)))
	 (k    (arch-word-size))
	 (expr (anode->inode value frame local-num)))))

;*---------------------------------------------------------------------*/
;*    anode->inode ::rset ...                                          */
;*    -------------------------------------------------------------    */
;*    Record set is implemented exactly as array set.                  */
;*---------------------------------------------------------------------*/
(define-method (anode->inode body::rset frame local-num)
   (with-access::rset body (rref value)
      (instantiate::move-mem
	 (addr (mem-addr (anode->inode rref frame local-num)))
	 (k    (arch-word-size))
	 (expr (anode->inode value frame local-num)))))

;*---------------------------------------------------------------------*/
;*    anode->inode ::return ...                                        */
;*---------------------------------------------------------------------*/
(define-method (anode->inode body::return frame local-num)
   (with-access::framedecl frame (epilogue)
      (instantiate::jump
	 (addr epilogue))))

;*---------------------------------------------------------------------*/
;*    anode->inode ::return ...                                        */
;*---------------------------------------------------------------------*/
(define-method (anode->inode body::return-value frame local-num)
   (with-access::return-value body (value)
      (let ((ret-value (arch-retv-expr))
	    (iexpr     (anode->inode value frame local-num)))
	 (instantiate::seq
	    (stmts (list (if (temp? ret-value)
			     (instantiate::move-temp
				(temp ret-value)
				(expr iexpr))
			     (instantiate::move-mem
				(addr iexpr)
				(k (arch-word-size))
				(expr iexpr)))
			 (call-next-method)))))))

;*---------------------------------------------------------------------*/
;*    anode->inode ::while ...                                         */
;*    -------------------------------------------------------------    */
;*    while can be expanded into a conditional and a jump. That what   */
;*    we do here.                                                      */
;*---------------------------------------------------------------------*/
(define-method (anode->inode bod::while frame local-num)
   (with-access::while bod (test body)
      (let* ((while (get-new-label frame "while"))
	     (true (get-new-label frame "true"))
	     (endwhile (get-new-label frame "endwhile"))
	     (itest (anode->inode test frame local-num))
	     (cjump (if (and (opfx? itest)
			     (memq (opfx-op itest) '(< > <= >= == !=)))
			(with-access::opfx itest (op left right)
			   (instantiate::cjump
			      (op op)
			      (left left)
			      (right right)
			      (true true)
			      (false endwhile)))
			(instantiate::cjump
			   (op '!=)
			   (left itest)
			   (right (instantiate::ir-const
				     (value 0)))
			   (true true)
			   (false endwhile))))
	     (ibody (anode->inode body frame local-num))
	     (again (instantiate::jump
		       (addr while))))
	 (instantiate::seq
	    (stmts (list while cjump true ibody again endwhile))))))
   
;*---------------------------------------------------------------------*/
;*    anode->inode ::exprstmt ...                                      */
;*    -------------------------------------------------------------    */
;*    This form correspond to a nop.                                   */
;*---------------------------------------------------------------------*/
(define-method (anode->inode body::exprstmt frame local-num)
   (let ((nop (no-op)))
      (instantiate::estmt
	 (>expr nop))))

;*---------------------------------------------------------------------*/
;*    anode->inode ::exprstmt/value ...                                */
;*---------------------------------------------------------------------*/
(define-method (anode->inode body::exprstmt-value frame local-num)
   (with-access::exprstmt-value body (>expr)
      (instantiate::estmt
	 (>expr (anode->inode >expr frame local-num)))))

;*---------------------------------------------------------------------*/
;*    anode->inode ::binop ...                                         */
;*---------------------------------------------------------------------*/
(define-method (anode->inode body::binop frame local-num)
   (with-access::binop body (id left right)
      (let ((ileft (anode->inode left frame local-num))
	    (iright (anode->inode right frame local-num)))
	 (instantiate::opfx
	    (op (string->symbol (ident-name id)))
	    (left ileft)
	    (right iright)))))

;*---------------------------------------------------------------------*/
;*    anode->inode ::funcall ...                                       */
;*    -------------------------------------------------------------    */
;*    We have to add the extra static link parameter.                  */
;*---------------------------------------------------------------------*/
(define-method (anode->inode body::funcall frame local-num)
   (define (mem-at offset base)
      (if (=fx offset 0)
	  (instantiate::mem
	     (addr base))
	  (instantiate::mem
	     (addr (plus base
			 (instantiate::ir-const
			    (value (*fx offset (arch-word-size)))))))))
   (with-access::funcall body (fun actuals)
      ;; We compile the actuals arguments and we add to the list
      ;; the current frame pointer that will play the role of the
      ;; static link in the callee frame (only when inner functions
      ;; are enabled).
      (let* ((iactuals     (map (lambda (actual)
				   (anode->inode actual frame local-num))
				actuals))
	     (callee-depth (fundecl-depth fun))
	     (caller-depth (if (fundecl? frame)
			       (fundecl-depth frame)
			       -1))
	     (static-link  (cond
			      ((or (=fx caller-depth -1)
				   (=fx callee-depth 0))
			       (instantiate::ir-const
				  (value 0)))
			      ((>fx callee-depth caller-depth)
			       (arch-fp-expr))
			      ((=fx callee-depth caller-depth)
			       (mem-at (arch-old-fp) (arch-fp-expr)))
			      (else
			       (let loop ((i callee-depth)
					  (e (mem-at (arch-old-fp)
						     (arch-fp-expr))))
				  (if (=fx i caller-depth)
				      e
				      (loop (+fx i 1)
					    (mem-at (arch-old-fp)
						    e)))))))
	     (iactuals+fp  (if *inner-functions*
			       (cons static-link iactuals)
			       iactuals)))
	 (instantiate::call
	    (framedecl fun)
	    (args      iactuals+fp)))))
	 
;*---------------------------------------------------------------------*/
;*    anode->inode ::aref ...                                          */
;*    -------------------------------------------------------------    */
;*    Array reference is similar to record reference but it require    */
;*    one more runtime computation and because the offset is not       */
;*    statically known.                                                */
;*---------------------------------------------------------------------*/
(define-method (anode->inode body::aref frame local-num)
   (with-access::aref body (array offset)
      (let* ((mul (instantiate::opfx
		     (op    '*)
		     (left  (anode->inode offset frame local-num))
		     (right (instantiate::ir-const
			       (value (arch-word-size))))))
	     (plus (plus (anode->inode array frame local-num) mul)))
	 (instantiate::mem
	    (addr plus)))))

;*---------------------------------------------------------------------*/
;*    anode->inode ::rref ...                                          */
;*    -------------------------------------------------------------    */
;*    A record reference is just an address computation. We add        */
;*    to the base address the offset of the field.                     */
;*---------------------------------------------------------------------*/
(define-method (anode->inode body::rref frame local-num)
   (with-access::rref body (record record-type field-id location)
      ;; we have to find the field offset in the structure
      (let* ((record-fields (structure-fields record-type))
	     (field-offset  (let loop ((fields record-fields)
				       (offset 0))
			       (cond
				  ((null? fields)
				   (source-error location
						 "Illegal structure access"
						 field-id))
				  ((ident=? (cdr (car fields)) field-id)
				   offset)
				  (else
				   (loop (cdr fields) (+fx offset 1))))))
	     (cnst-offset   (instantiate::ir-const
			       (value (*fx (arch-word-size) field-offset))))
	     (plus          (plus (anode->inode record frame local-num)
				  cnst-offset)))
	 (instantiate::mem
	    (addr plus)))))

;*---------------------------------------------------------------------*/
;*    anode->inode ::varref ...                                        */
;*    -------------------------------------------------------------    */
;*    There is three kinds of variables:                               */
;*       1. global variables.                                          */
;*       2. local non escaping variables.                              */
;*       3. local escaping variables.                                  */
;*    We use three different framework for these variables.            */
;*---------------------------------------------------------------------*/
(define-method (anode->inode varref::varref frame local-num)
   (define (global->inode::mem vardecl::global)
      ;; A global variable is associated to a memory location. Reading
      ;; the variable is reading at the address computed by adding the
      ;; offset of the global variable to the bss segment start address.
      (with-access::global vardecl (fetch)
	 (let* ((bss    (arch-bss-expr))
		(offset (instantiate::ir-const
			   (value (*fx fetch (arch-word-size)))))
		(plus   (plus bss offset)))
	    (instantiate::mem
	       (addr plus)))))
   (define (local-register->inode::temp vardecl::local)
      ;; The local variable is in a temporary. Reading the local is thus
      ;; just reading the temporary.
      (with-access::local vardecl (fetch)
	 fetch))
   (define (local-frame->inode::mem slink fp::ir-expr)
      (cond
	 ((vardecl? slink)
	  ;; We have found the frame holding the variable, reading the variable
	  ;; is reading at memory address computed by adding an offset to fp.
	  (with-access::vardecl slink (fetch)
	     (instantiate::mem
		(addr (plus fp fetch)))))
	 ((=fx slink 0)
	  (instantiate::mem
	     (addr (instantiate::ir-const (value slink)))))
	 (else
	  (instantiate::mem
	     (addr (plus fp slink))))))
   (with-access::varref varref (vardecl depth)
      (cond
	 ((global? vardecl)
	  ;; a global variable
	  (global->inode vardecl))
	 ((not (local-escape? vardecl))
	  ;; a local variable that is holded by a temporary
	  (local-register->inode vardecl))
	 (else
	  ;; we have to follow `depth' static links in order to
	  ;; find the stack frame holding the local variable.
	  (let loop ((depth depth)
		     (fp    (arch-fp-expr)))
	     (if (=fx depth 0)
		 ;; we have reach the frame
		 (local-frame->inode vardecl fp)
		 ;; we have to follow a static link (i.e. to fetch the
		 ;; enclosing frame pointer). The static link is a local
		 ;; framed variable (the first function parameters). Thus,
		 ;; we read it using the local-frame->inode function.
		 (let* ((fundecl   frame)
			(params    (fundecl-params fundecl))
			(slink     (if (arch-old-fp)
				       (arch-old-fp)
				       (car params)))
			(old-fp    (local-frame->inode slink fp)))
		    (loop (-fx depth 1) old-fp))))))))
		       
;*---------------------------------------------------------------------*/
;*    anode->inode ::const ...                                         */
;*    -------------------------------------------------------------    */
;*    This function splits between numerical and string constants.     */
;*    We use two different frameworks depending on the constant        */
;*    class.                                                           */
;*---------------------------------------------------------------------*/
(define-method (anode->inode body::const frame local-num)
   (with-access::const body (type value location)
      (cond
	 ((type=? type *type-int*)
	  ;; Numerical constant translation is straightforward. It is
	  ;; a simple ir constant.
	  (instantiate::ir-const
	     (value value)))
	 ((type=? type *type-string*)
	  ;; String constants are a little bit harder. We have to allocate
	  ;; a label that will represent the memory slot where the string
	  ;; constant will be dumped.
	  (let ((label (get-new-label frame "_str")))
	     (declare-string! label value)
	     (instantiate::name
		(label label))))
	 ((type=? type *type-nil*)
	  (instantiate::ir-const
	     (value 0)))
	 (else
	  ;; We have only two constant types.
	  (source-error location "Illegal constant type" type)))))

;*---------------------------------------------------------------------*/
;*    *string-constants* ...                                           */
;*---------------------------------------------------------------------*/
(define *string-constants* '())

;*---------------------------------------------------------------------*/
;*    declare-string! ...                                              */
;*---------------------------------------------------------------------*/
(define (declare-string! label::label value::bstring)
   (set! *string-constants* (cons (cons label value) *string-constants*)))

