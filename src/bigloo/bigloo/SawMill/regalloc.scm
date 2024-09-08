;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/SawMill/regalloc.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Jan 31 09:56:21 2005                          */
;*    Last change :  Fri Feb  4 11:17:56 2005 (serrano)                */
;*    Copyright   :  2005 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Compute the liveness analysis of the rtl instructions            */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module saw_register-allocation
   
   (include "Tools/trace.sch")
   
   (import  engine_param
	    ast_var
	    ast_node
	    type_type
	    tools_shape
	    tools_speek
	    backend_backend
	    saw_lib
	    saw_defs
	    saw_node2rtl)
   
   (static  (class regset
	       (length::int (default 0))
	       (string::bstring (default '#())))
            (wide-class block/ra::block
	       trsif::pair-nil)
	    (wide-class rtl_ins/ra::rtl_ins
	       (def (default #unspecified))
	       (out (default #unspecified))
	       (in (default #unspecified)))
	    (wide-class rtl_reg/ra::rtl_reg
	       (num::int read-only)
	       (color (default #f))
	       (move (default #unspecified))
	       (coalesce (default #f))
	       (interfere (default #unspecified))
	       (interfere2 (default #unspecified))))
   
   (export  (register-allocation::pair-nil ::backend
					   ::global
					   ::pair-nil
					   ::pair-nil)
	    
	    (generic rtl-type-interference ::backend ::pair-nil)))

;*---------------------------------------------------------------------*/
;*    register-allocation ...                                          */
;*---------------------------------------------------------------------*/
(define (register-allocation back global args l)
   (verbose 2 "        reg. alloc. " (shape global) ": ")
   (verbose 3 (if (or (= *saw-register-allocation-max-size* 0)
		      (<fx (rtl-size l) *saw-register-allocation-max-size*))
		  "yes [size="
		  "no [size=")
	    (rtl-size l) "] ")
   (when (or (= *saw-register-allocation-max-size* 0)
	     (<fx (rtl-size l) *saw-register-allocation-max-size*))
      (verbose 3  "\n          liveness..")
      (multiple-value-bind (regs protect-regs)
	 (rtl-liveness args l)
	 (verbose 3 " done\n")
	 (on-trace (cgen 3) (dump-basic-blocks 'liveness global args l))
	 (verbose 3  "          interference... ")
	 (rtl-interference back args l regs protect-regs)
	 (verbose 3 (length regs) " temporaries\n")
	 (rtl-ins-cleanup-dest! l)
	 (when *saw-register-coalesce?*
	    (verbose 3 "          coalescing... ")
	    (let ((nb (rtl-register-coalescing regs args l)))
	       (verbose 3 nb " registers\n")))
	 (verbose 3 "          coloring... ")
	 (let ((nb-reg (rtl-register-coloring args regs)))
	    (verbose 3 nb-reg " registers\n")
	    (rtl-map-registers! regs args l)
	    (verbose 3 "          move... ")
	    (let ((nb (rtl-remove-move! l)))
	       (verbose 3 nb " removed\n            "))
	    (verbose 2 (length regs) " temps -> " nb-reg " regs"))))
   (verbose 2 #\Newline)
   l)

;*---------------------------------------------------------------------*/
;*    rtl-liveness ...                                                 */
;*    -------------------------------------------------------------    */
;*    Computes the liveness of a list of blocks. Returns the           */
;*    list of used registers.                                          */
;*---------------------------------------------------------------------*/
(define (rtl-liveness args blocks)
   ;; allocate a unique number per register
   (let ((n 0)
	 (regs '())
	 (protect-regs '()))
      (define (reg-widen! o)
	 (cond
	    ((rtl_reg/ra? o)
	     #unspecified)
	    ((rtl_reg? o)
	     (widen!::rtl_reg/ra o
		(num n))
	     (set! regs (cons o regs))
	     (set! n (+fx 1 n)))
	    ((rtl_ins? o)
	     (ins-reg-widen! o))))
      (define (ins-reg-widen! i)
	 (with-access::rtl_ins i (dest fun args)
	    (reg-widen! dest)
	    (for-each reg-widen! args)
	    (when (and (rtl_protect? fun) (memq *target-language* '(jvm .net)))
	       (set! protect-regs (cons dest protect-regs)))))
      ;; collect all the registers
      (for-each (lambda (b) (for-each ins-reg-widen! (block-first b))) blocks)
      ;; init the argument
      (for-each reg-widen! args)
      ;; initialize the regset toolbox
      (regset-init! regs)
      ;; init the blocks and the instructions
      (for-each (lambda (b) (liveness-block-widen! b)) blocks)
      ;; add the parameters to the in set of the first instruction of the
      ;; first block
      (when (pair? blocks)
	 (let ((inss (block-first (car blocks))))
	    (when (pair? inss)
	       (let ((ins (car inss)))
		  (with-access::rtl_ins/ra ins (in)
		     (for-each (lambda (a) (regset-add! in a)) args))))))
      ;; compute the register move property
      (for-each (lambda (b)
		   (for-each (lambda (i)
				(with-access::rtl_ins i (fun dest args)
				   (when (and (rtl_mov? fun)
					      (rtl_reg? dest)
					      (and (pair? args)
						   (null? (cdr args))
						   (rtl_reg? (car args))))
				      (with-access::rtl_reg/ra dest (move)
					 ;; a true move
					 (unless (regset? move)
					    (set! move (make-empty-regset)))
					 (regset-add! move (car args))))))
			     (block-first b)))
		blocks)
      ;; fix-point iteration
      (let loop ((i 0))
	 (verbose 3 "." i)
	 (let liip ((bs blocks)
		    (t #f))
	    (if (null? bs)
		(if t
		    (loop (+fx i 1))
		    (values regs protect-regs))
		(liip (cdr bs) (or (liveness-block! (car bs)) t)))))))

;*---------------------------------------------------------------------*/
;*    liveness-block-widen! ...                                        */
;*---------------------------------------------------------------------*/
(define (liveness-block-widen! block)
   (let ((trsif (reverse (block-first block))))
      (for-each (lambda (i) (liveness-ins-widen! i)) trsif)
      (widen!::block/ra block 
	 (trsif trsif))))

;*---------------------------------------------------------------------*/
;*    liveness-ins-widen! ...                                          */
;*---------------------------------------------------------------------*/
(define (liveness-ins-widen! ins)
   (with-access::rtl_ins ins (dest args)
      (widen!::rtl_ins/ra ins
	 (def (if (not dest) (make-empty-regset) (create-regset (list dest))))
	 (in (create-regset (ins-args* ins)))
	 (out (make-empty-regset)))))

;*---------------------------------------------------------------------*/
;*    liveness-block! ...                                              */
;*---------------------------------------------------------------------*/
(define (liveness-block! block)
   (with-access::block/ra block (succs trsif)
      (let loop ((inss trsif)
		 (succ (map (lambda (b) (car (block-first b))) succs))
		 (t #f))
	 (if (pair? inss)
	     (with-access::rtl_ins/ra (car inss) (out in def)
		(let ((u (cond
			    ((pair? succ)
			     (regset-union*! out (map rtl_ins/ra-in succ)))
			    ((rtl_ins? succ)
			     (regset-union! out (rtl_ins/ra-in succ)))
			    (else
			     #f))))
		   (regset-for-each (lambda (r)
				       (if (not (regset-member? def r))
					   (set! u (or (regset-add! in r) u))))
				    out)
		   (loop (cdr inss) (car inss) (or t u))))
	     t))))

;*---------------------------------------------------------------------*/
;*    rtl-interference ...                                             */
;*    -------------------------------------------------------------    */
;*    Computes the interference graph for REGISTERS based on           */
;*    the liveness computed over BLOCKS.                               */
;*---------------------------------------------------------------------*/
(define (rtl-interference back args blocks registers protect-registers)
   (define (ins-interfere i::rtl_ins/ra)
      (with-access::rtl_ins/ra i (in out)
	 (regset-for-each (lambda (r1)
			     (regset-for-each (lambda (r2)
						 (reg-interfere! r1 r2))
					      in))
			  in)
	 (regset-for-each (lambda (r1)
			     (regset-for-each (lambda (r2)
						 (reg-interfere! r1 r2))
					      out))
			  out)))
   (let ((l (length registers)))
      ;; prepare the formal parameters
      (for-each (lambda (r)
		   (unless (regset? (rtl_reg/ra-interfere r))
		      (rtl_reg/ra-interfere-set! r (make-empty-regset))))
		(append args registers))
      ;; fill the interference graph with type information
      (rtl-type-interference back registers)
      ;; fill the interference graph with liveness information
      (for-each (lambda (b)
		   (for-each (lambda (i) (ins-interfere i)) (block-first b)))
		blocks)
      ;; the protect-registers interfere with everybody
      (for-each (lambda (pr)
		   (for-each (lambda (r) (reg-interfere! pr r)) registers))
		protect-registers)
      ;; return the list of registers
      registers))

;*---------------------------------------------------------------------*/
;*    reg-interfere! ...                                               */
;*---------------------------------------------------------------------*/
(define (reg-interfere! r1 r2)
   (unless (or (eq? r1 r2) (regset-member? (rtl_reg/ra-interfere r1) r2))
      (regset-add! (rtl_reg/ra-interfere r1) r2)
      (regset-add! (rtl_reg/ra-interfere r2) r1)))

;*---------------------------------------------------------------------*/
;*    rtl-type-interference ...                                        */
;*    -------------------------------------------------------------    */
;*    This function is generic in order to let backend to override     */
;*    the general definition. For instance, the JVM backend overrides  */
;*    it because the JVM does not use typed registers.                 */
;*---------------------------------------------------------------------*/
(define-generic (rtl-type-interference back::backend registers)
   (when (pair? registers)
      (let loop ((regs registers))
	 (when (pair? (cdr regs))
	    (let* ((r1 (car regs))
		   (t1 (rtl_reg-type r1)))
	       (when (type? t1)
		  (for-each (lambda (r2)
			       (let ((t2 (rtl_reg-type r2)))
				  (unless (eq? t1 t2)
				     (reg-interfere! r1 r2))))
			    (cdr regs)))
	       (loop (cdr regs)))))))

;*---------------------------------------------------------------------*/
;*    rtl-register-coalescing ...                                      */
;*---------------------------------------------------------------------*/
(define (rtl-register-coalescing registers params blocks)
   (define (move? i)
      (with-access::rtl_ins i (fun dest args)
	 (and (rtl_mov? fun)
	      (rtl_reg? dest)
	      (and (pair? args)
		   (null? (cdr args))
		   (rtl_reg? (car args))))))
   (define nb-coalescing 0)
   (define old-regs (make-vector (length registers) #f))
   (define new-regs (make-vector (length registers) #f))
   (define (reg-coalesced? r)
      (and (rtl_reg? r) (vector-ref old-regs (rtl_reg/ra-num r))))
   (define (reg-coalescing? r)
      (and (rtl_reg? r) (vector-ref new-regs (rtl_reg/ra-num r))))
   (define (reg-coalesce! r1 r2)
      (set! nb-coalescing (+fx 1 nb-coalescing))
      (let loop ((r (the-coalesce-reg r2))
		 (nr r2))
	 (if r
	     (loop (the-coalesce-reg r) r)
	     (begin
		(vector-set! new-regs (rtl_reg/ra-num nr) #t)
		(vector-set! old-regs (rtl_reg/ra-num r1) nr)))))
   (define (the-coalesce-reg r)
      (vector-ref old-regs (rtl_reg/ra-num r)))
   (define (ins-mark-coalesce! i)
      (when (move? i)
	 (with-access::rtl_ins i (dest args)
	    (with-access::rtl_reg/ra (car args) (interfere)
	       (unless (regset-member? interfere dest)
		  (cond
		     ((eq? (car args) dest)
		      #unspecified)
		     ((and (not (reg-coalescing? (car args)))
			   (not (memq (car args) params))
			   (not (reg-coalesced? (car args))))
		      (reg-coalesce! (car args) dest))
		     ((and (not (reg-coalescing? dest))
			   (not (memq dest params))
			   (not (reg-coalesced? dest)))
		      (reg-coalesce! dest (car args)))))))))
   (define (ins-coalesce! i)
      (with-access::rtl_ins i (dest args)
	 (when (reg-coalesced? dest)
	    (set! dest (the-coalesce-reg dest)))
	 (set! args (map (lambda (a)
			    (cond
			       ((rtl_ins? a)
				(ins-coalesce! a)
				a)
			       ((rtl_reg? a)
				(or (the-coalesce-reg a) a))
			       (else
				a)))
			 args))))
   (define (reg-interfere-cleanup! r1)
      (with-access::rtl_reg/ra r1 (interfere)
	 (regset-for-each (lambda (r2)
			     (when (reg-coalesced? r2)
				(let* ((nr2 (the-coalesce-reg r2))
				       (ninterfere2 (rtl_reg/ra-interfere nr2))
				       (interfere2 (rtl_reg/ra-interfere r2)))
				   (regset-remove! interfere r2)
				   (regset-add! interfere nr2)
				   (regset-remove! interfere2 r1)
				   (regset-add! ninterfere2 r1))))
			  interfere)))
   ;; find the moves that concern two registers that don't interfere
   (for-each (lambda (b) (for-each ins-mark-coalesce! (block-first b))) blocks)
   ;; change the interfere property
   (for-each reg-interfere-cleanup! registers)
   ;; change the instructions
   (for-each (lambda (b) (for-each ins-coalesce! (block-first b))) blocks)
   ;; the number of coal
   nb-coalescing)
	 
;*---------------------------------------------------------------------*/
;*    rtl-register-coloring ...                                        */
;*---------------------------------------------------------------------*/
(define (rtl-register-coloring args registers)
   (define (interference-size reg)
      (regset-length (rtl_reg/ra-interfere2 reg)))
   (define (select-register! registers size)
      (let loop ((regs registers)
		 (min #f)
		 (mval (+fx (interference-size (car registers)) 1))
		 (prev #f))
	 (cond
	    ((null? regs)
	     (values min (interference-size min) (remq! min registers)))
	    ((<fx (interference-size (car regs)) size)
	     (let* ((reg (car regs))
		    (sz (interference-size reg)))
		(if (pair? prev)
		    (begin
		       (set-cdr! prev (cdr regs))
		       (values reg sz registers))
		    (values reg sz (cdr regs)))))
	    ((<fx (interference-size (car regs)) mval)
	     (loop (cdr regs)
		   (car regs)
		   (interference-size (car regs))
		   regs))
	    (else
	     (loop (cdr regs)
		   min
		   mval
		   regs)))))
   (define (sort-registers registers)
      (sort registers
	    (lambda (r1 r2)
	       (<fx (regset-length (rtl_reg/ra-interfere2 r1))
		    (regset-length (rtl_reg/ra-interfere2 r2))))))
   (define (simplify! r)
      (with-access::rtl_reg/ra r (interfere2)
	 (regset-for-each (lambda (r2)
			     (regset-remove! (rtl_reg/ra-interfere2 r2) r)
			     (regset-remove! interfere2 r2))
			  interfere2)))
   (define colors (make-vector (length registers) #f))
   (define (find-color interfere max)
      (vector-fill! colors #f)
      (regset-for-each (lambda (r)
			  (with-access::rtl_reg/ra r (color)
			     (when (integer? color)
				(vector-set! colors color #t))))
		       interfere)
      (if (=fx (vector-length colors) max)
	  max
	  (let loop ((i 0))
	     (if (vector-ref colors i)
		 (loop (+fx i 1))
		 i))))
   (define (colorize! min stack)
      (let loop ((stack stack)
		 (max min))
	 (if (pair? stack)
	     (with-access::rtl_reg/ra (car stack) (interfere color)
		(set! color (find-color interfere max))
		(loop (cdr stack) (if (=fx color max) (+fx max 1) max)))
	     max)))
   (if (pair? registers)
       (begin
	  ;; duplicate the intefere sets
	  (for-each (lambda (r)
		       (with-access::rtl_reg/ra r (interfere interfere2)
			  (set! interfere2 (duplicate-regset interfere))))
		    registers)
	  (let ((min 0))
	     ;; pre-allocate the arguments
	     '(fprint (current-error-port)
		     "coloring: "
		     (map (lambda (r)
			     (regset-length (rtl_reg/ra-interfere2 r)))
			  registers))
	     (for-each (lambda (r)
			  (simplify! r)
			  (rtl_reg/ra-color-set! r min)
			  (set! min (+fx min 1)))
		       args)
	     ;; allocate the temporaries to registers
	     (let loop ((regs (sort-registers
			       (filter (lambda (r)
					  (not (rtl_reg/ra-color r)))
				       registers)))
			(stack '())
			(size 0))
		(if (null? regs)
		    (colorize! min stack)
		    (multiple-value-bind (reg nsize regs)
		       (select-register! regs size)
		       (simplify! reg)
		       (loop regs (cons reg stack) nsize))))))
       0))

;*---------------------------------------------------------------------*/
;*    rtl-register-post-coalescing ...                                 */
;*    -------------------------------------------------------------    */
;*    Compute a k-coloring with coalescing.                            */
;*    registers r1 and r2 coalesced iff:                               */
;*      1- it exists a node r1 <- r2 or r2 <- r1                       */
;*      2- r1 and r2 do not interfere                                  */
;*      3- coalescing r1 and r2 produces a fake register that has less */
;*         than k neighbours with more than k neighbours               */
;*---------------------------------------------------------------------*/
(define (rtl-register-post-coalescing k args regs)
   (define (interference-size reg)
      (regset-length (rtl_reg/ra-interfere2 reg)))
   (define colors (make-vector (length regs) #f))
   (define (find-color interfere)
      (vector-fill! colors #f)
      (regset-for-each (lambda (r)
			  (with-access::rtl_reg/ra r (color)
			     (when (integer? color)
				(vector-set! colors color #t))))
		       interfere)
      (let loop ((i 0))
	 (if (vector-ref colors i)
	     (loop (+fx i 1))
	     i)))
   (define (colorize! stack regs)
      ;; the stacked un-coalesced registers
      (let loop ((stack stack))
	 (if (pair? stack)
	     (with-access::rtl_reg/ra (car stack) (interfere color)
		(set! color (find-color interfere))
		(loop (cdr stack)))))
      ;; the coalesced registers
      (let ((nb 0))
	 (for-each (lambda (r)
		      (with-access::rtl_reg/ra r (color)
			 (unless color
			    (let ((c (let loop ((r (rtl_reg/ra-coalesce r)))
					(or (rtl_reg/ra-color r)
					    (loop (rtl_reg/ra-coalesce r))))))
			       (set! nb (+fx nb 1))
			       (set! color c)))))
		   regs)
	 nb))
   (define (simplify! r::rtl_reg/ra)
      (with-access::rtl_reg/ra r (interfere2)
	 (regset-for-each (lambda (r2)
			     (regset-remove! (rtl_reg/ra-interfere2 r2) r)
			     (regset-remove! interfere2 r2))
			  interfere2)))
   (define (reg-coalesce? r1 r2)
      (and (not (regset-member? (rtl_reg/ra-interfere2 r1) r2))
	   (let ((int (make-empty-regset)))
	      (regset-union! int (rtl_reg/ra-interfere2 r1))
	      (regset-union! int (rtl_reg/ra-interfere2 r2))
	      (or (<fx (regset-length int) k)
		  (let ((n 0))
		     (regset-for-each (lambda (r)
					 (if (>fx (regset-length
						   (rtl_reg/ra-interfere2 r))
						  k)
					     (set! n (+fx n 1))))
				      int)
		     (<fx n k))))))
   (define (reg-coalesce! r1 r2)
      (with-access::rtl_reg/ra r1 (move interfere interfere2)
	 (when (regset? (rtl_reg/ra-move r2))
	    (regset-union! move (rtl_reg/ra-move r2)))
	 (regset-union! interfere (rtl_reg/ra-interfere r2))
	 (regset-union! interfere2 (rtl_reg/ra-interfere2 r2))
	 (rtl_reg/ra-coalesce-set! r2 r1)
	 r1))
   (define (empty? reg)
      (or (not (regset? reg)) (regset-empty? reg)))
   (define (find-coalesce! regs)
      (let loop ((regs regs))
	 (cond
	    ((null? regs)
	     #f)
	    ((not (empty? (rtl_reg/ra-move (car regs))))
	     (let* ((r1 (car regs))
		    (r2 (regset-first (lambda (r2)
					 (and (reg-coalesce? r1 r2) r2))
				      (rtl_reg/ra-move r1))))
		(if r2
		    (reg-coalesce! r1 r2)
		    (loop (cdr regs)))))
	    (else
	     (loop (cdr regs))))))
   (define (find-register regs)
      (let loop ((regs regs))
	 (cond
	    ((null? regs)
	     #f)
	    ((empty? (rtl_reg/ra-move (car regs)))
	     (loop (cdr regs)))
	    ((<fx (regset-length (rtl_reg/ra-interfere2 (car regs))) k)
	     (car regs))
	    (else
	     (loop (cdr regs))))))
   (define (select-register registers)
      (let loop ((regs (cdr registers))
		 (reg (car registers)))
	 (cond
	    ((null? regs)
	     reg)
	    ((<fx (interference-size (car regs)) (interference-size reg))
	     (loop (cdr regs) (car regs)))
	    (else
	     (loop (cdr regs) reg)))))
   (define (coalesce)
      ;; duplicate the intefere sets
      (for-each (lambda (r)
		   (with-access::rtl_reg/ra r (interfere interfere2 color)
		      (set! color #f)
		      (set! interfere2 (duplicate-regset interfere))))
		regs)
      ;; pre-allocate the arguments
      (let ((min 0))
	 (for-each (lambda (r)
		      (rtl_reg/ra-color-set! r min)
		      (simplify! r)
		      (set! min (+fx min 1)))
		   args))
      ;; allocate the temporaries to registers
      (let loop ((rs (filter (lambda (r)
				(not (rtl_reg/ra-color r)))
			     regs))
		 (stack '()))
	 (if (null? rs)
	     (colorize! stack regs)
	     (let ((reg (or (find-coalesce! rs)
			    (find-register rs)
			    (select-register rs))))
		(simplify! reg)
		(loop (remq! reg rs) (cons reg stack))))))
   ;; cleanup the move property
   (let loop ((regs regs)
	      (b #f))
      (cond
	 ((null? regs)
	  (if b (coalesce) 0))
	 ((empty? (rtl_reg/ra-move (car regs)))
	  (loop (cdr regs) b))
	 (else
	  ;; remove from the move sets the registers that interfere
	  (with-access::rtl_reg/ra (car regs) (interfere move)
	     (regset-for-each (lambda (r)
				 (regset-remove! move r))
			      interfere)
	     (loop (cdr regs) (or b (not (regset-empty? move)))))))))

;*---------------------------------------------------------------------*/
;*    rtl-ins-cleanup-dest! ...                                        */
;*---------------------------------------------------------------------*/
(define (rtl-ins-cleanup-dest! blocks)
   (for-each (lambda (b)
		(for-each (lambda (i)
			     (with-access::rtl_ins/ra i (dest args in out)
				(when (rtl_reg? dest)
				   (unless (regset-member? out dest) 
				      (set! dest #f)))))
			  (block-first b)))
	     blocks))

;*---------------------------------------------------------------------*/
;*    rtl-map-registers! ...                                           */
;*    -------------------------------------------------------------    */
;*    This function maps the temporaries to the register allocated     */
;*    during the coloring. In addition, it removes the destination     */
;*    registers that are not present in the OUT set of the             */
;*    instructions.                                                    */
;*---------------------------------------------------------------------*/
(define (rtl-map-registers! regs args blocks)
   (let ((colors (make-vector (length regs))))
      (for-each (lambda (r)
		   (vector-set! colors (rtl_reg/ra-color r) r))
		regs)
      ;; override the register mapping for the formal parameters
      (for-each (lambda (r)
		   (vector-set! colors (rtl_reg/ra-color r) r))
		args)
      (define (map-register o)
	 (cond
	    ((rtl_reg? o)
	     (with-access::rtl_reg/ra o (color)
		(vector-ref colors color)))
	    ((rtl_ins? o)
	     (with-access::rtl_ins o (args)
		(set! args (map map-register args)))
	     o)
	    (else
	     o)))
      (for-each (lambda (b)
		   (for-each (lambda (i)
				(with-access::rtl_ins/ra i (dest args in out)
				   (when (rtl_reg? dest)
				      (set! dest (map-register dest)))
				   (set! args (map map-register args))))
			     (block-first b)))
		blocks)
      blocks))

;*---------------------------------------------------------------------*/
;*    rtl-remove-move! ...                                             */
;*---------------------------------------------------------------------*/
(define (rtl-remove-move! blocks)
   (define n 0)
   (define (move-nop? i)
      (with-access::rtl_ins/ra i (dest args fun)
	 (and (rtl_mov? fun)
	      (pair? args)
	      (null? (cdr args))
	      (eq? dest (car args)))))
   (define (remove-block! b)
      (let loop ((ins (block-first b))
		 (prev #f))
	 (when (pair? ins)
	    (if (move-nop? (car ins))
		(with-access::rtl_ins/ra (car ins) (dest args fun)
		   (set! n (+fx n 1))
		   (cond
		      ((pair? prev)
		       (set-cdr! prev (cdr ins))
		       (loop (cdr ins) prev))
		      ((null? (cdr ins))
		       (set! fun (instantiate::rtl_nop))
		       (set! dest #f)
		       (set! args '()))
		      (else
		       (block-first-set! b (cdr ins))
		       (loop (cdr ins) prev))))
		(loop (cdr ins) ins)))))
   (for-each remove-block! blocks)
   n)

;*---------------------------------------------------------------------*/
;*    rtl-size ...                                                     */
;*---------------------------------------------------------------------*/
(define (rtl-size blocks)
   (apply + (map (lambda (b) (length (block-first b))) blocks)))

;*---------------------------------------------------------------------*/
;*    dump ::rtl_ins/ra ...                                            */
;*---------------------------------------------------------------------*/
(define-method (dump o::rtl_ins/ra p m)
   (with-access::rtl_ins/ra o (out in)
      (call-next-method)
      (on-trace (cgen 3)
		(display "\n" p)
		(display "      [in:" p)
		(regset-for-each (lambda (r) (display " " p) (dump r p 0)) in)
		(display "]\n" p)
		(display "      [out:" p)
		(regset-for-each (lambda (r) (display " " p) (dump r p 0)) out)
		(display "]" p))))

;*---------------------------------------------------------------------*/
;*    dump ::rtl_reg/ra ...                                            */
;*---------------------------------------------------------------------*/
(define-method (dump o::rtl_reg/ra p m)
   (with-access::rtl_reg/ra o (var type color)
      (when (integer? color)
	 (display "%" p)
	 (display color p)
	 (display "-" p))
      (call-next-method)))

;*---------------------------------------------------------------------*/
;*    *registers* ...                                                  */
;*---------------------------------------------------------------------*/
(define *registers* #unspecified)
(define *registers-number* 0)

;*---------------------------------------------------------------------*/
;*    regset-ref ...                                                   */
;*---------------------------------------------------------------------*/
(define (regset-ref s i)
   (char->integer (string-ref (regset-string s) i)))

;*---------------------------------------------------------------------*/
;*    regset-set! ...                                                  */
;*---------------------------------------------------------------------*/
(define (regset-set! s i v)
   (string-set! (regset-string s) i (integer->char v)))

;*---------------------------------------------------------------------*/
;*    regset-init! ...                                                 */
;*---------------------------------------------------------------------*/
(define (regset-init! registers)
   (let ((l (length registers)))
      (set! *registers-number* l)
      (set! *registers* (make-vector (+fx 1 l)))
      (for-each (lambda (r)
		   (vector-set! *registers* (rtl_reg/ra-num r) r))
		registers)))
   
;*---------------------------------------------------------------------*/
;*    make-empty-regset ...                                            */
;*---------------------------------------------------------------------*/
(define (make-empty-regset::regset)
   (instantiate::regset
      (length 0)
      (string (make-string (+fx 1 (/fx *registers-number* 8)) #a000))))

;*---------------------------------------------------------------------*/
;*    create-regset ...                                                */
;*---------------------------------------------------------------------*/
(define (create-regset::regset lst::pair-nil)
   (let ((s (make-empty-regset)))
      (for-each (lambda (e) (regset-add! s e)) lst)
      s))

;*---------------------------------------------------------------------*/
;*    duplicate-regset ...                                             */
;*---------------------------------------------------------------------*/
(define (duplicate-regset::regset s0::regset)
   (let ((s (make-empty-regset)))
      (regset-union! s s0)
      s))

;*---------------------------------------------------------------------*/
;*    regset-member? ...                                               */
;*---------------------------------------------------------------------*/
(define (regset-member?::bool s::regset reg::rtl_reg/ra)
   (with-access::rtl_reg/ra reg (num)
      (let ((base (/fx num 8))
	    (bit (remainder num 8)))
	 (>fx (bit-and (regset-ref s base) (bit-lsh 1 bit)) 0))))

;*---------------------------------------------------------------------*/
;*    regset-empty? ...                                                */
;*---------------------------------------------------------------------*/
(define (regset-empty? s::regset)
   (=fx (regset-length s) 0))

;*---------------------------------------------------------------------*/
;*    regset-add! ...                                                  */
;*---------------------------------------------------------------------*/
(define (regset-add!::bool s::regset reg::rtl_reg/ra)
   (and (not (regset-member? s reg))
	(with-access::rtl_reg/ra reg (num)
	   (let ((base (/fx num 8))
		 (bit (remainder num 8)))
	      (with-access::regset s (length)
		 (set! length (+fx length 1))
		 (let ((nval (bit-or (regset-ref s base) (bit-lsh 1 bit))))
		    (regset-set! s base nval)
		    #t))))))
   
;*---------------------------------------------------------------------*/
;*    regset-remove! ...                                               */
;*---------------------------------------------------------------------*/
(define (regset-remove!::bool s::regset reg::rtl_reg/ra)
   (and (regset-member? s reg)
	(with-access::rtl_reg/ra reg (num)
	   (let ((base (/fx num 8))
		 (bit (remainder num 8)))
	      (with-access::regset s (length)
		 (set! length (-fx length 1)))
	      (let ((nval (bit-and (regset-ref s base)
				   (bit-not (bit-lsh 1 bit)))))
		 (regset-set! s base nval)
		 #t)))))
   
;*---------------------------------------------------------------------*/
;*    regset-union! ...                                                */
;*---------------------------------------------------------------------*/
(define (regset-union!::bool s1::regset s2::regset)
   (let ((st1 (regset-string s1))
	 (st2 (regset-string s2)))
      (let loop ((i (-fx (string-length st1) 1))
		 (res #f))
	 (cond
	    ((=fx i -1)
	     res)
	    ((char=? (string-ref st1 i) (string-ref st2 i))
	     (loop (-fx i 1) res))
	    (else
	     (let ((n2 (regset-ref s2 i)))
		(let liip ((j 1)
			   (res res)
			   (n1 (regset-ref s1 i)))
		   (cond
		      ((=fx j 256)
		       (loop (-fx i 1) res))
		      ((=fx (bit-and n1 j) (bit-and n2 j))
		       (liip (bit-lsh j 1) res n1))
		      ((=fx (bit-and n1 j) 0)
		       (with-access::regset s1 (length)
			  (set! length (+fx length 1))
			  (let ((n1 (bit-or n1 j)))
			     (regset-set! s1 i n1)
			     (liip (bit-lsh j 1) #t n1))))
		      (else
		       (liip (bit-lsh j 1) res n1))))))))))

;*---------------------------------------------------------------------*/
;*    regset-union*! ...                                               */
;*---------------------------------------------------------------------*/
(define (regset-union*!::bool s::regset ss::pair-nil)
   (let loop ((ss ss)
	      (res #f))
      (if (null? ss)
	  res
	  (loop (cdr ss) (or (regset-union! s (car ss)) res)))))

;*---------------------------------------------------------------------*/
;*    regset-for-each ...                                              */
;*---------------------------------------------------------------------*/
(define (regset-for-each proc::procedure s::regset)
   (let loop ((i 0))
      (when (<fx i *registers-number*)
	 (let ((e (vector-ref *registers* i)))
	    (when (regset-member? s e) (proc e))
	    (loop (+fx i 1))))))

;*---------------------------------------------------------------------*/
;*    regset-first ...                                                 */
;*---------------------------------------------------------------------*/
(define (regset-first proc::procedure s::regset)
   (let loop ((i 0))
      (if (<fx i *registers-number*)
	  (let ((e (vector-ref *registers* i)))
	     (if (and (regset-member? s e) (proc e))
		 e
		 (loop (+fx i 1))))
	  #f)))

;*---------------------------------------------------------------------*/
;*    regset-dump ...                                                  */
;*---------------------------------------------------------------------*/
(define (regset-dump s::regset . p)
   (let ((p (if (pair? p) (car p) (current-output-port))))
      (display "{" p)
      (regset-for-each (lambda (e)
			  (display " " p)
			  (dump e p 0))
		       s)
      (display "}" p)))
		       
