;*=====================================================================*/
;*    .../project/bglstone/src/cgc/bigloo/Canonical/canonical.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Feb 12 15:34:43 1998                          */
;*    Last change :  Fri Jun 20 06:22:00 2003 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The ir canonicalize transformation. This is mostly an hoisting   */
;*    transformation. We apply the following re-writings:              */
;*    1. (call fun args) -->                                           */
;*         (seq (move (temp t) (call fun args)) (temp t))              */
;*    2. (seq (list (seq l1) l2)) -->                                  */
;*         (seq (append l1 l2))                                        */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module canonical_canonical
   (import tools_speek
	   ast_node
	   ir_node)
   (export (canonicalize::ir ::ir)))

;*---------------------------------------------------------------------*/
;*    canonicalize ...                                                 */
;*---------------------------------------------------------------------*/
(define (canonicalize::ir ir::ir)
   (verbose 1 #"  - canonicalize\n")
   (with-access::ir ir (text-segment)
      (set! text-segment (map (lambda (stmt)
				 (multiple-value-bind (stmt expr)
				    (hoist stmt #unspecified)
				    stmt))
			      text-segment))
      ir))

;*---------------------------------------------------------------------*/
;*    hoist :: ...                                                     */
;*---------------------------------------------------------------------*/
(define-generic (hoist body context))

;*---------------------------------------------------------------------*/
;*    hoist ::framedecl ...                                            */
;*---------------------------------------------------------------------*/
(define-method (hoist body::framedecl context)
   (with-access::framedecl body (ir-stmt)
      (set! ir-stmt (multiple-value-bind (stmt expr)
		       (hoist ir-stmt 'framedecl)
		       stmt))
      [assert (ir-stmt) (seq? ir-stmt)]
      body))

;*---------------------------------------------------------------------*/
;*    hoist ::ir-const ...                                             */
;*---------------------------------------------------------------------*/
(define-method (hoist body::ir-const context)
   (values #unspecified body))

;*---------------------------------------------------------------------*/
;*    hoist ::name ...                                                 */
;*---------------------------------------------------------------------*/
(define-method (hoist body::name context)
   (values #unspecified body))

;*---------------------------------------------------------------------*/
;*    hoist ::temp ...                                                 */
;*---------------------------------------------------------------------*/
(define-method (hoist body::temp context)
   (values #unspecified body))

;*---------------------------------------------------------------------*/
;*    hoist ::opfx ...                                                 */
;*---------------------------------------------------------------------*/
(define-method (hoist body::opfx context)
   (with-access::opfx body (left right)
      (multiple-value-bind (sleft eleft)
	 (hoist left 'opfx)
	 (multiple-value-bind (sright eright)
	    (hoist right 'opfx)
	    (set! left eleft)
	    (set! right eright)
	    (cond
	       ((and (ir-stmt? sright) (ir-stmt? sleft))
		(multiple-value-bind (stmt _)
		   (hoist (instantiate::seq
			     (stmts (list sleft sright)))
			  context)
		   (values stmt body)))
	       ((ir-stmt? sright)
		(values sright body))
	       ((ir-stmt? sleft)
		(values sleft body))
	       (else
		(values #unspecified body)))))))

;*---------------------------------------------------------------------*/
;*    hoist ::mem ...                                                  */
;*---------------------------------------------------------------------*/
(define-method (hoist body::mem context)
   (with-access::mem body (addr)
      (multiple-value-bind (saddr eaddr)
	 (hoist addr 'mem)
	 (set! addr eaddr)
	 (values saddr body))))

;*---------------------------------------------------------------------*/
;*    hoist ::call ...                                                 */
;*---------------------------------------------------------------------*/
(define-method (hoist body::call context)
   (with-access::call body (args framedecl)
      (let loop ((args  args)
		 (stmts '()))
	 (if (null? args)
	     (if (and (null? stmts) (memq context '(move-temp move-mem estmt)))
		 (values #unspecified body)
		 (let* ((temp (make-new-temp))
			(seq  (instantiate::seq
				 (stmts (append stmts
						(list (instantiate::move-temp
							 (temp temp)
							 (expr body))))))))
		    (multiple-value-bind (sseq _)
		       (hoist seq context)
		       (values sseq temp))))
	     (multiple-value-bind (sarg earg)
		(hoist (car args) 'call)
		(set-car! args earg)
		(loop (cdr args)
		      (if (ir-stmt? sarg) (cons sarg stmts) stmts)))))))

;*---------------------------------------------------------------------*/
;*    hoist ::move-temp ...                                            */
;*---------------------------------------------------------------------*/
(define-method (hoist body::move-temp context)
   (with-access::move-temp body (expr)
      (multiple-value-bind (sexpr eexpr)
	 (hoist expr 'move-temp)
	 (set! expr eexpr)
	 (if (ir-stmt? sexpr)
	     (hoist (instantiate::seq (stmts (list sexpr body))) context)
	     (values body #unspecified)))))

;*---------------------------------------------------------------------*/
;*    hoist ::move-mem ...                                             */
;*---------------------------------------------------------------------*/
(define-method (hoist body::move-mem context)
   (with-access::move-mem body (addr expr)
      (multiple-value-bind (saddr eaddr)
	 (hoist addr 'addr)
	 (multiple-value-bind (sexpr eexpr)
	    (hoist expr 'move-mem)
	    (set! addr eaddr)
	    (set! expr eexpr)
	    (cond
	       ((and (ir-stmt? saddr) (ir-stmt? sexpr))
		(hoist (instantiate::seq
			  (stmts (list (seq-stmts saddr)
				       (seq-stmts sexpr)
				       body)))
		       context))
	       ((ir-stmt? sexpr)
		(hoist (instantiate::seq
			  (stmts (list (seq-stmts sexpr) body)))
		       context))
	       ((ir-stmt? saddr)
		(hoist (instantiate::seq
			  (stmts (list (seq-stmts saddr) body)))
		       context))
	       (else
		(values body #unspecified)))))))

;*---------------------------------------------------------------------*/
;*    hoist ::estmt ...                                                */
;*---------------------------------------------------------------------*/
(define-method (hoist body::estmt context)
   (with-access::estmt body (>expr)
      (multiple-value-bind (s>expr e>expr)
	 (hoist >expr 'estmt)
	 (set! >expr e>expr)
	 [assert (body) (ir-stmt? body)]
	 (if (ir-stmt? s>expr)
	     (hoist (instantiate::seq (stmts (list s>expr body))) context)
	     (values body #unspecified)))))

;*---------------------------------------------------------------------*/
;*    hoist ::jump ...                                                 */
;*---------------------------------------------------------------------*/
(define-method (hoist body::jump context)
   (values body #unspecified))

;*---------------------------------------------------------------------*/
;*    hoist ::cjump ...                                                */
;*---------------------------------------------------------------------*/
(define-method (hoist body::cjump context)
   (values body #unspecified))

;*---------------------------------------------------------------------*/
;*    hoist ::seq ...                                                  */
;*---------------------------------------------------------------------*/
(define-method (hoist body::seq context)
   (with-access::seq body (stmts)
      (let loop ((olds stmts)
		 (news '()))
	 (if (null? olds)
	     (values (instantiate::seq
			(stmts (reverse! news)))
		     #unspecified)
	     (multiple-value-bind (sn _)
		(hoist (car olds) 'seq)
		[assert (sn olds news body) (ir-stmt? sn)]
		(loop (cdr olds)
		      (if (seq? sn)
			  (append (reverse! (seq-stmts sn)) news)
			  (cons sn news))))))))

;*---------------------------------------------------------------------*/
;*    hoist ::label ...                                                */
;*---------------------------------------------------------------------*/
(define-method (hoist body::label context)
   (values body #unspecified))
