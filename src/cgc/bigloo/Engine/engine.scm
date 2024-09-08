;*=====================================================================*/
;*    serrano/trashcan/cgc/Engine/engine.scm                           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Jul 12 15:53:47 1995                          */
;*    Last change :  Wed Dec 27 16:00:16 2000 (serrano)                */
;*    -------------------------------------------------------------    */
;*    We have read the argument line. We start the real compilation    */
;*    process.                                                         */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module engine_engine
   (export (engine))
   (import tools_speek
           engine_param
	   parser_file
	   ast_escape
	   ast_node
	   symbol_bind
	   (bind-types! type_bind)
	   type_check
	   arch_arch
	   ir_node
	   ir_display
	   ir_translate
	   canonical_canonical
	   canonical_basic-block
	   canonical_trace
	   iselect_iselect
	   iselect_asm
	   btensioning_btensioning
	   nonop_nonop
	   liveness_liveness
	   regalloc_regalloc
	   arecord_arecord))

;*---------------------------------------------------------------------*/
;*    stop-on-pass! ...                                                */
;*---------------------------------------------------------------------*/
(define-macro (stop-on-pass! pass do-action stop-action)
   `(let ((res ,do-action)
	  (proc ,(if (boolean? stop-action)
		    `(lambda (x) ,stop-action)
		    stop-action)))
       (if (eq? *pass-stop* ',pass)
	   (begin
	      (if (procedure? proc)
		  (proc res))
	      (exit 0))
	   res)))

;*---------------------------------------------------------------------*/
;*    engine ...                                                       */
;*---------------------------------------------------------------------*/
(define (engine)
   ;; welcome message
   (if (string? *src*) (verbose 1 *src* #":\n"))
   ;; global abstract syntax tree building
   (let ((ast (if (string? *src*)
		  (file->ast *src*)
		  (console->ast))))
      ;; type construction
      (bind-types! ast) 
      ;; symbol resolution
      (stop-on-pass! symbol (bind-symbols! ast) #t)
      ;; type checking and operator overloading
      (stop-on-pass! types (type-checks ast) #t)
      ;; escape property
      (stop-on-pass! escape (escape-ast ast) #t)
      ;; we setup the target architecture
      (arch-setup! *target*)
      ;; the ast printing
      (stop-on-pass! ast ast print)
      ;; the ast to ir translation
      (let ((ir (stop-on-pass! ir (ast->ir ast) print)))
	 ;; the ir canonicalization
	 (set! ir (stop-on-pass! canonicalize (canonicalize ir) print))
	 ;; the basic blocks construction
	 (set! ir (stop-on-pass! basic-blocks (basic-blocks ir) print))
	 ;; the trace computation
	 (if (>fx *optim* 0)
	     (set! ir (stop-on-pass! trace (trace ir) print)))
	 (let* ((prn   (lambda (asms)
			  (let ((thunk (lambda ()
					  (for-each print asms)
					  ;; shall we add the runtime
					  ;; system code
					  (if *rts*
					      (print (arch-runtime-code))))))
			     (if (string? *dest*)
				 (with-output-to-file *dest* thunk)
				 (thunk)))))
		(asms  (stop-on-pass! iselect
				      (instruction-selection ir)
				      prn)))
	    ;; the branch tensionning optimization
	    (if (>fx *optim* 0)
		(set! asms (stop-on-pass! branch-tensioning
					  (branch-tensioning asms)
					  prn)))
	    ;; the nop removal optimization
	    (if (>fx *optim* 0)
		(set! asms (stop-on-pass! nonop
					  (nonop asms)
					  prn)))
	    ;; the liveness analysis
	    (stop-on-pass! liveness (liveness! asms) prn)
	    ;; the register allocation
	    (set! asms (stop-on-pass! regalloc (regalloc! asms) prn))
	    ;; the activation record allocation
	    (set! asms (stop-on-pass! arecord (arecord! asms) prn))
	    (prn asms)))))
      

