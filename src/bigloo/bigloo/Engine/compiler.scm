;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Engine/compiler.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri May 31 08:22:54 1996                          */
;*    Last change :  Wed Jan 19 17:03:47 2005 (serrano)                */
;*    Copyright   :  1996-2005 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The compiler driver                                              */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module engine_compiler
   (include "Ast/unit.sch"
	    "Engine/pass.sch")
   (import  tools_error
	    engine_pass
	    engine_signals
	    engine_param
	    engine_engine
	    read_src
	    write_expanded
	    write_ast
	    read_access
	    read_jvm
	    heap_restore
	    heap_make
	    ast_env
	    ast_check-sharing
	    type_type
	    ast_var
	    ast_node
	    ast_build
	    ast_unit
	    ast_check-global-init
	    ast_init
	    user_user
	    type_env
	    type_cache
	    module_module
	    module_include
	    expand_eps
	    expand_install
	    init_main
	    init_setrc
	    trace_walk
	    inline_walk
	    effect_walk
	    callcc_walk
	    fail_walk
	    globalize_walk
	    cfa_walk
	    cfa_tvector
	    integrate_walk
	    tailc_walk
	    coerce_walk
	    reduce_walk
	    cnst_walk
	    hgen_walk
	    bdb_setting
	    bdb_spread-obj
	    bdb_walk
	    prof_walk
	    cc_indent
	    cc_cc
	    cc_ld
	    backend_walk)
   (with    cgen_walk)
   (export  (compiler)))

;*---------------------------------------------------------------------*/
;*    compiler ...                                                     */
;*---------------------------------------------------------------------*/
(define (compiler)
   
   ;; we catch signals
   (profile signal (install-compiler-signals!))
   
   ;; we read the source file
   (let ((src (profile read (read-src))))
      ;; if src is false, we launch the interpreter because it means
      ;; that the reader has found a #!... expression instead of a
      ;; module clause
      (cond
	 ((not src) 
	  (set! *interpreter* #t)
	  (compiler-exit (engine)))
	 ((not (pair? src))
	  (user-error "Parse error" "Illegal source file" src)))

      ;; now (and only now) we can say hello
      (hello-world)

      ;; we set bdb options up
      (if (>fx *bdb-debug* 0)
	  (profile bdb (bdb-setting!)))

      ;; we check now if we have parsed all argument
      (if (not (null? *rest-args*))
          (warning "Don't know what to do with arguments: " *rest-args*))
      
      ;; we read access file
      (profile afile (read-access-file))
      (profile package (read-jfile))
      
      ;; we create (or restore) the compilation environment
      (if *lib-mode*
	  (profile env
		   (begin
		      (initialize-Genv!)
		      (initialize-Tenv!)))
	  (profile heap (restore-heap)))

      ;; we initialized the type caching system
      (profile itype (install-type-cache!))
      
      ;; when the vector->tvector optimization is enable we have to
      ;; patch the types of vector-set! familly function.
      (profile vect (patch-vector-set!))
       
      ;; we compile the module clause which leads to the
      ;; complete source code.
      (let* ((module   (car src))
	     (src-code (cdr src))
	     (units    (profile module (produce-module! module))))

	 (stop-on-pass 'dump-heap (lambda () (dump-additional-heaps)))
	 
	 ;; the prof initilization code
	 (if (>=fx *profile-mode* 1)
	     (set! units (cons (make-prof-unit) units)))
	    
	 ;; we produce the mco file
	 (if *module-checksum-object?*
	     (profile mco (module-checksum-object)))
	 (stop-on-pass 'mco (lambda () 0))

	 ;; we now build module body
	 (let* ((c0 (if (null? src-code) '(#unspecified) src-code))
		(c1 (if (>=fx (bigloo-compiler-debug) 2)
			(cons `(bigloo-debug-set! ,(bigloo-compiler-debug)) c0)
			c0))
		(c2 (if (and (fixnum? *heap-debug*) (>=fx *heap-debug* 1))
			;; the C function bdb_release_lock is defined
			;; the the bdb library:
			;; See:
			;; @ref ../../bdb/blib/gc_dump.c:bdb_release_lock@
			(cons '(pragma::int "bdb_release_lock()") c1)
			c1)))
	    (unit-sexp*-add! (get-toplevel-unit) c2))
			  
	 ;; we check error occured while building the ast
	 (pass-postlude #unspecified)
	 
	 ;; we check if all types are defined
	 (profile ctype (check-types))
	 
	 ;; C header generation
	 (stop-on-pass 'hgen hgen-walk)
					    
	 ;; we perfom user pass
	 (user-walk units)
	 (stop-on-pass 'user (lambda () (write-expanded units)))

	 ;; we install macros ...
	 (install-initial-expander)

	 ;; we load the library init files. This must be done after
	 ;; regular macros have been installed in order to enable these
	 ;; macro redefinitions
	 (load-library-init)

	 ;; ... and we macro expand
	 (profile expand (expand-units units))
	 (stop-on-pass 'expand (lambda () (write-expanded units)))
	 
	 ;; ok, now we build the ast
	 (let ((ast (profile ast (build-ast units))))
	    (stop-on-pass 'ast (lambda () (write-ast ast)))
	    (check-sharing "ast" ast)
	    
	    ;; when compiling with warning enabled we perform a check
	    ;; on global variable initialization
	    (if (>fx (bigloo-warning) 0)
		(check-global-initialization))
	    
	    ;; we make a heap on `mkheap' mode
	    (stop-on-pass 'make-heap (lambda () (make-heap)))
	    
	    ;; we make a heap on `mkheap' mode
	    (stop-on-pass 'make-add-heap (lambda () (make-add-heap)))

	    ;; when compiling for bdb we turn all _ type into Bigloo obj type
	    (if (>fx *bdb-debug* 0)
		(bdb-spread-obj! ast))
	    (stop-on-pass 'bdb-spread-obj (lambda () (write-ast ast)))

	    ;; when the compiler is invoked in -g2 mode, we install
	    ;; traces before the inlining
	    (if (and (>fx (bigloo-compiler-debug) 1)
		     (not (memq *target-language* '(jvm .net))))
		(set! ast (profile trace (trace-walk! ast))))
	    (check-sharing "trace" ast)
	    
	    ;; when we are compiling with call/cc we have to
	    ;; put all written local variables in cells
	    (if *call/cc?*
		(set! ast (profile callcc (callcc-walk! ast))))
	    (stop-on-pass 'callcc (lambda () (write-ast ast)))
	    (check-sharing "callcc" ast)
	    
	    ;; the effect property computation
	    (if *optim-unroll-loop?*
		(begin
		   (set! ast (profile effect (effect-walk! ast #f)))
		   (stop-on-pass 'effect (lambda () (write-ast ast)))))
	    (check-sharing "effect" ast)
	    
	    ;; we perform the inlining pass
	    (set! ast (profile inline (inline-walk! ast 'all)))
	    (stop-on-pass 'inline (lambda () (write-ast ast)))
	    (check-sharing "inline" ast)
	    
	    ;; we introduce traces in `small debug mode'
	    (if (and (>fx (bigloo-compiler-debug) 0)
		     (<=fx (bigloo-compiler-debug) 1)
		     (not (memq *target-language* '(jvm .net))))
		(set! ast (profile trace (trace-walk! ast))))
	    (stop-on-pass 'trace (lambda () (write-ast ast)))
	    (check-sharing "trace" ast)
	    
	    ;; we replace `failure' invokation by `error/location' when
	    ;; invoked in debug mode (to be performed after the coercion stage)
	    (if (and (>fx (bigloo-compiler-debug) 0) *error-localization*)
		(set! ast (profile fail (fail-walk! ast))))
	    (stop-on-pass 'fail  (lambda () (write-ast ast)))
	    (check-sharing "fail" ast)
	    
	    ;; the globalization stage
	    (set! ast (profile glo (globalize-walk! ast 'globalization)))
	    (stop-on-pass 'globalize (lambda () (write-ast ast)))
	    (check-sharing "globalize" ast)

	    ;; the control flow analysis
	    (set! ast (profile cfa (cfa-walk! ast)))
	    (stop-on-pass 'cfa (lambda () (write-ast ast)))
	    (check-sharing "cfa" ast)

	    ;; now we have done the cfa, type election has been performed
	    ;; we change the default type from *_* to *obj*.
	    (set-default-type! *obj*)
	    
	    ;; the integration pass
	    (set! ast (profile integ (integrate-walk! ast)))
	    (stop-on-pass 'integrate (lambda () (write-ast ast)))
	    (check-sharing "integrate" ast)

	    ;; the integration pass
	    (when (or (eq? *global-tail-call?* #t)
		      (and *global-tail-call?* *saw*))
	       (set! ast (profile integ (tailc-walk! ast)))
	       (check-sharing "tailc" ast))
	    (stop-on-pass 'tailc (lambda () (write-ast ast)))

	    ;; the reduction transformation for improving error detections
	    (when *optim-dataflow-for-errors?*
	       (set! ast (profile reduce- (reduce-walk! ast #t)))
	       (check-sharing "reduce-" ast))
	    (stop-on-pass 'reduce- (lambda () (write-ast ast)))

	    ;; we introduce type coercion and checking
	    (set! ast (profile coerce (coerce-walk! ast)))
	    (stop-on-pass 'coerce (lambda () (write-ast ast)))
	    (check-sharing "coerce" ast)

	    ;; we re-run the effect computations (for coercion and
	    ;; type checks)
	    (if (or (>=fx *optim* 1) (eq? *pass* 'egen))
		(begin
		   (set! ast (profile effect (effect-walk! ast #t)))
		   (stop-on-pass 'effect (lambda () (write-ast ast)))
		   (stop-on-pass 'egen (lambda () (write-effect ast)))))
	    (check-sharing "effect" ast)

	    ;; the reduction optimizations
	    (if (>=fx *optim* 1)
		(set! ast (profile reduce (reduce-walk! ast))))
	    (stop-on-pass 'reduce (lambda () (write-ast ast)))
	    (check-sharing "reduce" ast)

	    ;; the bdb initialization code
	    (if (>fx *bdb-debug* 0)
		(set! ast (profile bdb (bdb-walk! ast))))
	    (stop-on-pass 'bdb (lambda () (write-ast ast)))

	    ;; the constant computation
	    (set! ast (profile cnst (cnst-walk! ast)) )
	    (stop-on-pass 'cnst (lambda () (write-ast ast)))
	    (check-sharing "cnst" ast)
	    
	    ;; we re-perform the inlining pass in high optimization mode
	    ;; in order to inline all type checkers.
	    (set! ast (profile inline (inline-walk! ast 'reducer)))
	    (stop-on-pass 'inline+ (lambda () (write-ast ast)))
	    (check-sharing "inline+" ast)
	    
	    ;; the code production
	    (let ((ast2 (append-ast (ast-initializers) ast)))
	       (check-sharing "init" ast2)
	       
	       ;; the 2nd reduction optimizations
	       (if (or (>=fx *optim* 2)
		       (memq *target-language* '(jvm .net jsm)))
		   (begin
		      (set! ast2 (profile effect (effect-walk! ast2 #t)))
		      (stop-on-pass 'effect+ (lambda () (write-ast ast2)))
		      (set! ast2 (profile reduce (reduce-walk! ast2)))))
	       (stop-on-pass 'reduce+ (lambda () (write-ast ast2)))
	       (check-sharing "reduce+" ast2)
	       
	       (backend-walk *target-language* ast2))
	    
	    0))))
