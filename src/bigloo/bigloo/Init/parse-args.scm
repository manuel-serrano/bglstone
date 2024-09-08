;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Init/parse-args.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Aug  7 11:47:46 1994                          */
;*    Last change :  Thu Feb  3 14:10:18 2005 (serrano)                */
;*    Copyright   :  1992-2005 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The command line arguments parsing                               */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    Le module                                                        */
;*---------------------------------------------------------------------*/
(module init_parse-args
   (include "Tools/trace.sch")
   (export  (parse-args args))
   (import  engine_configure
	    engine_param
	    init_main
	    init_extend
	    init_setrc
	    expand_srfi-0
	    module_module
	    module_alibrary
	    module_eval
	    write_version
	    tools_trace
	    tools_speek
	    tools_license
	    read_access
	    read_jvm
	    cc_ld
	    jas_as
	    jas_peep
	    expand_srfi-0))

;*---------------------------------------------------------------------*/
;*    Global parse args parameters ...                                 */
;*---------------------------------------------------------------------*/
(define *extended-done?* #f)
(define *library-init*   '())
(define *trace-level*    0)
(define *user-load-path* '())
(define *saw-option* #unspecified)
(define *error-localization-opt* #unspecified)

;*---------------------------------------------------------------------*/
;*    parse-args ...                                                   */
;*---------------------------------------------------------------------*/
(define (parse-args args)
   (set! *bigloo-cmd-name* (car args))
   (set! *bigloo-args* args)
   (let ((heap-name *heap-name*))
      (do-parse-args (cdr args))
      ;; we setup the heap name
      (if (and (memq *target-language* '(jvm .net jsm))
	       (eq? *heap-name* heap-name))
	  ;; the user has not explictly requested an replacement heap file
	  (set! *heap-name* *heap-jvm-name*)))
   ;; saw or no saw
   (set! *saw*
	 (or (eq? *saw-option* #t)
	     (memq *target-language* '(jvm .net jsm))))
   (let ((pres (if *extended-done?*
		   #t
		   (let ((auto-mode (let loop ((sfiles *src-files*))
				       (if (null? sfiles)
					   #f
					   (let ((cell (and
							(string? (car sfiles))
							(assoc
							 (suffix (car sfiles))
							 *auto-mode*))))
					      (if (pair? cell)
						  (cdr cell)
						  (loop (cdr sfiles))))))))
		      (if auto-mode
			  (begin
			     (set! *src-files* '())
			     (do-parse-args `("-extend"
					      ,auto-mode
					      ,@(cdr args))))
			  #t)))))
      ;; we are done with the parsing, we invert all the lists
      (set! *src-files*       (reverse! *src-files*))
      (set! *o-files*         (reverse! *o-files*))
      (set! *c-files*         (reverse! *c-files*))
      (set! *load-path*       (append (reverse! *user-load-path*) *load-path*))
      (set! *bigloo-user-lib* (reverse! *bigloo-user-lib*))
      (set! *rest-args*       (reverse! *rest-args*))
      ;; we clean the optimization flags
      (if (not (boolean? *optim-unroll-loop?*))
	  (set! *optim-unroll-loop?* #f))
      (if (>fx *bdb-debug* 0)
	  (begin
	     (set! *inlining?* #f)
	     (if (>fx *optim* 0)
		 (begin
		    (warning "Incompatible options"
			     "-O/-gbdb"
			     " disabling debug")
		    (set! *bdb-debug* 0)))
	     (if (and (fixnum? *profile-mode*) (>fx *profile-mode* 0))
		 (begin
		    (warning "Incompatible options"
			     "-p/-gbdb"
			     " disabling debug")
		    (set! *bdb-debug* 0)))))
      ;; we always add a jvm package name for FOREIGN thus, a heap that
      ;; is not compiled in -jvm mode can still be used later with -jvm
      ;; option (provided that the heap file does not use any java clause).
      (add-qualified-type! 'foreign *jvm-foreign-class-name*)
      ;; when compiling for the Java back-end, the call/cc mode must
      ;; always be disabled (because call/cc is only supported in the
      ;; dynamic extend of the compilation).
      (if (memq *target-language* '(jvm .net jsm))
	  (begin
	     (set! *call/cc?* #f)
	     (set! *pragma?* #f)
	     (set! *bdb-debug* 0)
	     (set! *additional-heap-names*
		   (delete! "bdb.heap" *additional-heap-names*))
	     (if (or (not (number? *compiler-debug*))
		     (<= *compiler-debug* 1))
		 (begin
		    (set! *optim-dataflow?* #t)
		    (set! *optim-reduce-beta?* #t)))
	     (set! *debug-module* 0)))
      ;; profiling mode disable inlining
      (if (and (fixnum? *profile-mode*) (>fx *profile-mode* 0))
	  (begin
	     (set! *optim-loop-inlining?* #f)
	     (set! *optim-unroll-loop?* #f)
	     (set! *inlining-kfactor* (lambda (olevel) 0))
	     (if (>=fx *profile-mode* 2)
		 (set! *inlining?* #f))))
      ;; we start the trace if the level is > 0
      (if (>fx *trace-level* 0)
	  (let ((passes (trace get-pass-names)))
	     (if (memq *pass* passes)
		 (start-trace *trace-level* *pass*)
		 (warning "parse-args" "No trace for this pass -- " *pass*))
	     (for-each (lambda (pass)
			  (if (not (memq pass passes))
			      (warning "parse-args"
				       "No trace for this pass -- "
				       pass)))
		       *additional-traces*)))
      ;; when the reader is used to construct the constant, it must
      ;; be initialized very soon
      (if (eq? *init-mode* 'read)
	  (set! *early-with-modules*
		(cons '__reader *early-with-modules*)))
      ;; initialize the libraries
      (for-each setup-library-values *library-init*)
      ;; we check with back-end we are using for defining the correct
      ;; srfi ressources
      (if *saw* (add-supported-srfi! 'bigloo-saw))
      (case *target-language*
	 ((jvm)
	  (set! *use-private?* #t)
	  (add-supported-srfi! 'bigloo-jvm))
	 ((.net)
	  (set! *use-private?* #t)
	  (add-supported-srfi! 'bigloo-.net)
	  (add-supported-srfi! 'bigloo-dotnet))
	 ((jsm)
	  (set! *use-private?* #t)
	  (add-supported-srfi! 'bigloo-jsm))
	 ((c native)
	  (set! *target-language* 'c)
	  (set! *use-private?* #f)
	  (add-supported-srfi! 'bigloo-c)))
      ;; and we are done for the arguments parsing
      pres))
 
;*---------------------------------------------------------------------*/
;*    do-parse-args ...                                                */
;*---------------------------------------------------------------------*/
(define (do-parse-args args)
   (define (environment-usage manual?)
      (print "Shell Variables:")
      (for-each (lambda (var)
		   (if manual?
		       (begin
			  (print "   - " (car var))
			  (print "     " (cdr var)))
		       (print "   - " (car var) ": " (cdr var))))
		'(("TMPDIR" . "tmp directory (default \"/tmp\")")
		  ("BIGLOOLIB" . "libraries' directory")
		  ("BIGLOOHEAP" . "the initial heap size in megabytes (4 MB by default)")
		  ("BIGLOOSTACKDEPTH" . "the error stack depth printing")
		  ("BIGLOOLIVEPROCESS" . "the maximum number of Bigloo live processes")))
      (newline)
      (print "Runtime Command file:")
      (print "   - ~/.bigloorc"))
   (define (usage args-parse-usage level manual?)
      (version)
      (print "usage: bigloo [options] [name.suf]")
      (newline)
      (args-parse-usage #f)
      (newline)
      (environment-usage manual?)
      (newline)
      (newline)
      (print "------------")
      (print " * : only available in developing mode")
      (print " . : option enabled from -O3 mode")
      (newline)
      (if (> level 1)
	  (begin
	     (newline)
	     (print "Bigloo Control Variables:")
	     (bigloo-variables-usage manual?)))
      (if *bigloo-licensing?*
	  (begin
	     (newline)
	     (newline)
	     (print (bigloo-license))))
      (compiler-exit 0))

   (bigloo-warning-set! 2)
   
   (args-parse args
       
;*--- misc ------------------------------------------------------------*/
      (section "Misc")
      ;; priliminary test
      (("-" (help "Read source code on current input channel"))
       (set! *src-files* (cons 'stdin *src-files*)))
      ;; help
      (("?")
       (usage args-parse-usage 1 #f))
      ((("-help" "--help") (help "This help message"))
       (usage args-parse-usage 1 #f))
      (("-help2" (help "The exhaustive help message"))
       (usage args-parse-usage 2 #f))
      (("-help-manual" (help "The help message formatted for the manual"))
       (usage args-parse-usage 2 #t))
      ;; output name
      (("-o" ?file (help "Name the output FILE"))
       (set! *dest* file))
      ;; output to current output port
      (("--to-stdout" (help "Write C code on current output channel"))
       (set! *verbose* -1)
       (set! *dest* '--to-stdout))
      ;; stop after .o production
      (("-c" (help "Suppress linking and produce a .o file"))
       (set! *pass* 'cc))
      ;; suffixes
      (("-suffix" ?suffix (help "Recognize suffix as Scheme source"))
       (set! *src-suffix* (cons suffix *src-suffix*)))
      ;; access file name
      (("-afile" ?file (help "Name of the access file"))
       (set! *access-file* file))
      ;; one access
      (("-access" ?module ?file (help "Set access between module and file"))
       (add-access! (string->symbol module) (list file)))
      ;; package access file name
      (("-jfile" ?file (help "Name of the Jvm package file"))
       (set! *qualified-type-file* file))
      ;; one type addition
      (("-jadd" ?module ?qtype (help "Set JVM qualifed type name for module"))
       (add-qualified-type! (string->symbol module) qtype))
      ;; main function
      (("-main" ?fun (help "Set the main function"))
       (set! *main* fun))
      ;; with modules
      (("-with" ?module (help "Import addition module"))
       (set! *early-with-modules* (cons (string->symbol module)
					*early-with-modules*)))
      ;; multiple-include
      (("-multiple-inclusion" (help "Enables multiple inclusions of the Bigloo includes"))
       (set! *include-multiple* #t))
      ;; Bigloo libary
      (("-library" ?library (help "Compile/link with additional Bigloo library"))
       (set! *library-init* (cons (use-library! library 'delay)
				  *library-init*)))
      ;; srfi support
      (("-srfi" ?srfi (help "Declares srfi support"))
       (add-supported-srfi! (string->symbol srfi)))
      (("-dload-sym" (help "Emit a Bigloo dynamic loading entry point"))
       (set! *dlopen-init* #t))
      (("-heapsize" ?size (help "Set the initial heap size value (in megabyte)"))
       (set! *user-heap-size* (string->integer size)))
      
;*--- Configuration and version ---------------------------------------*/
      (section "Configuration and path")
      ;; version
      (("-version" (help "The current release"))
       (short-version)
       (compiler-exit 0))
      ;; revision
      (("-revision" (help "The current release (short format)"))
       (revision)
       (compiler-exit 0))
      ;; query
      (("-query" (help "Dump the current configuration"))
       (query))
      ;; -q
      (("-q" (help "Do not load any rc file"))
       'nothing-to-do)
      ;; -eval
      (("-eval" ?string (help "Evaluate STRING"))
       (let ((port (open-input-string string)))
	  (let laap ((exp (read port)))
	     (if (eof-object? exp)
		 'done
		 (begin
		    (eval exp)
		    (laap (read port)))))))
      ;; load path
      (("-I" ?dir (help "Add DIR to the load path"))
       (set! *user-load-path* (cons dir *user-load-path*)))
      ;; library path
      (("-lib-dir" ?dir (help "Set lib-path to DIR"))
       (set! *lib-dir* (list dir)))
      (("-lib-version" ?version (help "Set the version of the libraries"))
       (bigloo-library-version-set! version))
      (("-L" ?name (help "Set additional library path"))
       (set! *lib-dir* (cons name *lib-dir*)))
      
;*--- back-end --------------------------------------------------------*/
      (section "Back-end")
      ;; native code generation
      (("-native" (help "Compile module to native object file (via C)"))
       (set! *obj-suffix* (list *c-object-file-extension*))
       (set! *target-language* 'native))
      ;; jvm code generation
      (("-jvm" (help "Compile module to JVM .class files"))
       (set! *obj-suffix* '("class"))
       (set! *target-language* 'jvm))
      (("-dotnet" (help "Compile module to .NET object files"))
       (set! *obj-suffix* '("obj"))
       (set! *target-language* '.net))
      (("-jsm" (help "Compile module to JSM"))
       (set! *obj-suffix* '("bin"))
       (set! *target-language* 'jsm))
      ;; Bigloo Assembly code generation
      (("-saw" (help "Cut the AST in the saw-mill"))
       (set! *saw-option* #t))
      (("-no-saw" (help "Disable saw back-ends"))
       (set! *saw-option* #f))
      ;; interperter
      (("-i" (help "Interprete module"))
       (set! *interpreter* #t))
      
;*--- Dialect options -------------------------------------------------*/
      (section "Dialect")
      ;; nil
      (("-nil" (help "Evaluate '() as #f in `if' expression"))
       (set! *nil* #f))
      (("-call/cc" (help "Enable call/cc function"))
       ;; -g3 and -call/cc are incompatible
       (if (>fx *compiler-debug* 1)
	   (set! *compiler-debug* 1))
       (set! *call/cc?* #t))
      (("-hygien" (help "Obsolete (R5rs macros are always supported)"))
       #unspecified)
      ;; reflection
      (("-fno-reflection" (help "Disable reflection code production"))
       (set! *reflection?* #f))
      (("+fno-reflection" (help "Enable reflection code production"))
       (set! *reflection?* #t))
      ;; object-nil
      (("-fclass-nil" (help "Enables generation of \"class-nil\" function"))
       (set! *class-nil?* #t))
      (("-fno-class-nil" (help "Disables generation of \"class-nil\" function"))
       (set! *class-nil?* #t))
      ;; arithmetic
      (("-farithmetic" (help "Suppress genericity of arithmetic operators"))
       (set! *genericity* #f))
      ;; case sensitivity
      (("-fcase-sensitive" (help "Case sensitive reader (default)"))
       (bigloo-case-sensitivity-set! 'sensitive))
      (("-fcase-insensitive" (help "Case insensitive reader (downcase symbols)"))
       (bigloo-case-sensitivity-set! 'downcase))
      
;*--- Optimization ----------------------------------------------------*/
      (section "Optimization")
      ;; benchmarking
      (("-Obench" (help "Benchmarking mode"))
       (do-parse-args `("-O6" "-unsafe"
			     "-copt" ,*cflags-optim*
			     "-static-bigloo"))) 
      ;; optimization
      (("-O?opt" (help "-O[2..6]" "Optimization modes"))
       (parse-optim-args opt))
      ;; cfa arithmetic
      (("-fcfa-arithmetic" (help "Enable arithmetic spec. (enabled from -O2)"))
       (set! *optim-cfa-arithmetic?* #t))
      (("-fno-cfa-arithmetic" (help "Disable arithmetic spec."))
       (set! *optim-cfa-arithmetic?* #f))
      ;; loop unrolling
      (("-funroll-loop" (help "Enable loop unrolling (enabled from -O3)"))
       (set! *optim-unroll-loop?* #t))
      (("-fno-unroll-loop" (help "Disable loop unrolling"))
       (set! *optim-unroll-loop?* #f))
      (("-fno-loop-inlining" (help "Disable loop inlining"))
       (set! *optim-loop-inlining?* #f)) 
      (("-floop-inlining" (help "Enable loop inlining (default)"))
       (set! *optim-loop-inlining?* #t)) 
      (("-fno-inlining" (help "Disable inline optimization"))
       (set! *inlining?* #f))
      (("-fno-user-inlining" (help "Disable user inline optimization"))
       (set! *user-inlining?* #f))
      ;; data flow optimization
      (("-fbeta-reduce" (help "Enable simple beta reduction (enable from -O3)"))
       (set! *optim-reduce-beta?* #t))
      (("-fno-beta-reduce" (help "Disable simple beta reduction"))
       (set! *optim-reduce-beta?* #f))
      (("-fdataflow" (help "Enable dataflow optimizations (enable from -O)"))
       (set! *optim-dataflow?* #t))
      (("-fno-dataflow" (help "Disable dataflow optimizations"))
       (set! *optim-dataflow?* #f))
      (("-fdataflow-for-errors" (help "Enable dataflow optimizations for improviing type error messages"))
       (set! *optim-dataflow-for-errors?* #t))
      (("-fno-dataflow-for-errors" (help "Disable dataflow optimizations for improviing type error messages"))
       (set! *optim-dataflow-for-errors?* #f))
      ;; O macro
      (("-fO-macro" (help "Enable Optimization macro (default)"))
       (set! *optim-O-macro?* #t))
      (("-fno-O-macro" (help "Disable Optimization macro"))
       (set! *optim-O-macro?* #f))
      ;; tailc
      (("-fglobal-tailc" (help "Enable global tail-call optimization"))
       (set! *global-tail-call?* #t))
      (("-fno-global-tailc" (help "Disable global tail-call optimization"))
       (set! *global-tail-call?* #f))
      ;; saw register allocation
      (("-fsaw-register-allocation" (help "Enable saw register allocation"))
       (set! *saw-register-allocation?* #t))
      (("-fno-saw-register-allocation" (help "Disable saw register allocation"))
       (set! *saw-register-allocation?* #f))
      (("-fsaw-register-coalesce" (help "Enable saw register coalescing"))
       (set! *saw-register-coalesce?* #t))
      (("-fno-saw-register-coalesce" (help "Disable saw register coalescing"))
       (set! *saw-register-coalesce?* #f))
      (("-fsaw-register-allocation-max-size" ?size (help "Set the register allocation body size limit"))
       (set! *saw-register-allocation-max-size* (string->integer size)))
      
;*--- Safety ----------------------------------------------------------*/
      (section "Safety")
      ;; unsafe
      (("-unsafe?opt" (help "-unsafe[atrsvl]" "Don't check [type/arity/range/struct/version/library]"))
       (parse-unsafe-args opt))
      
;*--- Debug -----------------------------------------------------------*/
      (section "Debug")
      ;; -g
      (("-glines" (help "Emit # line directives"))
       (set! *c-debug-lines-info* #t))
      (("-gbdb-no-line" (help "Don't emit # line directives"))
       (set! *bdb-debug-no-line-directives?* #t))
      (("-gbdb?opt" (help "-gbdb[23]" "Compile with bdb debug informations"))
       (parse-bdb-args opt))
      (("-gself" (help "Enables self compiler debug options"))
       (set! *compiler-sharing-debug?* #t))
      (("-gheap" (help "Enables heap debugging (set with -gbdb2)"))
       (set! *heap-debug* 1))
      (("-gmodule" (help "Debug module initialization"))
       (set! *debug-module* 1))
      (("-gerror-localization" (help "Localize error calls in the source code"))
       (set! *error-localization-opt* #t)
       (set! *error-localization* #t))
      (("-gno-error-localization" (help "Don't localize error calls in the source code"))
       (set! *error-localization-opt* #f)
       (set! *error-localization* #f))
      (("-gjvm" (help "Annote JVM classes for debug"))
       (set! *jvm-debug* #t))
      (("-g?opt" (help "-g[234]" "Produce Bigloo debug informations"))
       (parse-debug-args opt))
      (("-cg" (help "Compile C files with debug option"))
       (set! *rm-tmp-files* #f)
       (set! *c-debug* #t)
       (set! *strip* #f))
      (("-export-all" (help "Eval export-all all routines"))
       (set! *all-eval?* #t))
      (("-export-exports" (help "Eval export-exports all routines"))
       (set! *all-export-eval?* #t))
      (("-export-mutable" (help "Enables Eval redefinition of all \"::obj\" routines"))
       (set! *all-export-mutable?* #t))
      
;*--- Profiling -------------------------------------------------------*/
      (section "Profiling")
      ;; -pg
      (("-p" (help "-p[2]" "Compile files for profiling"))
       (if (or (not (number? *profile-mode*))
	       (<fx *profile-mode* 1))
	   (begin
	      (set! *strip* #f)
	      (set! *profile-library* #t)
	      (set! *cc-options* (string-append *cc-options*
						" " *cflags-prof*))
	      (set! *profile-mode* 1))))
      (("-p2")
       (if (or (not (number? *profile-mode*))
	       (<fx *profile-mode* 2))
	   (begin
	      (set! *strip* #f)
	      (set! *profile-library* #t)
	      (set! *cc-options* (string-append *cc-options*
						" " *cflags-prof*))
	      (set! *profile-mode* 2)
	      (do-parse-args '("-static-bigloo")))))
      (("-pg" (help "Compile files with profiling option"))
       (set! *strip* #f)
       (set! *profile-library* #t)
       (set! *cc-options* (string-append *cc-options* " " *cflags-prof*)))
      
;*--- verbosity -------------------------------------------------------*/
      (section "Verbosity")
      ;; silence
      (("-s" (help "Be silent and inhibit all warning messages"))
       (set! *verbose* -1)
       (bigloo-warning-set! 0)
       (set! *load-verbose* #f))
      ;; verbose
      (("-v" (help "-v[23]" "Be verbose"))
       (set! *verbose* 1))
      (("-v2")
       (set! *verbose* 2))
      (("-v3")
       (set! *verbose* 3))
      (("-no-hello" (help "Dont' say hello even in verbose mode"))
       (set! *hello* #f))
      (("-w" (help "Inhibit all warning messages"))
       (bigloo-warning-set! 0))
      (("-wslots" (help "Inhibit overriden slots warning messages"))
       (set! *warning-overriden-slots* #f))
      ;; warning
      (("-Wall" (help "warn about all possible type errors"))
       (set! *warning-overriden-slots* #t)
       (bigloo-warning-set! 2))
      
;*--- Compilation modes -----------------------------------------------*/
      (section "Compilation modes")
      ;; remove temporary files
      (("-rm" (help "<-/+>rm" "Don't or force removing .c or .il files"))
       (set! *rm-tmp-files* #f))
      (("+rm")
       (set! *rm-tmp-files* #t))
      ;; Extended compiler
      (("-extend" ?name (help "Extend the compiler"))
       (set! *extended-done?* #t)
       (load-extend name)
       (if (procedure? *extend-entry*)
	   (set! the-remaining-args (*extend-entry* the-remaining-args))))
      (("-fsharing" (help "Attempt to share constant data"))
       (set! *shared-cnst?* #t))
      (("-fno-sharing" (help "Do not attempt to share constant data"))
       (set! *shared-cnst?* #f))
      (("-fmco" (help "Produce an .mco file"))
       (set! *module-checksum-object?* #t))
      (("-fmco-include-path" ?dir (help "Add dir to mco C include path"))
       (set! *mco-include-path* (cons dir *mco-include-path*)))
      
;*--- Back-end compilation and link -----------------------------------*/
      (section "Native specific options")
      ;; The C compiler
      (("-cc" ?compiler (help "Specify the C compiler"))
       (set! *cc* compiler))
      ;; ISO C
      (("-stdc" (help "Generate strict ISO C code"))
       (set! *stdc* #t))
      ;; cc options
      (("-copt" ?string (help "Invoke cc with STRING"))
       (set! *cc-options* (string-append *cc-options* " " string)))
      ;; link options
      (("-ldopt" ?string (help "Invoke ld with STRING"))
       (set! *ld-options* (string-append string " " *ld-options*)))
      (("-ldpostopt" ?string (help "Invoke ld with STRING (end of arguments)"))
       (set! *ld-post-options* (string-append string " " *ld-post-options*)))
      ;; move or -o ?
      (("--force-cc-o" (help "Force the C compiler to use -o instead of mv"))
       (set! *cc-move* #f))
      ;; link absolute or relative
      (("-ld-relative" (help "Link using -l notation for libraries (default)"))
       (set! *ld-relative* #t))
      (("-ld-absolute" (help "Link using absolute path names for libraries"))
       (set! *ld-relative* #f))
      ;; static Bigloo library
      (("-static-bigloo"
	(help "Link with the static bigloo library"))
       (set! *static-bigloo?* #t))
      ;; double libraries inclusions
      (("-ld-libs1"
	(help "Add once user libraries when linking"))
       (set! *double-ld-libs?* #f))
      (("-ld-libs2"
	(help "Add twice user libraries when linking (default)"))
       (set! *double-ld-libs?* #t))
      ;; C library linking
      (("-l?library" (help "Link with host library"))
       (set! *bigloo-user-lib* (cons (string-append "-l" library)
				     *bigloo-user-lib*)))
      
;*--- Jvm specific options --------------------------------------------*/
      (section "Jvm specific options")
      ;; jvm shell code generation
      (("-jvm-shell" ?shell (help "Shell for JVM scripts (\"sh\", \"msdos\")"))
       (cond
	  ((string=? shell "sh")
	   (set! *jvm-shell* shell))
	  ((string=? shell "msdos")
	   (set! *jvm-shell* shell))
	  (else
	   (error "parse-args" "Illegal `-jvm-shell' argument" shell))))
      ;; jvm bytecode verifier compliance
      (("-jvm-purify" (help "Produce byte code verifier compliant JVM code"))
       (set! *purify* #t))
      (("-no-jvm-purify" (help "Don't care about JVM code verifier (default)"))
       (set! *purify* #f))
      ;; main jvm class
      (("-jvm-mainclass" ?class (help "JVM main class"))
       (set! *jvm-mainclass* class))
      ;; path
      (("-jvm-classpath" ?path (help "JVM application classpath"))
       (set! *jvm-classpath* path))
      (("-jvm-bigloo-classpath" ?p (help "JVM Bigloo rts classpath"))
       (set! *jvm-bigloo-classpath* p))
      (("-jvm-path-separator" ?sep (help "Set the JVM classpath separator"))
       (set! *jvm-path-separator* sep))
      (("-jvm-directory" ?name (help "Directory where to store class files."))
       (set! *jvm-directory* name))
      (("-jvm-catch-errors" (help "Catch internal JVM errors"))
       (set! *jvm-catch* #t))
      (("-no-jvm-catch-errors" (help "Don't catch internal JVM errors"))
       (set! *jvm-catch* #f))
      (("-jvm-jarpath" ?name (help "Set the JVM classpath for the produced jar file"))
       (set! *jvm-jarpath* name))
      ;; misc
      (("-jvm-cinit-module" (help "Enable JVM class constructors to initiliaze bigloo modules"))
       (set! *jvm-cinit-module* #t))
      (("-no-jvm-cinit-module" (help "Disable JVM class constructors to initiliaze bigloo modules"))
       (set! *jvm-cinit-module* #f))
      ;; JVM optimization
      (("-fjvm-inlining" (help "Enable JVM back-end inlining"))
       (set! *optim-jvm-inlining* (+fx 1 *optim-jvm-inlining*)))
      (("-fjvm-constr-inlining" (help "Enable JVM back-end inlining for constructors"))
       (set! *optim-jvm-constructor-inlining*
	     (+fx 1 *optim-jvm-constructor-inlining*)))
      (("-fno-jvm-inlining" (help "Disable JVM back-end inlining"))
       (set! *optim-jvm-inlining* 0))
      (("-fno-jvm-constr-inlining" (help "Disable JVM back-end inlining for constructors"))
       (set! *optim-jvm-constructor-inlining* 0))
      (("-fjvm-peephole" (help "Enable JVM back-end peephole"))
       (set! *optim-jvm-peephole* (+fx 1 *optim-jvm-peephole*))
       (set! *jas-peephole* #t))
      (("-fno-jvm-peephole" (help "Disable JVM back-end peephole"))
       (set! *optim-jvm-peephole* 0)
       (set! *jas-peephole* #f))
      (("-fjvm-branch" (help "Enable JVM back-end branch"))
       (set! *optim-jvm-branch* (+fx 1 *optim-jvm-branch*)))
      (("-fno-jvm-branch" (help "Disable JVM back-end branch"))
       (set! *optim-jvm-branch* 0))
      (("-fjvm-fasteq" (help "EQ? no longer works on integers (use =FX)"))
       (set! *optim-jvm-fasteq* #t))
      (("-fno-jvm-fasteq" (help "Disable JVM back-end fasteq transformation"))
       (set! *optim-jvm-fasteq* #f))
      (("-jvm-env" ?var (help "Make the shell variable visible to GETENV"))
       (set! *jvm-env* (cons var *jvm-env*)))
      (("-jvm-jar" (help "Enable JVM jar files generation"))
       (set! *jvm-jar?* #t))
      (("-no-jvm-jar" (help "Disable JVM jar files generation (default)"))
       (set! *jvm-jar?* #f))
      (("-jvm-java" ?file (help "Use FILE as JVM"))
       (set! *jvm-java* file))
      (("-jvm-opt" ?string (help "JVM invocation option"))
       (set! *jvm-options* (string-append *jvm-options* " " string)))
      
;*--- Dotnet specific options -----------------------------------------*/
      (section ".NET specific options")
      ;; dotnet bytecode verifier compliance
      (("-dotnet-managed" (help "Produce byte code verifier compliant .NET code"))
       (set! *purify* #t))
      (("-dotnet-unmanaged" (help "Don't care about .NET code verifier (default)"))
       (set! *purify* #f))
      (("-dotnet-clr" ?file (help "Use FILE as .NET CLR"))
       (set! *dotnet-clr* file))
      (("-dotnet-clr-style" ?style (help "Use CLR invokation style"))
       (set! *dotnet-clr-style* style))
      (("-dotnet-clr-opt" ?s (help "Set the .NET CLR options"))
       (set! *dotnet-clr-opt* s))
      (("-dotnet-ld" ?file (help "Use FILE as .NET LD"))
       (set! *dotnet-ld* file))
      (("-dotnet-ld-style" ?style (help "Use LD invokation style"))
       (set! *dotnet-ld-style* style))
      (("-dotnet-dll-path" ?name (help "Set the .NET DLL search path"))
       (set! *dotnet-dll-path* name))
      (("-dotnet-external-asm" (help "Enable external assembler (default)"))
       (set! *dotnet-use-external-asm* #t))
      (("-no-dotnet-external-asm" (help "Disable external assembler"))
       (set! *dotnet-use-external-asm* #f))
      (("-ilasm" ?asm (help "Specify external IL assembler"))
       (set! *dotnet-external-asm* asm))
      (("-fdotnet-tailc" (help "Flag tail calls for inside module calls"))
       (set! *dotnet-tail* #t))
      (("-fno-dotnet-tailc" (help "Don't flag tail calls"))
       (set! *dotnet-tail* #f))
      (("-fdotnet-tailc-full" (help "Flag tail calls everywhere"))
       (set! *dotnet-tail* #t)
       (set! *dotnet-tail-across-modules* #t)
       (set! *dotnet-tail-funcall* #t))
      (("-fdotnet-tailc-module" (help "Flag tail calls across modules"))
       (set! *dotnet-tail* #t)
       (set! *dotnet-tail-across-modules* #t))
      (("-fno-dotnet-tailc-module" (help "Don't flag tail calls across modules"))
       (set! *dotnet-tail-across-modules* #f))
      (("-fdotnet-tailc-funcall" (help "Flag tail calls for funcalls"))
       (set! *dotnet-tail* #t)
       (set! *dotnet-tail-funcall* #t))
      (("-fno-dotnet-tailc-funcall" (help "Don't flag tail call for funcalls"))
       (set! *dotnet-tail-funcall* #f))
      (("-dotnet-mono-workaround" (help "Workaround Mono .NET buts (switch)"))
       (set! *dotnet-mono-workaround-switch* #t))
      (("-no-dotnet-mono-workaround" (help "Disable workaround Mono .NET buts (switch)"))
       (set! *dotnet-mono-workaround-switch* #f))
      (("-dotnet-pnet-workaround" (help "Workaround pnet swich bug"))
       (set! *dotnet-pnet-workaround-switch* #t))
      (("-no-dotnet-pnet-workaround" (help "Disable Workaround pnet swich bug"))
       (set! *dotnet-pnet-workaround-switch* #f))
      
;*--- trace options ---------------------------------------------------*/
      (section "Traces")
      ;; traces
      (("-t" (help "-t[2|3|4]" "Generate a trace file (*)"))
       (set! *trace-level* 1))
      (("-t2")
       (set! *trace-level* 2))
      (("-t3")
       (set! *trace-level* 3))
      (("-t4")
       (set! *trace-level* 4))
      (("+t?pass" (help "Force pass to be traced"))
       (set! *additional-traces* (cons pass *additional-traces*)))
      ;; shape
      (("-shape?opt" (help "-shape[mktTalu]" "Some debugging tools (private)"))
       (parse-shape-args opt))
      
;*--- Compiler stages -------------------------------------------------*/
      (section "Compilation stages")
      (("-mco" (help "Stop after .mco production"))
       (set! *module-checksum-object?* #t)
       (set! *pass* 'mco))
      (("-syntax" (help "Stop after the syntax stage (see -hygiene)"))
       (set! *pass* 'syntax))
      (("-expand" (help "Stop after the preprocessing stage"))
       (set! *pass* 'expand))
      (("-ast" (help "Stop after the ast construction stage"))
       (set! *pass* 'ast))
      (("-bdb-spread-obj" (help "Stop after the bdb obj spread stage"))
       (set! *pass* 'bdb-spread-obj))
      (("-trace" (help "Stop after the trace pass"))
       (set! *pass* 'trace))
      (("-callcc" (help "Stop after the callcc pass"))
       (set! *pass* 'callcc))
      (("-bivalue" (help "Stop after the bivaluation stage"))
       (set! *pass* 'bivalue))
      (("-inline" (help "Stop after the inlining stage"))
       (set! *pass* 'inline))
      (("-inline+" (help "Stop after the 2nd inlining stage"))
       (set! *pass* 'inline+))
      (("-fail" (help "Stop after the failure replacement stage"))
       (set! *pass* 'fail))
      (("-fuse" (help "Stop after the fuse stage"))
       (set! *pass* 'fuse)) 
      (("-user" (help "Stop after the user pass"))
       (set! *pass* 'user))
      (("-coerce" (help "Stop after the type coercing stage"))
       (set! *pass* 'coerce))
      (("-effect" (help "Stop after the effect stage"))
       (set! *pass* 'effect))
      (("-effect+" (help "Stop after the 2nd effect stage"))
       (set! *pass* 'effect+))
      (("-reduce" (help "Stop after the reduction opt. stage"))
       (set! *pass* 'reduce))
      (("-reduce+" (help "Stop after the 2nd reduction opt. stage"))
       (set! *pass* 'reduce+))
      (("-reduce-" (help "Stop after the very first reduction stage"))
       (set! *pass* 'reduce-))
      (("-assert" (help "Stop after the assertions stage"))
       (set! *pass* 'assert))
      (("-cfa" (help "Stop after the cfa stage"))
       (set! *pass* 'cfa))
      (("-closure" (help "Stop after the globalization stage"))
       (set! *pass* 'globalize))
      (("-recovery" (help "Stop after the type recovery stage"))
       (set! *pass* 'recovery))
      (("-bdb" (help "Stop after the Bdb code production"))
       (set! *pass* 'bdb))
      (("-cnst" (help "Stop after the constant allocation"))
       (set! *pass* 'cnst))
      (("-integrate" (help "Stop after the integration stage"))
       (set! *pass* 'integrate))
      (("-tailc" (help "Stop after the tailc stage"))
       (set! *pass* 'tailc))
      (("-egen" (help "Produce an include file for effects (requires -saw)"))
       (set! *pass* 'egen))
      (("-hgen" (help "Produce a C header file with class definitions"))
       (set! *pass* 'hgen))
      (("-cgen" (help "Do not C compile and produce a .c file"))
       (set! *pass* 'cgen))
      (("-indent" (help "Produce an indented .c file"))
       (set! *pass* 'cindent))
      (("-jvmas" (help "Produce a JVM .jas file"))
       (set! *target-language* 'jvm)
       (set! *use-private?* #t)
       (set! *pass* 'jvmas))
      (("-il" (help "Produce a .NET .asm file"))
       (set! *target-language* '.net)
       (set! *use-private?* #t)
       (set! *pass* 'il))
      
;*--- Constant initialization -----------------------------------------*/
      (section "Constant initialization")
      (("-init-lib" (help "-init-[lib|read|intern]" "Constants initialization mode"))
       (set! *init-mode* 'lib))
      (("-init-read")
       (set! *init-mode* 'read))
      (("-init-intern")
       (set! *init-mode* 'intern))
      
;*--- Private options -------------------------------------------------*/
      (section "Bootstrap and setup")
      ;; library construction
      (("-mklib" (help "Compile a library module"))
       (set! *lib-mode* #t)
       (set! *init-mode* 'lib))
      ;; additional library construction
      (("-mkaddlib" (help "Compile an additional library module"))
       (set! *init-mode* 'lib))
      ;; heap compilation
      (("-mkheap" (help "Build an heap file"))
       (set! *pass* 'make-heap))
      ;; heap compilation
      (("-mkaddheap" (help "Build an additional heap file"))
       (set! *pass* 'make-add-heap))
      ;; distribution compilation
      (("-mkdistrib" (help "Compile a main file for a distribution"))
       (set! *pass* 'distrib))
      ;; Bigloo license
      (("--license" (help "Display the Bigloo license and exit"))
       (print (bigloo-license))
       (compiler-exit 0))
      (("-LICENSE" (help "Add the license to the generated C files"))
       (set! *bigloo-licensing?* #t))
      ;; heap name
      (("-heap" ?name (help "Specify an heap file (or #f to not load heap)"))
       (set! *heap-name* name))
      ;; heap dumping
      (("-dump-heap" (help "Dump the contains of a heap"))
       (set! *pass* 'dump-heap))
      ;; heap dumping
      (("-dheap" ?name (help "Dump the contains of a heap"))
       (set! *heap-dump-names* (cons name *heap-dump-names*)))
      ;; additinal heap name
      (("-addheap" ?name (help "Specify an additional heap file"))
       (set! *additional-heap-name* name))
      ;; the reader option
      (("-fread-internal" (help "Read source from binary interned file"))
       (set! *reader* 'intern))
      (("-fread-plain" (help "Read source from plain text file"))
       (set! *reader* 'plain))
      ;; target
      (("-target" ?lang (help "DON'T USE, (see -native, -jvm, -dotnet)"))
       (case (string->symbol lang)
	  ((c)
	   'nothing)
	  ((jvm)
	   (set! *target-language* 'jvm))
	  ((.net)
	   (set! *target-language* '.net))
	  ((jsm)
	   (set! *target-language* 'jsm))
	  (else
	   (error "parse-args" "Unknown target" lang))))
      
;*--- Unknown arguments -----------------------------------------------*/
      (("-?dummy")
       (set! *rest-args* (cons (string-append "-" dummy) *rest-args*)))
      
;*--- The source file -------------------------------------------------*/
      (else
       (let ((suf (suffix else)))
	  (cond 
	     ((and (string? suf)
		   (=fx (string-length suf) 0)
		   (null? *src-files*))
	      (set! *src-files* (list else)))
	     ((member suf *src-suffix*)
	      (set! *src-files* (cons else *src-files*)))
	     (*interpreter*
	      'ignore)
	     ((member suf *c-suffix*)
	      (set! *c-files*  (cons else *c-files*)))
	     ((member suf *obj-suffix*)
	      (set! *o-files*  (cons else *o-files*)))
	     ((and (eq? *target-language* '.net) (member suf *csharp-suffix*))
	      (set! *o-files* (cons else *o-files*)))
	     (else
	      (set! *rest-args* (cons else *rest-args*))))))))

;*---------------------------------------------------------------------*/
;*    parse-shape-args ...                                             */
;*---------------------------------------------------------------------*/
(define (parse-shape-args string)
   (let ((len (string-length string)))
      (if (=fx len 0)
	  (begin
	     (set! *module-shape?*   #t)
	     (set! *key-shape?*      #t)
	     (set! *type-shape?*     #t)
	     (set! *typename-shape?* #t)
	     (set! *access-shape?*   #t)
	     (set! *location-shape?* #t)
	     (set! *user-shape?*     #t))
	  (let liip ((i 0))
	     (if (=fx i len)
		 'done
		 (begin
		    (case (string-ref string i)
		       ((#\m)
			(set! *module-shape?* #t))
		       ((#\k)
			(set! *key-shape?* #t))
		       ((#\t)
			(set! *type-shape?* #t))
		       ((#\T)
			(set! *type-shape?* #t)
			(set! *typename-shape?* #t))
		       ((#\a)
			(set! *access-shape?* #t))
		       ((#\l)
			(set! *location-shape?* #t))
		       ((#\u)
			(set! *user-shape?* #t))
		       (else
			(error "parse-arg" "Illegal -shape option" string)))
		    (liip (+fx i 1))))))))

;*---------------------------------------------------------------------*/
;*    parse-unsafe-args ...                                            */
;*    -------------------------------------------------------------    */
;*    The variable @label unsafe-rgc@ is defined in the library. See   */
;*    the __rgc module (@ref ../../runtime/Rgc/rgc.scm:unsafe-rgc@)    */
;*---------------------------------------------------------------------*/
(define (parse-unsafe-args string)
   (let ((len (string-length string)))
      (if (=fx len 0)
	  (begin
	     (set! *unsafe-rgc*     #t)
	     (set! *unsafe-library* #t)
	     (set! *unsafe-arity*   #t)
	     (set! *unsafe-type*    #t)
	     (set! *unsafe-struct*  #t)
	     (set! *unsafe-range*   #t)
	     (set! *unsafe-version* #t))
	  (let liip ((i 0))
	     (if (=fx i len)
		 'done
		 (begin
		    (case (string-ref string i)
		       ((#\r)
			(set! *unsafe-range* #t))
		       ((#\a)
			(set! *unsafe-arity* #t))
		       ((#\t)
			(set! *unsafe-rgc* #t)
			(set! *unsafe-type* #t))
		       ((#\s)
			(set! *unsafe-struct* #t))
		       ((#\v)
			(set! *unsafe-version* #t))
		       ((#\l)
			(set! *unsafe-library* #t))
		       (else
			(error "parse-arg" "Illegal -unsafe option" string)))
		    (liip (+fx i 1))))))))

;*---------------------------------------------------------------------*/
;*    parse-debug-args ...                                             */
;*---------------------------------------------------------------------*/
(define (parse-debug-args string)
   (define (-g2!)
      (bigloo-compiler-debug-set! 2)
      (set! *compiler-debug* 2)
      (set! *jas-peephole* #f))
   (define (-g3!)
      ;; -g3 and -call/cc are incompatible
      (unless *call/cc?*
	 (bigloo-compiler-debug-set! 3)
	 (set! *compiler-debug* 3)))
   (define (-g4!)
      (bigloo-compiler-debug-set! 4)
      (set! *compiler-debug* 4))
   (bigloo-compiler-debug-set! 1)
   (set! *compiler-debug* 1)
   (set! *purify* #t)
   (set! *jvm-debug* #t)
   (set! *jas-warning* #f)
   (when (eq? *error-localization-opt* #unspecified)
      (set! *error-localization* #t))
   (if (> (string-length string) 0)
       (case (string-ref string 0)
	  ((#\2)
	   (-g2!))
	  ((#\3)
	   (-g3!))
	  ((#\4)
	   (-g4!))
	  (else
	   (error "parse-arg" "Illegal -g option" string)))))

;*---------------------------------------------------------------------*/
;*    parse-bdb-args ...                                               */
;*---------------------------------------------------------------------*/
(define (parse-bdb-args string)
   (define (-gbdb2!)
      (set! *bdb-debug* 2)
      (set! *compiler-debug* 1)
      (set! *user-heap-size* 1)
      (set! *heap-debug* 1)
      (set! *jas-warning* #f))
   (define (-gbdb3!)
      (set! *bdb-debug* 3))
   (if bgl-configure-bdb-available?
       (begin 
	  (set! *additional-heap-names*
		(cons "bdb.heap" *additional-heap-names*))
	  (set! *user-heap-size* 1)
	  (set! *bdb-debug* 1)
	  (set! *purify* #t)
	  (set! *jvm-debug* #t)
	  (set! *jas-warning* #f)
	  (set! *c-debug-lines-info*  #t)
	  (if (> (string-length string) 0)
	      (case (string-ref string 0)
		 ((#\2)
		  (-gbdb2!))
		 ((#\3)
		  (-gbdb2!)
		  (-gbdb3!))
		 (else
		  (error "parse-arg" "Illegal -O option" string)))))
       (warning "-gbdb"
		"Bdb not available (see Bigloo configuration option)"
		" ignoring option")))
   
;*---------------------------------------------------------------------*/
;*    parse-optim-args ...                                             */
;*---------------------------------------------------------------------*/
(define (parse-optim-args string)
   (define (-O2!)
      (if (not *c-debug*)
	  (set! *cc-options* (string-append *cc-options* " " *cflags-optim*)))
      (set! *optim-cfa-arithmetic?* #t)
      (set! *optim-jvm-inlining* 2)
      (set! *optim-jvm-branch* 3)
      (set! *optim-jvm-fasteq* #t)
      '(set! *rgc-optim* #t))
   (define (-O3!)
      (-O2!)
      (set! *optim-jvm-inlining* 3)
      (set! *optim-jvm-branch* 5)
      (set! *optim-reduce-beta?* #t)
      (if (not (boolean? *optim-unroll-loop?*))
	  (set! *optim-unroll-loop?* #t)))
   (set! *optim* 1)
   (set! *optim-jvm-inlining* 1)
   (set! *optim-jvm-branch* 1)
   (set! *optim-O-macro?* #t)
   (set! *optim-dataflow?* #t)
   (if (> (string-length string) 0)
       (case (string-ref string 0)
	  ((#\2)
	   (-O2!)
	   (set! *optim* 2))
	  ((#\3)
	   (-O3!)
	   (set! *optim* 3))
	  ((#\4 #\5 #\6)
	   (-O3!)
	   (set! *optim* (-fx (char->integer (string-ref string 0))
			      (char->integer #\0))))
	  (else
	   (error "parse-arg" "Illegal -O option" string)))
       (if (not *c-debug*)
	   (set! *cc-options* (string-append *cc-options* " -O")))))

;*---------------------------------------------------------------------*/
;*    query ...                                                        */
;*---------------------------------------------------------------------*/
(define (query)
   (version)
   (newline)
   (print "setups:")
   (newline)
   (print "*cc*                   : " *cc*)
   (print "*cc-options*           : " *cc-options*)
   (print "*ld-options*           : " *ld-options*)
   (print "*ld-post-options*      : " *ld-post-options*)
   (print "*bigloo-vlib*          : " *bigloo-vlib*)
   (print "*bigloo-lib* (*)       : " (lib->string *bigloo-vlib*))
   (print "*bigloo-user-lib*      : " *bigloo-user-lib*)
   (print "*default-lib-dir*      : " *default-lib-dir*)
   (print "*lib-dir*              : " *lib-dir*)
   (print "*include-foreign*      : " *include-foreign*)
   (print "*heap-name*            : " *heap-name*)
   (print "*target-language*      : " *target-language*)
   (print "*jvm-shell*            : " *jvm-shell*)
   (print "*dot-shell*            : " *dotnet-shell*)
   (newline)
   (print "Too see all variables enter the interpreter")
   (print "or use the -help2 option.")
   (newline)
   (print "----")
   (print " (*) read-only variables")
   (compiler-exit 0))
