;*=====================================================================*/
;*    /tmp/bglstone/bglstone/src/cgc/bigloo/Init/parse-args.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jun 29 11:56:20 1995                          */
;*    Last change :  Tue Mar  1 08:03:50 2005 (serrano)                */
;*    -------------------------------------------------------------    */
;*    Command-line parsing                                             */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module init_parse-args
   (export  (parse-args args::pair))
   (import  engine_param
	    write_version))

;*---------------------------------------------------------------------*/
;*    parse-args ...                                                   */
;*---------------------------------------------------------------------*/
(define (parse-args cmd-args) 
   (args-parse (cdr cmd-args)
;*--- source file naming ----------------------------------------------*/
      (("-" ?name (synopsis "A source file name."))
       (set! *src* (cons name *src*)))
;*--- help message ----------------------------------------------------*/
      (("?") (help args-parse-usage))
      (("-help") (help args-parse-usage))
;*--- version number --------------------------------------------------*/
      (("-version" (synopsis "The current inline release."))
       (exit 0))
;*--- -q --------------------------------------------------------------*/
      (("-q" (synopsis "Do not load rc file."))
       'nothing)
;*--- output naming ---------------------------------------------------*/
      (("-o" ?name (synopsis "Name the output file <name>."))
       (set! *dest* name))
;*--- --to-stdout -----------------------------------------------------*/
      (("--to-stdout" (synopsis "Write on current output channel."))
       (set! *verbose* -1)
       (set! *dest* '--to-stdout))
;*--- verbosity -------------------------------------------------------*/
      (("-s" (synopsis "Be silent."))
       (set! *verbose* -1))
      (("-v" (synopsis "-v[23]" "Be verbose."))
       (set! *verbose* 1))
      (("-v2")
       (set! *verbose* 2))
      (("-v3")
       (set! *verbose* 3))
      (("-v4")
       (set! *verbose* 4))
      (("-w" (synopsis "Inhibit all warning messages."))
       (bigloo-warning-set! 0))
      (("-Wall" (synopsis "Warn about all possible errors."))
       (bigloo-warning-set! 2))
;*--- optim -----------------------------------------------------------*/
      (("-O" (synopsis "Optimizing mode"))
       (set! *optim* 1))
;*--- target specification --------------------------------------------*/
      (("-r3000" (synopsis #"Specify target architecture\n"
			   #"\t\tpossible architecture:\n"
			   #"\t\t\tmisp-r3000 [default]"))
       (set! *target* 'mips-r3000))
;*--- stages stopper --------------------------------------------------*/
      (("-symbol" (synopsis "Stop after the symbol resolution"))
       (set! *pass-stop* 'symbol))
      (("-types" (synopsis "Stop after the type checking"))
       (set! *pass-stop* 'types))
      (("-escape" (synopsis "Stop after the escape analysis"))
       (set! *pass-stop* 'escape))
      (("-ast" (synopsis "Stop after the ast construction"))
       (set! *pass-stop* 'ast))
      (("-ir" (synopsis "Stop after the ast->ir translation"))
       (set! *pass-stop* 'ir))
      (("-canonical" (synopsis "Stop after the canonicalize transformation"))
       (set! *pass-stop* 'canonicalize))
      (("-basic-blocks" (synopsis "Stop after the basic blocks splitting"))
       (set! *pass-stop* 'basic-blocks))
      (("-iselect" (synopsis "Stop after the instructions selection"))
       (set! *pass-stop* 'iselect))
      (("-trace" (synopsis "Stop after the trace computation"))
       (set! *pass-stop* 'trace))
      (("-branch-tensioning" (synopsis "Stop after the branch tensioning optimization"))
       (set! *pass-stop* 'branch-tensioning))
      (("-nonop" (synopsis "Stop after the nonop optimization"))
       (set! *pass-stop* 'nonop))
      (("-liveness" (synopsis "Stop after the liveness analysis"))
       (set! *pass-stop* 'liveness))
      (("-regalloc" (synopsis "Stop after the register allocation"))
       (set! *pass-stop* 'regalloc))
      (("-arecord" (synopsis "Stop after the activation record allocation"))
       (set! *pass-stop* 'arecord))
;*--- The runtime -----------------------------------------------------*/
      (("-rts" (synopsis "Include runtime system code"))
       (set! *rts* #t))
;*--- inner functions -------------------------------------------------*/
      (("-finner" (synopsis "Enable inner functions"))
       (set! *inner-functions* #t))
      (("-fno-inner" (synopsis "Disable inner functions"))
       (set! *inner-functions* #f))
;*--- move-mem-size ---------------------------------------------------*/
      (("-fmove-mem-size" (synopsis "Display the word size for ir:move-mem"))
       (set! *display-move-mem-length?* #t))
      (("-fno-move-mem-size" (synopsis "Don't display the word size for ir:move-mem"))
       (set! *display-move-mem-length?* #f))
;*--- text-only -------------------------------------------------------*/
      (("-ftext-only" (synopsis "Display the only text segment"))
       (set! *text-only?* #t))
      (("-fno-text-only" (synopsis "Display the all segments"))
       (set! *text-only?* #f))
;*--- register allocation specification -------------------------------*/
      (("-Ragaig" (synopsis "The `as good as it gets' register allocator"))
       (set! *register-allocator* 'agaig))
      (("-Ragaigl" (synopsis "The `as good as it gets but live' register allocator"))
       (set! *register-allocator* 'agaig/liveness))
      (("-Rpush" (synopsis "The `push' register allocator"))
       (set! *register-allocator* 'push))
      (("-Rminimal-push" (synopsis "The `minimal push' register allocator"))
       (set! *register-allocator* 'minimal-push))
      (("-Rcallee-push" (synopsis "The `callee push' register allocator"))
       (set! *register-allocator* 'callee-push))
;*--- -? --------------------------------------------------------------*/
      (("-?dummy")
       (help args-parse-usage))
;*--- regular sources -------------------------------------------------*/
      (else
       (if (eq? *src* #unspecified)
	   (set! *src* else)
	   (set! *rest-args* (cons else *rest-args*)))))
   (if (pair? *src*)
       (set! *src* (reverse! *src*)))
   #t)
	
;*---------------------------------------------------------------------*/
;*    help ...                                                         */
;*---------------------------------------------------------------------*/
(define (help usage)
   (version)
   (print "usage: cgc [options]* [src_name]")
   (newline)
   (usage #f)
   (newline)
   (print "Shell Variables:")
   (print "   - TMPDIR    --  Tmp directory (default \"/tmp\").")
   (newline)
   (print "Runtime Command file:")
   (print "   - ~/.cgc")
   (exit 0))


   
