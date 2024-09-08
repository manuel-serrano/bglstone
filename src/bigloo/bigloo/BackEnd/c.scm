;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/BackEnd/c.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Aug  4 14:10:06 2003                          */
;*    Last change :  Thu Feb  3 11:15:58 2005 (serrano)                */
;*    Copyright   :  2003-05 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The C back-end                                                   */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module backend_c
   (include "Engine/pass.sch"
	    "Ast/unit.sch"
	    "Tools/trace.sch")
   (import  tools_shape
	    tools_error
	    engine_param
	    engine_configure
	    module_module
	    module_library
	    type_type
	    ast_var
	    ast_node
	    ast_occur
	    ast_build
	    object_class
	    bdb_emit
	    prof_emit
	    backend_c_emit
	    backend_c_prototype
	    backend_c_main
	    backend_c_init
	    cgen_cop
	    cgen_cgen
	    saw_c_compile
	    backend_backend
	    cc_indent
	    cc_cc
	    cc_ld)
   (export  (build-sawc-backend)
	    (build-cgen-backend)
	    (cc-compiler ::bstring ::obj)))

;*---------------------------------------------------------------------*/
;*    build-sawc-backend ...                                           */
;*---------------------------------------------------------------------*/
(define (build-sawc-backend)
   (instantiate::sawc
      (language 'c)))

;*---------------------------------------------------------------------*/
;*    build-cgen-backend ...                                           */
;*---------------------------------------------------------------------*/
(define (build-cgen-backend)
   (instantiate::cgen
      (language 'c)))

;*---------------------------------------------------------------------*/
;*    backend-compile ...                                              */
;*---------------------------------------------------------------------*/
(define-method (backend-compile me::cvm)
   (let ((c-prefix (profile cgen (c-walk me))))
      (stop-on-pass 'cgen (lambda () 'done))
      (stop-on-pass 'distrib (lambda () 'done))
      (if (string? c-prefix)
	  (begin
	     (if (or (eq? *pass* 'cindent) *c-debug*)
		 (indent c-prefix))
	     (stop-on-pass 'cindent (lambda () 'done))))
      c-prefix))

;*---------------------------------------------------------------------*/
;*    backend-link ...                                                 */
;*---------------------------------------------------------------------*/
(define-method (backend-link me::cvm result)
   (if (string? result)
       (cc-compiler result #f)))

;*---------------------------------------------------------------------*/
;*    c-walk ...                                                       */
;*---------------------------------------------------------------------*/
(define (c-walk me::cvm)
   (pass-prelude (if *saw*
		     "C generation (saw)"
		     "C generation (cgen)")
		 (lambda () (start-emission! ".c")))
   
   ;; a very little comment 
   (emit-header)
   
   ;; emit the GC selection
   (emit-garbage-collector-selection)
   
   ;; if we are in debugging mode, we generate a macro
   (if (or (>fx *compiler-debug* 0) *c-debug*)
       (emit-debug-activation))
   
   ;; the include (both Bigloo's and user's ones)
   (emit-include)
   
   ;; we emit the generated type for the classes
   (emit-class-types *c-port*)
   
   ;; we declare prototypes
   ;; first, we print the prototype of variables
   (emit-prototypes)
   
   ;; then we emit the constants values
   (emit-cnsts)
   
   (let ((globals (cvm-functions me)))
      
      ;; when compiling in bdb mode we have to emit the identifier
      ;; translation table.
      (if (>fx *bdb-debug* 0)
	  (emit-bdb-info globals *c-port*))
      
      ;; when compiling for profile we emit identifier translation table
      (if (>fx *profile-mode* 0)
	  (emit-prof-info globals *c-port*))
      
      ;; we print the C main...
      (if (and (or *main* (memq *pass* '(ld distrib)))
	       (not (eq? *main* 'imported)))
	  (emit-main))
      
      ;; we emit the dynamic loading init point
      (let ((mod-init (get-module-init)))
	 (if (and bgl-configure-dlopen? *dlopen-init* (global? mod-init))
	     (emit-dlopen-init mod-init)))
      
      ;; we now emit the code for all the Scheme functions
      (cvm-compile-functions me)
      
      (stop-emission!)))

;*---------------------------------------------------------------------*/
;*    cc-compiler ...                                                  */
;*---------------------------------------------------------------------*/
(define (cc-compiler c-prefix o-prefix)
   ;; we invoke now the C compiler
   (cc c-prefix o-prefix (not (eq? *pass* 'cc)))
   (stop-on-pass 'cc (lambda () 'done))
   ;; and the linker
   (ld (if (string? o-prefix) o-prefix c-prefix) #f))
