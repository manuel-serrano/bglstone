;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/BackEnd/c-init.scm          */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Mar 16 17:59:38 1995                          */
;*    Last change :  Wed Jan 19 11:31:25 2005 (serrano)                */
;*    Copyright   :  1995-2005 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    We produce a Bigloo's `main' function.                           */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module backend_c_init
   (import  engine_param
	    module_module
	    tools_shape
	    type_type
	    type_cache
	    object_class
	    object_slots
	    ast_var
	    ast_node
	    ast_env
	    ast_sexp
	    ast_local
	    ast_glo-def
	    ast_unit
	    ast_occur
	    coerce_coerce
	    backend_c_prototype
	    backend_cplib)
   (export  (get-module-init)))

;*---------------------------------------------------------------------*/
;*    *module-init* ...                                                */
;*---------------------------------------------------------------------*/
(define *module-init* #unspecified)

;*---------------------------------------------------------------------*/
;*    get-module-init ...                                              */
;*---------------------------------------------------------------------*/
(define (get-module-init)
   (if (eq? *module-init* #unspecified)
       (set! *module-init* (make-module-init)))
   *module-init*)

;*---------------------------------------------------------------------*/
;*    make-module-init ...                                             */
;*---------------------------------------------------------------------*/
(define (make-module-init)
   (let* ((req  (def-global-svar! 'require-initialization::obj
		   *module*
		   'module-initalization
		   'now))
	  (ubody `(if require-initialization
		     (begin
			(set! require-initialization #f)
			,(if (>=fx *heap-debug* 1)
			     ;; the C function bdb_set_lock is defined
			     ;; the the bdb library:
			     ;; See:
			     ;; @ref ../../bdb/blib/gc_dump.c:bdb_set_lock@
			     '(pragma "bdb_set_lock()")
			     #unspecified)
			,@(if (>fx *debug-module* 0)
			      `((pragma
				 ,(string-append "puts(\"*** Init module: "
						 (symbol->string *module*)
						 "\")")))
			      '())
			,@(unit-init-calls)
			#unspecified)
		     #unspecified))
	  (body (if *unsafe-version*
		    ubody
		    `(if (=fx (bit-and checksum ,*module-checksum*) checksum)
			 ,ubody
			 ,(if (eq? *target-language* 'c)
			      `(let ((s::string
				      (pragma::string
				       ,(format "~s" (symbol->string *module*)))))
				  (module-init-error s from))
			      `(module-init-error ,(symbol->string *module*)
						  from)))))
	  (cvar (make-local-svar 'checksum *long*))
	  (nvar (make-local-svar 'from *string*))
	  (node (let ((_           *_*)
		      (unsafe-type *unsafe-type*))
		   (set! *_* *obj*)
		   (set! *unsafe-type* #t)
		   (let ((node (coerce! (sexp->node body
						    (list cvar nvar)
						    '()
						    'value)
					req
					*obj*)))
		      (set! *_* _)
		      (set! *unsafe-type* unsafe-type)
		      node)))
	  (init (def-global-sfun-no-warning!
		   (module-initialization-id *module*)
		   '(checksum from)
		   (list cvar nvar)
		   *module*
		   'sfun
		   'module-initialization
		   'now
		   node)))
      (set-variable-name! req)
      (global-import-set! init 'export)
      (global-type-set! init *obj*)
      (occur-node-in! node init)
      init))

