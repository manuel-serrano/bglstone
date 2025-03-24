;*=====================================================================*/
;*    .../project/bglstone/src/bigloo/bigloo/Module/library.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Jul  9 16:05:09 1996                          */
;*    Last change :  Thu Mar  6 16:33:08 2025 (serrano)                */
;*    Copyright   :  1996-2025 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    Library finalizer                                                */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module module_library
   (include "Ast/node.sch")
   (include "Ast/unit.sch")
   (import  type_type
	    ast_var
	    ast_env
	    module_module
	    module_impuse
	    engine_param
	    bdb_setting)
   (export  (library-finalizer)
	    (with-library-module! module::symbol)))

;*---------------------------------------------------------------------*/
;*    library-finalizer ...                                            */
;*---------------------------------------------------------------------*/
(define (library-finalizer)
   ;; we set the key
   (set! *key* (gensym))
   ;; we mark the already imported modules
   (for-each (lambda (module) (putprop! module *key* #t))
	     (get-imported-modules))
   ;; we also mark the dummy `foreign' module
   (putprop! 'foreign *key* #t)
   ;; and, of course, the current module
   (putprop! *module* *key* #t)
   ;; first, we collect all the needed library modules
   (for-each-global! (lambda (global)
			(with-access::global global (occurrence module
								library? value)
			   (if (and (>fx occurrence 0)
				    library?
				    (not (or (cfun? value) (cvar? value))))
			       (need-library-module! module)))))
   ;; when compiling for bdb we must initialize the bdb module
   (if (>fx *bdb-debug* 0)
       (for-each need-library-module! *bdb-module*))
   (if (and *jvm-debug* (memq *target-language* '(jvm .net)) (not *lib-mode*))
       (for-each need-library-module! *jvm-debug-module*))
   ;; we mark all the WITH modules (i.e. modules mentionned in a with module
   ;; clause or in a -with command line option)
   (for-each need-library-module! *with-library-modules*)
   ;; then we declare a special unit
   (let ((modules *needed-modules*))
      (if (null? modules)
	  '()
	  (let loop ((modules modules)
		     (init-call* '(#unspecified)))
	     (if (null? modules)
		 (let ((body (if (>fx *debug-module* 0)
				 `((begin
				      (pragma::int
				       ,(string-append
					 "puts(\"   Module init (library): "
					 (symbol->string *module*)
					 "\")"))
				      ,@init-call*))
				 init-call*)))
		    (unit 'library-modules 2 body #t))
		 (let* ((id (car modules))
			(init-fun-id (module-initialization-id id)))
		    (loop (cdr modules)
			  (cons `((@ ,init-fun-id ,id)
				  0
				  ;; 0 means here not to perform version
				  ;; checking about library
				  ,(symbol->string *module*))
				init-call*))))))))

;*---------------------------------------------------------------------*/
;*    *needed-modules* ...                                             */
;*---------------------------------------------------------------------*/
(define *needed-modules* '())

;*---------------------------------------------------------------------*/
;*    *key* ...                                                        */
;*---------------------------------------------------------------------*/
(define *key* #unspecified)

;*---------------------------------------------------------------------*/
;*    need-library-module! ...                                         */
;*---------------------------------------------------------------------*/
(define (need-library-module! module::symbol)
   (if (not (getprop module *key*))
       (begin
	  (putprop! module *key* #t)
	  (set! *needed-modules* (cons module *needed-modules*)))))

;*---------------------------------------------------------------------*/
;*    *with-key* ...                                                   */
;*---------------------------------------------------------------------*/
(define *with-key* (gensym 'with-key))

;*---------------------------------------------------------------------*/
;*    *with-library-modules* ...                                       */
;*---------------------------------------------------------------------*/
(define *with-library-modules* '())

;*---------------------------------------------------------------------*/
;*    with-library-module! ...                                         */
;*---------------------------------------------------------------------*/
(define (with-library-module! module::symbol)
   (if (not (getprop module *with-key*))
       (begin
	  (putprop! module *with-key* #t)
	  (set! *with-library-modules* (cons module *with-library-modules*)))))

