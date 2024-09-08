;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Heap/restore.scm            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Dec 26 10:53:23 1994                          */
;*    Last change :  Wed Jan 19 17:06:18 2005 (serrano)                */
;*    Copyright   :  1994-2005 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    We restore an heap                                               */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module heap_restore
   (include "Engine/pass.sch")
   (export  (restore-heap)
	    (restore-additional-heaps)
	    (dump-additional-heaps)
	    (heap-module-list . args))
   (import  engine_param
	    engine_engine
	    init_main
	    tools_error
	    ast_env
	    type_type
	    type_env
	    ast_var
	    read_jvm
	    tools_shape))

;*---------------------------------------------------------------------*/
;*    restore-heap ...                                                 */
;*---------------------------------------------------------------------*/
(define (restore-heap)
   (when (string? *heap-name*)
      (pass-prelude "Heap")
      (let ((fname (find-file/path *heap-name* *lib-dir*)))
	 (if (string? fname)
	     (let ((port (open-input-binary-file fname)))
		(if (not (binary-port? port))
		    (let ((m (format "Cannot open heap file ~s" fname)))
		       (error 'restore-heap m *lib-dir*)
		       (compiler-exit 5))
		    (begin
		       (verbose 2 "      [reading " fname "]" #\Newline)
		       (unwind-protect
			  (let* ((Envs (input-obj port))
				 (_ (if (not (and (vector? Envs)
						  (=fx (vector-length Envs) 3)))
					(error *heap-name*
					       "Corrupted heap"
					       Envs)))
				 (target (vector-ref Envs 0))
				 (Genv (vector-ref Envs 1))
				 (Tenv (vector-ref Envs 2)))
			     ;; check the target languages
			     (if (and (not (eq? target *target-language*))
				      (not (and (eq? target 'jvm)
						(or (eq? *target-language* '.net)
						    (eq? *target-language* 'jsm)))))
				 (error *heap-name*
					"Target language mismatch"
					(cons target *target-language*)))
			     (set-genv! Genv)
			     ;; for class handling see the note set
			     ;; for add-Tenv!:
			     ;; @ref restore.scm:heap class handling@
			     (set-tenv! Tenv)
			     (if (and (not *call/cc?*)
				      (not (memq *target-language*
						 '(jvm .net jsm))))
				 (unbind-call/cc!))
			     ;; in jvm mode, we have to propagate
			     ;; the package/module association
			     (if (memq *target-language* '(jvm .net))
				 (for-each-global!
				  (lambda (new)
				     (add-qualified-type!
				      (global-module new)
				      (global-jvm-type-name new)
				      (shape new)))))
			     ;; we add all the heap modules
			     (for-each-global!
			      (lambda (new)
				 (heap-module-list (global-module new))))
			     ;; we check if we have to dump what we have
			     ;; restored
			     (if (member *heap-name* *heap-dump-names*)
				 (dump-heap *heap-name* Genv Tenv))
			     #t)
			  (close-binary-port port)))))
	     (let ((m (format "Cannot open heap file ~s" *heap-name*)))
		(error 'restore-heap m *lib-dir*)
		(compiler-exit 5))))))

;*---------------------------------------------------------------------*/
;*    unbind-call/cc! ...                                              */
;*---------------------------------------------------------------------*/
(define (unbind-call/cc!)
   (if (find-global/module 'call/cc '__r4_control_features_6_9)
       (unbind-global! 'call/cc '__r4_control_features_6_9))
   (if (find-global/module 'call-with-current-continuation
			   '__r4_control_features_6_9)
       (unbind-global! 'call-with-current-continuation
		       '__r4_control_features_6_9)))

;*---------------------------------------------------------------------*/
;*    restore-additional-heaps ...                                     */
;*---------------------------------------------------------------------*/
(define (restore-additional-heaps)
   (if (pair? *additional-heap-names*)
       (begin
	  (pass-prelude "Library")
	  (for-each restore-additional-heap
		    (reverse *additional-heap-names*)))))

;*---------------------------------------------------------------------*/
;*    dump-additional-heaps ...                                        */
;*---------------------------------------------------------------------*/
(define (dump-additional-heaps)
   (for-each (lambda (heap)
		(if (member heap *heap-dump-names*)
		    (let ((fname (find-file/path heap *lib-dir*)))
		       (let ((port (open-input-binary-file fname)))
			  (let* ((Envs (input-obj port))
				 (Genv (vector-ref Envs 1))
				 (Tenv (vector-ref Envs 2)))
			     (dump-heap heap Genv Tenv))))))
	     *additional-heap-names*))

;*---------------------------------------------------------------------*/
;*    restore-additional-heap ...                                      */
;*---------------------------------------------------------------------*/
(define (restore-additional-heap heap)
   (let ((fname (find-file/path heap *lib-dir*)))
      (if (string? fname)
	  (let ((port (open-input-binary-file fname)))
	     (if (not (binary-port? port))
		 (let ((m (format "Cannot open heap file ~s" fname)))
		    (error "restore-additional-heap" m *lib-dir*)
		    (compiler-exit 6))
		 (begin
		    (verbose 2 "      [reading " fname "]" #\Newline)
		    (unwind-protect
		       (let* ((Envs (input-obj port))
			      (_ (if (not (and (vector Envs)
					       (=fx (vector-length Envs) 4)))
				     (error heap "Corrupted heap" Envs)))
			      (target (vector-ref Envs 0))
			      (Genv (vector-ref Envs 1))
			      (Tenv (vector-ref Envs 2))
			      (includes (vector-ref Envs 3)))
			  ;; check the target languages
			  (if (and (not (eq? target *target-language*))
				   (not (and (eq? target 'jvm)
					     (eq? *target-language* '.net))))
			      (error heap
				     "Target language mismatch"
				     (cons target *target-language*)))
			  ;; @label heap class handling@
			  ;; The function add-Tenv! manages the importation
			  ;; of class definitions. That is, if the additional
			  ;; heap contains class definition, add-Tenv! will
			  ;; create the accessors for that classes. Note
			  ;; that set-Tenv! *doesn't* do the same job, it
			  ;; supposes that the class doesn't contains classes
			  [assert (Tenv) (hashtable? Tenv)]
			  [assert (Genv) (hashtable? Genv)]
			  (add-tenv! Tenv)
			  (add-genv! Genv)
			  ;; we add all the heap modules
			  (hashtable-for-each
			   Genv
			   (lambda (k bucket)
			      (for-each (lambda (new)
					   (heap-module-list
					    (global-module new)))
					(cdr bucket))))
			  ;; we store the list of includes
			  (set! *additional-include-foreign*
				(append *additional-include-foreign*
					includes))
			  #t)
		       (close-binary-port port)))))
	  (let ((m (format "Cannot open heap file ~s" heap)))
	     (error 'restore-additional-heap m *lib-dir*)
	     (compiler-exit 6)))))

;*---------------------------------------------------------------------*/
;*    dump-heap ...                                                    */
;*    -------------------------------------------------------------    */
;*    Dump (for debug purposes) a Bigloo heap.                         */
;*---------------------------------------------------------------------*/
(define (dump-heap heap Genv Tenv)
   (with-output-to-port (current-error-port)
      (lambda ()
	 (print "(heap heap")
	 (print " (variables")
	 (hashtable-for-each
	  Genv
	  (lambda (k bucket)
	     (for-each (lambda (new)
			  (let* ((module (global-module new))
				 (id (global-id new))
				 (qt (module->qualified-type module)))
			     (print "   " `(,(cond
						((sfun? (global-value new))
						 'function)
						((cfun? (global-value new))
						 'native)
						(else
						 'variable))
					    ,(shape new)
					    (id ,id)
					    (module ,module)
					    (name ,(global-name new))
					    (qualified-type ,qt)))))
		       (cdr bucket))))
	 (print " )\n")
	 (print " (types")
	 (hashtable-for-each Tenv
			     (lambda (k new)
				(let* ((id  (type-id new))
				       (name (type-name new)))
				   (print "   " `(type ,id (name ,name))))))
	 (print " ))\n"))))

;*---------------------------------------------------------------------*/
;*    *heap-module-list* ...                                           */
;*    -------------------------------------------------------------    */
;*    The list of modules imported in a heap (i.e. a Bigloo            */
;*    library).					                       */
;*---------------------------------------------------------------------*/
(define *heap-module-list* '())

;*---------------------------------------------------------------------*/
;*    *heap-mark* ...                                                  */
;*---------------------------------------------------------------------*/
(define *heap-mark* 'heap-mark)

;*---------------------------------------------------------------------*/
;*    heap-module-list ...                                             */
;*---------------------------------------------------------------------*/
(define (heap-module-list . args)
   (cond
      ((null? args)
       *heap-module-list*)
      ((not (getprop (car args) *heap-mark*))
       (putprop! (car args) *heap-mark* #t)
       (set! *heap-module-list* (cons (car args) *heap-module-list*)))))

