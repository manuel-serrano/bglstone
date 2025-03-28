;*=====================================================================*/
;*    .../project/bglstone/src/bigloo/bigloo/Module/module.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri May 31 10:29:03 1996                          */
;*    Last change :  Thu Mar  6 16:27:36 2025 (serrano)                */
;*    Copyright   :  1996-2025 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The compilation of a Module clause                               */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module module_module
   (include "Ast/node.sch")
   
   (include "Ast/unit.sch"
	    "Engine/pass.sch"
	    "Module/module.sch")
   
   (import  engine_param
	    engine_pass
	    tools_error
	    heap_restore
	    module_main
	    module_statexp
	    module_impuse
	    module_include
	    module_with
	    module_type
	    module_foreign
	    module_java
	    module_eval
	    module_load
	    module_pragma
	    module_checksum
	    module_option
	    module_alibrary
	    (additional-heap-restore-globals! ast_env))
   
   (export  (class ccomp::object
	       ;; the compiler identifier
	       (id::symbol read-only)
	       ;; the compilation method when compiling the module
	       ;; which contains this clause
	       (producer::procedure read-only (default (lambda (c) '())))
	       ;; what to do when importing a module which contains
	       ;; this clause
	       (consumer::procedure read-only (default (lambda (m c) '()))) 
	       ;; what to do after the overall module compilation
	       (finalizer::procedure read-only (default (lambda () 'void))))
	   
	    (produce-module!                    <module-clause>)
	    (produce-module-clause!             <clause>)
	    (consume-module!                    name::symbol <module-clause>)
	    (consume-module-clause!             name::symbol <clause>)

	    (module-checksum-object)
	    
	    (module-initialization-id::symbol ::symbol)
	    
	    *main*             
	    *module*
	    *module-clause*
	    *module-checksum*
	    *module-location*))

;*---------------------------------------------------------------------*/
;*    Informations about the module being compiled                     */
;*---------------------------------------------------------------------*/
(define *module*          #f)    ;; module identifer
(define *module-clause*   #f)    ;; module clause (for pretty-printing)
(define *main*            #f)    ;; the main entry point identifier
(define *module-checksum* #f)    ;; the checksum of the current module
(define *module-location* #f)    ;; the location of the module clause

;*---------------------------------------------------------------------*/
;*    produce-module! ...                                              */
;*    -------------------------------------------------------------    */
;*    The library load order is very tricky. To be short, library      */
;*    heaps have to be loaded before the module is parsed (because     */
;*    module parsing may declare some classes). Thus we make a kind    */
;*    of dummy pre-parsing to get the library clauses then we          */
;*    restore the additional heaps (that may also be requested         */
;*    by compiler options) and we start the real module processing.    */
;*---------------------------------------------------------------------*/
(define (produce-module! mclause)
   (pass-prelude "Module")
   (match-case mclause
      ((module (and (? symbol?) ?name) . ?clauses)
       (let ((clauses (cons (early-with-clauses)
			    (if (symbol? *main*)
				(cons `(main ,*main*) clauses)
				clauses))))
	  (if (not (legal-module-name? name))
	      (user-error "Parse error"
			  "Illegal module name"
			  mclause)
	      (begin
		 (set! *module* name)
		 (set! *module-clause* mclause)
		 (set! *module-location* (if (epair? mclause)
					     (cer mclause)
					     #f))
		 (install-clauses-compiler!)
		 (let ((clauses (produce-library-clauses clauses)))
		    ;; one library clauses have been parsed
		    ;; we must restore additional heap
		    (restore-additional-heaps)
		    ;; now we resume the module parsing process
		    (for-each produce-module-clause! clauses)
		    (set! *module-checksum* (checksum-module mclause))
		    (pass-postlude (finalize-clause-compilations)
				   additional-heap-restore-globals!))))))
				   
      (else
       (user-error "Parse error" "Illegal module form" mclause))))

;*---------------------------------------------------------------------*/
;*    produce-library-clauses ...                                      */
;*---------------------------------------------------------------------*/
(define (produce-library-clauses clauses)
   (let ((producer (find-clause-producer 'library '(library _))))
      (let loop ((clauses clauses)
		 (res     '()))
	 (cond
	    ((null? clauses)
	     (reverse! res))
	    (else
	     (let ((clause (car clauses)))
		(match-case clause
		   ((library . ?-)
		    (producer clause)
		    (loop (cdr clauses) res))
		   (else
		    (loop (cdr clauses) (cons clause res))))))))))

;*---------------------------------------------------------------------*/
;*    finalize-clause-compilations ...                                 */
;*---------------------------------------------------------------------*/
(define (finalize-clause-compilations)
   (let loop ((cc    *clause-compilers*)
	      (units '()))
      (if (null? cc)
	  units
	  (let* ((finalizer (ccomp-finalizer (car cc)))
		 (finalres  (finalizer)))
	     (loop (cdr cc)
		   (if (pair? finalres)
		       (append finalres units)
		       units))))))

;*---------------------------------------------------------------------*/
;*    produce-module-clause! ...                                       */
;*---------------------------------------------------------------------*/
(define (produce-module-clause! clause)
   (match-case clause
      (((and (? symbol?) ?id) . ?-)
       ((find-clause-producer id clause) clause))
      (else
       (user-error "Parse error"
		   "Illegal module clause"
		   clause
		   '()))))
	     
;*---------------------------------------------------------------------*/
;*    *clause-compilers* ...                                           */
;*---------------------------------------------------------------------*/
(define *clause-compilers* '())

;*---------------------------------------------------------------------*/
;*    install-clauses-compiler ...                                     */
;*---------------------------------------------------------------------*/
(define (install-clauses-compiler!)
   ;; the order of the compilers is important. Don't change it
   (set! *clause-compilers*
	 (list (make-eval-compiler)
	       (make-main-compiler)
	       (make-load-compiler)
	       (make-use-compiler)
	       (make-import-compiler)
	       (make-from-compiler)
	       (make-java-compiler)
	       (make-static-compiler)
	       (make-export-compiler)
	       (make-include-compiler)
	       (make-with-compiler)
	       (make-foreign-compiler)
	       (make-extern-compiler)
	       (make-type-compiler)
	       (make-pragma-compiler)
	       (make-option-compiler)
	       (make-alibrary-compiler))))

;*---------------------------------------------------------------------*/
;*    find-clause-producer ...                                         */
;*---------------------------------------------------------------------*/
(define (find-clause-producer keyword clause)
   (define (unknown-clause-producer values)
      (user-error "Parse error"
		  "Unknown module clause"
		  clause
		  '()))
   (let loop ((cc *clause-compilers*))
      (cond
	 ((null? cc)
	  unknown-clause-producer)
	 ((eq? (ccomp-id (car cc)) keyword)
	  (ccomp-producer (car cc)))
	 (else
	  (loop (cdr cc))))))

;*---------------------------------------------------------------------*/
;*    module-initialization-id ...                                     */
;*---------------------------------------------------------------------*/
(define (module-initialization-id id)
   'module-initialization)

;*---------------------------------------------------------------------*/
;*    consume-module! ...                                              */
;*---------------------------------------------------------------------*/
(define (consume-module! pname mclause)
   (match-case mclause
      ((module (and (? symbol?) ?name) . ?clauses)
       (cond
	((not (legal-module-name? name))
	 (user-error "Parse error" "Illegal module name" mclause '()))
	((not (eq? pname name))
	 (user-error "Module declaration"
		     (string-append "conflict in module's name: "
				    (symbol->string name) " vs "
				    (symbol->string pname))
		     mclause
		     '()))
	(else
	 (apply append (map (lambda (c) (consume-module-clause! name c))
			    clauses)))))
      (else
       (user-error "Parse error"
		   "Illegal module declaration"
		   mclause
		   '()))))

;*---------------------------------------------------------------------*/
;*    consume-module-clause! ...                                       */
;*---------------------------------------------------------------------*/
(define (consume-module-clause! module clause)
   (match-case clause
      (((and (? symbol?) ?id) . ?values)
       ((find-clause-consumer id clause) module clause))
      (else
       (user-error "Parse error"
		   "Illegal module clause"
		   clause
		   '()))))
	     
;*---------------------------------------------------------------------*/
;*    find-clause-consumer ...                                         */
;*---------------------------------------------------------------------*/
(define (find-clause-consumer keyword clause)
   (define (unknown-clause-consumer module values)
      (user-error "Parse error"
		  "Unknown module clause"
		  clause
		  '()))
   (let loop ((cc *clause-compilers*))
      (cond
	 ((null? cc)
	  unknown-clause-consumer)
	 ((eq? (ccomp-id (car cc)) keyword)
	  (ccomp-consumer (car cc)))
	 (else
	  (loop (cdr cc))))))

;*---------------------------------------------------------------------*/
;*    legal-module-name? ...                                           */
;*---------------------------------------------------------------------*/
(define (legal-module-name? name)
   (not (memq name '(eval foreign t))))

;*---------------------------------------------------------------------*/
;*    checksum-module ...                                              */
;*---------------------------------------------------------------------*/
(define (checksum-module mclause)
   (match-case mclause
      ((module (and (? symbol?) ?name) . ?clauses)
       (cond
	  ((not (legal-module-name? name))
	   (user-error "Parse error" "Illegal module name" mclause '()))
	  (else
	   (module-checksum mclause))))
      (else
       (user-error "Parse error"
		   "Illegal module declaration"
		   mclause
		   '()))))

;*---------------------------------------------------------------------*/
;*    module-checksum-object ...                                       */
;*---------------------------------------------------------------------*/
(define (module-checksum-object)
   (pass-prelude "Module checksum object")
   (let ((checksum *module-checksum*)
	 (dest     (if (not (string? *dest*))
		       (if (and (pair? *src-files*)
				(string? (car *src-files*)))
			   (string-append (prefix (car *src-files*)) ".mco")
			   #f)
		       (if (eq? *pass* 'mco)
			   *dest*
			   (string-append (prefix *dest*) ".mco")))))
      (define (generate-mco)
	 (if (string? dest)
	     (let ((oport (open-output-file dest)))
		(if (output-port? oport)
		    (begin
		       (fprint oport checksum)
		       (close-output-port oport))
		    (error "module checksum"
			   "Can't open file for output"
			   dest)))
	     (print checksum)))
      ;; we check if the mco file already exists
      (if (and (string? dest) (file-exists? dest))
	  ;; if it exists we check if it alread
	  ;; contains the correct checksum
	  (let ((iport (open-input-file dest)))
	     (if (not (input-port? iport))
		 (user-error "module checksum"
			     "Can't open file for input"
			     dest)
		 (let ((cs (read iport)))
		    (close-input-port iport)
		    (if (not (=fx cs checksum))
			(begin
			   (delete-file dest)
			   (generate-mco))))))
	  (generate-mco)))
   ;; we are done
   (pass-postlude #t))
      

