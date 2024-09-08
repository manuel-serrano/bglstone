;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Module/impuse.scm           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Jun  4 12:25:53 1996                          */
;*    Last change :  Fri Nov 12 10:44:28 2004 (serrano)                */
;*    Copyright   :  1996-2004 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The import/use/from clauses compilation                          */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module module_impuse
   
   (include "Ast/unit.sch"
	    "Tools/trace.sch")
   
   (import  read_reader
	    module_module
	    module_prototype
	    module_class
	    module_include
	    module_checksum
	    tools_speek
	    tools_error
	    tools_location
	    type_type
	    object_class
	    read_access
	    read_inline
	    ast_var
	    ast_find-gdefs
	    ast_glo-decl
	    ast_ident
	    engine_param
	    init_main)
	   
   (export  (make-import-compiler)
	    (make-use-compiler)
	    (make-from-compiler)
	    (get-imported-modules)
	    (import-with-module! ::symbol loc)
	    (import-parser       ::symbol prototype . import-src)))

;*---------------------------------------------------------------------*/
;*    make-import-compiler ...                                         */
;*---------------------------------------------------------------------*/
(define (make-import-compiler)
   (instantiate::ccomp (id 'import)
		       (producer impuse-producer)))
 
;*---------------------------------------------------------------------*/
;*    make-use-compiler ...                                            */
;*---------------------------------------------------------------------*/
(define (make-use-compiler)
   (instantiate::ccomp (id 'use)
		       (producer impuse-producer)
		       (finalizer impuse-finalizer)))

;*---------------------------------------------------------------------*/
;*    make-from-compiler ...                                           */
;*---------------------------------------------------------------------*/
(define (make-from-compiler)
   (instantiate::ccomp (id 'from)
		       (producer impuse-producer)
		       (consumer (lambda (module clause)
				    (impuse-producer clause)
				    '()))))

;*---------------------------------------------------------------------*/
;*    impuse-producer ...                                              */
;*---------------------------------------------------------------------*/
(define (impuse-producer clause)
   (let ((mode (car clause)))
      (match-case clause
	 ((?- . ?protos)
	  (for-each (lambda (proto) (impuse-parser proto mode clause)) protos))
	 (else
	  (user-error/location (find-location *module-clause*)
			       "Parse error"
			       (string-append
				"Illegal `"
				(string-downcase (symbol->string mode))
				"' clause")
			       clause
			       '())))))
   
;*---------------------------------------------------------------------*/
;*    import-all-module ...                                            */
;*---------------------------------------------------------------------*/
(define (import-all-module module::symbol mode src)
   (let ((b (assq module *import-module-list*))
	 (loc (find-location/loc src (find-location *module-clause*))))
      (if (pair? b)
	  (imode-vars-set! (cdr b) 'all)
	  (set! *import-module-list*
		(cons
		 (cons module (imode module mode 'all #unspecified loc src))
		 *import-module-list*)))))

;*---------------------------------------------------------------------*/
;*    import-1-module ...                                              */
;*---------------------------------------------------------------------*/
(define (import-1-module module::symbol var mode src)
   (let ((cell (assq module *import-module-list*))
	 (loc (find-location/loc src (find-location *module-clause*))))
      (if (not (pair? cell))
	  (set! *import-module-list*
		(cons (cons module (imode module
					  mode
					  (list var)
					  #unspecified
					  loc
					  src))
		      *import-module-list*))
	  (let ((b (cdr cell)))
	     (case (imode-vars b)
		((with)
		 (imode-mode-set! b mode)
		 (imode-vars-set! b (list var)))
		((all)
		 'nothing)
		(else
		 (imode-vars-set! b (cons var (imode-vars b)))))))))

;*---------------------------------------------------------------------*/
;*    import-with-module! ...                                          */
;*---------------------------------------------------------------------*/
(define (import-with-module! module src)
   (let ((b (assq module *import-module-list*))
	 (loc (find-location/loc src (find-location *module-clause*))))
      (if (not (pair? b))
	  (set! *import-module-list*
		(cons (cons module (imode module 'with '() 0 loc src))
		      *import-module-list*)))))
   
;*---------------------------------------------------------------------*/
;*    impuse-parser ...                                                */
;*    -------------------------------------------------------------    */
;*    The syntaxe of importation clause is:                            */
;*    import ::= module-name                                           */
;*            |  (module-name "file-name" *)                           */
;*            |  (variable module-name)                                */
;*            |  (variable module-name "file-name" *)                  */
;*---------------------------------------------------------------------*/
(define (impuse-parser prototype mode import-src)
   (trace (ast 2) "impuse-parser: " prototype " " mode #\Newline)
   (cond
      ((symbol? prototype)
       ;; module-name
       (import-all-module prototype mode import-src))
      ((list? prototype)
       (let ((inv (reverse prototype)))
	  (let loop ((lst inv)
		     (files '()))
	     (cond
		((not (pair? lst))
		 (user-error "Parse error"
			     "Illegal import/use/from clause"
			     prototype
			     '()))
		((string? (car lst))
		 (loop (cdr lst) (cons (car lst) files)))
		((symbol? (car lst))
		 (let ((mod (car lst))
		       (vars (cdr lst)))
		    (cond
		       ((null? vars)
			;; (module-name "file-name"+)
			(if (pair? files) (add-access! mod files))
			(import-all-module mod mode prototype))
		       ((every? symbol? vars)
			;; (var1 var2 ... varN module-name "file-name"*)
			(if (pair? files) (add-access! mod files))
			(for-each (lambda (v)
				     (import-1-module mod v mode prototype))
				  vars))
		       (else
			(user-error "Parse error"
				    "Illegal import/use/from clause"
				    prototype
				    '())))))
		(else
		 (user-error "Parse error"
			     "Illegal import/use/from clause"
			     prototype
			     '()))))))
      (else
       (user-error "Parse error"
			     "Illegal import/use/from clause"
			     prototype
			     '()))))
   
;*---------------------------------------------------------------------*/
;*    imode ...                                                        */
;*---------------------------------------------------------------------*/
(define-struct imode id mode vars checksum loc src)

;*---------------------------------------------------------------------*/
;*    *import-module-list* ...                                         */
;*---------------------------------------------------------------------*/
(define *import-module-list* '())

;*---------------------------------------------------------------------*/
;*    *imported-module-list* ...                                       */
;*    -------------------------------------------------------------    */
;*    This variable contains all the imported modules (while           */
;*    *import-module-list* is resetted each time the import finalizer  */
;*    is invoked). This variable is used by `module_library' to        */
;*    detect module already initialized.                               */
;*---------------------------------------------------------------------*/
(define *imported-module-list* '())

;*---------------------------------------------------------------------*/
;*    get-imported-modules ...                                         */
;*---------------------------------------------------------------------*/
(define (get-imported-modules)
   *imported-module-list*)

;*---------------------------------------------------------------------*/
;*    import-finalizer ...                                             */
;*    -------------------------------------------------------------    */
;*    @label importation unit@                                         */
;*    -------------------------------------------------------------    */
;*    In the `impuse' finalizer we read all the imported modules and   */
;*    if we have to, we create a unit in order to initialize imported  */
;*    modules.                                                         */
;*---------------------------------------------------------------------*/
(define (import-finalizer)
   (set! *import-module-list* (reverse! *import-module-list*))
   (trace (ast 2) "import-finalizer: " *import-module-list* #\Newline)
   ;; when not in imported-module-list mode, we do the regular job, that
   ;; is, we open the imported files.
   (let loop ((init* '()))
      (if (null? *import-module-list*)
	  (if (pair? init*)
	      (let loop ((init*      init*)
			 (init-call* '()))
		 (if (null? init*)
		     ;; the unit number of importation must be smaller
		     ;; than the one for classes. See module_class module
		     ;; (@ref class.scm:class unit@).
		     (let ((body (if (>fx *debug-module* 0)
				     `((begin
					 (pragma::int
					  ,(string-append
					    "puts(\"    Module init (import): "
					    (symbol->string *module*)
					    "\")"))
					 ,@init-call*))
				     init-call*)))
			(list (unit 'imported-modules 12 body #t)))
		     (let* ((id          (imode-id (car init*)))
			    (checksum    (imode-checksum (car init*)))
			    (init-fun-id (module-initialization-id id)))
			(let ((global (import-parser id (list init-fun-id
							      'checksum::long
							      'from::string))))
			   ;; module initializer can't be reachable
			   ;; from eval. We mark this.
			   (global-evaluable?-set! global #f))
			(loop (cdr init*)
			      (cons `((@ ,init-fun-id ,id)
				      ,checksum
				      ,(symbol->string *module*))
				    init-call*)))))
	      '())
	  (let ((module (car (car *import-module-list*)))
		(imode  (cdr (car *import-module-list*))))
	     (set! *import-module-list* (cdr *import-module-list*))
	     (set! *imported-module-list* (cons module *imported-module-list*))
	     (if (not (eq? (imode-mode imode) 'with))
		 (read-imported-module imode))
	     (loop (if (memq (imode-mode imode) '(import with from))
		       (cons imode init*)
		       init*))))))

;*---------------------------------------------------------------------*/
;*    impuse-finalizer ...                                             */
;*    -------------------------------------------------------------    */
;*    In the `impuse' finalizer we read all the imported modules and   */
;*    if we have to, we create a unit in order to initialize imported  */
;*    modules.                                                         */
;*---------------------------------------------------------------------*/
(define (impuse-finalizer)
   (let* ((import-finalizer (import-finalizer))
	  (inline-finalizer (inline-finalizer))
	  (finalizers       (append import-finalizer inline-finalizer)))
      (if (null? finalizers)
	  'void
	  finalizers)))

;*---------------------------------------------------------------------*/
;*    import-parser ...                                                */
;*---------------------------------------------------------------------*/
(define (import-parser module::symbol prototype . import-src)
   (let ((proto (parse-prototype prototype))
	 (src   (if (pair? import-src)
		    (car import-src)
		    #f)))
      (if (not (pair? proto))
	  (user-error/location (find-location *module-clause*)
			       "Parse error"
			       "Illegal prototype"
			       prototype
			       '())
	  (case (car proto)
	     ((sfun sifun sgfun)
	      (declare-global-sfun! (cadr proto)
				    (caddr proto)
				    module
				    'import
				    (car proto)
				    prototype
				    src))
	     ((svar)
	      (declare-global-svar! (cadr proto) module	'import	prototype src))
	     ((class)
	      (declare-class! (cdr proto) module 'import #f #f prototype src))
	     ((abstract-class)
	      (declare-class! (cdr proto) module 'import #f #t prototype src))
	     ((final-class)
	      (declare-class! (cdr proto) module 'import #t #f prototype src))
	     ((wide-class)
	      (declare-wide-class! (cdr proto) module 'import prototype src))
	     (else
	      (user-error "Parse error" "Illegal prototype" prototype '()))))))
   
;*---------------------------------------------------------------------*/
;*    read-imported-module ...                                         */
;*---------------------------------------------------------------------*/
(define (read-imported-module imode)
   (trace (ast 2) #\Newline "read-imported-module: " imode #\Newline)
   (let* ((module (imode-id imode))
	  (wanted (imode-vars imode))
	  (b      (assq module *access-table*)))
      (verbose 2 "      [reading "
	       (if (eq? (imode-mode imode) 'use) "used" "imported")
	       " module " module "]" #\Newline)
      (if (not b)
	  (user-error/location (imode-loc imode)
			       "read-imported-module"
			       "Can't find such module"
			       module
			       '())
	  (let* ((fnames (cdr b))
		 (fname  (find-file/path (car fnames) *load-path*)))
	     (if (not (string? fname))
		 (user-error/location (imode-loc imode)
				      "read-imported-module"
				      "Can't open such file"
				      (car fnames)
				      '())
		 (let ((port (open-input-file fname)))
		    (reader-reset!)
		    (if (not (input-port? port))
			(user-error/location (imode-loc imode)
					     "read-imported-module"
					     "Can't open such file"
					     (car fnames)
					     '())
			(unwind-protect
			   (let* ((mdecl (compiler-read port #t))
				  (prov (append
					 (begin
					    (reset-include-consumed-directive!)
					    (reset-include-consumed-code!)
					    (consume-module! module mdecl))
					 (get-include-consumed-directive)))
				  (code (get-include-consumed-code))
				  (check (module-checksum mdecl))
				  (src (imode-src imode)))
			      (imode-checksum-set! imode check)
			      (look-for-inline
			       (if (not (pair? wanted))
				   (import-everything prov module src)
				   (import-wanted prov wanted module src))
			       code
			       port
			       (cdr fnames)
			       module)
			      (close-input-port port))
			   (close-input-port port)))))))))
   
;*---------------------------------------------------------------------*/
;*    import-everything ...                                            */
;*---------------------------------------------------------------------*/
(define (import-everything provided module::symbol src)
   (let loop ((provided provided)
	      (inline   '()))
      (if (null? provided)
	  inline
	  (let ((p (import-parser module (car provided) src)))
	     (if (global? p)
		 (let ((val (global-value p)))
		    (loop (cdr provided)
			  (cond
			     ((or (not (global? p))
				  (not (sfun? val)))
			      inline)
			     ((eq? (sfun-class val) 'sifun)
			      (cons (cons (global-id p) 'sifun) inline))
			     (else
			      inline))))
		 (loop (cdr provided) inline))))))
	     
;*---------------------------------------------------------------------*/
;*    import-wanted ...                                                */
;*---------------------------------------------------------------------*/
(define (import-wanted provided wanted module::symbol src)
   (let loop ((provided provided)
	      (inline   '())
	      (wanted   wanted))
      ;; we check that all wanted functions are in the list and in
      ;; the same time, we compute the list of all inline to be fetch.
      (cond
	 ((null? wanted) 
	  inline)
	 ((null? provided)
	  (user-error/location (find-location *module-clause*)
			       module
			       "Can't find exportation for these identifiers"
			       wanted
			       '()))
	 (else
	  (let ((proto (parse-prototype (car provided))))
	     (if (pair? proto)
		 (let ((id (fast-id-of-id (cadr proto)
					  (find-location (car provided)))))
		    (if (not (memq id wanted))
			(loop (cdr provided)
			      inline
			      wanted)
			(let ((p (import-parser module (car provided) src)))
			   (cond
			      ((global? p)
			       (loop (cdr provided)
				     (cond
					((eq? (car proto) 'sifun)
					 (cons (cons id 'sifun) inline))
					(else
					 inline))
				     (remq! id wanted)))
			      ((type? p)
			       (loop (cdr provided)
				     inline
				     (remq! id wanted)))
			      (else
			       (loop (cdr provided)
				     inline
				     (remq! id wanted)))))))
		 (loop (cdr provided)
		       inline
		       wanted)))))))

