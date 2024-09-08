(module backend_jvm
   (include "Engine/pass.sch")
   (import engine_param
	   engine_pass
	   tools_error
	   module_module
	   type_type
	   backend_backend
	   read_jvm	; BAD module->qualified-type
	   backend_c_main	; BAD make-bigloo-main
	   saw_jvm_compile
	   jas_as
	   jas_peep
	   saw_jvm_ld
	   )
   (export ;(class jvm::backend)
	   (build-jvm-backend) ))

(define (build-jvm-backend)
   (let ( (me (instantiate::jvm (language 'jvm))) )
      (jvm-qname-set! me (string->symbol (module->qualified-type *module*)))
      me ))

(define-method (backend-compile me::jvm)
   ;; the jvm prelude (hello message and *DEST* update)
   (pass-prelude "Jvm" start-jvm-emission!)
   (verbose 2 "      [module: " *module* " qualified type name: "
	    (module->qualified-type *module*) "]"#\Newline)
   ;; if we are going to link and we have not found a main yet, we
   ;; have to produce a fake one
   (if (and (not *main*) (memq *pass* '(ld distrib)))
       (set! *main* (make-bigloo-main)))
   ;; the jvm driver
   (define (emit classfile dest)
      (let ((dir *jvm-dir-name*))
	 (if (eq? *pass* 'jvmas)
	     (let ((port (if (not (string? dest))
			     (current-output-port)
			     (open-output-file
			      (string-append dir "/" dest)))))
		(jvmasdump classfile port)
		(if (not (eq? port (current-output-port)))
		    (close-output-port port)))
	     (let* ((cname (if (not (string? dest))
			       (string-append dir "/a.class")
			       (string-append dir "/" dest)))
		    (port (open-output-binary-file cname)))
		(if (not (binary-port? port))
		    (error "jvm-dump" "Can't open file for output" cname))
		(jvm-as classfile port)
		(close-binary-port port)))))
   (jvm-check-package *module* *jvm-dir-name*)
   (let ((l* (saw_jvm-compile me))
	 (bname (cond
		   ((eq? *pass* 'ld)
		    (if (pair? *src-files*)
			(addsuffix (prefix (basename (car *src-files*))))
			"a.class"))
		   ((not (string? *dest*))
		    (if (pair? *src-files*)
			(addsuffix (prefix (basename (car *src-files*))))
			#f))
		   (else
		    (addsuffix (prefix (basename *dest*)))))))
      (emit (car l*) bname)
      (for-each (lambda (cf) (emit cf (jasname cf)))
		(cdr l*) )
      (stop-on-pass 'cc (lambda () 'done))
      (stop-on-pass 'jvmas (lambda () 'done))
      (stop-on-pass 'jast (lambda () 'done)) ))

;*---------------------------------------------------------------------*/
;*    jvm-check-package ...                                            */
;*    -------------------------------------------------------------    */
;*    We check that the class file name is compatible with the         */
;*    JVM qualified type name declared for the class.                  */
;*---------------------------------------------------------------------*/
(define (jvm-check-package module path)
   (define (compare-path? base path)
      (let ((lbase (string-length base))
	    (lpath (string-length path)))
	 (if (< lpath lbase)
	     #f
	     (let loop ((rpath (-fx lpath 1))
			(rbase (-fx lbase 1)))
		(if (=fx rbase -1)
		    #t
		    (let ((cbase (string-ref base rbase))
			  (cpath (string-ref path rpath)))
		       (if (or (char=? cbase cpath)
			       (and (char=? cpath #\/)
				    (char=? cbase #\.)))
			   (loop (-fx rpath 1) (-fx rbase 1))
			   #f)))))))
   (let* ((qtype (module->qualified-type module))
	  (base (let ((pre (prefix qtype)))
		   (cond
		      ((string=? pre "")
		       ".")
		      ((string=? pre qtype)
		       ".")
		      (else
		       pre)))))
      (if (not (compare-path? (jvm-filename base) path))
	  (warning "Incompatible package name and class path."
		   "Package name for module " *module* " is `" base
		   "', class path is `" path "'."))))

(define *jvm-dir-name* ".")

(define (jvmasdump classfile port)
   (let ((ow *pp-width*) (oc *pp-case*))
      (set! *pp-width* 10240)
      (set! *pp-case* 'lower)
      (pp classfile port)
      (set! *pp-case* oc)
      (set! *pp-width* ow)))

(define (addsuffix name)
   (string-append name
		  (case *pass*
		     ((jast)
		      ".jast")
		     ((jvmas)
		      ".jas")
		     (else
		      ".class"))))

(define (jasname cf)
   (match-case cf
      (((class ?name) . ?-)
       (addsuffix (symbol->string name)))))

;*---------------------------------------------------------------------*/
;*    jvm-filename ...                                                 */
;*---------------------------------------------------------------------*/
(define (jvm-filename name)
   (if (string? *jvm-directory*)
       (if (string=? name ".")
	   *jvm-directory*
	   (make-file-name *jvm-directory* name))
       name))

;*---------------------------------------------------------------------*/
;*    jvm-dirname ...                                                  */
;*---------------------------------------------------------------------*/
(define (jvm-dirname file)
   (let* ((dfile (dirname file))
	  (dir (jvm-filename dfile)))
      (if (and (not (string=? dfile ""))
	       (not (directory? dfile))
	       (not (file-exists? dfile))
	       (or (not (string? *jvm-directory*))
		   (directory? *jvm-directory*)))
	  ;; we create the necessary directories to put the JVM class file
	  (make-directories dir))
      dir))

;*---------------------------------------------------------------------*/
;*    start-jvm-emission! ...                                          */
;*---------------------------------------------------------------------*/
(define (start-jvm-emission!)
   (cond
      ((string? *dest*)
       (let ((dname (dirname *dest*)))
	  (if (not (string=? dname ""))
	      (set! *jvm-dir-name* (jvm-dirname *dest*)))))
      ((eq? *pass* 'ld)
       (if (pair? *src-files*)
	   (set! *jvm-dir-name* (jvm-dirname (car *src-files*))))))
   (if (not (and (file-exists? *jvm-dir-name*) (directory? *jvm-dir-name*)))
       (error "start-jvm-emission!"
	      "Can't write dest file because directory doesn't exist"
	      *jvm-dir-name*)
       #t))

      
;*---------------------------------------------------------------------*/
;*    Link                                                             */
;*---------------------------------------------------------------------*/
(define-method (backend-link me::jvm result)
   ;; CARE move the code here...
   (jvm-ld) )
