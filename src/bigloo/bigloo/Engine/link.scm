;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Engine/link.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Jan 15 11:16:02 1994                          */
;*    Last change :  Fri Nov  5 16:09:25 2004 (serrano)                */
;*    Copyright   :  1994-2004 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    On link quand l'utilisateur n'a passe que des `.o'               */
;*    -------------------------------------------------------------    */
;*    Pour ce faire on essaye de trouver des `.scm' correspondants.    */
;*    On genere un petit fichier `.scm' qui les initialise puis on     */
;*    le compile normalement ou alors, on se contente d'invoquer le    */
;*    linker `*ld*'.                                                   */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    Le module                                                        */
;*---------------------------------------------------------------------*/
(module engine_link
   (export (link)
	   (unprof-src-name name)
	   (find-src-file prefix bname)
	   *link-main-module*)
   (import cc_ld
	   read_reader
	   engine_compiler
	   engine_param
	   bdb_setting
	   init_setrc
	   module_alibrary
	   tools_error
	   tools_misc
	   saw_jvm_ld
	   msil_ld
	   read_jvm
	   module_module))

;*---------------------------------------------------------------------*/
;*    *tmp-main-file-name* ...                                         */
;*---------------------------------------------------------------------*/
(define *tmp-main-file-name* "<unknown-yet>")
(define *link-module* #unspecified)
(define *link-package* #unspecified)
(define *link-main-module* #unspecified)

;*---------------------------------------------------------------------*/
;*    setup-tmp-main-file-name! ...                                    */
;*---------------------------------------------------------------------*/
(define (setup-tmp-main-file-name!)
   (define (jvm-setup-tmp-file-name!)
      (let ((pref "JVMMAIN"))
	 (set! *link-package* pref)
	 (set! *link-module* (string->symbol *link-package*))
	 (set! *tmp-main-file-name* (string-append pref ".bgl"))
	 (if (file-exists? *tmp-main-file-name*)
	     (error "link"
		    "Can't write tmp file (because it already exists)"
		    *tmp-main-file-name*))))
   (define (dotnet-setup-tmp-file-name!)
      (let ((pref "dotnetmain"))
	 (set! *link-package* pref)
	 (set! *link-module* (string->symbol *link-package*))
	 (set! *tmp-main-file-name* (string-append pref ".bgl"))
	 (if (file-exists? *tmp-main-file-name*)
	     (error "link"
		    "Can't write tmp file (because it already exists)"
		    *tmp-main-file-name*))))
   (define (c-setup-tmp-file-name!)
      (set! *tmp-main-file-name*
	    (make-file-name *bigloo-tmp*
			    (string-append "main-tmp"
					   "@"
					   (let ((user (getenv "USER")))
					      (if (not (string? user))
						  ""
						  user))
					   "."
					   (car *src-suffix*))))
      (set! *link-module* (gensym 'module))
      (set! *link-package* (symbol->string *link-module*)))
   (case *target-language*
      ((jvm)
       (jvm-setup-tmp-file-name!))
      ((.net)
       (dotnet-setup-tmp-file-name!))
      (else
       (c-setup-tmp-file-name!))))
			      
;*---------------------------------------------------------------------*/
;*    link ...                                                         */
;*---------------------------------------------------------------------*/
(define (link)
   ;; prepare the temporary filename
   (setup-tmp-main-file-name!)
   ;; we set bdb options
   (if (>fx *bdb-debug* 0)
       (bdb-setting!))
   ;; we start by looking for the source files
   (let loop ((objects *o-files*)
	      (sources '()))
      (if (null? objects)
	  ;; and with launch the linking process
	  (link-with sources)
	  (let* ((object   (car objects))
		 (pref     (unprof-src-name (prefix object)))
		 (bpref    (basename pref))
		 (scm-file (find-src-file pref bpref)))
	     (if (string? scm-file)
		 (loop (cdr objects) (cons (cons scm-file object) sources))
		 (begin
		    (if (>=fx (bigloo-warning) 2)
			(warning  "link"
				  "No Bigloo module found for -- "
				  (car objects)))
		    (loop (cdr objects) sources)))))))

;*---------------------------------------------------------------------*/
;*    unprof-src-name ...                                              */
;*---------------------------------------------------------------------*/
(define (unprof-src-name name)
   (if (not *profile-mode*)
       name
       (let ((len (string-length name)))
	  (if (and (>fx len 2)
		   (char=? (string-ref name (-fx len 1)) #\p)
		   (char=? (string-ref name (-fx len 2)) #\_))
	      (substring name 0 (-fx len 2))
	      name))))

;*---------------------------------------------------------------------*/
;*    find-file-for-link ...                                           */
;*---------------------------------------------------------------------*/
(define (find-file-for-link file)
   (if (file-exists? file)
       file
       (find-file/path file *load-path*)))

;*---------------------------------------------------------------------*/
;*    find-src-file ...                                                */
;*---------------------------------------------------------------------*/
(define (find-src-file prefix bname)
   (let loop ((suffix *src-suffix*)
	      (files '()))
      (if (null? suffix)
	  (cond
	     ((null? files)
	      #f)
	     ((null? (cdr files))
	      (car files))
	     (else
	      (warning "link" "Several source files found for object `"
		       bname "'. Using file -- " (car files))
	      (car files)))
	  (let* ((suf (car suffix))
		 (f   (find-file-for-link (string-append prefix "." suf))))
	     (if (string? f)
		 (loop (cdr suffix) (cons f files))
		 (let ((f (find-file-for-link (string-append bname "." suf))))
		    (if (string? f)
			(loop (cdr suffix) (cons f files))
			(loop (cdr suffix) files))))))))

;*---------------------------------------------------------------------*/
;*    link-with ...                                                    */
;*---------------------------------------------------------------------*/
(define (link-with sources)
   (define (do-link first)
      (case *target-language*
	 ((c)
	  (set! *o-files* (cdr *o-files*))
	  (ld first #f))
	 ((jvm)
	  (read-jfile)
	  (jvm-ld))
	 ((.net)
	  (set! *src-files* '())
	  (dotnet-ld))
	 (else
	  (error "linker"
		 "Unimplemented target language"
		 *target-language*))))
   (if (null? sources)
       (let ((first (prefix (car *o-files*))))
	  (warning "link" "No source file found" " -- " *o-files*)
	  ;; we load the library init files.
	  (load-library-init)
	  (do-link first))
       ;; on construit la clause du module
       (let loop ((sources   sources)
		  (cls       '())
		  (main      #f)
		  (fmain     "")
		  (libraries '()))
	  (if (null? sources)
	      (if main
		  ;; ce n'est pas la peine de generer un main, il y en a
		  ;; deja un
		  (let ((first (prefix (car *o-files*))))
		     ;; if libraries are used by some module we add them
		     ;; to the link
		     (for-each (lambda (lib)
				  (use-library! (make-library-name lib) 'now))
			       libraries)
		     ;; we load the library init files.
		     (load-library-init)
		     (set! *src-files* (list fmain))
		     (do-link first))
		  ;; on genere un main puis on link.
		  (begin
		     (make-tmp-main cls main libraries)
		     (set! *src-files* (list *tmp-main-file-name*))
		     ;; we have to remove extra mco files before compiler
		     ;; otherwise the compiler will warn about that files.
		     (let loop ((ra  *rest-args*)
				(res '()))
			(cond
			   ((null? ra)
			    (set! *rest-args* (reverse! res)))
			   ((member (suffix (car ra)) *mco-suffix*)
			    (loop (cdr ra) res))
			   (else
			    (loop (cdr ra) (cons (car ra) res)))))
		     (unwind-protect
			(compiler)
			;; we load the library init files.
			(load-library-init)
			(let* ((scm-file *tmp-main-file-name*)
			       (pre        (prefix scm-file))
			       (c-file     (string-append pre ".c"))
			       (o-file     (string-append pre "." *c-object-file-extension*))
			       (class-file (string-append pre ".class"))
			       (obj-file (string-append pre ".obj")))
			   (if (memq *target-language* '(jvm .net))
			       (if (file-exists? scm-file)
				   (delete-file scm-file))
			       (for-each (lambda (f)
					    (if (file-exists? f)
						(delete-file f)))
					 (list scm-file
					       c-file
					       o-file
					       class-file
					       obj-file)))))
		     0))
	      (let ((port (open-input-file (caar sources))))
		 (if (not (input-port? port))
		     (error "" "Illegal file" (caar sources))
		     (let ((exp (compiler-read port)))
			(close-input-port port)
			(match-case exp
			   ((module ?name ??- (main ?new-main) . ?-)
			    (add-qualified-type!
			     name
			     (string-replace (jvm-class-sans-directory
					      (prefix (cdar sources)))
					     (file-separator)
					     #\.))
			    (if main
				(error ""
				       (string-append
					"Redeclaration of the main (files "
					fmain
					" and "
					(caar sources) ")")
				       (cons main new-main)))
			    (set! *link-main-module* name)
			    (loop (cdr sources)
				  (cons (list name
					      (string-append
					       "\"" (caar sources) "\""))
					cls)
				  new-main
				  (caar sources)
				  (append (find-libraries (cddr exp))
					  libraries)))
			   ((module ?name . ?-)
			    (add-qualified-type!
			     name
			     (string-replace (jvm-class-sans-directory
					      (prefix (cdar sources)))
					     (file-separator)
					     #\.))
			    (loop (cdr sources)
				  (cons (list name
					      (string-append
					       "\"" (caar sources) "\""))
					cls)
				  main
				  fmain
				  (append (find-libraries (cddr exp))
					  libraries)))
			   (else
			    ;; ah, ce n'etait pas un fichier bigloo,
			    ;; on saute (en meprisant :-)
			    (loop (cdr sources)
				  cls
				  main
				  fmain
				  libraries))))))))))

;*---------------------------------------------------------------------*/
;*    find-libraries ...                                               */
;*---------------------------------------------------------------------*/
(define (find-libraries clauses)
   (let loop ((clauses   clauses)
	      (libraries '()))
      (match-case clauses
	 (()
	  (reverse! libraries))
	 (((library . ?libs) . ?rest)
	  (loop rest (append libs libraries)))
	 (else
	  (loop (cdr clauses) libraries)))))

;*---------------------------------------------------------------------*/
;*    make-tmp-main ...                                                */
;*---------------------------------------------------------------------*/
(define (make-tmp-main clauses main libraries)
   (let ((pout (open-output-file *tmp-main-file-name*))
	 (generate-main? (and (memq *target-language* '(jvm .net))
			      (not main))))
      (if (not (output-port? pout))
	  (error ""
		 "Can't open output file"
		 *tmp-main-file-name*)
	  (begin
	     (fprint pout ";; " *bigloo-name*)
	     (fprint pout ";; !!! generated file, don't edit !!!")
	     (fprint pout ";; ==================================")
	     (newline pout)
	     (let* ((libs   (if (and #f (>fx *bdb-debug* 0))
				(cons 'bdb libraries)
				libraries))
		    (module `(module ,*link-module*
				(import ,@(reverse clauses))
				,@(if generate-main?
				      '((main main))
				      '())
				,@(if (pair? libs)
				      `((library ,@libs))
				      '()))))
		(fprint pout module)
		(newline pout))
	     (if generate-main?
		 (fprint pout '(define (main argv) #unspecified)))
	     (if main
		 (begin
		    (fprint pout "(main *the-command-line*)")
		    (newline pout)))
	     (set! *module* *link-module*)
	     (close-output-port pout)))))

