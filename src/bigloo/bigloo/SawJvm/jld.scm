;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/SawJvm/jld.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Oct 24 10:32:46 2000                          */
;*    Last change :  Fri Nov  5 15:34:29 2004 (serrano)                */
;*    Copyright   :  2000-04 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The pseudo Jvm link (generation of a script shell that will run  */
;*    the application).                                                */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module saw_jvm_ld
   (import engine_param
	   engine_configure
	   engine_link
	   tools_speek
	   tools_error
	   tools_misc
	   read_reader
	   module_module
	   backend_cplib
	   ast_var
	   ast_node
	   type_type
	   read_jvm
	   type_type
	   object_class
	   object_slots)
   (export (jvm-ld)))

;*---------------------------------------------------------------------*/
;*    jvm-ld ...                                                       */
;*---------------------------------------------------------------------*/
(define (jvm-ld)
   (let* ((target (if (string? *dest*)
		      *dest*
		      (default-script-name)))
	  (jarname (string-append (prefix target) ".jar"))
	  (zips (append (map library->zip *additional-bigloo-libraries*)
			*additional-bigloo-zips*)))
      (if *jvm-jar?*
	  (let* ((manifest (make-manifest-name))
		 (o-files (append (map source->jvm-class *src-files*)
				  *o-files*))
		 (all-objects (unique (objects->classes o-files))))
	     (verbose 1 "   . jar")
	     (verbose 2 " (" jarname ")")
	     (verbose 1 #\Newline)
	     (generate-jvm-manifest manifest
				    (find-jvm-main o-files)
				    jarname
				    zips)
	     (jvm-jar jarname manifest all-objects)))
      (generate-jvm-script target
			   (find-jvm-mainclass)
			   jarname
			   zips)))

;*---------------------------------------------------------------------*/
;*    source->jvm-class ...                                            */
;*---------------------------------------------------------------------*/
(define (source->jvm-class s)
   (let ((q (source->qualified-type s)))
      (jvm-class-with-directory
       (if (string? q)
	   (string-append (string-replace! q #\. #\/) ".class")
	   (string-append (prefix s) ".class")))))
   
;*---------------------------------------------------------------------*/
;*    find-jvm-mainclass ...                                           */
;*---------------------------------------------------------------------*/
(define (find-jvm-mainclass)
   (cond
      ((string? *jvm-mainclass*)
       *jvm-mainclass*)
      (*main*
       (module->qualified-type *module*))
      ((symbol? *link-main-module*)
       (module->qualified-type *link-main-module*))
      (else
       (error "ld"
	      "Can't established JVM main class"
	      "see option -jvm-mainclass"))))

;*---------------------------------------------------------------------*/
;*    library->zip ...                                                 */
;*---------------------------------------------------------------------*/
(define (library->zip lib)
   (let* ((dir (if (string=? *jvm-shell* "msdos")
		   (cons (jvm-bigloo-classpath) *lib-dir*)
		   *lib-dir*))
	  (f (find-file/path (string-append lib "_s.zip") dir)))
      (if (string? f)
	  f
	  (begin
	     (warning "ld:" "Can't find zip file for library -- " lib)
	     #f))))
       
;*---------------------------------------------------------------------*/
;*    find-jvm-main ...                                                */
;*---------------------------------------------------------------------*/
(define (find-jvm-main o-files)
   (if (global? *main*)
       (prefix (car *src-files*))
       (let loop ((o-files o-files))
	  (if (null? o-files)
	      (error "jar" "No main clause found" o-files)
	      (let* ((pref (unprof-src-name (prefix (car o-files))))
		     (bpref    (basename pref))
		     (scm-file (find-src-file pref bpref)))
		 (if (or (not (string? scm-file))
			 (not (file-exists? scm-file)))
		     (loop (cdr o-files))
		     (or (with-input-from-file scm-file
			    (lambda ()
			       (match-case (compiler-read)
				  ((module ?mod ??- (main ?-) . ?-)
				   (module->qualified-type mod))
				  (else #f) )))
			 (loop (cdr o-files)))))))))

;*---------------------------------------------------------------------*/
;*    objects->classes ...                                             */
;*---------------------------------------------------------------------*/
(define (objects->classes objects)
   (define (untype-ident id)
      (let* ((string (symbol->string id))
	     (len    (string-length string)))
	 (let loop ((walker  0))
	    (cond
	       ((=fx walker len)
		id)
	       ((and (char=? (string-ref string walker) #\:)
		     (<fx walker (-fx len 1))
		     (char=? (string-ref string (+fx walker 1)) #\:))
		(string->symbol (substring string 0 walker)))
	       (else
		(loop (+fx walker 1)))))))
   (define (find-classes::pair-nil mod::symbol base clauses)
      (let loop ((clauses clauses)
		 (classes '()))
	 (if (null? clauses)
	     classes
	     (match-case (car clauses)
		(((or export static) . ?statexp)
		 (let liip ((statexp statexp)
			    (classes classes))
		    (if (null? statexp)
			(loop (cdr clauses)
			      classes)
			(match-case (car statexp)
			   (((or class abstract-class final-class wide-class)
			     ?ident . ?-)
			    (let* ((id (untype-ident ident))
				   (mgl (class-id->type-name id mod)))
			       (liip (cdr statexp)
				     (cons (make-file-name
					    base
					    (string-append mgl ".class"))
					   classes))))
			   (else
			    (liip (cdr statexp)
				  classes))))))
		(else
		 (loop (cdr clauses) classes))))))
   (define (module->package mod)
      (let ((d (dirname
		(string-replace! (module->qualified-type mod)
				 #\.
				 (file-separator)))))
	 (if (string=? d ".")
	     ""
	     d)))
   (define (source->classes::pair-nil source)
      (if (and (not (string=? (suffix source) "mco"))
	       (file-exists? source))
	  (with-input-from-file source
	     (lambda ()
		(bind-exit (return)
		   (let ((m (with-exception-handler
			       (lambda (_) (return '()))
			       (lambda () (read)))))
		      (match-case m
			 ((module ?mod . ?clauses)
			  (find-classes mod
					(jvm-class-with-directory
					 (module->package mod))
					clauses))
			 (else
			  '()))))))
	  '()))
   (let loop ((objects objects)
	      (classes '()))
      (if (null? objects)
	  classes
	  (let* ((object (car objects))
		 (pref (unprof-src-name (prefix object)))
		 (bpref (basename pref))
		 (scm-file (find-src-file pref bpref)))
	     (if (and (string? scm-file) (file-exists? scm-file))
		 (loop (cdr objects)
		       (cons object
			     (append (source->classes scm-file) classes)))
		 (loop (cdr objects)
		       (cons object classes)))))))

;*---------------------------------------------------------------------*/
;*    unique ...                                                       */
;*---------------------------------------------------------------------*/
(define (unique lst)
   (let ((t (make-hashtable)))
      (for-each (lambda (f) (hashtable-put! t f f)) lst)
      (hashtable->list t)))
      
;*---------------------------------------------------------------------*/
;*    jvm-jar ...                                                      */
;*---------------------------------------------------------------------*/
(define (jvm-jar target manifest objects)
   (let ((cmd (if (string? *jvm-directory*)
		  (let loop ((objects objects)
			     (cmd ""))
		     (if (null? objects)
			 (string-append bgl-configure-jar " " 
					manifest " "
					target " " 
					cmd)
			 (loop (cdr objects)
			       (string-append "-C "
					      *jvm-directory* " "
					      (jvm-class-sans-directory (car objects)) " " cmd))))
		  (let loop ((objects objects)
			     (cmd ""))
		     (if (null? objects)
			 (string-append bgl-configure-jar " " 
					manifest " "
					target " " 
					cmd)
			 (loop (cdr objects)
			       (string-append (car objects) " " cmd)))))))
		  
      (verbose 2 "      [" cmd #"]\n")
      (unwind-protect
	 (if (not (=fx (system cmd) 0))
	     (error bgl-configure-jar "Can't create jar file" target)
	     #t)
	 (delete-file manifest))))

;*---------------------------------------------------------------------*/
;*    make-manifest-name ...                                           */
;*---------------------------------------------------------------------*/
(define (make-manifest-name)
   (let loop ((name "Manifest"))
      (if (not (file-exists? name))
	  name
	  (loop (string-append name "X")))))

;*---------------------------------------------------------------------*/
;*    jvm-bigloo-classpath ...                                         */
;*---------------------------------------------------------------------*/
(define (jvm-bigloo-classpath)
   (uncygdrive
    (if (string? *jvm-bigloo-classpath*)
	*jvm-bigloo-classpath*
	bgl-configure-zip-directory)))

;*---------------------------------------------------------------------*/
;*    jvm-jarpath ...                                                  */
;*---------------------------------------------------------------------*/
(define (jvm-jarpath path)
   (if (string? *jvm-jarpath*)
       *jvm-jarpath*
       path))

;*---------------------------------------------------------------------*/
;*    generate-jvm-manifest ...                                        */
;*---------------------------------------------------------------------*/
(define (generate-jvm-manifest fname::bstring main jarname::bstring zips)
   (cond
      ((string=? *jvm-shell* "sh")
       (generate-sh-jvm-manifest fname main zips))
      ((string=? *jvm-shell* "msdos")
       (generate-msdos-jvm-manifest fname main jarname))
      (else
       (warning "generate-jvm-manifest"
		"Illegal shell `" *jvm-shell* "' -- using `sh'")
       (generate-sh-jvm-manifest fname main zips))))

;*---------------------------------------------------------------------*/
;*    generate-sh-jvm-manifest ...                                     */
;*---------------------------------------------------------------------*/
(define (generate-sh-jvm-manifest fname main zips)
   (with-output-to-file fname
      (lambda ()
	 (print "Manifest-Version: 1.0")
	 (print "Main-Class: " main)
	 (display* "Class-Path: " *jvm-classpath* " ")
	 (display (make-file-name (jvm-bigloo-classpath)
				  (if (and *unsafe-library* (not *purify*))
				      "bigloo_u.zip"
				      "bigloo_s.zip")))
	 (for-each (lambda (l)
		      (display " ")
		      (display (user-library l)))
		   zips)
	 (newline)
	 (print "Created-By: " *bigloo-name*)
	 (newline))))

;*---------------------------------------------------------------------*/
;*    generate-msdos-jvm-manifest ...                                  */
;*---------------------------------------------------------------------*/
(define (generate-msdos-jvm-manifest fname main jarname)
   (with-output-to-file fname
      (lambda ()
	 (print "Manifest-Version: 1.0")
	 (print "Main-Class: " main)
	 (print "Created-By: " *bigloo-name*)
	 (newline))))

;*---------------------------------------------------------------------*/
;*    generate-jvm-sh-script ...                                       */
;*---------------------------------------------------------------------*/
(define (generate-jvm-sh-script target main-class zips)
   (define (quotify str)
      (string-append "'" str "'"))
   (define (generate-jvm-env)
      (let loop ((env *jvm-env*)
		 (res ""))
	 (if (null? env)
	     res
	     (loop (cdr env)
		   (string-append "-Dbigloo." (car env) "=$" (car env)
				  " " res)))))
   (define (generate-jvm-jar-script)
      (with-output-to-file target
	 (lambda ()
	    (print "#!/bin/sh")
	    (newline)
	    (print "CLASSPATH=\""
		   (list->sh-path-string
		    `(,*jvm-classpath* "$BUGLOOCLASSPATH"))
		   "\"")
	    (print "export CLASSPATH")
	    (newline)
	    (print "exec " *jvm-java* " "
		   (if (not *purify*) bgl-configure-jvflags "")
		   " $BIGLOOJAVAOPT $BUGLOOJAVAOPT " bgl-configure-jflags
		   " -jar "
		   *jvm-options* " "
		   (generate-jvm-env)
		   (if (not (string=? bgl-configure-dirname-cmd ""))
		       (make-file-name (string-append "`"
						      bgl-configure-dirname-cmd
						      " $0`")
				       (prefix (basename target)))
		       (prefix target))
		   ".jar $*")))
      (chmod target 'read 'write 'execute))
   (define (generate-jvm-class-script)
      (with-output-to-file target
	 (lambda ()
	    (print "#!/bin/sh")
	    (newline)
	    (print "CLASSPATH="
		   (list->sh-path-string
		    `(,*jvm-classpath*
		      "$BIGLOOCLASSPATH"
		      "$BUGLOOCLASSPATH"
		      ,(quotify
			(make-file-name (jvm-bigloo-classpath)
					(if (and *unsafe-library*
						 (not *purify*))
					    "bigloo_u.zip"
					    "bigloo_s.zip")))
		      ,@(map (lambda (f) (quotify (user-library f))) zips))))
	    (print "export CLASSPATH")
	    (newline)
	    (print "exec " *jvm-java* " "
		   (if (not *purify*) bgl-configure-jvflags "")
		   " $BIGLOOJAVAOPT $BUGLOOJAVAOPT "
		   bgl-configure-jflags
		   " "
		   *jvm-options* " "
		   (generate-jvm-env)
		   (string-replace! main-class (file-separator) #\.)
		   " $*")))
      (chmod target 'read 'write 'execute))
   (if *jvm-jar?*
       (generate-jvm-jar-script)
       (generate-jvm-class-script)))

;*---------------------------------------------------------------------*/
;*    generate-jvm-msdos-script ...                                    */
;*---------------------------------------------------------------------*/
(define (generate-jvm-msdos-script target main-class jarname zips)
   (define (generate-jvm-env)
      (let loop ((env *jvm-env*)
		 (res ""))
	 (if (null? env)
	     res
	     (loop (cdr env)
		   (string-append "-Dbigloo." (car env) "=$" (car env)
				  " " res)))))
   (define (generate-jvm-jar-script)
      (with-output-to-file target
	 (lambda ()
	    (print "@" *jvm-java* " "
		   bgl-configure-jflags " "
		   (if (not *purify*) bgl-configure-jvflags "")
		   " -cp \""
		   (list->msdos-path-string
		    `(,*jvm-classpath*
		      "%BUGLOOCLASSPATH%"
		      ,(string-append (jvm-jarpath (dirname jarname))
				      "\\"
				      (basename jarname))
		      ,(string-append (string-replace
				       (jvm-bigloo-classpath)
				       #\/ #\\)
				      (if *unsafe-library*
					  "\\bigloo_u.zip"
					  "\\bigloo_s.zip"))
		      ,@(map (lambda (x)
				(string-replace! (user-library x) #\/ #\\))
			     zips)))
                   "\" "
		   *jvm-options* " "
		   (generate-jvm-env)
		   " %BIGLOOJAVAOPT% %BUGLOOJAVAOPT% "
		   (string-replace! main-class (file-separator) #\.)
		   " %*")))
      (chmod target 'read 'write 'execute))
   (define (generate-jvm-class-script)
      (with-output-to-file target
	 (lambda ()
	    (print "@" *jvm-java* " "
		   bgl-configure-jflags " "
		   (if (not *purify*) bgl-configure-jvflags "")
		   " "
		   *jvm-options*
		   " -cp \""
		   (list->msdos-path-string
		    `(,*jvm-classpath*
		      "%BUGLOOCLASSPATH%"
		      ,(string-append (string-replace
				       (jvm-bigloo-classpath)
				       #\/ #\\)
				      (if *unsafe-library*
					  "\\bigloo_u.zip"
					  "\\bigloo_s.zip"))
		      ,@(map (lambda (x)
				(string-replace! (user-library x) #\/ #\\))
			     zips)))
                   "\" "
		   (generate-jvm-env)
		   " %BIGLOOJAVAOPT% %BUGLOOJAVAOPT% "
		   (string-replace! main-class (file-separator) #\.)
		   " %*")))
      (chmod target 'read 'write 'execute))
   (if *jvm-jar?*
       (generate-jvm-jar-script)
       (generate-jvm-class-script)))

;*---------------------------------------------------------------------*/
;*    generate-jvm-script ...                                          */
;*---------------------------------------------------------------------*/
(define (generate-jvm-script target main-class jarname zips)
   (verbose 1 "   . " *jvm-shell*)
   (verbose 2 " (" target ")")
   (verbose 1 #\Newline)
   (cond
      ((string=? *jvm-shell* "sh")
       (generate-jvm-sh-script target main-class zips))
      ((string=? *jvm-shell* "msdos")
       (generate-jvm-msdos-script target main-class jarname zips))
      (else
       (warning "generate-jvm-script"
		"Illegal shell `" *jvm-shell* "' -- using `sh'")
       (generate-jvm-sh-script target main-class zips))))

;*---------------------------------------------------------------------*/
;*    user-library ...                                                 */
;*---------------------------------------------------------------------*/
(define (user-library lib)
   (define (relative-name? lib)
      (let ((len (string-length lib)))
	 (let loop ((i 0))
	    (cond
	       ((=fx i len)
		#f)
	       ((char=? (string-ref lib i) (file-separator))
		#t)
	       (else
		(loop (+fx i 1)))))))
   (if (relative-name? lib)
       lib
       (make-file-name (jvm-bigloo-classpath) lib)))

;*---------------------------------------------------------------------*/
;*    list->path-string ...                                            */
;*---------------------------------------------------------------------*/
(define (list->path-string path separator)
   (let ((rpath (reverse path)))
      (let loop ((path (cdr rpath))
		 (res (car rpath)))
	 (if (null? path)
	     res
	     (loop (cdr path) (string-append (car path) separator res))))))

;*---------------------------------------------------------------------*/
;*    list->sh-path-string ...                                         */
;*---------------------------------------------------------------------*/
(define (list->sh-path-string path)
   (list->path-string path
		      (if (string? *jvm-path-separator*)
			  *jvm-path-separator*
			  ":")))

;*---------------------------------------------------------------------*/
;*    list->msdos-path-string ...                                      */
;*---------------------------------------------------------------------*/
(define (list->msdos-path-string path)
   (list->path-string path
		      (if (string? *jvm-path-separator*)
			  *jvm-path-separator*
			  ";")))
