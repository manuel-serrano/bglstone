;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Cc/ld.scm                   */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Jul 17 09:37:55 1992                          */
;*    Last change :  Sat Oct 30 05:50:35 2004 (serrano)                */
;*    Copyright   :  1992-2004 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The (system) link.                                               */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    Le module                                                        */
;*---------------------------------------------------------------------*/
(module cc_ld
   (export  (ld name ::bool)
	    (lib->string lib . su)
	    (library-suffix)
	    (user-lib-name libname))
   (import  tools_speek
	    tools_error
	    tools_misc
	    cc_exec
	    engine_param
	    engine_configure))

;*---------------------------------------------------------------------*/
;*    ld ...                                                           */
;*---------------------------------------------------------------------*/
(define (ld name need-to-return)
   (cond
      ((string=? (os-class) "unix")
       (unix-ld name need-to-return))
      ((string=? (os-class) "mingw")
       (mingw-ld name need-to-return))
      ((string=? (os-class) "win32")
       (win32-ld name))
      (else
       (user-error "ld" "Unknown os" (os-class)))))

;*---------------------------------------------------------------------*/
;*    decode-lib-name ...                                              */
;*---------------------------------------------------------------------*/
(define (decode-lib-name lib)
   (match-case lib
      ((? string?)
       (values lib #f))
      (((and ?base (? string?)) . (and ?ver (? string?)))
       (values base ver))
      (else
       (error "ld" "Illegal library name" lib))))

;*---------------------------------------------------------------------*/
;*    lib->string ...                                                  */
;*---------------------------------------------------------------------*/
(define (lib->string lib . su)
   (multiple-value-bind (base ver)
      (decode-lib-name lib)
      (cond
	 ((or (string=? (os-class) "unix")
	      (string=? (os-class) "mingw"))
	  (if ver
	      (string-append base (if (pair? su) (car su) "") "-" ver)
	      (string-append base (if (pair? su) (car su) ""))))
	 ((string=? (os-class) "win32")
	  (case *target-language*
	     ((c)
	      (string-append base (if (pair? su) (car su) "")))
	     ((.net)
	      (if ver
		  (string-append base (if (pair? su) (car su) "") "-" ver)
		  (string-append base (if (pair? su) (car su) ""))))
	     (else
	      (error "linker"
		     "Unimplemented target language"
		     *target-language*))))
	 (else
	  (user-error "ld" "Unknown os" (os-class))))))

;*---------------------------------------------------------------------*/
;*    lib+suffix ...                                                   */
;*---------------------------------------------------------------------*/
(define (lib+suffix lib static? force?)
   (let* ((n (lib->string lib))
	  (b (if (or (and force? static?)
		     (and (not force?)
			  (or static?
			      *static-bigloo?*
			      (not bgl-configure-shared-library-available?))))
		 (make-static-library-name n)
		 (make-shared-library-name n))))
      (cond
	 ((or (string=? (os-class) "unix")
	      (string=? (os-class) "mingw"))
	  (string-append "lib" b))
	 ((string=? (os-class) "win32")
	  b)
	 (else
	  (user-error "ld" "Unknown os" (os-class))))))

;*---------------------------------------------------------------------*/
;*    make-lib-name ...                                                */
;*---------------------------------------------------------------------*/
(define (make-lib-name::bstring lname ss::pair static? force? force-relative?)
   (if (or force-relative?
           (and *ld-relative*
                (not *profile-library*)
                (not (or static? *static-bigloo?*))))
       (string-append (cond
                        ((or (string=? (os-class) "unix")
			     (string=? (os-class) "mingw"))
                         "-l")
                        ((string=? (os-class) "win32")
                         "")
                        (else
                         (user-error "ld" "Unknown os" (os-class))))
                      (lib->string lname (car ss)))
       (let loop ((ss ss))
	  (if (null? ss)
	      (error "bigloo"
		     (string-append "Can't find library \""
				    (lib->string lname)
				    "\"")
		     *lib-dir*)
	      (let* ((fname (lib+suffix (lib->string lname (car ss))
					static?
					force?))
		     (name (find-file/path fname *lib-dir*)))
		 (if (string? name)
		     name
		     (begin
			(warning "bigloo"
				 (string-append "Can't find library \""
						fname
						"\"")
				 " in path \"" *lib-dir* "\""
				 (if (pair? (cdr ss))
				     (string-append
				      ".\nTrying replacement library \""
				      (lib+suffix
				       (lib->string lname (cadr ss))
				       static?
				       force?)	
				      "\".")
				     ""))
			(loop (cdr ss)))))))))

;*---------------------------------------------------------------------*/
;*    user-lib-name ...                                                */
;*---------------------------------------------------------------------*/
(define (user-lib-name libname)
   (let ((c (assoc libname *bigloo-libraries-translation-table*)))
      (match-case c
	 (#f libname)
	 ((?- . (? string?)) (cdr c))
	 ((?- . ((? string?) . (? string?))) (cdr c))
	 (else
	  (warning "make-library-name"
		   "Illegal table library name entry -- "
		   c)
	  libname))))

;*---------------------------------------------------------------------*/
;*    library-suffix ...                                               */
;*---------------------------------------------------------------------*/
(define (library-suffix)
   (cond
      (*profile-library* "_p")
      ((>fx *bdb-debug* 1) "_d")
      (*unsafe-library* "_u")
      (else "_s")))

;*---------------------------------------------------------------------*/
;*    secondary-library-suffix ...                                     */
;*---------------------------------------------------------------------*/
(define (secondary-library-suffix)
   (if (>fx *bdb-debug* 1) "_d" "_s"))
   
;*---------------------------------------------------------------------*/
;*    profile-gc-debug-library-suffix ...                              */
;*---------------------------------------------------------------------*/
(define (profile-gc-debug-library-suffix)
   (cond
      (*profile-library* "_p")
      ((>fx *bdb-debug* 1) "_d")
      (else "")))
   
;*---------------------------------------------------------------------*/
;*    ld ...                                                           */
;*    -------------------------------------------------------------    */
;*    Le link se fait avec plusieurs fichiers:                         */
;*       1- Le fichier .o resultat de la compilation courante          */
;*       2- Tous les .o qui ont ete passes en argument                 */
;*       3- Tous les .o qui correspondent aux fichiers presents dans   */
;*          les clauses `with' du main.                                */
;*---------------------------------------------------------------------*/
(define (unix-ld name need-to-return)
   (verbose 1 "   . ld (" *cc* ")" #\Newline)
   ;; we add additional, machine specific, link options.
   (let ((static? (string-case *ld-options*
		     ((: (* all) "-static")
		      #t)
		     (else
		      #f))))
      (if static?
	  (set! *ld-options* (string-append bgl-configure-static-link-option
					    " " *ld-options*))
	  (set! *ld-options* (string-append bgl-configure-shared-link-option
					    " " *ld-options*)))
      (let* ((dest        (if (string? *dest*)
			      *dest*
			      (default-executable-name)))
	     ;; the standard bigloo library
	     (bigloo-lib  (make-lib-name (user-lib-name *bigloo-vlib*)
					 (list (library-suffix)
					       (secondary-library-suffix))
					 static? #f #f))
	     ;; the garbarge collector libary
	     (gc-lib      (make-lib-name (user-lib-name *gc-lib*)
					 (list (profile-gc-debug-library-suffix) "")
					 (or *profile-library* static?)
					 #f
					 (not *gc-custom?*)))
	     ;; the extra bigloo libraries
	     (add-libs    (let loop ((lib (map user-lib-name
					       *additional-bigloo-libraries*))
				     (res  ""))
			     (if (null? lib)
				 res
				 (loop (cdr lib)
				       (string-append
					(make-lib-name
					 (car lib)
					 (list (library-suffix)
					       (secondary-library-suffix))
					 static?
					 #f
					 #f)
					" "
					res)))))
	     ;; the extra user C libraries
	     (other-libs  (let loop ((lib (reverse *bigloo-user-lib*))
				     (res ""))
			     (if (null? lib)
				 res
				 (loop (cdr lib)
				       (string-append (lib->string (car lib))
						      " " res)))))
	     (ld-args     (string-append
			   ;; object file name
			   name "." *c-object-file-extension* " "
			   ;; to be linked with files
			   (string*->string *with-files*)
			   ;; other object files
			   (string*->string *o-files*)
			   ;; the executable name
			   " " *ld-o-option* dest
			   ;; cc options
			   " "  *cc-options*
			   ;; optional debug option
			   (if (or *c-debug* (>fx *bdb-debug* 0))
			       (string-append " " *ld-debug-option*)
			       "")
			   ;; optional executable stripping
			   (if *strip*
			       (string-append " " bgl-configure-c-strip-flag)
			       "")
			   ;; user ld options
			   " " *ld-options*
			   ;; the library path
			   (let loop ((path *lib-dir*))
			      (if (null? path)
				  ""
				  (string-append "-L"
						 (car path)
						 " "
						 (loop (cdr path)))))
                           ;; ld optimization flags
                           (if (not *c-debug*)
                               (string-append " " *ld-optim-flags*)
                               "")
			   ;; additional Bigloo libaries
			   " " add-libs
			   ;; standard bigloo library
			   " " bigloo-lib
			   ;; standard GC library
			   " " gc-lib
			   ;; dloptn library
			   " " bgl-configure-dlopen-lib
			   ;; user libraries
			   " " other-libs
			   ;; then we insert a second time the additional libs
			   " " (if *double-ld-libs?* add-libs "")
			   ;; post user ld options
			   " " *ld-post-options*))
	     (cmd         (string-append *cc* " " ld-args)))
	 (verbose 2 "      ["  cmd #\] #\Newline)
	 (exec cmd need-to-return "ld"))))

;*---------------------------------------------------------------------*/
;*    ld ...                                                           */
;*    -------------------------------------------------------------    */
;*    Le link se fait avec plusieurs fichiers:                         */
;*       1- Le fichier .o resultat de la compilation courante          */
;*       2- Tous les .o qui ont ete passes en argument                 */
;*       3- Tous les .o qui correspondent aux fichiers presents dans   */
;*          les clauses `with' du main.                                */
;*---------------------------------------------------------------------*/
(define (mingw-ld name need-to-return)
   (verbose 1 "   . ld (" *cc* ")" #\Newline)
   ;; we add additional, machine specific, link options.
   (let ((static? (string-case *ld-options*
		     ((: (* all) "-static")
		      #t)
		     (else
		      #f))))
      (if static?
	  (set! *ld-options* (string-append bgl-configure-static-link-option
					    " " *ld-options*))
	  (set! *ld-options* (string-append bgl-configure-shared-link-option
					    " " *ld-options*)))
      (let* ((dest        (if (string? *dest*)
			      *dest*
			      (default-executable-name)))
	     ;; the standard bigloo library
	     (bigloo-lib  (make-lib-name (user-lib-name *bigloo-vlib*)
					 (list (library-suffix)
					       (secondary-library-suffix))
					 static? #f #f))
	     ;; the garbarge collector libary
	     (gc-lib      (make-lib-name (user-lib-name *gc-lib*)
					 (list (profile-gc-debug-library-suffix) "")
					 (or *profile-library* static?)
					 #f
					 (not *gc-custom?*)))
	     ;; the extra bigloo libraries
	     (add-libs    (let loop ((lib (map user-lib-name
					       *additional-bigloo-libraries*))
				     (res  ""))
			     (if (null? lib)
				 res
				 (loop (cdr lib)
				       (string-append
					(make-lib-name
					 (car lib)
					 (list (library-suffix)
					       (secondary-library-suffix))
					 static?
					 #f
					 #f)
					" "
					res)))))
	     ;; the extra user C libraries
	     (other-libs  (let loop ((lib (reverse *bigloo-user-lib*))
				     (res ""))
			     (if (null? lib)
				 res
				 (loop (cdr lib)
				       (string-append (lib->string (car lib))
						      " " res)))))
	     (ld-args     (string-append
			   ;; object file name
			   name "." *c-object-file-extension* " "
			   ;; to be linked with files
			   (string*->string *with-files*)
			   ;; other object files
			   (string*->string *o-files*)
			   ;; the executable name
			   " " *ld-o-option* dest
			   ;; cc options
			   " "  *cc-options*
			   ;; optional debug option
			   (if (or *c-debug* (>fx *bdb-debug* 0))
			       (string-append " " *ld-debug-option*)
			       "")
			   ;; optional executable stripping
			   (if *strip*
			       (string-append " " bgl-configure-c-strip-flag)
			       "")
			   ;; user ld options
			   " " *ld-options*
			   ;; the library path
			   (let loop ((path *lib-dir*))
			      (if (null? path)
				  ""
				  (string-append "-L"
						 (car path)
						 " "
						 (loop (cdr path)))))
                           ;; ld optimization flags
                           (if (not *c-debug*)
                               (string-append " " *ld-optim-flags*)
                               "")
			   ;; additional Bigloo libaries
			   " " add-libs
			   ;; standard bigloo library
			   " " bigloo-lib
			   ;; standard GC library
			   " " gc-lib
			   ;; dloptn library
			   " " bgl-configure-dlopen-lib
			   ;; user libraries
			   " " other-libs
			   ;; then we insert a second time the additional libs
			   " " (if *double-ld-libs?* add-libs "")
			   ;; post user ld options
			   " " *ld-post-options*))
	     (cmd         (string-append *cc* " " ld-args)))
	 (verbose 2 "      ["  cmd #\] #\Newline)
	 (print "ld-mingw: " cmd)
	 (exec cmd need-to-return "ld"))))

;*---------------------------------------------------------------------*/
;*    ld ...                                                           */
;*    -------------------------------------------------------------    */
;*    Le link se fait avec plusieurs fichiers:                         */
;*       1- Le fichier .o resultat de la compilation courante          */
;*       2- Tous les .o qui ont ete passes en argument                 */
;*       3- Tous les .o qui correspondent aux fichiers presents dans   */
;*          les clauses `with' du main.                                */
;*---------------------------------------------------------------------*/
(define (win32-ld name)
   (verbose 1 "   . ld (" *cc* ")" #\Newline)
   ;; we add additional, machine specific, link options.
   (let ((static? (string-case *ld-options*
		     ((: (* all) "-static")
		      #t)
		     (else
		      #f))))
      (if static?
	  (set! *ld-options* (string-append bgl-configure-static-link-option
					    " " *ld-options*))
	  (set! *ld-options* (string-append bgl-configure-shared-link-option
					    " " *ld-options*)))
      (let* ((dest (if (string? *dest*)
                       *dest*
                       (default-executable-name)))
	     ;; the standard bigloo library
	     (bigloo-lib (make-static-library-name (string-append "bigloo"
                                                                  (if *multi-threaded-gc?* "_mt" "")
                                                                  (library-suffix))))
	     ;; the extra bigloo libraries
	     (add-libs    (let loop ((lib (map user-lib-name
                                               *additional-bigloo-libraries*))
				     (res '()))
			     (if (null? lib)
				 res
				 (loop (cdr lib)
				       (cons  (normalize-win32-lib-name
                                               (make-lib-name
                                                (car lib)
                                                (list (library-suffix)
                                                      (secondary-library-suffix))
                                                static?
                                                #f
                                                #f))
					      res)))))
	     ;; the extra user C libraries
	     (other-libs  (let loop ((lib (reverse *bigloo-user-lib*))
				     (res '()))
			     (if (null? lib)
				 res
				 (loop (cdr lib)
				       (cons (lib->string (car lib))
                                             res)))))
	     (ld-args     (append
			   ;; object file name
                           (list (string-append name
						"."
						*c-object-file-extension*))
			   ;; to be linked with files
                           *with-files*
			   ;; other object files
                           *o-files*
			   ;; the executable name
                           (list (string-append *ld-o-option* dest))
			   ;; cc options
                           (string-split *cc-options* #\space)
                           ;; linker options
                           (list "/link")
			   ;; optional debug option
                           (if (or *c-debug* (>fx *bdb-debug* 0))
			       (string-split *ld-debug-option* #\space)
			       '())
                           ;; ld optimization flags
                           (if (not *c-debug*)
                               (string-split *ld-optim-flags* #\space)
                               '())
			   ;; optional executable stripping
                           (if (and *strip*
				    (not (string=? bgl-configure-c-strip-flag "")))
			       (list bgl-configure-c-strip-flag)
			       '())
			   ;; user ld options
                           (string-split *ld-options* #\space)
			   ;; the library path
                           (let loop ((path *lib-dir*))
			      (if (null? path)
				  '()
				  (cons (string-append "/LIBPATH:" (car path))
                                        (loop (cdr path)))))
			   ;; additional Bigloo libaries
			   add-libs
			   ;; standard bigloo library
                           (list bigloo-lib)
			   ;; dloptn library
			   (if (string=? bgl-configure-dlopen-lib "")
                               '()
                               (list bgl-configure-dlopen-lib))
			   ;; user libraries
			   other-libs
			   ;; then we insert a second time the additional libs
			   (if *double-ld-libs?* add-libs '())
			   ;; post user ld options
			   (string-split *ld-post-options* #\space))))
	 (verbose 2 "      " (map (lambda (str)
				     (string-append "[" str "]"))
				  (cons *cc* ld-args))
		  #\Newline)
	 (apply run-process *cc* (append ld-args '(wait: #t))))))

;*---------------------------------------------------------------------*/
;*    normalize-win32-lib-name ...                                     */
;*    -------------------------------------------------------------    */
;*    Ensures that library names are suffixes by ".lib".               */
;*---------------------------------------------------------------------*/
(define (normalize-win32-lib-name lib-name)
  (if (not (string=? (suffix lib-name) ".lib"))
      (string-append lib-name ".lib")
      lib-name))
