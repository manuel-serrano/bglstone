;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Cc/cc.scm                   */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Apr 29 09:51:32 1995                          */
;*    Last change :  Wed Mar 24 13:57:12 2004 (serrano)                */
;*    Copyright   :  1995-2004 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The C compilation                                                */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module cc_cc
   (export  (cc name oname ::bool))
   (import  tools_speek
	    tools_error
	    cc_exec
	    engine_param
	    tools_misc))

;*---------------------------------------------------------------------*/
;*    cc ...                                                           */
;*---------------------------------------------------------------------*/
(define (cc name oname need-to-return)
   (cond
      ((string=? (os-class) "unix")
       (unix-cc name oname need-to-return))
      ((string=? (os-class) "win32")
       (win32-cc name oname))
      ((string=? (os-class) "mingw")
       (mingw-cc name oname need-to-return))
      (else
       (user-error "cc" "Unknown os" (os-class)))))

;*---------------------------------------------------------------------*/
;*    unix-cc ...                                                      */
;*---------------------------------------------------------------------*/
(define (unix-cc name oname need-to-return)
   (verbose 1 "   . cc (" *cc* ")" #\Newline)
   (cond
      ((not (string? name))
       (error "cc" "can't process cc on stdout" name))
      (else
       (let* ((oname (if (string? oname) oname name))
	      (dest-obj (if *cc-move* ""
			    (string-append " " *cc-o-option* oname "."
					   *c-object-file-extension* " ")))
	      (cc (string-append *cc*
				 " "
				 *cc-options*
				 " "
				 *cflags*
				 " -c "
				 dest-obj
				 " -I. "
				 (let loop ((path *lib-dir*))
				    (if (null? path)
					""
					(string-append "-I"
						       (car path)
						       " "
						       (loop (cdr path)))))
				 (if (or *c-debug* (>fx *bdb-debug* 0))
				     (string-append " " *c-debug-option*)
				     "")
				 (if (>fx *heap-debug* 0)
				     (string-append " " *heap-debug-copt*)
				     "")
				 " " name ".c "))
	      (basename (basename oname))
	      (rm-csrc  (if *rm-tmp-files*
			    (string-append "&& /bin/rm -f " name ".c ")
			    ""))
	      (mv-obj   (if (and *cc-move*
				 (not (string=? basename oname))
				 (not (string=? (pwd) (dirname oname)))
				 (not (string=? "." (dirname oname))))
			    (string-append "&& /bin/mv "
					   basename "."
					   *c-object-file-extension*
					   " "
					   oname "."
                                           *c-object-file-extension*
					   " 2>&1 >/dev/null ")
			    ""))
	      (cmd      (string-append cc mv-obj rm-csrc)))
	  (verbose 2 "      [" cmd #\] #\Newline)
	  (exec cmd need-to-return "cc")))))

;*---------------------------------------------------------------------*/
;*    mingw-cc ...                                                      */
;*---------------------------------------------------------------------*/
(define (mingw-cc name oname need-to-return)
   (verbose 1 "   . cc (" *cc* ")" #\Newline)
   (print "libdir:" *lib-dir* " - " *default-lib-dir*)
   (cond
      ((not (string? name))
       (error "cc" "can't process cc on stdout" name))
      (else
       (let* ((oname (if (string? oname) oname name))
	      (dest-obj (if *cc-move* ""
			    (string-append " " *cc-o-option* oname "."
					   *c-object-file-extension* " ")))
	      (cc (string-append *cc*
				 " "
				 *cc-options*
				 " "
				 *cflags*
				 " -c "
				 dest-obj
				 " -I. "
				 (let loop ((path *lib-dir*))
				    (if (null? path)
					""
					(string-append "-I"
						       (car path)
						       " "
						       (loop (cdr path)))))
				 (if (or *c-debug* (>fx *bdb-debug* 0))
				     (string-append " " *c-debug-option*)
				     "")
				 (if (>fx *heap-debug* 0)
				     (string-append " " *heap-debug-copt*)
				     "")
				 " " name ".c "))
	      (basename (basename oname))
	      (rm-csrc  "")
	      ;(if *rm-tmp-files*
	      ;    (string-append "&& /bin/rm -f " name ".c ")
	      ;    ""))
	      (mv-obj   "")
	      ;(if (and *cc-move*
	      ;	 (not (string=? basename oname))
	      ;	 (not (string=? (pwd) (dirname oname)))
	      ;	 (not (string=? "." (dirname oname))))
	      ;    (string-append "&& move /Y "
	      ;		   basename "."
	      ;		   *c-object-file-extension*
	      ;		   " "
	      ;		   oname "."
	      ;                   *c-object-file-extension*
	      ;		   " 2>&1 >NUL ")
	      ;    ""))
	      (cmd      (string-append cc mv-obj rm-csrc)))
	  (print cmd)
	  (verbose 2 "      [" cmd #\] #\Newline)
	  (exec cmd need-to-return "cc")
	  (if (and *cc-move*
		   (not (string=? basename oname))
		   (not (string=? (pwd) (dirname oname)))
		   (not (string=? "." (dirname oname))))
	      (rename-file (string-append basename "." *c-object-file-extension*) 
			   (string-append oname "." *c-object-file-extension*)))
	  (if *rm-tmp-files* 
	      (delete-file (string-append name ".c ")))))))


;*---------------------------------------------------------------------*/
;*    win32-cc ...                                                     */
;*---------------------------------------------------------------------*/
(define (win32-cc name oname)
   (verbose 1 "   . cc (" *cc* ")" #\Newline)
   (cond
      ((not (string? name))
       (error "cc" "can't process cc on stdout" name))
      (else
       (let* ((oname (if (string? oname) oname name))
	      (cc-args (append (string-split *cc-options* #\space)
			       (string-split *cflags* #\space)
			       '("-c")
			       '("-I.")
			       (let loop ((path *lib-dir*))
				  (if (null? path)
				      '()
				      (cons (string-append "-I" (car path))
					    (loop (cdr path)))))
			       (if (or *c-debug* (>fx *bdb-debug* 0))
				   (string-split *c-debug-option* #\space)
				   '())
			       (if (>fx *heap-debug* 0)
				   (string-split *heap-debug-copt* #\space)
				   '())
			       (list (string-append name ".c"))
			       (if (char=? (string-ref
					    *cc-o-option*
					    (- (string-length *cc-o-option*) 1))
					   #\space)
				   (list *cc-o-option*
					 (string-append
					  oname "."
					  *c-object-file-extension*))
				   (list (string-append
					  *cc-o-option*
					  oname "."
					  *c-object-file-extension*))))))
	  (verbose 2 "      " (cons *cc* cc-args) #\Newline)
	  (apply run-process *cc* (append cc-args '(wait: #t)))
	  (if *rm-tmp-files*
	      (delete-file (string-append name ".c ")))))))
