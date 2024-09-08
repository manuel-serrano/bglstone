(module backend_dotnet
   (include "Engine/pass.sch")
   (import engine_param
	   engine_pass
	   tools_error
	   module_module
	   type_type
	   backend_backend
	   read_jvm	; BAD module->qualified-type
	   backend_c_main	; BAD make-bigloo-main
	   cc_exec	; BAD exec
	   msil_compile
	   msil_ld
	   )
   (export ;(class dotnet::backend)
	   (build-dotnet-backend) ))

(define (build-dotnet-backend)
   (let ( (me (instantiate::dotnet (language '.Net))) )
      (dotnet-qname-set! me (module->qualified-type *module*))
      me ))

;*---------------------------------------------------------------------*/
;*    Compilation                                                      */
;*---------------------------------------------------------------------*/
(define-method (backend-compile me::dotnet)
   ;; the dotnet prelude (hello message and *DEST* update)
   (pass-prelude ".Net (saw)" start-dotnet-emission!)
   (verbose 2 "      [module: " *module* " qualified type name: "
	    (dotnet-qname me) "]"#\Newline)
   ;; if we are going to link and we have not found a main yet, we
   ;; have to produce a fake one
   (if (and (not *main*) (memq *pass* '(ld distrib)))
       (set! *main* (make-bigloo-main)))
   ;; the dotnet driver
   (define (emit dest)
      (let ((dir *dotnet-dir-name*))
	 (let ((port (if (not (string? dest))
			 (current-output-port)
			 (open-output-file (make-file-name dir dest)))))
	    (msil-compile me port)
	    (if (not (eq? port (current-output-port)))
		(close-output-port port)))))
   (let ((bname (cond
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
      ;; assembly code emission
      (cond
	 ((eq? *pass* 'il)
	  (emit bname))
	 (*dotnet-use-external-asm*
	  (emit bname)
	  (dotnet-external-asm (make-file-name *dotnet-dir-name* bname)))
	 (else
	  #unspecified)))
   (stop-on-pass 'il (lambda () 'done))
   (stop-on-pass 'cc (lambda () 'done))
   'ok )

(define *dotnet-dir-name* ".")

(define (addsuffix name)
   (string-append name ".il") )

(define (ilname cf)
   (match-case cf
      (((class ?name) . ?-)
       (addsuffix (symbol->string name)))))

;*---------------------------------------------------------------------*/
;*    start-dotnet-emission! ...                                       */
;*---------------------------------------------------------------------*/
(define (start-dotnet-emission!)
   (cond
      ((string? *dest*)
       (let ((dname (dirname *dest*)))
	  (if (not (string=? dname ""))
	      (set! *dotnet-dir-name* (dirname *dest*)))))
      ((eq? *pass* 'ld)
       (if (pair? *src-files*)
	   (set! *dotnet-dir-name* (dirname (car *src-files*))))))
   (if (not (and (file-exists? *dotnet-dir-name*)
		 (directory? *dotnet-dir-name*) ))
       (error "start-dotnet-emission!"
	      "Can't write dest file because directory doesn't exist"
	      *dotnet-dir-name*)
       #t))

;*---------------------------------------------------------------------*/
;*    *dotnet-external-asms* ...                                       */
;*---------------------------------------------------------------------*/
(define *dotnet-external-asms* 
   (list (cons 'pnet dotnet-external-pnet-asm)))

;*---------------------------------------------------------------------*/
;*    dotnet-external-asm ...                                          */
;*---------------------------------------------------------------------*/
(define (dotnet-external-asm name)
   (let* ((id *dotnet-external-asm-style*)
	  (c (assq id *dotnet-external-asms*)))
      (if (and (pair? c)
	       (procedure? (cdr c))
	       (correct-arity? (cdr c) 1))
	  ((cdr c) name)
	  (error "asm (dotnet)"
		 (apply string-append
			"Unknown linker style, supported: "
			(map (lambda (x)
				(if (and (pair? x)
					 (symbol? (car x)))
				    (string-append (symbol->string (car x))
						   " ")
				    ""))
			     *dotnet-external-asms*))
		 *dotnet-external-asm-style*))))

;*---------------------------------------------------------------------*/
;*    dotnet-external-pnet-asm ...                                     */
;*---------------------------------------------------------------------*/
(define (dotnet-external-pnet-asm name)
   (let ((cmd (string-append *dotnet-external-asm* " " (prefix name) ".il")))
      (verbose 1 "   . ilasm (" *dotnet-external-asm* ")" #\Newline)
      (verbose 2 "      ["  cmd #\] #\Newline)
      (exec cmd #t "ilasm")))
      
;*---------------------------------------------------------------------*/
;*    Link                                                             */
;*---------------------------------------------------------------------*/
(define-method (backend-link me::dotnet result)
   ;; CARE move the code here...
   (dotnet-ld) )
