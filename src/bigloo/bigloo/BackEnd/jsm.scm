(module backend_jsm
   (include "Engine/pass.sch")
   (import engine_param
	   engine_pass
	   tools_error
	   module_module
	   type_type
	   backend_backend
	   read_jvm	; BAD module->qualified-type
	   backend_c_main	; BAD make-bigloo-main
	   jsm_compile
	   )
   (export ;(class jsm::backend)
	   (build-jsm-backend) ))

(define (build-jsm-backend)
   (let ( (me (instantiate::jsm (language 'jsm))) )
      (jsm-qname-set! me (module->qualified-type *module*))
      me ))

(define-method (backend-compile me::jsm)
   ;; the jsm prelude (hello message and *DEST* update)
   (pass-prelude "Jsm" start-jsm-emission!)
   (verbose 2 "      [module: " *module* " qualified type name: "
	    (module->qualified-type *module*) "]"#\Newline)
   ;; if we are going to link and we have not found a main yet, we
   ;; have to produce a fake one
   (if (and (not *main*) (memq *pass* '(ld distrib)))
       (set! *main* (make-bigloo-main)))
   ;; the jsm driver
   ;; the dotnet driver
   (define (emit dest)
      (let ((dir *jsm-dir-name*))
	 (let ((port (if (not (string? dest))
			 (current-output-port)
			 (open-output-file (make-file-name dir dest)))))
	    (jsm-compile me port)
	    (if (not (eq? port (current-output-port)))
		(close-output-port port)))))
   ;;
   (stop-on-pass 'cc (lambda () 'done))
   (stop-on-pass 'jsmas (lambda () 'done))
   (stop-on-pass 'jast (lambda () 'done)) )


(define *jsm-dir-name* ".")

;*---------------------------------------------------------------------*/
;*    start-jsm-emission! ...                                          */
;*---------------------------------------------------------------------*/
(define (start-jsm-emission!)
   'ok )
      
;*---------------------------------------------------------------------*/
;*    Link                                                             */
;*---------------------------------------------------------------------*/
(define-method (backend-link me::jsm result)
   'ok )
