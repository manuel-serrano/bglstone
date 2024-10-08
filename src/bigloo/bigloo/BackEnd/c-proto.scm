;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/BackEnd/c-proto.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Jul  2 09:57:04 1996                          */
;*    Last change :  Wed Jan 19 14:02:09 2005 (serrano)                */
;*    Copyright   :  1996-2005 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The emission of prototypes                                       */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module backend_c_prototype
   (include "Tvector/tvector.sch"
	    "Tools/fprint.sch")
   (import  tools_error
	    tools_shape
	    engine_param
	    module_module
	    type_type
	    type_cache
	    type_tools
	    type_env
	    object_class
	    object_slots
	    ast_var
	    ast_node
	    ast_env
	    ast_ident
	    tvector_tvector
	    tvector_cnst
	    cnst_alloc
	    cgen_cop
	    backend_c_emit
	    backend_cplib
	    cgen_emit-cop
	    cnst_node) 
   (export  (require-prototype?::bool ::global)
	    (emit-prototypes)
	    (emit-cnsts)))

;*---------------------------------------------------------------------*/
;*    require-prototype? ...                                           */
;*---------------------------------------------------------------------*/
(define (require-prototype? global)
   (and (or (eq? (global-module global) *module*)
	    (not (eq? (global-import global) 'static)))
	(or (and (eq? (global-module global) *module*)
		 (eq? (global-import global) 'export))
	    (>fx (global-occurrence global) 0)
	    (eq? (global-removable global) 'never))))

;*---------------------------------------------------------------------*/
;*    emit-prototypes ...                                              */
;*---------------------------------------------------------------------*/
(define (emit-prototypes)
   ;; set the proper name for bigloo-initialized! that is used
   ;; when a main is produced
   (let ((init (find-global 'bigloo-initialized! '__param)))
      (when init (set-variable-name! init)))
   ;; first, we print the prototype of non procedure
   (for-each-global!
    (lambda (global)
       (if (and (require-prototype? global)
		(not (scnst? (global-value global))))
	   (emit-prototype (global-value global) global))))
   ;; since cnst-table is a hack, it nevers appears but it is used
   ;; by a bunch of C macros (this variable cannot appears because
   ;; it has a strange name).
   (let ((cnst-init (get-cnst-table)))
      (emit-prototype (global-value cnst-init) cnst-init))
   ;; emit the type declarations for the typed vectors
   (emit-tvector-types *c-port*)
   ;; we are done now for prototypes.
   (newline *c-port*))

;*---------------------------------------------------------------------*/
;*    emit-cnsts ...                                                   */
;*---------------------------------------------------------------------*/
(define (emit-cnsts)
   (for-each-global!
    (lambda (global)
       (if (and (require-prototype? global)
		(scnst? (global-value global)))
	   (emit-cnst (global-value global) global))))
   (newline *c-port*))

;*---------------------------------------------------------------------*/
;*    emit-prototype ...                                               */
;*---------------------------------------------------------------------*/
(define-generic (emit-prototype value::value variable::variable))

;*---------------------------------------------------------------------*/
;*    emit-prototype ::svar ...                                        */
;*---------------------------------------------------------------------*/
(define-method (emit-prototype value::svar variable)
   (emit-prototype/svar/scnst value variable))

;*---------------------------------------------------------------------*/
;*    emit-prototype ::scnst ...                                       */
;*---------------------------------------------------------------------*/
(define-method (emit-prototype value::scnst variable)
   (emit-prototype/svar/scnst value variable))

;*---------------------------------------------------------------------*/
;*    emit-prototype/svar/scnst ...                                    */
;*---------------------------------------------------------------------*/
(define (emit-prototype/svar/scnst value variable)
   (with-access::variable variable (type id name)
      (set-variable-name! variable)
      (cond
	 ((eq? (global-import variable) 'static)
	  (fprint *c-port*
		  "static"
		  #\space
		  (make-typed-declaration type name)
		  (if (sub-type? type *obj*) " = BUNSPEC;" #\;)))
	 ((eq? (global-import variable) 'export)
	  (fprint *c-port*
		  "BGL_EXPORTED_DEF "
		  (make-typed-declaration type name)
		  (if (sub-type? type *obj*) " = BUNSPEC;" #\;)))
	 (else
	  (fprint *c-port*
		  (get-c-scope variable)
		  #\space
		  (make-typed-declaration type name)
		  #\;)))))

;*---------------------------------------------------------------------*/
;*    emit-prototype ::sfun ...                                        */
;*---------------------------------------------------------------------*/
(define-method (emit-prototype value::sfun variable)
   (with-access::variable variable (type id name)
      (set-variable-name! variable)
      (fprint *c-port*
	      (get-c-scope variable)
	      #\space
	      (make-typed-declaration
	       type
	       (string-append
		name
		(let ((args (sfun-args value)))
		   (if (and (pair? args) (type? (car args)))
		       (emit-prototype-formal-types args)
		       (emit-prototype-formals args)))))
	      ";")))

;*---------------------------------------------------------------------*/
;*    emit-prototype-formal-types ...                                  */
;*---------------------------------------------------------------------*/
(define (emit-prototype-formal-types types)
   (if (null? types)
       "()"
       (string-append
	"("
	(let loop ((types types))
	   (if (null? (cdr types))
	       (string-append (type-name-sans-$ (car types)) ")")
	       (string-append
		(type-name-sans-$ (car types)) ", "
		(loop (cdr types))))))))

;*---------------------------------------------------------------------*/
;*    emit-prototype-formals ...                                       */
;*---------------------------------------------------------------------*/
(define (emit-prototype-formals args)
   (if (null? args)
       "()"
       (string-append
	  "("
	  (let loop ((args args))
	     (if (null? (cdr args))
		 (with-access::local (car args) (type)
		    (string-append (type-name-sans-$ type) ")"))
		 (with-access::local (car args) (type)
		    (string-append (type-name-sans-$ type) ", "
				   (loop (cdr args)))))))))

;*---------------------------------------------------------------------*/
;*    emit-prototype ::cfun ...                                        */
;*---------------------------------------------------------------------*/
(define-method (emit-prototype value::cfun variable)
   (if (not (cfun-macro? value))
       (with-access::global variable (id type name library?)
	  (let* ((arity (cfun-arity value))
		 (targs (cfun-args-type value)))
	     (fprint *c-port*
		     (if library? "BGL_IMPORT " "extern ")
		     (make-typed-declaration
		      type
		      (string-append
		       name
		       "("
		       (cond
			  ((null? targs) ")")
			  ((<=fx arity -1)
			   (string-append (type-name-sans-$ (car targs))
					  ", ...)"))
			  (else
			   (let loop ((targs targs))
			      (if (null? (cdr targs))
				  (if (<fx arity 0)
				      "...)"
				      (string-append
				       (type-name-sans-$ (car targs))
				       ")"))
				  (string-append
				   (type-name-sans-$ (car targs)) ", "
				   (loop (cdr targs)))))))))
		     ";")))))

;*---------------------------------------------------------------------*/
;*    emit-prototype ::cvar ...                                        */
;*---------------------------------------------------------------------*/
(define-method (emit-prototype value::cvar variable)
   (if (not (cvar-macro? value))
       (with-access::global variable (type name library?)
	  (fprint *c-port*
		  (if library? "BGL_IMPORT " "extern ")
		  (make-typed-declaration type name) #\;))))

;*---------------------------------------------------------------------*/
;*    emit-cnst ...                                                    */
;*---------------------------------------------------------------------*/
(define (emit-cnst value::scnst variable::global)
   (with-access::scnst value (class node)
      (case class
	 ((sstring)
	  (emit-cnst-string node variable))
	 ((sreal)
	  (emit-cnst-real node variable))
	 ((selong)
	  (emit-cnst-elong node variable))
	 ((sllong)
	  (emit-cnst-llong node variable))
	 ((sfun)
	  (emit-cnst-sfun node variable))
	 ((sgfun)
	  (emit-cnst-sgfun node variable))
	 ((stvector)
	  (emit-cnst-stvector node variable))
	 (else
	  (internal-error 'backend:emit-cnst "Unknown cnst class" class)))))

;*---------------------------------------------------------------------*/
;*    emit-cnst-string ...                                             */
;*---------------------------------------------------------------------*/
(define (emit-cnst-string ostr global)
   (set-variable-name! global)
   (if (and (> (string-length ostr) *max-c-token-length*)
            *c-split-string*)
       ;; weak C compiler (e.g., VisualC++) ...
       (let ((aux (id->name (gensym (global-name global))))) 
	  (fprint *c-port*
		  "DEFINE_STRING_START( "
		  (global-name global)
		  ", "
		  aux
		  ", "
		  (string-length ostr)
		  "), ")
	  (display "{" *c-port*)
	  (let ((rlen (string-length ostr)))
	     (let laap ((i 0))
		(if (=fx i rlen)
		    (fprint *c-port*
			    "0 } \n"
			    "DEFINE_STRING_STOP( "
			    (global-name global)
			    ", "
			    aux
			    ");")
		    (begin
		       (display (char->integer (string-ref ostr i)) *c-port*)
		       (display "," *c-port*)
		       (laap (+fx i 1)))))))
       (let ((str (string-for-read ostr)))
	  ;; regular C compilers
	  (fprin *c-port*
		 "DEFINE_STRING( "
		 (global-name global)
		 ", "
		 (id->name (gensym (global-name global)))
		 ", \"")
	  (let loop ((read 0)
		     (rlen (string-length str)))
	     (cond
		((<=fx rlen *max-c-token-length*)
		 (display (untrigraph (substring str read (+fx read rlen)))
			  *c-port*)
		 (fprint *c-port* "\", " (string-length ostr) " );"))
		(else
		 (let laap ((offset (+fx read *max-c-token-length*)))
		    (cond
		       ((>=fx (+fx read 3) offset)
			(internal-error "emit-cnst-string"
					"Can't emit string"
					ostr))
		       ((char=? (string-ref str (-fx offset 1)) #\\)
			(laap (-fx offset 1)))
		       ((and (char=? (string-ref str (-fx offset 2)) #\\)
			     (char-numeric? (string-ref str (-fx offset 1))))
			(laap (-fx offset 2)))
		       ((and (char=? (string-ref str (-fx offset 3)) #\\)
			     (char-numeric? (string-ref str (-fx offset 2)))
			     (char-numeric? (string-ref str (-fx offset 1))))
			(laap (-fx offset 3)))
		       (else
			(fprin *c-port* (substring str read offset)
			       #"\"\n\"")
			(loop offset
			      (-fx rlen (-fx offset read))))))))))))

;*---------------------------------------------------------------------*/
;*    emit-cnst-real ...                                               */
;*---------------------------------------------------------------------*/
(define (emit-cnst-real real global)
   (set-variable-name! global)
   (fprint *c-port*
	   "DEFINE_REAL( "
	   (global-name global)
	   ", "
	   (id->name (gensym (global-name global)))
	   ", "
	   real
	   " );"))

;*---------------------------------------------------------------------*/
;*    emit-cnst-elong ...                                              */
;*---------------------------------------------------------------------*/
(define (emit-cnst-elong elong global)
   (set-variable-name! global)
   (fprint *c-port*
	   "DEFINE_ELONG( "
	   (global-name global)
	   ", "
	   (id->name (gensym (global-name global)))
	   ", "
	   (elong->string elong)
	   " );"))

;*---------------------------------------------------------------------*/
;*    emit-cnst-llong ...                                              */
;*---------------------------------------------------------------------*/
(define (emit-cnst-llong llong global)
   (set-variable-name! global)
   (fprint *c-port*
	   "DEFINE_LLONG( "
	   (global-name global)
	   ", "
	   (id->name (gensym (global-name global)))
	   ", "
	   (llong->string llong)
	   " );"))

;*---------------------------------------------------------------------*/
;*    emit-cnst-sfun ...                                               */
;*---------------------------------------------------------------------*/
(define (emit-cnst-sfun sfun global)
   (emit-cnst-sfun/sgfun sfun global "PROCEDURE"))

;*---------------------------------------------------------------------*/
;*    emit-cnst-sgfun ...                                              */
;*---------------------------------------------------------------------*/
(define (emit-cnst-sgfun sgfun global)
   (emit-cnst-sfun/sgfun sgfun global "GENERIC"))

;*---------------------------------------------------------------------*/
;*    emit-cnst-sfun/sgfun ...                                         */
;*---------------------------------------------------------------------*/
(define (emit-cnst-sfun/sgfun fun global kind)
   (if (eq? (global-import global) 'import)
       (emit-prototype (global-value global) global)
       (let* ((actuals (app-args fun))
	      (entry   (car actuals))
	      (arity   (get-node-atom-value (cadr actuals)))
	      (vname   (set-variable-name! global))
	      (name    (set-variable-name! (var-variable entry))))
	  (if (>=fx arity 0)
	      (fprint *c-port*
		      (if (eq? (global-import global) 'static)
			  (string-append "DEFINE_STATIC_" kind "( ")
			  (string-append "DEFINE_EXPORT_" kind "( "))
		      vname
		      ", "
		      (id->name (gensym name))
		      ", "
		      name
		      ", 0L, "
		      arity
		      " );")
	      (fprint *c-port*
		      (if (eq? (global-import global) 'static)
			  (string-append "DEFINE_STATIC_" kind "( ")
			  (string-append "DEFINE_EXPORT_" kind "( "))
		      vname
		      ", "
		      (id->name (gensym name))
		      ", va_generic_entry"
		      ", "
		      name
		      ", "
		      arity
		      " );")))))
   
;*---------------------------------------------------------------------*/
;*    emit-cnst-stvector ...                                           */
;*---------------------------------------------------------------------*/
(define (emit-cnst-stvector tvec global)
   (let* ((vec   (a-tvector-vector tvec))
	  (itype (tvec-item-type (a-tvector-type tvec)))
	  (c-vec (tvector->c-vector tvec)))
      (set-variable-name! global)
      (let ((aux (id->name (gensym (global-name global)))))
	 (fprint *c-port*
		 "DEFINE_TVECTOR_START( "
		 aux
		 ", "
		 (vector-length vec)
		 ", "
		 (string-sans-$ (type-name itype))
		 " ) "
		 c-vec
		 " DEFINE_TVECTOR_STOP( "
		 (global-name global)
		 ", "
		 aux
		 " );"))))

;*---------------------------------------------------------------------*/
;*    get-c-scope ...                                                  */
;*---------------------------------------------------------------------*/
(define-generic (get-c-scope::bstring variable::variable))

;*---------------------------------------------------------------------*/
;*    get-c-scope ::global ...                                         */
;*---------------------------------------------------------------------*/
(define-method (get-c-scope variable::global)
   (with-access::global variable (import library?)
      (case import
	 ((static)
	  "static")
	 ((import)
	  (if library? "BGL_IMPORT" "extern"))
	 ((export)
	  "BGL_EXPORTED_DECL")
	 (else
	  (internal-error "get-c-scope" "Unknown importation" import)))))

;*---------------------------------------------------------------------*/
;*    get-c-scope ::local ...                                          */
;*---------------------------------------------------------------------*/
(define-method (get-c-scope variable::local)
   "static")
