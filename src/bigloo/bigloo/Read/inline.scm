;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Read/inline.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Dec 29 10:30:51 1994                          */
;*    Last change :  Mon May 15 08:00:17 2000 (serrano)                */
;*    Copyright   :  1994-2000 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    We scan files in order to find `inline' definitions.             */
;*=====================================================================*/
 
;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module read_inline
   (include "Tools/trace.sch"
	    "Ast/unit.sch")
   (import  read_reader
	    tools_error
	    tools_speek
	    tools_shape
	    ast_ident
	    ast_env
	    type_type
	    ast_var
	    (find-location tools_location))
   (export  (look-for-inline inline code port fnames module)
	    (inline-finalizer)))

;*---------------------------------------------------------------------*/
;*    look-for-inline ...                                              */
;*    -------------------------------------------------------------    */
;*    We read until we have found all inline definitions or EOF.       */
;*---------------------------------------------------------------------*/
(define (look-for-inline inlines code port fnames module)
   (let loop ((inlines (look-for-inline/exp inlines code module))
	      (exp     (compiler-read port #t))
	      (fnames  fnames)
	      (port    port))
      (cond
	 ((null? inlines)
	  'done)
	 ((eof-object? exp)
	  (if (null? fnames)
	      (user-error "import"
			  "Can't find inline/generic definition(s)"
			  (map car inlines))
	      (let ((fname (find-file/path (car fnames) *load-path*)))
		 (if (not (string? fname))
		     (user-error "import" "Can't find file" (car fnames))
		     (let ((port (open-input-file fname)))
			(unwind-protect (loop inlines
					      (compiler-read port #t)
					      (cdr fnames)
					      port)
					(if (input-port? port)
					    (close-input-port port))))))))
	 (else
	  (loop (look-for-inline/exp inlines exp module)
		(compiler-read port #t)
		fnames
		port)))))

;*---------------------------------------------------------------------*/
;*    look-for-inline/exp ...                                          */
;*---------------------------------------------------------------------*/
(define (look-for-inline/exp inlines exp module)
   (if (null? inlines)
       '()
       (match-case exp
	  ((define-inline (and ?proto (?name . ?-)) . ?body)
	   (let* ((id   (id-of-id name (find-location exp)))
		  (cell (assq id inlines)))
	      (if (and (pair? cell) (eq? (cdr cell) 'sifun))
		  (begin
		     (set-car! proto `(@ ,id ,module))
		     (set! *inline-definitions*
			   (cons exp *inline-definitions*))
		     (remq! cell inlines))
		  inlines)))
	  ((begin . ?exp*)
	   (let loop ((inlines inlines)
		      (exp*    exp*))
	      (if (null? exp*)
		  inlines
		  (loop (look-for-inline/exp inlines (car exp*) module)
			(cdr exp*)))))
	  (else
	   inlines))))

;*---------------------------------------------------------------------*/
;*    *inline-definitions* ...                                         */
;*---------------------------------------------------------------------*/
(define *inline-definitions* '())

;*---------------------------------------------------------------------*/
;*    inline-finalizer ...                                             */
;*---------------------------------------------------------------------*/
(define (inline-finalizer)
   (if (null? *inline-definitions*)
       '()
       (list (unit 'imported-inlines 0 *inline-definitions* #t))))

       
     
   
