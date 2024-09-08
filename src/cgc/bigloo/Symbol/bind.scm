;*=====================================================================*/
;*    serrano/prgm/project/bglstone/src/cgc/bigloo/Symbol/bind.scm     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Jan 27 10:52:02 1998                          */
;*    Last change :  Tue Nov 24 15:59:51 2015 (serrano)                */
;*    -------------------------------------------------------------    */
;*    Symbol bindings. This module binds variable names, function      */
;*    names and _also_ type names.                                     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module symbol_bind
   (include "Symbol/bind.sch")
   (import ast_node
	   ast_ident
	   ast_location
	   tools_speek
	   type_type
	   type_bind
	   runtime_runtime)
   (export (bind-symbols! ::ast))
   (static (class symbol-entry
	      name::bstring
	      decl::decl)))

;*---------------------------------------------------------------------*/
;*    bind-symbols! ...                                                */
;*---------------------------------------------------------------------*/
(define (bind-symbols! ast)
   (verbose 1 #"  - symbol resolution\n")
   (with-access::ast ast (decl-list)
      ;; We start defining some symbol that are associated to the
      ;; runtime services
      (for-each declare-symbol! (runtime-fundecls))
      ;; In the first decl-list traversal we bind all the user
      ;; variables and user functions
      (for-each declare-symbol! decl-list)
      ;; Now we have to walk thru the function definition in order
      ;; to bind all local variables.
      (for-each (lambda (decl)
		   (cond
		      ((fundecl? decl)
		       (bind-fundecl decl (list decl) 0))
		      ((vardecl? decl)
		       (bind-vardecl decl))))
		(append (runtime-fundecls) decl-list))
      ;; we reset the two variables for the collector
      (set! *variable-table* #unspecified)
      (set! *function-table* #unspecified)))

;*---------------------------------------------------------------------*/
;*    bind-fundecl ...                                                 */
;*---------------------------------------------------------------------*/
(define (bind-fundecl decl::fundecl stack nesting)
   (with-access::fundecl decl (id params body type depth)
      (set! depth nesting)
      (verbose 2 "    . scanning " id #\Newline)
      (set! type (find-type type))
      (for-each bind-vardecl params)
      (bind-local body (append params stack) nesting)))

;*---------------------------------------------------------------------*/
;*    bind-vardecl ...                                                 */
;*---------------------------------------------------------------------*/
(define (bind-vardecl decl::vardecl)
   (with-access::vardecl decl (type id)
      (set! type (find-type type))
      (if (type=? type *type-void*)
	  (source-error (ident-location id) "Illegal variable type" type))))

;*---------------------------------------------------------------------*/
;*    declare-symbol! ::decl ...                                       */
;*---------------------------------------------------------------------*/
(define-generic (declare-symbol! decl::decl)
   #unspecified)

;*---------------------------------------------------------------------*/
;*    declare-symbol! ::vardecl ...                                    */
;*---------------------------------------------------------------------*/
(define-method (declare-symbol! decl::vardecl)
   (bind-variable! decl))

;*---------------------------------------------------------------------*/
;*    declare-symbol! ::fundecl ...                                    */
;*---------------------------------------------------------------------*/
(define-method (declare-symbol! decl::fundecl)
   (bind-function! decl))
				
;*---------------------------------------------------------------------*/
;*    *variable-table* ...                                             */
;*---------------------------------------------------------------------*/
(define *variable-table*
   (make-hashtable 1024))

;*---------------------------------------------------------------------*/
;*    bind-variable! ...                                               */
;*---------------------------------------------------------------------*/
(define (bind-variable! vardecl::vardecl)
   (let* ((ident    (vardecl-id vardecl))
	  (name     (ident-name ident))
	  (location (ident-location ident))
	  (old      (hashtable-get *variable-table* name)))
      (verbose 2 "    . " ident #\Newline)
      (if (symbol-entry? old)
	  (source-error location "Illegal variable redefinition" ident)
	  (let ((new (instantiate::symbol-entry
			(name name)
			(decl vardecl))))
	     (hashtable-put! *variable-table* name new)))))

;*---------------------------------------------------------------------*/
;*    find-variable ...                                                */
;*    -------------------------------------------------------------    */
;*    It is an error to look for an unbound variable.                  */
;*---------------------------------------------------------------------*/
(define (find-variable ident::ident)
   (let* ((name     (ident-name ident))
	  (location (ident-location ident))
	  (entry    (hashtable-get *variable-table* name)))
      (if (symbol-entry? entry)
	  (symbol-entry-decl entry)
	  (source-error location "Unbound variable" ident))))

;*---------------------------------------------------------------------*/
;*    *function-table* ...                                             */
;*---------------------------------------------------------------------*/
(define *function-table*
   (make-hashtable 1024))

;*---------------------------------------------------------------------*/
;*    bind-function! ...                                               */
;*---------------------------------------------------------------------*/
(define (bind-function! fundecl::fundecl)
   (let* ((ident    (fundecl-id fundecl))
	  (name     (ident-name ident))
	  (location (ident-location ident))
	  (old      (hashtable-get *function-table* name)))
      (verbose 2 "    . " ident #"( ... )\n")
      (if (symbol-entry? old)
	  (source-error location "Illegal function redefinition" ident)
	  (let ((new (instantiate::symbol-entry
		       (name name)
		       (decl fundecl))))
	     (hashtable-put! *function-table* name new)))))

;*---------------------------------------------------------------------*/
;*    find-function ...                                                */
;*    -------------------------------------------------------------    */
;*    It is an error to look for an unbound function.                  */
;*---------------------------------------------------------------------*/
(define (find-function ident::ident)
   (let* ((name     (ident-name ident))
	  (location (ident-location ident))
	  (entry    (hashtable-get *function-table* name)))
      (if (symbol-entry? entry)
	  (symbol-entry-decl entry)
	  (source-error location "Unbound function" ident))))

;*---------------------------------------------------------------------*/
;*    bind-local ::stmt ...                                            */
;*---------------------------------------------------------------------*/
(define-generic (bind-local body stack depth)
   body)

;*---------------------------------------------------------------------*/
;*    bind-local ::block ...                                           */
;*---------------------------------------------------------------------*/
(define-method (bind-local body::block stack depth)
   (with-access::block body (decl-list stmt-list)
      (let ((new-stack (append decl-list stack)))
	 (for-each (lambda (decl)
		      (cond
			 ((fundecl? decl)
			  (bind-fundecl decl new-stack (+fx depth 1)))
			 ((vardecl? decl)
			  (bind-vardecl decl))))
		   decl-list)
	 (for-each (lambda (stmt)
		      (bind-local stmt new-stack depth))
		   stmt-list))))

;*---------------------------------------------------------------------*/
;*    bind-local ::if-then ...                                         */
;*---------------------------------------------------------------------*/
(define-method (bind-local body::if-then stack depth)
   (with-access::if-then body (test then)
      (bind-local test stack depth)
      (bind-local then stack depth)))

;*---------------------------------------------------------------------*/
;*    bind-local ::if-then-else ...                                    */
;*---------------------------------------------------------------------*/
(define-method (bind-local body::if-then-else stack depth)
   (with-access::if-then-else body (otherwise)
      (call-next-method)
      (bind-local otherwise stack depth)))

;*---------------------------------------------------------------------*/
;*    bind-local ::setq ...                                            */
;*---------------------------------------------------------------------*/
(define-method (bind-local body::setq stack depth)
   (with-access::setq body (varref value)
      (set! varref (bind-local varref stack depth))
      (set! value (bind-local value stack depth))))

;*---------------------------------------------------------------------*/
;*    bind-local ::aset ...                                            */
;*---------------------------------------------------------------------*/
(define-method (bind-local body::aset stack depth)
   (with-access::aset body (aref value)
      (bind-local aref stack depth)
      (set! value (bind-local value stack depth))))

;*---------------------------------------------------------------------*/
;*    bind-local ::rset ...                                            */
;*---------------------------------------------------------------------*/
(define-method (bind-local body::rset stack depth)
   (with-access::rset body (rref value)
      (bind-local rref stack depth)
      (set! value (bind-local value stack depth))))

;*---------------------------------------------------------------------*/
;*    bind-local ::return ...                                          */
;*---------------------------------------------------------------------*/
(define-method (bind-local body::return-value stack depth)
   (with-access::return-value body (value)
      (set! value (bind-local value stack depth))))

;*---------------------------------------------------------------------*/
;*    bind-local ::while ...                                           */
;*---------------------------------------------------------------------*/
(define-method (bind-local bod::while stack depth)
   (with-access::while bod (test body)
      (set! test (bind-local test stack depth))
      (bind-local body stack depth)))
   
;*---------------------------------------------------------------------*/
;*    bind-local ::exprstmt/value ...                                  */
;*---------------------------------------------------------------------*/
(define-method (bind-local body::exprstmt-value stack depth)
   (with-access::exprstmt-value body (>expr)
      (set! >expr (bind-local >expr stack depth))))

;*---------------------------------------------------------------------*/
;*    bind-local ::binop ...                                           */
;*---------------------------------------------------------------------*/
(define-method (bind-local body::binop stack depth)
   (with-access::binop body (left right)
      (set! left (bind-local left stack depth))
      (set! right (bind-local right stack depth))
      body))

;*---------------------------------------------------------------------*/
;*    bind-local ::funcall ...                                         */
;*---------------------------------------------------------------------*/
(define-method (bind-local body::funcall stack depth)
   (with-access::funcall body (fun actuals)
      (set! fun (lookup-fun fun stack))
      (let loop ((actuals actuals))
	 (if (pair? actuals)
	     (begin
		(set-car! actuals (bind-local (car actuals) stack depth))
		(loop (cdr actuals)))))
      body))

;*---------------------------------------------------------------------*/
;*    bind-local ::aref ...                                            */
;*---------------------------------------------------------------------*/
(define-method (bind-local body::aref stack depth)
   (with-access::aref body (array offset)
      (set! array (bind-local array stack depth))
      (set! offset (bind-local offset stack depth))
      body))

;*---------------------------------------------------------------------*/
;*    bind-local ::rref ...                                            */
;*---------------------------------------------------------------------*/
(define-method (bind-local body::rref stack depth)
   (with-access::rref body (record)
      (set! record (bind-local record stack depth))
      body))

;*---------------------------------------------------------------------*/
;*    bind-local ::varref ...                                          */
;*---------------------------------------------------------------------*/
(define-method (bind-local body::varref stack depth)
   (with-access::varref body (vardecl)
      (set! vardecl (lookup-var vardecl stack))
      body))

;*---------------------------------------------------------------------*/
;*    bind-local ::const ...                                           */
;*---------------------------------------------------------------------*/
(define-method (bind-local body::const stack depth)
   (with-access::const body (type)
      (set! type (find-type type))
      body))

;*---------------------------------------------------------------------*/
;*    lookup/pred ...                                                  */
;*---------------------------------------------------------------------*/
(define (lookup/pred pred? find id::ident stack)
   (let loop ((stack stack))
      (cond
	 ((null? stack)
	  (find id))
	 ((not (pred? (car stack)))
	  (loop (cdr stack)))
	 ((not (ident=? id (decl-id (car stack))))
	  (loop (cdr stack)))
	 (else
	  (car stack)))))
	  
;*---------------------------------------------------------------------*/
;*    lookup-var ...                                                   */
;*---------------------------------------------------------------------*/
(define (lookup-var::vardecl id::ident stack)
   (lookup/pred vardecl? find-variable id stack))

;*---------------------------------------------------------------------*/
;*    lookup-fun ...                                                   */
;*---------------------------------------------------------------------*/
(define (lookup-fun::fundecl id::ident stack)
   (lookup/pred fundecl? find-function id stack))
	    
   

