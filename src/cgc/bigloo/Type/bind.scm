;*=====================================================================*/
;*    serrano/prgm/project/bglstone/src/cgc/bigloo/Type/bind.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Jan 27 08:14:24 1998                          */
;*    Last change :  Tue Nov 24 16:02:19 2015 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The types binding                                                */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module type_bind
   (import type_type
	   ast_node
	   ast_ident
	   ast_location
	   tools_speek)
   (export (bind-types! ::ast)
	   (find-type::type ::ident)
	   *type-int*
	   *type-string*
	   *type-void*
	   *type-nil*)
   (include "Type/bind.sch")
   (static (class type-entry
	      name::bstring
	      (type (default #unspecified)))))

;*---------------------------------------------------------------------*/
;*    Global types ...                                                 */
;*---------------------------------------------------------------------*/
(define *type-int*    #unspecified)
(define *type-string* #unspecified)
(define *type-void*   #unspecified)
(define *type-nil*    #unspecified)

;*---------------------------------------------------------------------*/
;*    declare-primitive-type! :: ...                                   */
;*---------------------------------------------------------------------*/
(define-macro (declare-primitive-type! name)
   `(let* ((id         (string->ident ,name))
	   (type-entry (bind-type-entry! id))
	   (type       (instantiate::type
			  (id id))))
       (type-entry-type-set! type-entry type)
       (set! ,(string->symbol (string-append "*type-" name "*"))
	     type)))

;*---------------------------------------------------------------------*/
;*    bind-types! ...                                                  */
;*    -------------------------------------------------------------    */
;*     - We bind all the types defined in the ast.                     */
;*     - We check that all used types are bound.                       */
;*     - We replace all type identifiers by type structures.           */
;*---------------------------------------------------------------------*/
(define (bind-types! ast)
   (verbose 1 #"  - type binding\n")
   ;; We bind primitive types
   (declare-primitive-type! "string")
   (declare-primitive-type! "int")
   (declare-primitive-type! "void")
   (declare-primitive-type! "nil")
   (with-access::ast ast (decl-list)
      ;; In the first decl-list traversal we bind all the user types
      (for-each (lambda (decl)
		   (if (typespec? decl)
		       (declare-type! decl)))
		decl-list)
      ;; From now it is an error to mention an unbound type. We now
      ;; complete the type binding by buildling types
      (for-each (lambda (decl)
		   (if (typespec? decl)
		       (type-setup! decl)))
		decl-list)))

;*---------------------------------------------------------------------*/
;*    declare-type! ::typespec ...                                     */
;*---------------------------------------------------------------------*/
(define-generic (declare-type! typespec::typespec))

;*---------------------------------------------------------------------*/
;*    declare-type! ...                                                */
;*---------------------------------------------------------------------*/
(define-method (declare-type! typespec::typespec-alias)
   (with-access::typespec typespec (id)
      (let ((type-entry (bind-type-entry! id))
	    (type       (instantiate::alias
			   (id id))))
	 (type-entry-type-set! type-entry type))))

;*---------------------------------------------------------------------*/
;*    declare-type! ...                                                */
;*---------------------------------------------------------------------*/
(define-method (declare-type! typespec::typespec-record)
   (with-access::typespec typespec (id)
      (let ((type-entry (bind-type-entry! id))
	    (type       (instantiate::structure
			   (id id))))
	 (type-entry-type-set! type-entry type))))

;*---------------------------------------------------------------------*/
;*    declare-type! ...                                                */
;*---------------------------------------------------------------------*/
(define-method (declare-type! typespec::typespec-array)
   (with-access::typespec typespec (id)
      (let ((type-entry (bind-type-entry! id))
	    (type       (instantiate::array
			   (id id))))
	 (type-entry-type-set! type-entry type))))

;*---------------------------------------------------------------------*/
;*    type-setup! ...                                                  */
;*---------------------------------------------------------------------*/
(define-generic (type-setup! typespec::typespec))

;*---------------------------------------------------------------------*/
;*    type-setup! ::typespec-alias ...                                 */
;*---------------------------------------------------------------------*/
(define-method (type-setup! typespec::typespec-alias)
   (with-access::typespec-alias typespec (id of)
      (let* ((type-entry (find-type-entry id))
	     (type       (type-entry-type type-entry))
	     (type-of    (type-entry-type (find-type-entry of))))
	 (alias-of-set! type type-of)
	 (verbose 2 "    . " type #\Newline))))

;*---------------------------------------------------------------------*/
;*    type-setup! ::typespec-record ...                                */
;*---------------------------------------------------------------------*/
(define-method (type-setup! typespec::typespec-record)
   (with-access::typespec-record typespec (id fields)
      (let* ((typed-fields (map (lambda (field)
				   (cons (type-entry-type
					  (find-type-entry (car field)))
					 (cdr field)))
				fields))
	     (type-entry   (find-type-entry id))
	     (type         (type-entry-type type-entry)))
	 (structure-fields-set! type typed-fields)
	 (verbose 2 "    . " type #\Newline))))

;*---------------------------------------------------------------------*/
;*    type-setup! ::typespec-array ...                                 */
;*---------------------------------------------------------------------*/
(define-method (type-setup! typespec::typespec-array)
   (with-access::typespec-array typespec (id of)
      (let* ((type-entry (find-type-entry id))
	     (type       (type-entry-type type-entry)))
	 (array-of-set! type (type-entry-type (find-type-entry of)))
	 (verbose 2 "    . " type #\Newline))))
			 
;*---------------------------------------------------------------------*/
;*    *type-table* ...                                                 */
;*---------------------------------------------------------------------*/
(define *type-table*
   (make-hashtable 1024))

;*---------------------------------------------------------------------*/
;*    bind-type-entry! ...                                             */
;*    -------------------------------------------------------------    */
;*    This function bind a new type. It is an error if a type with     */
;*    the same name already exists.                                    */
;*---------------------------------------------------------------------*/
(define (bind-type-entry! ident::ident)
   (let* ((name       (ident-name ident))
	  (location   (ident-location ident))
	  (type-entry (hashtable-get *type-table* name)))
      (if (type-entry? type-entry)
	  (source-error location "Illegal type redefinition" ident)
	  (let ((new (instantiate::type-entry (name name))))
	     (hashtable-put! *type-table* name new)
	     new))))
   
;*---------------------------------------------------------------------*/
;*    find-type-entry ...                                              */
;*    -------------------------------------------------------------    */
;*    It is an error to look for a non existing type.                  */
;*---------------------------------------------------------------------*/
(define (find-type-entry::type-entry ident::ident)
   (let* ((name       (ident-name ident))
	  (location   (ident-location ident))
	  (type-entry (hashtable-get *type-table* name)))
      (if (type-entry? type-entry)
	  type-entry
	  (source-error location "Undefined type" ident))))

;*---------------------------------------------------------------------*/
;*    find-type ...                                                    */
;*---------------------------------------------------------------------*/
(define (find-type::type ident::ident)
   (type-entry-type (find-type-entry ident)))
   
