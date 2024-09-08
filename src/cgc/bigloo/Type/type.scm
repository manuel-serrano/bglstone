;*=====================================================================*/
;*    serrano/prgm/project/bglstone/src/cgc/bigloo/Type/type.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Jan 27 08:14:47 1998                          */
;*    Last change :  Tue Nov 24 16:01:52 2015 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The type definition                                              */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module type_type
   (import ast_location
	   ast_node)
   (use    type_bind)
   (include "Type/type.sch")
   (export (class type
	      ;; the type identifier
	      id::ident)
	   (class alias::type
	      (of (default #unspecified)))
	   (class structure::type
	      (fields (default #unspecified)))
	   (class array::type
	      (of (default #unspecified)))
	   (type=?::bool ::type ::type)
	   (type-compatible?::bool ::type ::obj)
	   (type-error location expected provided)))

;*---------------------------------------------------------------------*/
;*    type=? ...                                                       */
;*    -------------------------------------------------------------    */
;*    We implement here a very straightforward policy. Two types       */
;*    are said to be equal if their names are the same. That is,       */
;*    two aliased types are not the same.                              */
;*    -------------------------------------------------------------    */
;*    This simple policy can be implemented via a simple `eq?'.        */
;*---------------------------------------------------------------------*/
(define (type=? left right)
   (eq? left right))

;*---------------------------------------------------------------------*/
;*    type-compatible? ...                                             */
;*    -------------------------------------------------------------    */
;*    This is a laxist version of type=?. Here the second argument     */
;*    can be a non type. In such case, type-compatible? returns #t.    */
;*---------------------------------------------------------------------*/
(define (type-compatible? left right)
   (if (type? right)
       (or (type=? left right)
	   (and (type=? left *type-nil*)
		(or (array? right) (structure? right)))
	   (and (type=? right *type-nil*)
		(or (array? left) (structure? left))))
       #t))

;*---------------------------------------------------------------------*/
;*    type-error ...                                                   */
;*---------------------------------------------------------------------*/
(define (type-error location expected provided)
   (let ((port (open-output-string)))
      (fprin port "Type `" expected "' expected, `" provided "' provided.")
      (let ((msg (close-output-port port)))
	 (source-error location "Type error" msg))))

;*---------------------------------------------------------------------*/
;*    fprin ...                                                        */
;*---------------------------------------------------------------------*/
(define (fprin port . objs)
   (for-each (lambda (x) (display x port)) objs))

;*---------------------------------------------------------------------*/
;*    object-display ::type ...                                        */
;*---------------------------------------------------------------------*/
(define-method (object-display type::type . port)
   (with-access::type type (id)
      (let ((port (if (pair? port) (car port) (current-output-port))))
	 (fprin port "<" id ">"))))

;*---------------------------------------------------------------------*/
;*    object-display ::alias ...                                       */
;*---------------------------------------------------------------------*/
(define-method (object-display type::alias . port)
   (with-access::alias type (id of)
      (let ((port (if (pair? port) (car port) (current-output-port))))
	 (fprin port "<" id ":alias of " (if (type? of)
					     (type-id of)
					     "???")
		">"))))

;*---------------------------------------------------------------------*/
;*    object-display ::structure ...                                   */
;*---------------------------------------------------------------------*/
(define-method (object-display type::structure . port)
   (with-access::structure type (id fields)
      (let ((port (if (pair? port) (car port) (current-output-port))))
	 (fprin port "<" id ":struct ")
	 (for-each (lambda (field)
		      (fprin port
			     "("
			     (if (type? (car field))
				 (type-id (car field))
				 "???")
			     " "
			     (cdr field)
			     ")"))
		   fields)
	 (display ">" port))))

;*---------------------------------------------------------------------*/
;*    object-display ::array ...                                       */
;*---------------------------------------------------------------------*/
(define-method (object-display type::array . port)
   (with-access::array type (id of)
      (let ((port (if (pair? port) (car port) (current-output-port))))
	 (fprin port "<" id ":array of " (if (type? of)
					     (type-id of)
					     "???")
		">"))))

