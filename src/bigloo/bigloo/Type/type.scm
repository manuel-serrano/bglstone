;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Type/type.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu May 30 16:38:54 1996                          */
;*    Last change :  Tue Oct  5 13:00:14 2004 (serrano)                */
;*    Copyright   :  1996-2004 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The type class definition                                        */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module type_type

   (import tools_location)
   
   (export (final-class type::object
	      ;; the type identifier
	      (id::symbol read-only (default '_))
	      ;; the target type name
	      (name (default #unspecified))
	      ;; the expression used by sizeof
	      (size (default #unspecified))
	      ;; the kind of type
	      (class (default 'bigloo))
	      ;; the coercion-to list
	      (coerce-to::obj (default '()))
	      ;; its parents
	      (parents::obj (default '()))
	      ;; initialized ? (a type can be only declared (use-type))
	      init?::bool
	      ;; is this type can be converted into every thing ?
	      (magic?::bool (default #f))
	      ;; is the type name containing a `$' ?
	      ($ (default #t))
	      ;; a type than self is aliasing
	      (alias (default #f))
	      ;; a type that points to self
	      (pointed-to-by (default #f))
	      ;; a tvector associated to this type
	      ;; (only used by the cfa but much easier to make it general)
	      (tvector (default #unspecified))
	      ;; location (for the first use of that type)
	      (location (default #unspecified))
	      ;; if that type is imported the source of the import clause
	      (import-location (default #f)))

	   (get-aliased-type::type ::type)

	   (bigloo-type?::bool ::type)))

;*---------------------------------------------------------------------*/
;*    get-aliased-type ...                                             */
;*---------------------------------------------------------------------*/
(define (get-aliased-type type)
   (let loop ((type type))
      (if (type? (type-alias type))
	  (loop (type-alias type))
	  type)))

;*---------------------------------------------------------------------*/
;*    bigloo-type? ...                                                 */
;*---------------------------------------------------------------------*/
(define (bigloo-type? type)
   (eq? (type-class type) 'bigloo))
   
