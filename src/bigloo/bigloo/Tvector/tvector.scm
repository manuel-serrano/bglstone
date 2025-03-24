;*=====================================================================*/
;*    .../project/bglstone/src/bigloo/bigloo/Tvector/tvector.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Mar 27 11:21:53 1995                          */
;*    Last change :  Fri Mar  7 08:02:59 2025 (serrano)                */
;*    Copyright   :  1995-2025 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The declaration of `tvector' types.                              */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module tvector_tvector
   (include "Ast/node.sch" "Type/type.sch" "Tvector/tvector.sch")
   
   (import  type_type
	    type_env
	    tools_error
	    module_module
	    (find-location tools_location))
   
   (export  (wide-class tvec::type
	      ;; the item type
	      (item-type::type read-only))
	      
	    (declare-tvector-type!::tvec ::symbol ::symbol ::obj)
	    (emit-tvector-types ::output-port)))

;*---------------------------------------------------------------------*/
;*    *tvector-type-list* ...                                          */
;*---------------------------------------------------------------------*/
(define *tvector-type-list* '())

;*---------------------------------------------------------------------*/
;*    declare-tvector-type! ...                                        */
;*---------------------------------------------------------------------*/
(define (declare-tvector-type! tvect-id item-id src)
   (let ((obj (find-type 'obj)))
      (if (not (type? obj))
	  (user-error "declare-tvector-type!" "Unable to find `obj' type" exp)
	  (let ((type (declare-subtype! tvect-id
					(type-name obj)
					(list 'obj)
					'bigloo))
		(item-type (use-type! item-id (find-location src))))
	     ;; we create the tvector type
	     (widen!::tvec type (item-type item-type))
	     ;; we have declared the type, we add the coercion
	     (produce-module-clause! (make-coercion-clause tvect-id))
	     ;; we add the tvector for the C type emission
	     (set! *tvector-type-list* (cons type *tvector-type-list*))
	     ;; we remember than a tvector type exists on item-type
	     (type-tvector-set! item-type type)
	     ;; we are done
	     type))))

;*---------------------------------------------------------------------*/
;*    emit-tvector-types ...                                           */
;*---------------------------------------------------------------------*/
(define (emit-tvector-types oport)
   (if (pair? *tvector-type-list*)
       (fprint oport #\Newline "/* Tvector type definitions */"))
   (for-each (lambda (tvector)
		(with-access::tvec tvector (item-type)
		   (let ((item-type-name (type-name item-type)))
		      (fprint oport "struct bgl_tvector_of_"
			 (bigloo-mangle item-type-name) " {")
		      (fprint oport "   header_t header;")
		      (fprint oport "   long     length;")
		      (fprint oport "   obj_t    descr;")
		      (fprint oport "   " item-type-name " el0;")
		      (fprint oport "};\n"))))
	     (reverse! *tvector-type-list*))
   (newline oport))

;*---------------------------------------------------------------------*/
;*    make-coercion-clause ...                                         */
;*---------------------------------------------------------------------*/
(define (make-coercion-clause tvect-id)
   `(type (coerce ,tvect-id tvector () ())
	  (coerce tvector ,tvect-id () ())))
