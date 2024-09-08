;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Module/statexp.scm          */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Jun  4 10:58:45 1996                          */
;*    Last change :  Fri Nov  2 12:08:53 2001 (serrano)                */
;*    Copyright   :  1996-2001 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The static clauses compilation.                                  */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module module_statexp
   (include "Ast/unit.sch")
   (import  module_module
	    module_prototype
	    module_class
	    tools_error
	    tools_args
	    tools_location
	    type_type
	    object_class
	    ast_var
	    ast_ident
	    ast_find-gdefs
	    ast_glo-decl)
   (export  (make-static-compiler)
	    (make-export-compiler)))

;*---------------------------------------------------------------------*/
;*    make-static-compiler ...                                         */
;*---------------------------------------------------------------------*/
(define (make-static-compiler)
   (instantiate::ccomp (id 'static)
		       (producer statexp-producer)))

;*---------------------------------------------------------------------*/
;*    make-export-compiler ...                                         */
;*---------------------------------------------------------------------*/
(define (make-export-compiler)
   (instantiate::ccomp (id 'export)
		       (producer statexp-producer)
		       (consumer export-consumer)
		       (finalizer statexp-finalizer)))

;*---------------------------------------------------------------------*/
;*    statexp-producer ...                                             */
;*---------------------------------------------------------------------*/
(define (statexp-producer clause)
   (let ((mode (car clause)))
      (match-case clause
	 ((?- . ?protos)
	  (for-each (lambda (proto) (statexp-parser proto mode)) protos)
	  '())
	 (else
	  (user-error/location (find-location *module-clause*)
			       "Parse error"
			       (string-append
				"Illegal `"
				(string-downcase (symbol->string mode))
				"' clause")
			       clause
			       '())))))

;*---------------------------------------------------------------------*/
;*    export-consumer ...                                              */
;*---------------------------------------------------------------------*/
(define (export-consumer module clause)
   (match-case clause
      ((?- . ?protos)
       protos)
      (else
       (user-error/location (find-location *module-clause*)
			    "Parse error"
			    "Illegal `export' clause"
			    clause
			    '()))))
   
;*---------------------------------------------------------------------*/
;*    statexp-parser ...                                               */
;*---------------------------------------------------------------------*/
(define (statexp-parser prototype import)
   (let ((proto (parse-prototype prototype)))
      (if (not (pair? proto))
	  (user-error/location (find-location *module-clause*)
			       "Parse error"
			       "Illegal prototype"
			       prototype
			       '())
	  (case (car proto)
	     ((sfun sifun sgfun)
	      (to-be-define! (declare-global-sfun! (cadr proto)
						   (caddr proto)
						   *module*
						   import
						   (car proto)
						   prototype
						   #f)))
	     ((svar)
	      (to-be-define! (declare-global-svar! (cadr proto)
						   *module*
						   import
						   prototype
						   #f)))
	     ((class)
	      (to-be-declare! (delay (declare-class! (cdr proto)
						     *module*
						     import
						     #f
						     #f
						     prototype
						     #f))))
	     ((abstract-class)
	      (to-be-declare! (delay (declare-class! (cdr proto)
						     *module*
						     import
						     #f
						     #t
						     prototype
						     #f))))
	     ((final-class)
	      (to-be-declare! (delay (declare-class! (cdr proto)
						     *module*
						     import
						     #t
						     #f
						     prototype
						     #f))))
	     ((wide-class)
	      (to-be-declare! (delay (declare-wide-class! (cdr proto)
							  *module*
							  import
							  prototype
							  #f))))
	     (else
	      (user-error "Parse error" "Illegal prototype" prototype '()))))))

;*---------------------------------------------------------------------*/
;*    *local-classes* ...                                              */
;*---------------------------------------------------------------------*/
(define *local-classes* '())

;*---------------------------------------------------------------------*/
;*    to-be-declare! ...                                               */
;*---------------------------------------------------------------------*/
(define (to-be-declare! exp)
   (set! *local-classes* (cons exp *local-classes*)))

;*---------------------------------------------------------------------*/
;*    statexp-finalizer ...                                            */
;*    -------------------------------------------------------------    */
;*    we declare local classes. They must be declared after imported   */
;*    classes (then after the finalization of imported modules)        */
;*    otherwise the class declaration process would fail when checking */
;*    the super class types (saying something like they are not        */
;*    classes). That why we have froozen their declaration until now.  */
;*---------------------------------------------------------------------*/
(define (statexp-finalizer)
   ;; we declare local classes
   (for-each force (reverse! *local-classes*))
   (set! *local-classes* '())
   ;; and we can finalize them
   (let ((classes (class-finalizer)))
      (if (pair? classes)
	  classes
	  'void)))


  
   
