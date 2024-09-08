;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Object/class.scm            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu May 30 16:46:40 1996                          */
;*    Last change :  Wed Jan 26 17:42:03 2005 (serrano)                */
;*    Copyright   :  1996-2005 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The class definition                                             */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module object_class

   (import  tools_error
	    type_type
	    type_cache
	    type_env
	    type_coercion
	    object_tools
	    object_slots
	    object_coercion
	    module_module
	    engine_param
	    foreign_jtype
	    ast_var
	    ast_ident
	    (find-location tools_location))

   (export  (wide-class tclass::type
	       ;; the `super' field
	       its-super
	       ;; the slots of the class
	       (slots (default #unspecified))
	       ;; a global variable holding the class info
	       (holder::global read-only)
	       ;; widening
	       (widening (default #f) read-only)
	       ;; the depth of the class in the inheritance tree
	       (depth::long (default 0))
	       ;; final
	       (final?::bool read-only (default #f))
	       ;; constructor
	       (constructor read-only)
	       ;; the number of virtual slots this class implements
	       (virtual-slots-number (default 0))
	       ;; abstract class
	       (abstract?::bool read-only (default #f))
	       ;; the true wide type associated with the wide classes
	       (wide-type (default #f)))

	    (wide-class jclass::type
	       ;; the `super' field
	       (its-super (default #unspecified))
	       ;; the slots of the class
	       (slots (default #unspecified))
	       ;; package
	       (package::bstring read-only (default "")))

	    (wide-class wclass::type
	       ;; the plain class that uses this wide chunk
	       (its-class (default #unspecified)))
	    
	    (get-class-list::pair-nil)
	    (heap-add-class! ::tclass)
	    (saw-wide-class-id::symbol ::symbol)
	    (type-class-name::bstring ::type)
	    (declare-class-type!::type ::obj ::global ::obj ::bool ::bool ::obj)
	    (declare-java-class-type!::type ::symbol ::obj ::bstring ::bstring ::pair)
	    (dump-class-types ::output-port)
	    (emit-class-types ::output-port)
	    (emit-bdb-class-types ::output-port)
	    (final-class?::bool ::obj)
	    (wide-class?::bool ::obj)
	    (find-class-constructors ::tclass)
	    (type-subclass?::bool ::type ::type)
	    (tclass-all-slots::pair-nil ::tclass)
	    (class-make::obj ::tclass)
	    (class-predicate::symbol ::tclass)))

;*---------------------------------------------------------------------*/
;*    *class-type-list* ...                                            */
;*---------------------------------------------------------------------*/
(define *class-type-list* '())

;*---------------------------------------------------------------------*/
;*    get-class-list ...                                               */
;*---------------------------------------------------------------------*/
(define (get-class-list)
   *class-type-list*)

;*---------------------------------------------------------------------*/
;*    heap-add-class! ...                                              */
;*    -------------------------------------------------------------    */
;*    This function is to be used when restoring class from a heap     */
;*    file.                                                            */
;*---------------------------------------------------------------------*/
(define (heap-add-class! type::tclass)
   (set! *class-type-list* (cons type *class-type-list*)))

;*---------------------------------------------------------------------*/
;*    saw-wide-class-id ...                                            */
;*    -------------------------------------------------------------    */
;*    This function construct type name of the wide component          */
;*    of a wide class. The idea is to generate a private name that the */
;*    user cannot specify himself in his programs.                     */
;*---------------------------------------------------------------------*/
(define (saw-wide-class-id class-id)
   (string->symbol (string-append "#!" (symbol->string class-id))))

;*---------------------------------------------------------------------*/
;*    type-class-name ...                                              */
;*---------------------------------------------------------------------*/
(define (type-class-name class)
   (cond
      ((not (tclass? class))
       (type-name class))
      ((and (tclass-widening class) *saw*)
       (type-name (tclass-wide-type class)))
      (else
       (type-name class))))

;*---------------------------------------------------------------------*/
;*    declare-class-type! ...                                          */
;*    -------------------------------------------------------------    */
;*    declare-class-type! is said to be returning a type and not       */
;*    a class in order to help the error management.                   */
;*    -------------------------------------------------------------    */
;*    No check is processed in this function about the super class.    */
;*    This check is performed by the function that creates the         */
;*    accessors for the class (make-class-accesses! and make-wide      */
;*    -class-accesses of the module object_access).                    */
;*---------------------------------------------------------------------*/
(define (declare-class-type!::type class-def class-holder widening final? abstract? src)
   (let* ((class-ident (parse-id (car class-def) (find-location src)))
	  (class-id    (car class-ident))
	  (super       (let ((super (cdr class-ident)))
			  (cond
			     ((eq? (type-id super) class-id)
			      #f)
			     ((eq? super *_*)
			      (get-object-type))
			     (else
			      super))))
	  (name        (id->name class-id))
	  (sizeof      (string-append "struct " name "_bgl"))
	  (t-name      (string-append name "_bglt"))
	  (type        (declare-type! class-id t-name 'bigloo)))
      ;; we mark that the holder is a read-only variable
      (global-set-read-only! class-holder)
      ;; By now we make the assumption that super is a correct class.
      ;; Super will be checked in `make-class-accesses!' (see module
      ;; object_access).
      (widen!::tclass type
	 (its-super   super)
	 (depth       (if (not (tclass? super))
			  0
			  (+fx (tclass-depth super) 1)))
	 (holder      class-holder)
	 (widening    widening)
	 (final?      final?)
	 (abstract?   abstract?)
	 (constructor (cadr class-def)))
      ;; For the saw back-end, wide classes creates a new type denoting
      ;; the wide part of the wide class. In addition, in the saw compilation
      ;; mode we change the type name for wide classes. The type is turned to
      ;; the type name of there super class
      (if (and (eq? widening 'widening) *saw*)
	  (let* ((wtid (saw-wide-class-id class-id))
		 (wt (widen!::wclass (declare-type! wtid t-name 'bigloo)
			(its-class type))))
	     (tclass-wide-type-set! type wt)
	     (type-name-set! type (type-name super))
	     (gen-coercion-clause! wtid super #f)
	     (gen-class-coercers! wt super)))
      ;; we set the sizeof field
      (type-size-set!  type sizeof)
      ;; we add the class for the C type emission
      (set! *class-type-list* (cons type *class-type-list*))
      ;; we are done
      type))

;*---------------------------------------------------------------------*/
;*    declare-java-class-type! ...                                     */
;*    -------------------------------------------------------------    */
;*    declare-class-type! is said to be returning a type and not       */
;*    a class in order to help the error management.                   */
;*---------------------------------------------------------------------*/
(define (declare-java-class-type!::type class-id super jname package src)
   (let ((super (cond
		   ((eq? (type-id super) class-id)
		    #f)
		   ((eq? super *_*)
		    #f)
		   (else
		    super)))
	 (type  (declare-type! class-id jname 'java)))
      ;; By now we make the assumption that super is a correct class.
      ;; Super will be checked in `make-class-accesses!' (see module
      ;; object_access).
      (widen!::jclass type
	 (its-super super)
	 (package package))
      ;; we are done
      type))

;*---------------------------------------------------------------------*/
;*    cross-name ...                                                   */
;*    -------------------------------------------------------------    */
;*    This function return the typedef name for non class objects      */
;*    and the `struct ??? *' name for classes. We need this function   */
;*    because C does not support cross typedefed references.           */
;*---------------------------------------------------------------------*/
(define (cross-name type)
   (if (tclass? type)
       (string-append (type-size type) " *")
       (type-name type)))

;*---------------------------------------------------------------------*/
;*    dump-class-types ...                                             */
;*---------------------------------------------------------------------*/
(define (dump-class-types oport)
   (define (dump-slot slot)
      (with-access::slot slot (virtual-num indexed name)
	 (let ((cname (cross-name (slot-type slot))))
	    (cond
	       ((>=fx virtual-num 0)
		#unspecified)
	       (indexed
		(fprint oport "   (" cname " *" name ")"))
	       (else
		(fprint oport "   (" cname " " name ")"))))))
   (for-each (lambda (class)
		(if (not (eq? class (get-object-type)))
		    (begin
		       (fprint oport "(define-class " (type-name class))
		       (fprint oport "   (constructor ...)")
		       (fprint oport "   (obj_t widening)")
		       (for-each dump-slot (tclass-slots class))
		       (fprint oport  #")\n"))))
	     (reverse! *class-type-list*)))

;*---------------------------------------------------------------------*/
;*    emit-class-types ...                                             */
;*---------------------------------------------------------------------*/
(define (emit-class-types oport)
   (define (emit-slot slot)
      (with-access::slot slot (type virtual-num indexed name)
	 (let ((cname (cross-name (slot-type slot))))
	    (cond
	       ((>=fx virtual-num 0)
		#unspecified)
	       (indexed
		(fprint oport "   " cname " *" name ";"))
	       (else
		(fprint oport "   " cname " " name ";"))))))
   (when (pair? *class-type-list*)
      (fprint oport #\Newline "/* Object type definitions */")
      (for-each (lambda (class)
		   (if (not (eq? class (get-object-type)))
		       (begin
			  (fprint oport "typedef " (type-size class) " {")
			  (if (not (tclass-widening class))
			      (begin
				 (fprint oport "   header_t header;")
				 (fprint oport "   obj_t widening;"))
			      (cond
				 ((and (fixnum? *heap-debug*)
				       (>=fx *heap-debug* 1))
				  (fprint oport "   header_t header;"))
				 ((null? (tclass-slots class))
				  ;; this is an empty object (with no fields)
				  ;; and some ISO C compilers (is it in the
				  ;; definition ?) does not support empty
				  ;; types. Hence, we generate a dummy field
				  ;; as small as possible.
				  (fprint oport "   char dummy;"))))
			  (for-each emit-slot (tclass-slots class))
			  (fprint oport "} *" (type-class-name class) ";\n"))))
		(reverse! *class-type-list*))
      (newline oport)))

;*---------------------------------------------------------------------*/
;*    emit-bdb-class-types ...                                         */
;*---------------------------------------------------------------------*/
(define (emit-bdb-class-types oport)
   (if (pair? *class-type-list*)
       (fprint oport "   /* Bigloo classes */"))
   (for-each (lambda (class)
		(with-access::tclass class (holder)
		   (if (eq? (global-module holder) *module*)
		       (fprint oport
			       "   {\"" (type-size class)
			       " *\", 0 },"))))
	     *class-type-list*))

;*---------------------------------------------------------------------*/
;*    final-class? ...                                                 */
;*    -------------------------------------------------------------    */
;*    Is a class a final class ?                                       */
;*---------------------------------------------------------------------*/
(define (final-class? class)
   (and (tclass? class) (tclass-final? class)))

;*---------------------------------------------------------------------*/
;*    wide-class? ...                                                  */
;*    -------------------------------------------------------------    */
;*    Is a class a wide-class ?                                        */
;*---------------------------------------------------------------------*/
(define (wide-class? class)
   (and (tclass? class) (tclass-widening class)))

;*---------------------------------------------------------------------*/
;*    type-subclass? ...                                               */
;*---------------------------------------------------------------------*/
(define (type-subclass? subclass class)
   (cond
      ((and (tclass? class) (tclass? subclass))
       (let loop ((subclass subclass))
	  (cond
	     ((eq? subclass class)
	      #t)
	     ((not (tclass? subclass))
	      #f)
	     ((eq? (tclass-its-super subclass) subclass)
	      #f)
	     (else
	      (loop (tclass-its-super subclass))))))
      ((and (jclass? class) (jclass? subclass))
       (let loop ((subclass subclass))
	  (cond
	     ((eq? subclass class)
	      #t)
	     ((not (jclass? subclass))
	      #f)
	     ((eq? (jclass-its-super subclass) subclass)
	      #f)
	     (else
	      (loop (jclass-its-super subclass))))))
      (else
       #f)))

;*---------------------------------------------------------------------*/
;*    find-class-constructors ...                                      */
;*    -------------------------------------------------------------    */
;*    I just don't know what to do here. i) Shall we invoke the        */
;*    all constructors (a la C++). ii) Shall we call the first         */
;*    constructor defined? iii) Shall we call the constructor          */
;*    if it exists? For now I have chosen ii) because it fits the need */
;*    for all the code I have currently that make use of constructors. */
;*---------------------------------------------------------------------*/
(define (find-class-constructors class::tclass)
   (let loop ((class class))
      (with-access::tclass class (constructor its-super)
	 (cond
	    ((or (not (tclass? class)) (eq? class its-super))
	     '())
	    (constructor
	     (list constructor))
	    (else
	     (loop its-super))))))

;*---------------------------------------------------------------------*/
;*    tclass-all-slots ...                                             */
;*---------------------------------------------------------------------*/
(define (tclass-all-slots::pair-nil class::tclass)
   (if (not (tclass-widening class))
       (tclass-slots class)
       (append (tclass-slots (tclass-its-super class))
	       (tclass-slots class))))

;*---------------------------------------------------------------------*/
;*    class-make ...                                                   */
;*    -------------------------------------------------------------    */
;*    The name of the constructor                                      */
;*---------------------------------------------------------------------*/
(define (class-make t::tclass)
   (if (tclass-abstract? t)
       #f
       (symbol-append 'make- (type-id t))))

;*---------------------------------------------------------------------*/
;*    class-predicate ...                                              */
;*    -------------------------------------------------------------    */
;*    The name of the predicate                                        */
;*---------------------------------------------------------------------*/
(define (class-predicate::symbol t::tclass)
   (symbol-append (type-id t) '?))
