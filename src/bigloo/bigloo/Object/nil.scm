;*=====================================================================*/
;*    .../prgm/project/bglstone/src/bigloo/bigloo/Object/nil.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Feb 22 08:05:17 2004                          */
;*    Last change :  Fri Mar  7 08:34:52 2025 (serrano)                */
;*    Copyright   :  2004-25 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The `class-nil' function                                         */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module object_nil
   (include "Ast/node.sch" "Type/type.sch")
   (include "Tools/trace.sch")
   (import  tools_error
	    tools_misc
	    tools_location
	    type_type
	    type_env
	    type_tools
	    type_cache
	    tvector_tvector
	    ast_var
	    ast_ident
	    ast_private
	    object_class
	    object_struct
	    object_slots
	    object_tools
	    module_module
	    module_impuse
	    engine_param
	    foreign_jtype)
   (export  (import-class-nil ::tclass ::pair ::symbol)
	    (gen-plain-class-nil ::tclass ::pair ::symbol)
	    (gen-wide-class-nil ::tclass ::pair ::symbol)))

;*---------------------------------------------------------------------*/
;*    import-class-nil ...                                             */
;*---------------------------------------------------------------------*/
(define (import-class-nil class src-def module)
   (when *class-nil?*
      (let* ((id (tclass-id class))
	     (id-nil (symbol-append id '-nil))
	     (tid-nil (make-typed-ident id-nil id)))
	 (import-parser module `(,tid-nil)))))

;*---------------------------------------------------------------------*/
;*    gen-plain-class-nil ...                                          */
;*    -------------------------------------------------------------    */
;*    Even abstract classes have a -nil instance.                      */
;*---------------------------------------------------------------------*/
(define (gen-plain-class-nil class src-def import)
   (when *class-nil?*
      (with-access::tclass class (slots)
	 (let* ((id (tclass-id class))
		(id-nil (symbol-append id '-nil))
		(tid-nil (make-typed-ident id-nil id))
		(the-id-nil (symbol-append '%the- id-nil))
		(the-tid-nil (make-typed-ident the-id-nil 'obj))
		(alloc (symbol-append '%allocate- id))
		(fill (symbol-append 'fill- id '!)))
	    (produce-module-clause! `(,import (,tid-nil)))
	    (list
	     `(define ,the-tid-nil #unspecified)
	     (epairify `(define (,tid-nil)
			   (when (eq? ,the-id-nil #unspecified)
			      (set! ,the-id-nil (,alloc))
			      (,fill ,the-id-nil ,@(fill-nil slots)))
			   ,the-id-nil)
		       src-def))))))

;*---------------------------------------------------------------------*/
;*    gen-wide-class-nil ...                                           */
;*    -------------------------------------------------------------    */
;*    Even abstract classes have a -nil instance.                      */
;*---------------------------------------------------------------------*/
(define (gen-wide-class-nil class src-def import)
   (when *class-nil?*
      (with-access::tclass class (its-super slots holder)
	 (let* ((id (tclass-id class))
		(super-id (tclass-id its-super))
		(super-slots (tclass-slots its-super))
		(id-nil (symbol-append id '-nil))
		(tid-nil (make-typed-ident id-nil id))
		(the-id-nil (symbol-append '%the- id-nil))
		(the-tid-nil (make-typed-ident the-id-nil 'obj))
		(widening (symbol-append (tclass-widening class) '- id))
		(super-alloc (symbol-append '%allocate- super-id))
		(super-fill (symbol-append 'fill- super-id '!)))
	    (produce-module-clause! `(,import (,tid-nil)))
	    (list
	     `(define ,the-tid-nil #unspecified)
	     (epairify `(define (,tid-nil)
			   (when (eq? ,the-id-nil #unspecified)
			      ;; allocate the super instance
			      (set! ,the-id-nil (,super-alloc))
			      ;; fill it
			      (,super-fill ,the-id-nil
					   ,@(fill-nil super-slots))
			      ;; set the new class type
			      (object-class-num-set!
			       ,the-id-nil
			       (class-num (@ ,(global-id holder)
					     ,(global-module holder))))
			      ;; set the widening property
			      (object-widening-set! 
			       ,the-id-nil
			       (,widening ,@(fill-nil slots))))
			   ,the-id-nil)
		       src-def))))))

;*---------------------------------------------------------------------*/
;*    fill-nil ...                                                     */
;*---------------------------------------------------------------------*/
(define (fill-nil slots)
   (let loop ((slots slots)
	      (inits '()))
      (if (null? slots)
	  (reverse! inits)
	  (let ((slot (car slots)))
	     (cond
		((slot-indexed slot)
		 (loop (cdr slots)
		       (cons (slot-get-nil slot) inits)))
		((slot-virtual? slot)
		 (loop (cdr slots) inits))
		(else
		 (loop (cdr slots)
		       (cons (slot-get-nil slot) inits))))))))

;*---------------------------------------------------------------------*/
;*    slot-get-nil ...                                                 */
;*---------------------------------------------------------------------*/
(define (slot-get-nil slot)
   (with-access::slot slot (type)
      (cond
	 ((tclass? type)
	  `(,(symbol-append (type-id type) '-nil)))
	 (((@ isa? __object) type tvec)
	  `(list->tvector ',(type-id type) '()))
	 ((jarray? type)
	  `(,(symbol-append 'make- (type-id type)) 0))
	 ((bigloo-type? type)
	  (bigloo-primitive-type-nil type slot))
	 (else
	  (extern-type-nil type slot)))))

;*---------------------------------------------------------------------*/
;*    bigloo-primitive-type-nil ...                                    */
;*---------------------------------------------------------------------*/
(define (bigloo-primitive-type-nil type slot)
   (cond
      ((or (eq? type *obj*) (eq? type *unspec*)) #unspecified)
      ((eq? type *cell*) '(make-cell #unspecified))
      ((eq? type *bint*) 0)
      ((eq? type *bllong*) '(string->llong "0"))
      ((eq? type *bool*) #f)
      ((eq? type *breal*) 0.0)
      ((eq? type *bchar*) #\_)
      ((or (eq? type *bnil*) (eq? type *pair-nil*)) ''())
      ((eq? type *pair*) '(cons #f #f))
      ((eq? type *epair*) '(econs #f #f #f))
      ((eq? type *bstring*) "")
      ((eq? type *symbol*) ''_)
      ((eq? type *keyword*) ':_)
      ((eq? type *vector*) ''#())
      ((eq? type *procedure*) 'cons)
      (else
       (case (type-id type)
	  ((input-port) '(current-input-port))
	  ((output-port) '(current-output-port))
	  ((error-port) '(current-output-port))
	  ((binary-port) '(current-output-port))
	  ((date) '(current-date))
	  ((obj unspec) #unspecified)
	  ((cell) '(make-cell #unspecified))
	  ((bint) 0)
	  ((llong) '(string->llong "0"))
	  ((elong) '(string->elong "0"))
	  ((bool bbool) #f)
	  ((breal) 0.0)
	  ((bchar) #\_)
	  ((nil pair-nil) ''())
	  ((pair) '(cons #f #f))
	  ((epair) '(econs #f #f #f))
	  ((bstring) "")
	  ((symbol) ''_)
	  ((keyword) ':_)
	  ((vector) ''#())
	  ((struct) `(make-struct ',(gensym) 0 #f))
	  ((procedure) 'cons)
	  ((process) '(process-nil))
	  ((custom) '(custom-nil))
	  ((opaque) '(opaque-nil))
	  ((socket) '(make-server-socket))
	  ((bucs2) '(char->ucs2 #\_))
	  ((ucs2string) '(utf8-string->ucs2-string ""))
	  ((mutex) '(mutex-nil))
	  ((condvar) '(condition-variable-nil))
	  (else (error (type-id (slot-class-owner slot))
		       (format "Illegal slot type `~a'" (type-id type))
		       (slot-id slot)))))))
   
;*---------------------------------------------------------------------*/
;*    extern-type-nil ...                                              */
;*---------------------------------------------------------------------*/
(define (extern-type-nil type slot)
   (cond
      ((or (eq? type *int*) (eq? type *long*)) 0)
      ((eq? type *bool*) #f)
      ((eq? type *real*) 0.0)
      ((eq? type *llong*) '(string->llong "0"))
      ((or (eq? type *char*) (eq? type *schar*)) #\_)
      ((eq? type *string*) "")
      ((or (jclass? type) (jarray? type))
       (make-private-sexp 'cast-null (type-id type)))
      (else
       (case (type-id type)
	  ((int long) 0)
	  ((bool) #f)
	  ((real double) 0.0)
	  ((char) #\_)
	  ((string) "")
	  (else
	   ;; a cast
	   (if *saw*
	       (make-private-sexp 'cast-null (type-id type))
	       `(,(make-typed-ident 'free-pragma (type-id type)) "0L")))))))
