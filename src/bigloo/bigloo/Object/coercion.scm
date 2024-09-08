;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Object/coercion.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Jul 17 10:02:36 2000                          */
;*    Last change :  Sat Sep 27 21:37:40 2003 (serrano)                */
;*    Copyright   :  2000-03 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    We make the class coercions functions.                           */
;*    -------------------------------------------------------------    */
;*    In this module we cannot use consume-module-clause! because      */
;*    the importation are already done.                                */
;*    -------------------------------------------------------------    */
;*    This constructors does not require any importation information   */
;*    since all accessors are always static.                           */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module object_coercion
   (include "Tools/trace.sch")
   (import  tools_error
	    tools_misc
	    tools_shape
	    type_type
	    type_env
	    type_tools
	    type_cache
	    ast_var
	    ast_ident
	    object_class
	    object_struct
	    object_slots
	    object_tools
	    module_module
	    module_impuse
	    engine_param
	    ast_private)
   (export (gen-class-coercions! class)
	   (gen-java-class-coercions! class)
	   (gen-coercion-clause! c-id super . testing)
	   (gen-class-coercers! class super)))

;*---------------------------------------------------------------------*/
;*    gen-class-coercions! ...                                         */
;*---------------------------------------------------------------------*/
(define (gen-class-coercions! class)
   (with-access::tclass class (id its-super)
      (gen-coercion-clause! id its-super)
      (gen-class-coercers! class its-super)))

;*---------------------------------------------------------------------*/
;*    gen-java-class-coercions! ...                                    */
;*---------------------------------------------------------------------*/
(define (gen-java-class-coercions! class)
   (with-access::jclass class (id its-super)
      (gen-coercion-clause! id its-super)
      (gen-class-coercers! class its-super)))

;*---------------------------------------------------------------------*/
;*    gen-coercion-clause! ...                                         */
;*    -------------------------------------------------------------    */
;*    This function has to take care that the super class may be       */
;*    incorrect (because this error is now detected very late in       */
;*    compilation). Thus on the iteration on super, we have to check   */
;*    that super is a class. If not, it is not a problem, we can       */
;*    simply stop the iteration. We can do this simple thing because   */
;*    eventually the super error will be detected and the compilation  */
;*    will be stopped.                                                 */
;*---------------------------------------------------------------------*/
(define (gen-coercion-clause! c-id super . testing)
   (produce-module-clause! 
    (case *target-language*
       ((jvm .net)
	(jvm-make-coercion-clause c-id super testing))
       (else
	(c-make-coercion-clause c-id super testing)))))

;*---------------------------------------------------------------------*/
;*    c-make-coercion-clause ...                                       */
;*---------------------------------------------------------------------*/
(define (c-make-coercion-clause c-id super testing)
   (let* ((class->obj (class->obj-id c-id))
	  (obj->class (obj->class-id c-id))
	  (ttest  (if (null? testing)
		      (list (class?-id c-id))
		      '())))
      (let loop ((super   super)
		 (coercer (list
			   `(coerce obj ,c-id ,ttest (,obj->class))
			   `(coerce ,c-id obj () (,class->obj))
			   `(coerce ,c-id bool () ((lambda (x::char) #t))))))
	 (if (not (tclass? super))
	     `(type ,@coercer)
	     (let* ((super-id     (tclass-id super))
		    (class->super (class->super-id c-id super-id))
		    (super->class (super->class-id super-id c-id)))
		(loop (tclass-its-super super)
		      (cons
		       `(coerce ,super-id ,c-id	,ttest (,super->class))
		       (cons
			`(coerce ,c-id ,super-id () (,class->super))
			coercer))))))))

;*---------------------------------------------------------------------*/
;*    jvm-make-coercion-clause ...                                     */
;*---------------------------------------------------------------------*/
(define (jvm-make-coercion-clause c-id super testing)
   (let* ((class->obj `(lambda (x)
			  ,(make-private-sexp 'cast 'obj 'x)))
	  (obj->class `(lambda (x)
			  ,(make-private-sexp 'cast c-id 'x)))
	  (ttest (if (null? testing)
		     (list (class?-id c-id))
		     '())))
      (let loop ((super   super)
		 (coercer (list
			   `(coerce obj ,c-id ,ttest (,obj->class))
			   `(coerce ,c-id obj () (,class->obj))
			   `(coerce ,c-id bool () ((lambda (x::char) #t))))))
	 (if (not (or (jclass? super) (tclass? super)))
	     `(type ,@coercer)
	     (let* ((super-id     (if (tclass? super)
				      (tclass-id super)
				      (jclass-id super)))
		    (class->super `(lambda (x)
				      ,(make-private-sexp 'cast super-id 'x)))
		    (super->class `(lambda (x)
				      ,(make-private-sexp 'cast c-id 'x))))
		(loop (if (tclass? super)
			  (tclass-its-super super)
			  (jclass-its-super super))
		      (cons
		       `(coerce ,super-id ,c-id	,ttest (,super->class))
		       (cons
			`(coerce ,c-id ,super-id () (,class->super))
			coercer))))))))

;*---------------------------------------------------------------------*/
;*    gen-class-coercers! ...                                          */
;*    -------------------------------------------------------------    */
;*    We create all the coercers between type, obj and its super       */
;*    classes.                                                         */
;*    -------------------------------------------------------------    */
;*    This function has to take care that the super class may be       */
;*    incorrect (because this error is now detected very late in       */
;*    compilation). Thus on the iteration on super, we have to check   */
;*    that super is a class. If not, it is not a problem, we can       */
;*    simply stop the iteration. We can do this simple thing because   */
;*    eventually the super error will be detected and the compilation  */
;*    will be stopped.                                                 */
;*---------------------------------------------------------------------*/
(define (gen-class-coercers! class super)
   (case *target-language*
      ((jvm .net)
       (jvm-gen-class-coercers! class super))
      (else
       (c-gen-class-coercers! class super))))

;*---------------------------------------------------------------------*/
;*    c-gen-class-coercers! ...                                        */
;*---------------------------------------------------------------------*/
(define (c-gen-class-coercers! class super)
   (define (make-one-coercion from-id from-name to-id to-name)
      (let ((t->f (symbol-append to-id '-> from-id))
	    (f->t (symbol-append from-id '-> to-id)))
	 (produce-module-clause!
	  `(pragma (,t->f side-effect-free no-cfa-top (effect))
		   (,f->t side-effect-free no-cfa-top (effect))))
	 (list `(macro ,from-id ,t->f (,to-id)
		       ,(string-append "(" from-name ")"))
	       `(macro ,to-id ,f->t (,from-id)
		       ,(string-append "(" to-name ")")))))
   (let ((tid   (type-id   class))
	 (tname (type-name class)))
      (let loop ((super   super)
		 (coercer (make-one-coercion tid tname 'obj "obj_t")))
	 (if (not (tclass? super))
	     (produce-module-clause! `(foreign ,@coercer))
	     (let ((sid   (type-id super))
		   (sname (type-name super)))
		(loop (tclass-its-super super)
		      (append (make-one-coercion tid tname sid sname)
			      coercer)))))))

;*---------------------------------------------------------------------*/
;*    jvm-gen-class-coercers! ...                                      */
;*---------------------------------------------------------------------*/
(define (jvm-gen-class-coercers! class super)
   #unspecified)
   

