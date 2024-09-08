;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Object/generic.scm          */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed May  1 12:05:09 1996                          */
;*    Last change :  Wed Nov  8 21:42:52 2000 (serrano)                */
;*    Copyright   :  1996-2000 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The generic management                                           */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module object_generic
   (import engine_param
	   tools_args
	   tools_error
	   type_type
	   type_cache
	   ast_var
	   ast_ident
	   object_class
	   module_module
	   (find-location tools_location))
   (export (make-generic-body ::symbol ::obj ::obj ::obj)))

;*---------------------------------------------------------------------*/
;*    make-generic-body ...                                            */
;*---------------------------------------------------------------------*/
(define (make-generic-body id locals args src)
   (define (err msg)
      (user-error id msg src '()))
   (if (null? args)
       (err "Illegal generic definition (no formal arguments provided)")
       (let* ((method-arg (car locals))
	      (method-arg-type (local-type method-arg)))
	  (cond
	     ((tclass? method-arg-type)
	      (make-object-generic-body id locals args src))
	     ((and (type? method-arg-type)
		   (not (eq? method-arg-type *obj*))
		   (not (eq? method-arg-type (get-default-type))))
	      (err "generic function has a non-class dispatching type arg"))
	     (else
	      (make-non-object-generic-body id locals args src))))))
   
;*---------------------------------------------------------------------*/
;*    make-object-generic-body ...                                     */
;*---------------------------------------------------------------------*/
(define (make-object-generic-body id locals args src)
   (let* ((pid             (parse-id id (find-location src)))
	  (id              (car pid))
	  (arity           (arity args))
	  (args-id         (map local-id locals))
	  (method-arg      (car locals))
	  (method-arg-id   (local-id method-arg))
	  (method-arg-type (local-type method-arg))
	  (method          (gensym 'method))
	  (tmethod         (make-typed-ident method 'procedure)))
      `(let ((,tmethod (find-method ,method-arg-id (@ ,id ,*module*))))
	  ,(if (>=fx arity 0)
	       `(,method ,@args-id)
	       `(apply ,method (cons* ,@args-id))))))

;*---------------------------------------------------------------------*/
;*    make-non-object-generic-body ...                                 */
;*---------------------------------------------------------------------*/
(define (make-non-object-generic-body id locals args src)
   (let* ((pid             (parse-id id (find-location src)))
	  (id              (car pid))
	  (arity           (arity args))
	  (args-id         (map local-id locals))
	  (default-body    (if (>=fx arity 0)
			       `((generic-default
				  (@ ,id ,*module*))
				 ,@args-id)
			       `(apply (generic-default
					(@ ,id ,*module*))
				       ((@ cons*
					   __r4_pairs_and_lists_6_3)
					,@args-id))))
	  (method-arg      (car locals))
	  (method-arg-id   (local-id method-arg))
	  (method-arg-type (local-type method-arg))
	  (method          (gensym 'method))
	  (tmethod         (make-typed-ident method 'procedure))
	  (default-name    (symbol-append id '-default))
	  (app-ly-method   `(let ((,tmethod (find-method ,method-arg-id
							 (@ ,id
							    ,*module*))))
			       (if (procedure? ,method)
				   ,(if (>=fx arity 0)
					`(,method ,@args-id)
					`(apply ,method (cons* ,@args-id)))
				   (begin
				      (,default-name))))))
      ;; we now create the body of the generic
      `(labels ((,default-name () ,default-body))
	  (if (object? ,method-arg-id)
	       ,app-ly-method
	       (,default-name)))))

   
