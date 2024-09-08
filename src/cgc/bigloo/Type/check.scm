;*=====================================================================*/
;*    serrano/trashcan/cgc/Type/check.scm                              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Jan 28 08:02:48 1998                          */
;*    Last change :  Wed Dec 27 15:23:20 2000 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The type checking                                                */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module type_check
   (import ast_node
	   ast_location
	   type_type
	   type_bind
	   tools_speek
	   runtime_runtime)
   (export (type-checks ::ast)))

;*---------------------------------------------------------------------*/
;*    type-checks ...                                                  */
;*---------------------------------------------------------------------*/
(define (type-checks ast)
   (verbose 1 #"  - type checking\n")
   (with-access::ast ast (decl-list)
      ;; we walk thru all function definition
      (for-each (lambda (decl)
		   (if (fundecl? decl)
		       (type-check/fundecl decl)))
		(append (runtime-fundecls) decl-list))))

;*---------------------------------------------------------------------*/
;*    type-check/fundecl ...                                           */
;*---------------------------------------------------------------------*/
(define (type-check/fundecl decl::fundecl)
   (with-access::fundecl decl (id params body type)
      (verbose 2 "    . scanning " id ": ")
      (let loop ((params params))
	 (cond
	    ((null? params)
	     (verbose 2 " -> " type #"\n"))
	    ((null? (cdr params))
	     (verbose 2 (vardecl-type (car params)))
	     (loop (cdr params)))
	    (else
	     (verbose 2 (vardecl-type (car params)) " x ")
	     (loop (cdr params)))))
      (type-check body #unspecified type)))

;*---------------------------------------------------------------------*/
;*    type-check ::stmt ...                                            */
;*    -------------------------------------------------------------    */
;*    When applied to an expression this function returns the          */
;*    type of the expression. Otherwise the return value is            */
;*    unspecified.                                                     */
;*---------------------------------------------------------------------*/
(define-generic (type-check body totype rettype))

;*---------------------------------------------------------------------*/
;*    type-check ::stmt ...                                            */
;*    -------------------------------------------------------------    */
;*    This method is given only for runtime library functions that     */
;*    have a dummy body. This function body is just an instance        */
;*    of the stmt class (that can be still considered as an abstract   */
;*    class).                                                          */
;*---------------------------------------------------------------------*/
(define-method (type-check body::stmt totype rettype)
   totype)
   
;*---------------------------------------------------------------------*/
;*    type-check ::block ...                                           */
;*---------------------------------------------------------------------*/
(define-method (type-check body::block totype rettype)
   (with-access::block body (decl-list stmt-list)
      (for-each (lambda (decl)
		   (if (fundecl? decl)
		       (type-check/fundecl decl)))
		decl-list)
      (if (pair? stmt-list)
	  (let loop ((stmt-list stmt-list))
	     (cond
		((null? (cdr stmt-list))
		 (type-check (car stmt-list) totype rettype))
		(else
		 (type-check (car stmt-list) #unspecified rettype)
		 (loop (cdr stmt-list))))))))

;*---------------------------------------------------------------------*/
;*    type-check ::if-then ...                                         */
;*---------------------------------------------------------------------*/
(define-method (type-check body::if-then totype rettype)
   (with-access::if-then body (test then)
      (type-check test *type-int* *type-void*)
      (type-check then totype rettype)))

;*---------------------------------------------------------------------*/
;*    type-check ::if-then-else ...                                    */
;*---------------------------------------------------------------------*/
(define-method (type-check body::if-then-else totype rettype)
   (with-access::if-then-else body (otherwise)
      (call-next-method)
      (type-check otherwise totype rettype)))

;*---------------------------------------------------------------------*/
;*    type-check ::setq ...                                            */
;*---------------------------------------------------------------------*/
(define-method (type-check body::setq totype rettype)
   (with-access::setq body (varref value)
      (with-access::varref varref (vardecl)
	 (with-access::vardecl vardecl (type)
	    (type-check value type *type-void*)))))

;*---------------------------------------------------------------------*/
;*    type-check ::aset ...                                            */
;*---------------------------------------------------------------------*/
(define-method (type-check body::aset totype rettype)
   (with-access::aset body (location aref value)
      (let ((type (type-check aref #unspecified *type-void*)))
	 (type-check value type *type-void*)))
   (if (not (type-compatible? *type-void* totype))
       ((@ type-error type_type) (stmt-location body) totype *type-void*)))

;*---------------------------------------------------------------------*/
;*    type-check ::rset ...                                            */
;*---------------------------------------------------------------------*/
(define-method (type-check body::rset totype rettype)
   (with-access::rset body (location rref value)
      (let ((type (type-check rref #unspecified *type-void*)))
	 (type-check value type *type-void*)))
   (if (not (type-compatible? *type-void* totype))
       ((@ type-error type_type) (stmt-location body) totype *type-void*)))

;*---------------------------------------------------------------------*/
;*    type-check ::return ...                                          */
;*---------------------------------------------------------------------*/
(define-method (type-check body::return totype rettype)
   (if (not (type-compatible? *type-void* rettype))
       ((@ type-error type_type) (stmt-location body) rettype *type-void*)))

;*---------------------------------------------------------------------*/
;*    type-check ::return/value ...                                    */
;*---------------------------------------------------------------------*/
(define-method (type-check body::return-value totype rettype)
   (with-access::return-value body (value)
      (type-check value rettype *type-void*)))

;*---------------------------------------------------------------------*/
;*    type-check ::while ...                                           */
;*---------------------------------------------------------------------*/
(define-method (type-check bod::while totype rettype)
   (with-access::while bod (test body)
      (type-check test *type-int* rettype)
      (type-check body totype rettype)))
   
;*---------------------------------------------------------------------*/
;*    type-check ::exprstmt ...                                        */
;*---------------------------------------------------------------------*/
(define-method (type-check body::exprstmt totype rettype)
   (if (not (type-compatible? *type-void* totype))
       ((@ type-error type_type) (stmt-location body) totype *type-void*)))

;*---------------------------------------------------------------------*/
;*    type-check ::exprstmt-value ...                                  */
;*---------------------------------------------------------------------*/
(define-method (type-check body::exprstmt-value totype rettype)
   (with-access::exprstmt-value body (>expr)
      (type-check >expr #unspecified rettype)))

;*---------------------------------------------------------------------*/
;*    type-check ::binop ...                                           */
;*---------------------------------------------------------------------*/
(define-method (type-check body::binop totype rettype)
   (with-access::binop body (location id left right)
      (cond
	 ((ident-is? id "==")
	  (let* ((tleft  (type-check left #unspecified *type-void*))
		 (tright (type-check right tleft *type-void*)))
	     (if (not (type-compatible? *type-int* totype))
		 ((@ type-error type_type) location totype *type-int*)
		 *type-int*)))
	 (else
	  (let ((tleft  (type-check left *type-int* *type-void*))
		(tright (type-check right *type-int* *type-void*)))
	     (if (not (type-compatible? *type-int* totype))
		 ((@ type-error type_type) location totype *type-int*)
		 *type-int*))))))

;*---------------------------------------------------------------------*/
;*    type-check ::funcall ...                                         */
;*---------------------------------------------------------------------*/
(define-method (type-check body::funcall totype rettype)
   (with-access::funcall body (location fun actuals)
      (with-access::fundecl fun (type params id)
	 (let ((len-params  (length params))
	       (len-actuals (length actuals)))
	    (cond
	       ((< len-params len-actuals)
		(source-error location "too many arguments" id))
	       ((> len-params len-actuals)
		(source-error location "too few arguments" id))
	       (else
		(for-each (lambda (param actual)
			     (with-access::vardecl param (type)
				(type-check actual type *type-void*)))
			  params actuals)
		(if (not (type-compatible? type totype))
		       ((@ type-error type_type) location totype type)
		    type)))))))

;*---------------------------------------------------------------------*/
;*    type-check ::aref ...                                            */
;*---------------------------------------------------------------------*/
(define-method (type-check body::aref totype rettype)
   (with-access::aref body (location array offset)
      (type-check offset *type-int* *type-void*)
      (let ((tarray (type-check array #unspecified *type-void*)))
	 (if (not (array? tarray))
	     ((@ type-error type_type) location "array" tarray)
	     (let ((tof (array-of tarray)))
		(if (not (type-compatible? tof totype))
		    ((@ type-error type_type) location totype tof)
		    tof))))))

;*---------------------------------------------------------------------*/
;*    type-check ::rref ...                                            */
;*---------------------------------------------------------------------*/
(define-method (type-check body::rref totype rettype)
   (with-access::rref body (location record field-id record-type)
      (let ((trecord (type-check record #unspecified *type-void*)))
	 (set! record-type trecord)
	 (if (not (structure? trecord))
	     ((@ type-error type_type) location "record" trecord)
	     (let loop ((fields (structure-fields trecord)))
		(cond
		   ((null? fields)
		    (source-error location "No such field" field-id))
		   ((ident=? field-id (cdr (car fields)))
		    (let ((type (car (car fields))))
		       (if (not (type-compatible? type totype))
			   ((@ type-error type_type) location totype type)
			   type)))
		   (else
		    (loop (cdr fields)))))))))

;*---------------------------------------------------------------------*/
;*    type-check ::varref ...                                          */
;*---------------------------------------------------------------------*/
(define-method (type-check body::varref totype rettype)
   (with-access::varref body (location vardecl)
      (with-access::vardecl vardecl (type)
	 (if (type-compatible? type totype)
	     type
	     ((@ type-error type_type) location totype type)))))

;*---------------------------------------------------------------------*/
;*    type-check ::const ...                                           */
;*---------------------------------------------------------------------*/
(define-method (type-check body::const totype rettype)
   (with-access::const body (location type)
      (if (type-compatible? type totype)
	  type
	  ((@ type-error type_type) location totype type))))

	    

   

