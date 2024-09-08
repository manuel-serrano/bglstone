;*=====================================================================*/
;*    serrano/prgm/project/bglstone/src/cgc/bigloo/Ast/escape.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Jan 28 08:02:48 1998                          */
;*    Last change :  Fri Oct 24 09:36:14 2003 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The escape property.                                             */
;*    -------------------------------------------------------------    */
;*    A variable is said to escape if and only if it is used inside a  */
;*    function that is not the one declaring the variable.             */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module ast_escape 
   (import ast_node
	   ast_location
	   type_type
	   type_bind
	   tools_speek
	   runtime_runtime)
   (export (escape-ast ::ast)))

;*---------------------------------------------------------------------*/
;*    escape-ast ...                                                   */
;*---------------------------------------------------------------------*/
(define (escape-ast ast)
   (verbose 1 #"  - escape property\n")
   (with-access::ast ast (decl-list)
      ;; We walk thru variable declaration to widen global variables.
      (let loop ((decl-list decl-list)
		 (num       0))
	 (if (pair? decl-list)
	     (let ((decl (car decl-list)))
		(if (vardecl? decl)
		    (with-access::vardecl decl (fetch)
		       (set! fetch num)
		       (widen!::global decl)
		       (loop (cdr decl-list) (+fx num 1)))
		    (loop (cdr decl-list) num)))))
      ;; We walk thru all function definition and runtime declarations
      (for-each (lambda (decl)
		   (if (fundecl? decl)
		       (escape/fundecl decl '())))
		(append (runtime-fundecls) decl-list))))

;*---------------------------------------------------------------------*/
;*    escape/fundecl ...                                               */
;*---------------------------------------------------------------------*/
(define (escape/fundecl decl::fundecl frame-list)
   (with-access::fundecl decl (id params body)
      (for-each (lambda (param)
		   (widen!::local param))
		params)
      (verbose 3 "    . scanning " id #\Newline)
      (escape body (cons params frame-list))))
 
;*---------------------------------------------------------------------*/
;*    escape :: ...                                                    */
;*---------------------------------------------------------------------*/
(define-generic (escape body frame-list))

;*---------------------------------------------------------------------*/
;*    escape ::stmt ...                                                */
;*    -------------------------------------------------------------    */
;*    This method is given only for runtime library functions that     */
;*    have a dummy body. This function body is just an instance        */
;*    of the stmt class (that can be still considered as an abstract   */
;*    class).                                                          */
;*---------------------------------------------------------------------*/
(define-method (escape body::stmt frame-list)
   body)
		       
;*---------------------------------------------------------------------*/
;*    escape ::block ...                                               */
;*---------------------------------------------------------------------*/
(define-method (escape body::block frame-list)
   (with-access::block body (decl-list stmt-list)
      (for-each (lambda (decl)
		   (if (vardecl? decl)
		       (widen!::local decl)))
		decl-list)
      (let* ((frame          (car frame-list))
	     (new-frame      (append decl-list frame))
	     (new-frame-list (cons new-frame (cdr frame-list))))
	 (for-each (lambda (decl)
		      (if (fundecl? decl)
			  (escape/fundecl decl new-frame-list)))
		   decl-list)
	 (for-each (lambda (stmt)
		      (escape stmt new-frame-list))
		   stmt-list))))

;*---------------------------------------------------------------------*/
;*    escape ::if-then ...                                             */
;*---------------------------------------------------------------------*/
(define-method (escape body::if-then frame-list)
   (with-access::if-then body (test then)
      (escape test frame-list)
      (escape then frame-list)))

;*---------------------------------------------------------------------*/
;*    escape ::if-then-else ...                                        */
;*---------------------------------------------------------------------*/
(define-method (escape body::if-then-else frame-list)
   (with-access::if-then-else body (otherwise)
      (call-next-method)
      (escape otherwise frame-list)))

;*---------------------------------------------------------------------*/
;*    escape ::setq ...                                                */
;*---------------------------------------------------------------------*/
(define-method (escape body::setq frame-list)
   (with-access::setq body (varref value)
      (escape varref frame-list)
      (escape value frame-list)))

;*---------------------------------------------------------------------*/
;*    escape ::aset ...                                                */
;*---------------------------------------------------------------------*/
(define-method (escape body::aset frame-list)
   (with-access::aset body (aref value)
      (escape aref frame-list)
      (escape value frame-list)))

;*---------------------------------------------------------------------*/
;*    escape ::rset ...                                                */
;*---------------------------------------------------------------------*/
(define-method (escape body::rset frame-list)
   (with-access::rset body (rref value)
      (escape rref frame-list)
      (escape value frame-list)))

;*---------------------------------------------------------------------*/
;*    escape ::return ...                                              */
;*---------------------------------------------------------------------*/
(define-method (escape body::return frame-list)
   #unspecified)

;*---------------------------------------------------------------------*/
;*    escape ::return/value ...                                        */
;*---------------------------------------------------------------------*/
(define-method (escape body::return-value frame-list)
   (with-access::return-value body (value)
      (escape value frame-list)))

;*---------------------------------------------------------------------*/
;*    escape ::while ...                                               */
;*---------------------------------------------------------------------*/
(define-method (escape bod::while frame-list)
   (with-access::while bod (test body)
      (escape test frame-list)
      (escape body frame-list)))
   
;*---------------------------------------------------------------------*/
;*    escape ::exprstmt ...                                            */
;*---------------------------------------------------------------------*/
(define-method (escape body::exprstmt frame-list)
   #unspecified)

;*---------------------------------------------------------------------*/
;*    escape ::exprstmt/value ...                                      */
;*---------------------------------------------------------------------*/
(define-method (escape body::exprstmt-value frame-list)
   (with-access::exprstmt-value body (>expr)
      (escape >expr frame-list)))

;*---------------------------------------------------------------------*/
;*    escape ::binop ...                                               */
;*---------------------------------------------------------------------*/
(define-method (escape body::binop frame-list)
   (with-access::binop body (left right)
      (escape left frame-list)
      (escape right frame-list)))

;*---------------------------------------------------------------------*/
;*    escape ::funcall ...                                             */
;*---------------------------------------------------------------------*/
(define-method (escape body::funcall frame-list)
   (with-access::funcall body (location fun actuals)
      (for-each (lambda (actual)
		      (escape actual frame-list))
		   actuals)))

;*---------------------------------------------------------------------*/
;*    escape ::aref ...                                                */
;*---------------------------------------------------------------------*/
(define-method (escape body::aref frame-list)
   (with-access::aref body (array offset)
      (escape offset frame-list)
      (escape array frame-list)))

;*---------------------------------------------------------------------*/
;*    escape ::rref ...                                                */
;*---------------------------------------------------------------------*/
(define-method (escape body::rref frame-list)
   (with-access::rref body (record)
      (escape record frame-list)))

;*---------------------------------------------------------------------*/
;*    escape ::varref ...                                              */
;*---------------------------------------------------------------------*/
(define-method (escape body::varref frame-list)
   (with-access::varref body (vardecl)
      (let ((frame (car frame-list)))
	 (if (and (local? vardecl) (not (memq vardecl frame)))
	     (with-access::local vardecl (id escape?)
		(if (not escape?)
		    (begin
		       (verbose 2 "       . " id #" escapes\n")
		       (set! escape? #t)))
		;; We know that the variable escapes, we have to compute
		;; its escape level. That is, we look for the frames in
		;; frame list to find the one that defines it. The position
		;; of the frame in the list is the depth.
		(let loop ((frames (cdr frame-list))
			   (depth  1))
		   (cond
		      ((null? frames)
		       (error "escape::varref" "Unbound local variable" id))
		      ((memq vardecl (car frames))
		       (varref-depth-set! body depth))
		      (else
		       (loop (cdr frames) (+fx depth 1))))))))))

;*---------------------------------------------------------------------*/
;*    escape ::const ...                                               */
;*---------------------------------------------------------------------*/
(define-method (escape body::const frame-list)
   #unspecified)

	    

   

