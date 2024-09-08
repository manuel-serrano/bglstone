;*=====================================================================*/
;*    serrano/prgm/project/inline/Tools/multiv.sch                     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Jul  3 17:12:07 1996                          */
;*    Last change :  Thu Sep 11 08:56:58 1997 (serrano)                */
;*    -------------------------------------------------------------    */
;*    A naive implementation of multiple-values (a la Common Lisp).    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    multiple-value-bind ...                                          */
;*---------------------------------------------------------------------*/
(define-macro (multiple-value-bind vars call . exps)
   (let* ((res    (gensym 'res))
	  (pickup (let loop ((vars vars)
			     (exp  '())
			     (i    0))
		     (if (null? vars)
			 (reverse! exp)
			 (loop (cdr vars)
			       (cons `(,(car vars) (vector-ref ,res ,i))
				     exp)
			       (+fx i 1))))))
      `(let ((,res ,call))
	  (let ,pickup
	     ,@exps))))

;*---------------------------------------------------------------------*/
;*    values ...                                                       */
;*---------------------------------------------------------------------*/
(define-macro (values . exps)
   `(vector ,@exps))
