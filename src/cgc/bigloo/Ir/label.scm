;*=====================================================================*/
;*    serrano/uni/99-00/compilation/cgc0.1/Ir/label.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Feb  4 14:27:35 1998                          */
;*    Last change :  Wed Apr 26 15:11:41 2000 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The label managment (mostly creation).                           */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module ir_label
   (import ast_node
	   ir_node)
   (export get-new-label
	   (get-label ::bstring ::obj)))

;*---------------------------------------------------------------------*/
;*    get-new-label ...                                                */
;*---------------------------------------------------------------------*/
(define get-new-label
   (let ((count 1))
      (lambda (framedecl . prefix)
	 (let* ((prefix (match-case prefix
			   (((and ?value (? string?)))
			    value)
			   (else
			    "_")))
		(str    (string-append prefix (integer->string count)))
		(ident  (string->ident str)))
	    (set! count (+fx count 1))
	    (instantiate::label
	       (framedecl framedecl)
	       (ident ident))))))

;*---------------------------------------------------------------------*/
;*    get-label ...                                                    */
;*---------------------------------------------------------------------*/
(define (get-label prefix fundecl)
   (if (not (fundecl? fundecl))
       (error "get-label" "Illegal fundecl" fundecl)
       (instantiate::label
	  (framedecl fundecl)
	  (ident     (string->ident prefix)))))
      
