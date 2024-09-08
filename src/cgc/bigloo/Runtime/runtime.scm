;*=====================================================================*/
;*    serrano/uni/97-98/maitrise/compil/cgc/Runtime/runtime.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Mar 22 13:28:26 1998                          */
;*    Last change :  Sun Mar 22 13:39:37 1998 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The description of the runtime system                            */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module runtime_runtime
   (import  parser_lexer
	    tools_speek
	    type_type
	    ast_node
	    ast_location)
   (export (runtime-fundecls)))

;*---------------------------------------------------------------------*/
;*    runtime-fundecls ...                                             */
;*    -------------------------------------------------------------    */
;*    This function returns the list of the pre-defined function       */
;*    declaration that corresponds to library functions.               */
;*---------------------------------------------------------------------*/
(define (runtime-fundecls)
   (if (pair? *fundecls*)
       *fundecls*
       (begin
	  (set! *fundecls*
		(list (make-runtime-fundecl "print_int" "void" "int")
		      (make-runtime-fundecl "print_string" "void" "string")))
	  *fundecls*)))

;*---------------------------------------------------------------------*/
;*    *fundecls* ...                                                   */
;*    -------------------------------------------------------------    */
;*    The list of all runtime functions.                               */
;*---------------------------------------------------------------------*/
(define *fundecls* #unspecified)

;*---------------------------------------------------------------------*/
;*    make-runtime-fundecl ...                                         */
;*---------------------------------------------------------------------*/
(define (make-runtime-fundecl name type . args)
   (instantiate::fundecl
      (type   (string->ident type))
      (id     (string->ident name))
      (params (map (lambda (targ)
		      (instantiate::vardecl
			 (type (string->ident targ))
			 (id   (string->ident "_"))))
		   args))
      (body   (instantiate::stmt))))




