;*=====================================================================*/
;*    serrano/trashcan/cgc/Parser/parser.scm                           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Nov 24 11:17:34 1995                          */
;*    Last change :  Wed Dec 27 15:22:26 2000 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The Cgc syntax                                                   */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module parser_parser
   (import  parser_lexer
	    tools_speek
	    type_type
	    ast_node
	    ast_location)
   (export  *c-parser*))
 
;*---------------------------------------------------------------------*/
;*    *c-parser* ...                                                   */
;*---------------------------------------------------------------------*/
(define *c-parser*
   (lalr-grammar

      ;; tokens
      (SEMI-COMMA COMMA
       TYPEDEF IDENT STRUCT IF ELSE RETURN WHILE BREAK
       PAR-OPEN PAR-CLO BRA-OPEN BRA-CLO ANGLE-OPEN ANGLE-CLO
       = DOT BINARY-OP
       CONSTANT)

      ;; file (compilation unit) definition
      (file
       ((decls block)
	;; we build a dummy function that is this entry point of the
	;; program (the equivalent to the C main).
	(let ((main (instantiate::fundecl
		       (type (string->ident "void"))
		       (id   (string->ident "main"))
		       (params '())
		       (body block))))
	   (set! *main-decl* main)
	   (instantiate::ast
	      (decl-list (cons main decls))))))

      ;; declarations
      (decls
       (()
	'())
       ((typedecl SEMI-COMMA decls)
	(cons typedecl decls))
       ((vardecl SEMI-COMMA decls)
	(cons vardecl decls))
       ((fundecl decls)
	(cons fundecl decls)))

      ;; type declaration
      (typedecl
       ((TYPEDEF typespec IDENT)
	(typespec-id-set! typespec (cadr IDENT))
	typespec))
      
      ;; type specifier
      (typespec
       ((IDENT)
	(instantiate::typespec-alias
	   (of (cadr IDENT))))
       ((STRUCT BRA-OPEN structfields BRA-CLO)
	(instantiate::typespec-record
	   (fields structfields)))
       ((IDENT ANGLE-OPEN ANGLE-CLO)
	(instantiate::typespec-array
	   (of (cadr IDENT)))))

      ;; struct fields
      (structfields
       (()
	'())
       ((IDENT@type IDENT@id SEMI-COMMA structfields)
	(cons (cons (cadr type) (cadr id)) structfields)))

      ;; variable declaration
      (vardecl
       ((IDENT@type IDENT@id)
	(instantiate::vardecl
	   (type (cadr type))
	   (id   (cadr id)))))

      ;; function declaration
      (fundecl
       ((IDENT@type IDENT@id PAR-OPEN PAR-CLO block)
	(instantiate::fundecl
	   (type   (cadr type))
	   (id     (cadr id))
	   (params '())
	   (body   block)))
       ((IDENT@type IDENT@id PAR-OPEN paradecls PAR-CLO block)
	(instantiate::fundecl
	   (type   (cadr type))
	   (id     (cadr id))
	   (params paradecls)
	   (body   block))))

      ;; formals parameters declaration
      (paradecls
       ((vardecl)
	(list vardecl))
       ((vardecl COMMA paradecls)
	(cons vardecl paradecls)))

      ;; block statement
      (block
       ((BRA-OPEN decl-list stmt-list BRA-CLO)
	(instantiate::block
	   (decl-list decl-list)
	   (stmt-list stmt-list)))
       ((BRA-OPEN decl-list BRA-CLO)
	(instantiate::block
	   (decl-list decl-list)
	   (stmt-list '()))))

      ;; declarations associated to a block
      (decl-list
       (()
	'())
       ((decl-list decl)
	`(,@decl-list ,decl)))

      ;; a unique declaration
      (decl
       ((vardecl SEMI-COMMA)
	vardecl)
       ((fundecl)
	fundecl))
      
      ;; a list of statements
      (stmt-list
       ((stmt)
	(list stmt))
       ((stmt-list stmt)
	(append stmt-list (list stmt))))

      ;; one statement
      (stmt
       ((IF PAR-OPEN expr PAR-CLO stmt)
	(instantiate::if-then
	   (location (cadr IF))
	   (test     expr)
	   (then     stmt)))
       ((IF PAR-OPEN expr PAR-CLO stmt@true ELSE stmt@false)
	(instantiate::if-then-else
	   (location  (cadr IF))
	   (test      expr)
	   (then      true)
	   (otherwise false)))
       ((IDENT = expr SEMI-COMMA)
	(instantiate::setq
	   (location (cadr =))
	   (varref   (instantiate::varref
			(location (ident-location (cadr IDENT)))
			(vardecl  (cadr IDENT))))
	   (value    expr)))
       ((aref-expr = expr SEMI-COMMA)
	(instantiate::aset
	   (location (cadr =))
	   (aref     aref-expr)
	   (value    expr)))
       ((rref-expr = expr SEMI-COMMA)
	(instantiate::rset
	   (location (cadr =))
	   (rref     rref-expr)
	   (value    expr)))
       ((block)
	block)
       ((RETURN SEMI-COMMA)
	(instantiate::return
	   (location (cadr RETURN))))
       ((RETURN expr SEMI-COMMA)
	(instantiate::return-value
	   (location (cadr RETURN))
	   (value    expr)))
       ((WHILE PAR-OPEN expr PAR-CLO stmt)
	(instantiate::while
	   (location (cadr WHILE))
	   (test     expr)
	   (body     stmt)))
       ((BREAK SEMI-COMMA)
	(instantiate::break
	   (location (cadr BREAK))))
       ((expr-stmt)
	expr-stmt))

      ;; an expression seen as a statement
      (expr-stmt
       ((SEMI-COMMA)
	(instantiate::exprstmt
	   (location (cadr SEMI-COMMA))))
       ((expr SEMI-COMMA)
	(instantiate::exprstmt-value
	   (location (expr-location expr))
	   (>expr    expr))))

      ;; an expression
      (expr
       ((unary-expr)
	unary-expr)
       ((expr BINARY-OP unary-expr)
	(instantiate::binop
	   (location (ident-location (cadr BINARY-OP)))
	   (id       (cadr BINARY-OP))
	   (left     expr)
	   (right    unary-expr))))

      ;; a unary expression
      (unary-expr
       ((IDENT PAR-OPEN PAR-CLO)
	(instantiate::funcall
	   (location (ident-location (cadr IDENT)))
	   (fun      (cadr IDENT))
	   (actuals  '())))
       ((IDENT PAR-OPEN actuals PAR-CLO)
	(instantiate::funcall
	   (location (ident-location (cadr IDENT)))
	   (fun      (cadr IDENT))
	   (actuals  actuals)))
       ((aref-expr)
	aref-expr)
       ((rref-expr)
	rref-expr)
       ((CONSTANT)
	(let ((value    (cadr CONSTANT))
	      (location (caddr CONSTANT)))
	   (instantiate::const
	      (location location)
	      (value    value)
	      (type     (cond
			   ((integer? value)
			    (string->ident "int"))
			   ((string? value)
			    (string->ident "string"))
			   ((eq? value 'nil)
			    (string->ident "nil"))
			   (else
			    (source-error location
					  "Unknown constant type"
					  value)))))))
       ((IDENT)
	(instantiate::varref
	   (location (ident-location (cadr IDENT)))
	   (vardecl  (cadr IDENT))))
       ((PAR-OPEN expr PAR-CLO)
	expr))
      
      ;; the actual values
      (actuals
       ((expr)
	(list expr))
       ((expr COMMA actuals)
	(cons expr actuals)))

      ;; array references
      (aref-expr
       ((unary-expr ANGLE-OPEN expr ANGLE-CLO)
	(instantiate::aref
	   (location (expr-location unary-expr))
	   (array    unary-expr)
	   (offset   expr))))
      
      ;; record references
      (rref-expr
       ((unary-expr DOT IDENT)
 	(instantiate::rref
	   (location (expr-location unary-expr))
	   (record   unary-expr)
	   (field-id (cadr IDENT)))))))
      
      
