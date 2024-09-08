;*=====================================================================*/
;*    serrano/prgm/project/bglstone/src/cgc/bigloo/Ast/anode.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  SERRANO Manuel                                    */
;*    Creation    :  Mon Aug 25 14:02:52 1997                          */
;*    Last change :  Tue Nov 24 16:00:57 2015 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The definition of the Abstract Syntax Tree                       */
;*=====================================================================*/
 
;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module ast_node

   (from   ast_ident)

   (include "Ast/anode.sch")
   
   (export ;; a compilation unit
           (class ast
	      decl-list)
	   
	   ;; expression
	   (class expr
	      (location (default #unspecified)))

           ;; declaration. It is convenient to have decl < expr. That
	   ;; way, a variable can be, in the ast a simple variable
	   ;; declaration
           (class decl::expr
	      (id (default #unspecified)))

	   ;; type declaration
	   (class typespec::decl)

	   ;; alias type
	   (class typespec-alias::typespec
	      of::ident)

	   ;; record type
	   (class typespec-record::typespec
	      fields)

	   ;; array type
	   (class typespec-array::typespec
	      of::ident)

	   ;; variable declaration
	   (final-class vardecl::decl
	      ;; This field is used to compute the code to fetch the
	      ;; variable. It has three different meaning depending
	      ;; on the variable type (global, local escaping, local non
	      ;; escaping).
	      (fetch (default #unspecified))
	      ;; the type of the variable
	      (type (default #unspecified)))

	   ;; local variable declaration
	   (wide-class local::vardecl
	      ;; Does the variable escapes? (i.e. is the variable
	      ;; used inside a nested function?).
	      (escape?::bool (default #f)))
	      
	   ;; global variable declaration
	   (wide-class global::vardecl)
	      
	   ;; function declaration
	   (final-class fundecl::decl
	      ;; the return type
	      (type (default #unspecified))
	      ;; the formals vardecl
	      (params (default '()))
	      ;; the body of the function
	      body::stmt
	      ;; the nesting level
	      (depth (default 0)))


	   ;; statement
	   (class stmt
	      (location (default #unspecified)))
	   
	   ;; block statement
	   (class block::stmt
	      decl-list
	      stmt-list)

	   ;; if-then statement
	   (class if-then::stmt
	      test::expr
	      then::stmt)

	   ;; if-then-else statement
	   (class if-then-else::if-then
	      otherwise::stmt)

	   ;; setq statement
	   (class setq::stmt
	      varref
	      value::expr)

	   ;; array set
	   (class aset::stmt
	      aref::aref
	      value::expr)

	   ;; record set
	   (class rset::stmt
	      rref::rref
	      value::expr)

	   ;; return and break
	   (class return::stmt)
	   (class break::return)
	   (class return-value::return
	      value::expr)
	   
	   ;; while statement
	   (class while::stmt
	      test::expr
	      body::stmt)

	   ;; expression statement
	   (class exprstmt::stmt)
	   (class exprstmt-value::exprstmt
	      >expr::expr)

	   ;; binary expression
	   (class binop::expr
	      id::ident
	      left::expr
	      right::expr)

	   ;; funcall
	   (class funcall::expr
	      fun
	      (actuals (default '())))

	   ;; array ref
	   (class aref::expr
	      array::expr
	      offset::expr)

	   ;; record ref
	   (class rref::expr
	      record::expr
	      (record-type (default #unspecified))
	      field-id::ident)

	   ;; constant
	   (class const::expr
	      value
	      (type (default #unspecified)))

	   ;; variable reference
	   (class varref::expr
	      ;; the nested level of the variable access
	      ;; (0 means that the variable is defined in the current
	      ;; frame, > 0 means the number of static to be followed
	      ;; to fetch the frame declaring the variable).
	      (depth::int (default 0))
	      ;; the associated variable declaration 
	      vardecl)

	   ;; the main function declaration
	   *main-decl*))

;*---------------------------------------------------------------------*/
;*    *main-decl* ...                                                  */
;*    -------------------------------------------------------------    */
;*    This variable is set by the parser (parser_parser) and use       */
;*    in the canonicalize module                                       */
;*---------------------------------------------------------------------*/
(define *main-decl* #unspecified)
