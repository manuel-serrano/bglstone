;*=====================================================================*/
;*    serrano/prgm/project/bglstone/src/cgc/bigloo/Ir/inode.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Feb  4 09:02:52 1998                          */
;*    Last change :  Tue Nov 24 16:07:57 2015 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The intermediate representation                                  */
;*=====================================================================*/
 
;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module ir_node

   (import ast_node
           arch_arch)

   (include "Ir/inode.sch")

   (export ;; a complete intermediate tree
           (class ir
	      ;; the text of the compilation
	      text-segment
	      ;; the segment for global variables
	      bss-segment
	      ;; the segment for literal
	      data-segment)

	   ;; A statement (another abstract class)
	   (class ir-stmt)

	   ;; A frame declaration. As every construction here it is
	   ;; target independant _and_ source language independant.
	   ;; Every cgc specific construction can be found in the module
	   ;; implementing the translation from ast to ir.
	   (wide-class framedecl::fundecl
	      ;; A fresh label for the function entrance
	      prolog::label
	      ;; A link to an enclosing frame declaration (if needed)
	      (link (default #unspecified))
	      ;; The ast body translated into an ir + the shift of view.
	      ir-stmt::ir-stmt
	      ;; A fresh label for the function epilogue
	      epilogue::label
	      ;; Do we have to save the frame pointer ?
	      (frame-pointer-saved? (default (not (eq? (arch-fp-expr) #f))))
	      ;; The number of local in the frame because of escaping.
	      (local-num::int (default 0))
	      ;; The list of register to be pushed when entering the
	      ;; function
	      (saved-registers (default (list (arch-reta-expr))))
	      ;; The number of saved variable because of spills
	      ;; (this field is used by the instructions selection,
	      ;; the registers allocation and the activation records
	      ;; allocation).
	      (spill-num::int (default 0))
	      ;; The spilled variables. This vector is used be allocators
	      ;; that re-use stack rooms for spilled variables (such as
	      ;; the minimal push).
	      (spilled (default #unspecified)))

	   ;; A basic block is a list of ir-stmt.
	   ;; The first statement is a label
	   ;; The last statement is a jump or cjump.
	   ;; The list of statement include neither labels, jumps nor cjumps.
	   (class basic-block
	      (mark (default #f))
	      prolog::label
	      body
	      epilogue::ir-stmt)
	   
           ;; An expression (an abstract class)
           (class ir-expr)
	   
           ;; An integer constant
           (class ir-const::ir-expr
	      value::int)

	   ;; A symbolic constant
	   (class name::ir-expr
	      label::label)

	   ;; A temporary (similar to a register in a real machine)
	   (class temp::ir-expr
	      ;; a key used by temp sets
	      (key (default -1))
	      ;; the name of the temporary
	      name::bstring
	      ;; the interference raw for this temporay (i.e. the set
	      ;; of temporary live at the same moment as this temporary)
	      (interference (default #unspecified))
	      ;; the physical room associated to this temporary. This
	      ;; slot is filled by the register allocator.
	      (hardware (default #unspecified)))

	   ;; A register. This is used for implementing fix temporaries.
	   ;; For instance, we have a fp register. We know that we will
	   ;; allocate it in a register. Thus fp is a register instead of a
	   ;; temporary
	   (class ireg::temp
	      ;; if this register is associated to a temporary (during
	      ;; the register allocation stage), a pointer to this temporary.
	      ;; This field is used when spilling a temporary.
	      (temp (default #unspecified)))
	   
	   ;; A binary operator (e.g. +, -, ...)
	   (class opfx::ir-expr
	      op::symbol
	      left::ir-expr
	      right::ir-expr)

	   ;; The contents of one word-size of memory, starting at
	   ;; address addr. Note that when mem is used as the left
	   ;; child of a move, it means `store', but anywhere else, it means
	   ;; `fetch'
	   (class mem::ir-expr
	      addr::ir-expr)

	   ;; A procedure call; the application of fun to args. 
	   (class call::ir-expr
	      ;; It is possible that we assign to this field a fundecl
	      ;; that as not been processed yet. Thus the type of this
	      ;; field is not framedecl. Later on, when we will use the
	      ;; field we are guaranted that the value will be a framedecl.
	      framedecl::fundecl
	      args)
	   
	   ;; Evaluate expr and move it into temporary tmp
	   (class move-temp::ir-stmt
	      temp::temp
	      expr::ir-expr)

	   ;; Evaluate expression addr, yielding address a. Then evaluate
	   ;; expression value and store the result into the k bytes of
	   ;; memory starting at address a
	   (class move-mem::ir-stmt
	      addr::ir-expr
	      k::int
	      expr::ir-expr)

	   ;; Evaluate expr and discard the result
	   (class estmt::ir-stmt
	      >expr::ir-expr)

	   ;; Transfer control (jump) to address addr. The destination
	   ;; addr may be a literal label, as in name( lab ), or it may
	   ;; be an address calculated by any other kind of expression.
	   ;; The list of labels labs specifies all the possible locations
	   ;; that the expression addr can be evaluated to.
	   (class jump::ir-stmt
	      addr::label)

	   ;; Evaluate left and right in that order, yielding a and b.
	   ;; Then compare a and b with operator op (e.g. <, ==, ...).
	   ;; If the result is true jump to true; otherwise jump to false.
	   (class cjump::ir-stmt
	      op::symbol
	      left::ir-expr
	      right::ir-expr
	      true::label
	      false::label)

	   ;; A list of statement evaluated from left to right
	   (class seq::ir-stmt
	      stmts)

	   ;; Define the constant value of `name' to be the current
	   ;; machine code address. This is like a label definition in
	   ;; assembly language. The value of `name' may be the target of
	   ;; jumps, calls, etc.
	   (final-class label::ir-stmt
	      ;; the function declaration this label belongs to
	      (framedecl (default #f))
	      ;; the identifier of this label
	      ident::ident)

	   ;; This wide implements label/basic-block. The idea is to help
	   ;; the basic-block finding.
	   (wide-class label-bb::label
	      basic-block::basic-block)

	   ;; Pseudo instructions are used by host assembly to define
	   ;; the beginning and the end of function definitions. We use some
	   ;; too that will be expanded later on.
	   (class pseudo-fundef::ir-stmt
	      name::bstring
	      framedecl::framedecl)
	   (class pseudo-return::ir-stmt
	      framedecl::framedecl))

   (export (plus::opfx left::ir-expr right::ir-expr)
	   (no-op)
	   (make-new-temp)
	   (make-new-ireg ::bstring)
	   (get-all-temps::vector)))

;*---------------------------------------------------------------------*/
;*    plus ...                                                         */
;*    -------------------------------------------------------------    */
;*    This function is just a tool. It allocates a opfx for the        */
;*    operator +.                                                      */
;*---------------------------------------------------------------------*/
(define (plus::opfx left::ir-expr right::ir-expr)
   (instantiate::opfx
      (op '+)
      (left left)
      (right right)))

;*---------------------------------------------------------------------*/
;*    no-op ...                                                        */
;*    -------------------------------------------------------------    */
;*    A no-operation instruction.                                      */
;*---------------------------------------------------------------------*/
(define (no-op)
   (instantiate::ir-const
      (value 0)))

;*---------------------------------------------------------------------*/
;*    *temp-count* ...                                                 */
;*---------------------------------------------------------------------*/
(define *temp-count* 0)
(define *all-temps*  '())

;*---------------------------------------------------------------------*/
;*    make-new-ireg ...                                                */
;*---------------------------------------------------------------------*/
(define (make-new-ireg name::bstring)
   (set! *temp-count* (+fx *temp-count* 1))
   (let ((new (instantiate::ireg
		 (name name))))
      (set! *all-temps* (cons new *all-temps*))
      new))

;*---------------------------------------------------------------------*/
;*    make-new-temp ...                                                */
;*---------------------------------------------------------------------*/
(define (make-new-temp)
   (set! *temp-count* (+fx *temp-count* 1))
   (let ((new (instantiate::temp
		 (name (string-append "t" (integer->string *temp-count*))))))
      (set! *all-temps* (cons new *all-temps*))
      new))

;*---------------------------------------------------------------------*/
;*    get-all-temps ...                                                */
;*---------------------------------------------------------------------*/
(define (get-all-temps::vector)
   (list->vector *all-temps*))
