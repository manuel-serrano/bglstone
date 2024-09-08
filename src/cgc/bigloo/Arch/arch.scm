;*=====================================================================*/
;*    serrano/prgm/project/bglstone/src/cgc/bigloo/Arch/arch.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Feb  5 10:45:18 1998                          */
;*    Last change :  Tue Nov 24 16:04:39 2015 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The description of the target architecture                       */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module arch_arch
   
   (import engine_param
	   ast_node
	   ir_node
	   target_mips-r3000
	   iselect_mips-r3000-munch
	   iselect_asm)

   (include "Arch/arch.sch")
   
   (static ;; the description of the target machine
           (class target-info
	      word-size::int
	      sp-expr::ir-expr
	      fp-expr::ir-expr
	      (old-fp (default #f))
	      retv-expr::ir-expr
	      reta-expr::ir-expr
	      zero-expr::ir-expr
	      nop-expr
	      (arg-registers (default '()))
	      (caller-save-registers (default '()))
	      (callee-save-registers  (default '()))
	      (calldef-registers (default '()))
	      (calluse-registers (default '()))
	      bss-expr::ir-expr
	      iselect::procedure
	      save-register::procedure
	      restore-register::procedure
	      enlarge-stack::procedure
	      shrink-stack::procedure
	      (stack-grows-down?::bool (default #t))
	      move-registers::procedure
	      add-registers::procedure
	      text-section
	      runtime-code::bstring
	      compile-data::procedure
	      bss-segment::procedure
	      set-gp::procedure))
	   
   (export ;; we setup the target
	   (arch-setup! target)
	   ;; the architecture word size
	   (arch-word-size::int)
	   ;; the expression implementing the stack pointer
	   (arch-sp-expr::ir-expr)
	   ;; the expression implementing the frame pointer
	   (arch-fp-expr::ir-expr)
	   ;; the offset in the stack where is saved the old frame pointer
	   ;; (if any). Not all architecture use this save thus this
	   ;; expression may be just #f
	   (arch-old-fp)
	   ;; the return value expression
	   (arch-retv-expr::ir-expr)
	   ;; the return address expression
	   (arch-reta-expr::ir-expr)
	   ;; the zero value expression
	   (arch-zero-expr::ir-expr)
	   ;; the nop value expression
	   (arch-nop-expr)
	   ;; the register devoted to argument passing
	   (arch-arg-registers)
	   ;; the caller save temporary register
	   (arch-caller-save-registers)
	   ;; the callee save temporary register
	   (arch-callee-save-registers)
	   ;; the register that are returned on function calls
	   (arch-calldef-registers)
	   ;; the register that are trashed on function calls
	   (arch-calluse-registers)
	   ;; the bss segment
	   (arch-bss-expr)
	   ;; the instruction selection associated to the architectur
	   (arch-iselect ::ir)
	   ;; the smallest frame possible on the architecture
	   (arch-save-register ::ir-expr ::int . base)
	   ;; restoring a register from the stack
	   (arch-restore-register ::ir-expr ::int . base)
	   ;; enlarging the stack
	   (arch-enlarge-stack ::int)
	   ;; does the stack grow down?
	   (arch-stack-grows-down?::bool)
	   ;; shrinking the stack
	   (arch-shrink-stack ::int)
	   ;; moving registers
	   (arch-move-registers ::ir-expr ::ir-expr)
	   ;; adding to registers and a constant
	   (arch-add-registers ::ir-expr ::ir-expr ::int)
	   ;; how to start a text section
	   (arch-text-section)
	   ;; the runtime implementation
	   (arch-runtime-code::bstring)
	   ;; the literal compilation
	   (arch-compile-data ::obj)
	   ;; the bss segment allocation
	   (arch-bss-segment ::obj)
	   ;; set the global pointer register
	   (arch-set-gp)))

;*---------------------------------------------------------------------*/
;*    arch-setup! ...                                                  */
;*---------------------------------------------------------------------*/
(define (arch-setup! target)
   (case target
      ((mips-r3000)
       (set! *arch-info*
	     (instantiate::target-info
		(word-size (mips-r3000-word-size))
		(sp-expr (mips-r3000-sp-expr))
		(fp-expr (mips-r3000-fp-expr))
		(old-fp (mips-r3000-old-fp))
		(arg-registers (mips-r3000-arg-registers))
		(caller-save-registers (mips-r3000-caller-save-registers))
		(callee-save-registers (mips-r3000-callee-save-registers))
		(calldef-registers (mips-r3000-calldef-registers))
		(calluse-registers (mips-r3000-calluse-registers))
		(retv-expr (mips-r3000-return-value-expr))
		(reta-expr (mips-r3000-return-address-expr))
		(zero-expr (mips-r3000-zero-expr))
		(nop-expr (mips-r3000-nop-expr))
		(bss-expr (mips-r3000-bss-expr))
		(iselect mips-r3000-munch)
		(save-register mips-r3000-save-register)
		(restore-register mips-r3000-restore-register)
		(enlarge-stack mips-r3000-enlarge-stack)
		(shrink-stack mips-r3000-shrink-stack)
		(move-registers mips-r3000-move-registers)
		(add-registers mips-r3000-add-registers)
		(text-section (mips-r3000-text-section))
		(runtime-code (mips-r3000-runtime-code))
		(compile-data mips-r3000-compile-data)
		(bss-segment mips-r3000-bss-segment)
		(set-gp mips-r3000-set-gp))))
      (else
       (error *cgc-name* "Unimplemented architecture" target))))

;*---------------------------------------------------------------------*/
;*    *arch-info* ...                                                  */
;*    -------------------------------------------------------------    */
;*    The architecture are going to compile to.                        */
;*---------------------------------------------------------------------*/
(define *arch-info* #unspecified)

;*---------------------------------------------------------------------*/
;*    arch-word-size ...                                               */
;*---------------------------------------------------------------------*/
(define (arch-word-size)
   (target-info-word-size *arch-info*))

;*---------------------------------------------------------------------*/
;*    arch-sp-expr ...                                                 */
;*---------------------------------------------------------------------*/
(define (arch-sp-expr)
   (target-info-sp-expr *arch-info*))

;*---------------------------------------------------------------------*/
;*    arch-fp-expr ...                                                 */
;*---------------------------------------------------------------------*/
(define (arch-fp-expr)
   (target-info-fp-expr *arch-info*))

;*---------------------------------------------------------------------*/
;*    arch-old-fp ...                                                  */
;*---------------------------------------------------------------------*/
(define (arch-old-fp)
   (target-info-old-fp *arch-info*))

;*---------------------------------------------------------------------*/
;*    arch-retv-expr ...                                               */
;*---------------------------------------------------------------------*/
(define (arch-retv-expr)
   (target-info-retv-expr *arch-info*))

;*---------------------------------------------------------------------*/
;*    arch-reta-expr ...                                               */
;*---------------------------------------------------------------------*/
(define (arch-reta-expr)
   (target-info-reta-expr *arch-info*))

;*---------------------------------------------------------------------*/
;*    arch-zero-expr ...                                               */
;*---------------------------------------------------------------------*/
(define (arch-zero-expr)
   (target-info-zero-expr *arch-info*))

;*---------------------------------------------------------------------*/
;*    arch-nop-expr ...                                                */
;*---------------------------------------------------------------------*/
(define (arch-nop-expr)
   (target-info-nop-expr *arch-info*))

;*---------------------------------------------------------------------*/
;*    arch-arg-registers ...                                           */
;*---------------------------------------------------------------------*/
(define (arch-arg-registers)
   (target-info-arg-registers *arch-info*))

;*---------------------------------------------------------------------*/
;*    arch-caller-save-registers ...                                   */
;*---------------------------------------------------------------------*/
(define (arch-caller-save-registers)
   (target-info-caller-save-registers *arch-info*))

;*---------------------------------------------------------------------*/
;*    arch-callee-save-registers ...                                   */
;*---------------------------------------------------------------------*/
(define (arch-callee-save-registers)
   (target-info-callee-save-registers *arch-info*))

;*---------------------------------------------------------------------*/
;*    arch-calldef-registers ...                                       */
;*---------------------------------------------------------------------*/
(define (arch-calldef-registers)
   (target-info-calldef-registers *arch-info*))

;*---------------------------------------------------------------------*/
;*    arch-calluse-registers ...                                       */
;*---------------------------------------------------------------------*/
(define (arch-calluse-registers)
   (target-info-calluse-registers *arch-info*))

;*---------------------------------------------------------------------*/
;*    arch-bss-expr ...                                                */
;*---------------------------------------------------------------------*/
(define (arch-bss-expr)
   (target-info-bss-expr *arch-info*))

;*---------------------------------------------------------------------*/
;*    arch-iselect ...                                                 */
;*---------------------------------------------------------------------*/
(define (arch-iselect ir::ir)
   ((target-info-iselect *arch-info*) ir))

;*---------------------------------------------------------------------*/
;*    arch-save-register ...                                           */
;*---------------------------------------------------------------------*/
(define (arch-save-register reg offset . base)
   ((target-info-save-register *arch-info*) reg
					    offset
					    (if (pair? base)
						(car base)
						(arch-sp-expr))))

;*---------------------------------------------------------------------*/
;*    arch-restore-register ...                                        */
;*---------------------------------------------------------------------*/
(define (arch-restore-register reg offset . base)
   ((target-info-restore-register *arch-info*) reg
					       offset
					       (if (pair? base)
						   (car base)
						   (arch-sp-expr))))

;*---------------------------------------------------------------------*/
;*    arch-enlarge-stack ...                                           */
;*---------------------------------------------------------------------*/
(define (arch-enlarge-stack offset)
   ((target-info-enlarge-stack *arch-info*) offset))

;*---------------------------------------------------------------------*/
;*    arch-shrink-stack ...                                            */
;*---------------------------------------------------------------------*/
(define (arch-shrink-stack offset)
   ((target-info-shrink-stack *arch-info*) offset))

;*---------------------------------------------------------------------*/
;*    arch-stack-grows-down? ...                                       */
;*---------------------------------------------------------------------*/
(define (arch-stack-grows-down?)
   (target-info-stack-grows-down? *arch-info*))
   
;*---------------------------------------------------------------------*/
;*    arch-move-registers ...                                          */
;*---------------------------------------------------------------------*/
(define (arch-move-registers dest src)
   ((target-info-move-registers *arch-info*) dest src))

;*---------------------------------------------------------------------*/
;*    arch-add-registers ...                                           */
;*---------------------------------------------------------------------*/
(define (arch-add-registers dest src imm)
   ((target-info-add-registers *arch-info*) dest src imm))

;*---------------------------------------------------------------------*/
;*    arch-text-section ...                                            */
;*---------------------------------------------------------------------*/
(define (arch-text-section)
   (target-info-text-section *arch-info*))

;*---------------------------------------------------------------------*/
;*    arch-runtime-code ...                                            */
;*---------------------------------------------------------------------*/
(define (arch-runtime-code)
   (target-info-runtime-code *arch-info*))

;*---------------------------------------------------------------------*/
;*    arch-compile-data ...                                            */
;*---------------------------------------------------------------------*/
(define (arch-compile-data data)
   ((target-info-compile-data *arch-info*) data))

;*---------------------------------------------------------------------*/
;*    arch-bss-segment ...                                             */
;*---------------------------------------------------------------------*/
(define (arch-bss-segment size)
   ((target-info-bss-segment *arch-info*) size))

;*---------------------------------------------------------------------*/
;*    arch-set-gp ...                                                  */
;*---------------------------------------------------------------------*/
(define (arch-set-gp)
   ((target-info-set-gp *arch-info*)))
