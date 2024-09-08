;*=====================================================================*/
;*    serrano/trashcan/cgc/Arch/mips-r3000.scm                         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Feb  5 10:58:58 1998                          */
;*    Last change :  Wed Dec 27 20:27:50 2000 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The mips-r3000 description                                       */
;*=====================================================================*/
 
;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module target_mips-r3000
   (import ast_node
	   ir_node
	   iselect_asm
	   engine_param)
   (export (mips-r3000-word-size::int)
	   (mips-r3000-sp-expr::ir-expr)
	   (mips-r3000-fp-expr::ir-expr)
	   (mips-r3000-old-fp::int)
	   (mips-r3000-zero-expr::ir-expr)
	   (mips-r3000-nop-expr)
	   (mips-r3000-return-value-expr::ir-expr)
	   (mips-r3000-return-address-expr::ir-expr)
	   (mips-r3000-arg-registers::pair)
	   (mips-r3000-caller-save-registers::pair)
	   (mips-r3000-callee-save-registers::pair)
	   (mips-r3000-calldef-registers::pair)
	   (mips-r3000-calluse-registers)
	   (mips-r3000-bss-expr::ir-expr)
	   (mips-r3000-save-register::asm-instr ::ir-expr ::int base)
	   (mips-r3000-restore-register::asm-instr ::ir-expr ::int ::ir-expr)
	   (mips-r3000-enlarge-stack::asm-instr ::int)
	   (mips-r3000-shrink-stack::asm-instr ::int)
	   (mips-r3000-move-registers::asm-instr ::ir-expr ::ir-expr)
	   (mips-r3000-add-registers::asm-instr ::ir-expr ::ir-expr ::int)
	   (mips-r3000-text-section::asm-instr)
	   (mips-r3000-runtime-code::bstring)
	   (mips-r3000-compile-data::asm-instr ::obj)
	   (mips-r3000-bss-segment::asm-instr ::int)
	   (mips-r3000-set-gp::asm-instr)))

;*---------------------------------------------------------------------*/
;*    mips-r3000-word-size ...                                         */
;*    -------------------------------------------------------------    */
;*    We size of a memory word on the mips                             */
;*---------------------------------------------------------------------*/
(define (mips-r3000-word-size)
   4)

;*---------------------------------------------------------------------*/
;*    mips-r3000-sp-expr ...                                           */
;*---------------------------------------------------------------------*/
(define (mips-r3000-sp-expr)
   (make-new-ireg "$sp"))

;*---------------------------------------------------------------------*/
;*    mips-r3000-fp-expr ...                                           */
;*---------------------------------------------------------------------*/
(define (mips-r3000-fp-expr)
   (make-new-ireg "$fp"))

;*---------------------------------------------------------------------*/
;*    mips-r3000-old-fp ...                                            */
;*---------------------------------------------------------------------*/
(define (mips-r3000-old-fp)
   0)

;*---------------------------------------------------------------------*/
;*    mips-r3000-return-value-expr ...                                 */
;*---------------------------------------------------------------------*/
(define (mips-r3000-return-value-expr)
   (make-new-ireg "$v0"))

;*---------------------------------------------------------------------*/
;*    mips-r3000-return-address-expr ...                               */
;*---------------------------------------------------------------------*/
(define (mips-r3000-return-address-expr)
   (make-new-ireg "$ra"))

;*---------------------------------------------------------------------*/
;*    mips-r3000-zero-expr ...                                         */
;*---------------------------------------------------------------------*/
(define (mips-r3000-zero-expr)
   (make-new-ireg "$zero"))

;*---------------------------------------------------------------------*/
;*    mips-r3000-nop-expr ...                                          */
;*---------------------------------------------------------------------*/
(define (mips-r3000-nop-expr)
   (instantiate::asm-oper
      (shape (string-append "addu %d0, %s0, %s1"))
      (use   (list (mips-r3000-zero-expr) (mips-r3000-zero-expr)))
      (def   (list (mips-r3000-zero-expr)))))

;*---------------------------------------------------------------------*/
;*    mips-r3000-arg-registers ...                                     */
;*---------------------------------------------------------------------*/
(define (mips-r3000-arg-registers)
   (list (make-new-ireg "$a0")
	 (make-new-ireg "$a1")
	 (make-new-ireg "$a2")
	 (make-new-ireg "$a3")
	 (make-new-ireg "$a4")))

;*---------------------------------------------------------------------*/
;*    mips-r3000-caller-save-registers ...                             */
;*---------------------------------------------------------------------*/
(define (mips-r3000-caller-save-registers)
   (list (make-new-ireg "$t0")
	 (make-new-ireg "$t1")
	 (make-new-ireg "$t2")
	 (make-new-ireg "$t3")
	 (make-new-ireg "$t4")
	 (make-new-ireg "$t5")
	 (make-new-ireg "$t6")
	 (make-new-ireg "$t7")))

;*---------------------------------------------------------------------*/
;*    mips-r3000-callee-save-registers ...                             */
;*---------------------------------------------------------------------*/
(define (mips-r3000-callee-save-registers)
   (list (make-new-ireg "$s0")
	 (make-new-ireg "$s1")
	 (make-new-ireg "$s2")
	 (make-new-ireg "$s3")
	 (make-new-ireg "$s4")
	 (make-new-ireg "$s5")
	 (make-new-ireg "$s6")
	 (make-new-ireg "$s7")))

;*---------------------------------------------------------------------*/
;*    mips-r3000-calldef-registers ...                                 */
;*---------------------------------------------------------------------*/
(define (mips-r3000-calldef-registers)
   (cons (mips-r3000-return-value-expr) '()))

;*---------------------------------------------------------------------*/
;*    mips-r3000-calluse-registers ...                                 */
;*---------------------------------------------------------------------*/
(define (mips-r3000-calluse-registers)
   (mips-r3000-arg-registers))

;*---------------------------------------------------------------------*/
;*    mips-r3000-bss-expr ...                                          */
;*---------------------------------------------------------------------*/
(define (mips-r3000-bss-expr)
   (make-new-ireg "$gp"))

;*---------------------------------------------------------------------*/
;*    mips-r3000-save-register ...                                     */
;*---------------------------------------------------------------------*/
(define (mips-r3000-save-register::asm-instr reg::ir-expr offset::int base)
   (instantiate::asm-oper
      (shape (string-append "sw %s0, " (integer->string offset) "(%s1)"))
      (use   (list reg base))))

;*---------------------------------------------------------------------*/
;*    mips-r3000-restore-register ...                                  */
;*---------------------------------------------------------------------*/
(define (mips-r3000-restore-register::asm-instr reg::ir-expr offset::int base)
   (instantiate::asm-oper
      (shape (string-append "lw %d0, " (integer->string offset) "(%s0)"))
      (use   (list base))
      (def   (list reg))))

;*---------------------------------------------------------------------*/
;*    mips-r3000-enlarge-stack ...                                     */
;*---------------------------------------------------------------------*/
(define (mips-r3000-enlarge-stack::asm-instr offset::int)
   (instantiate::asm-oper
      (shape (string-append "addiu %d0, %s0, -" (integer->string offset)))
      (use   (list (mips-r3000-sp-expr)))
      (def   (list (mips-r3000-sp-expr)))))

;*---------------------------------------------------------------------*/
;*    mips-r3000-shrink-stack ...                                      */
;*---------------------------------------------------------------------*/
(define (mips-r3000-shrink-stack::asm-instr offset::int)
   (instantiate::asm-oper
      (shape (string-append "addiu %d0, %s0, " (integer->string offset)))
      (use   (list (mips-r3000-sp-expr)))
      (def   (list (mips-r3000-sp-expr)))))

;*---------------------------------------------------------------------*/
;*    mips-r3000-move-registers ...                                    */
;*---------------------------------------------------------------------*/
(define (mips-r3000-move-registers::asm-instr dest::ir-expr src::ir-expr)
   (instantiate::asm-oper
      (shape "move %d0, %s0")
      (use   (list src))
      (def   (list dest))))

;*---------------------------------------------------------------------*/
;*    mips-r3000-add-registers ...                                     */
;*---------------------------------------------------------------------*/
(define (mips-r3000-add-registers::asm-instr dest::ir-expr src::ir-expr imm)
   (instantiate::asm-oper
      (shape (string-append "addu %d0, %s0, " (integer->string imm)))
      (use   (list src))
      (def   (list dest))))

;*---------------------------------------------------------------------*/
;*    mips-r3000-text-section ...                                      */
;*---------------------------------------------------------------------*/
(define (mips-r3000-text-section)
   (instantiate::asm-instr
      (shape #".text")))

;*---------------------------------------------------------------------*/
;*    mips-r3000-compile-data ...                                      */
;*---------------------------------------------------------------------*/
(define (mips-r3000-compile-data data)
   (let loop ((data data)
	      (res  ""))
      (if (null? data)
	  (instantiate::asm-instr
	     (shape res))
	  (let* ((first (car data))
		 (lbl   (car first))
		 (lbls  (ident-name (label-ident lbl)))
		 (str   (cdr first)))
	     (loop (cdr data)
		   (string-append lbls #":\t.asciiz " str #"\n" res))))))

;*---------------------------------------------------------------------*/
;*    mips-r3000-bss-segment ...                                       */
;*---------------------------------------------------------------------*/
(define (mips-r3000-bss-segment size)
   (instantiate::asm-instr
      (shape (string-append #".data\n_gbls:\t.space "
			    (integer->string (*fx size
						  (mips-r3000-word-size)))))))

;*---------------------------------------------------------------------*/
;*    mips-r3000-set-gp ...                                            */
;*    -------------------------------------------------------------    */
;*    Set the gp register for an execution. The label gp is set to     */
;*    is a global compiler constant.                                   */
;*---------------------------------------------------------------------*/
(define (mips-r3000-set-gp)
   (instantiate::asm-instr
      (shape "la $gp, _gbls")))

;*---------------------------------------------------------------------*/
;*    mips-r3000-runtime-code ...                                      */
;*---------------------------------------------------------------------*/
(define (mips-r3000-runtime-code)
   (if *inner-functions*
   #"
# ------------------------------------------
# The runtime system embedded implementation

print_int:       # print_int
\t.globl print_int
\taddiu $sp, $sp, -8
\tsw $fp, 8($sp)
\tsw $ra, 4($sp)
\taddu $fp, $sp, 8
\tli $v0, 1      # system call number 1
\tmove $a0, $a1
\tsyscall
\tlw $ra, 4($sp)
\tmove $sp, $fp
\tlw $fp, 0($fp)
\tj $ra
\taddu $zero, $zero, 0

print_string:    # print_string
\t.globl print_string
\taddiu $sp, $sp, -8
\tsw $fp, 8($sp)
\tsw $ra, 4($sp)
\taddu $fp, $sp, 8
\tli $v0, 4      # system call number 4
\tmove $a0, $a1
\tsyscall
\tlw $ra, 4($sp)
\tmove $sp, $fp
\tlw $fp, 0($fp)
\tj $ra
\taddu $zero, $zero, 0
"
   ;; the list for a runtime that does not support inner functions
   #"
# ------------------------------------------
# The runtime system embedded implementation

print_int:       # print_int
\t.globl print_int
\taddiu $sp, $sp, -8
\tsw $fp, 8($sp)
\tsw $ra, 4($sp)
\taddu $fp, $sp, 8
\tli $v0, 1      # system call number 1
\tsyscall
\tlw $ra, 4($sp)
\tmove $sp, $fp
\tlw $fp, 0($fp)
\tj $ra
\taddu $zero, $zero, 0

print_string:    # print_string
\t.globl print_string
\taddiu $sp, $sp, -8
\tsw $fp, 8($sp)
\tsw $ra, 4($sp)
\taddu $fp, $sp, 8
\tli $v0, 4      # system call number 4
\tsyscall
\tlw $ra, 4($sp)
\tmove $sp, $fp
\tlw $fp, 0($fp)
\tj $ra
\taddu $zero, $zero, 0
"))
   
