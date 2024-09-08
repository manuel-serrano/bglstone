;*=====================================================================*/
;*    serrano/uni/97-98/maitrise/compil/cgc/Regalloc/regalloc.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Mar 22 11:18:27 1998                          */
;*    Last change :  Thu Apr  9 14:00:49 1998 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The register allocation entry point                              */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module regalloc_regalloc
   (import ast_node
	   ir_node
	   tools_speek
	   tools_set
	   engine_param
	   arch_arch
	   ast_node
	   iselect_asm
	   iselect_iselect
	   regalloc_agaig
	   regalloc_agaig/live
	   regalloc_push
	   regalloc_minimal-push
	   regalloc_callee-push)
   (export (regalloc! asms)))

;*---------------------------------------------------------------------*/
;*    regalloc! ...                                                    */
;*---------------------------------------------------------------------*/
(define (regalloc! asms)
   (verbose 1 #"  - Register allocation")
   (case *register-allocator*
      ((agaig)
       (agaig! asms))
      ((agaig/liveness)
       (agaig/liveness! asms))
      ((push)
       (push! asms))
      ((minimal-push)
       (minimal-push! asms))
      ((callee-push)
       (callee-push! asms))
      (else
       (error "cgc" "Unknown register allocator" *register-allocator*))))
