;*=====================================================================*/
;*    serrano/trashcan/cgc/Liveness/liveness.scm                       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Mar  2 14:40:47 1998                          */
;*    Last change :  Wed Dec 27 18:58:25 2000 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The liveness analaysis (prior to register allocation).           */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module liveness_liveness
   
   (import ast_node
	   ir_node)
   
   (import tools_speek
	   tools_set
	   engine_param
	   arch_arch
	   ast_node
	   iselect_asm
	   iselect_iselect)
   
   (export (liveness! il)))

;*---------------------------------------------------------------------*/
;*    liveness! ...                                                    */
;*    -------------------------------------------------------------    */
;*    This stage proceed to a side effect. It write live property      */
;*    inside the iselect_asm instructions.                             */
;*    -------------------------------------------------------------    */
;*    This function implements a fix point interation to find the      */
;*    maximal solution of the equations:                               */
;*       in[ n ] = use[ n ] U (out[ n ] - def[ n ])                    */
;*      out[ n ] = Union(succ[ n ]) in[ s ]                            */
;*---------------------------------------------------------------------*/
(define (liveness! il)
   (verbose 1 #"  - Liveness analysis\n")
   ;; First, we walk thru all expression and we prepare the set.
   ;; That is, for each instruction, we allocate a set that can
   ;; hold as many temporary as the one that are used in the
   ;; entire program.
   (let ((meta-set (declare-set! (get-all-temps) temp-key temp-key-set!)))
      (for-each
       (lambda (asm)
	  (cond
	     ((asm-oper? asm)
	      (with-access::asm-oper asm (live-in live-out use def trash)
		 (set! live-out (make-set! meta-set))
		 (set! live-in (make-set! meta-set))
		 ;; we start in with the set use
		 (for-each (lambda (temp)
			      (set-extend! live-in temp))
			   use)))
	     ((asm-label? asm)
	      (with-access::asm-label asm (live-in live-out)
		 (set! live-out (make-set! meta-set))
		 (set! live-in (make-set! meta-set))))))
       il)
      ;; After the initialization, we make the fix point iteration. In
      ;; order to improve the chances not to be sub-optimal, we first
      ;; reverse the list.
      (let ((il (reverse il)))
	 (let loop ((y #t))
	    (if y
		(begin
		   (set! y #f)
		   (for-each (lambda (asm) (if (live-at asm) (set! y #t))) il)
		   (loop y))))))
   ;; Once the liveness is computed we still have to compute the
   ;; interference graph. That is, for each temp we need to know
   ;; the set of temporary that are live at the same time.
   il)

;*---------------------------------------------------------------------*/
;*    live-at :: ...                                                   */
;*---------------------------------------------------------------------*/
(define-generic (live-at asm)
   (error "live-at" (find-runtime-type asm) asm))

;*---------------------------------------------------------------------*/
;*    live-at ...                                                      */
;*---------------------------------------------------------------------*/
(define-method (live-at asm::asm-oper)
   (with-access::asm-oper asm (live-in live-out use def succ)
      (let ((live-in-len  (set-length live-in))
	    (live-out-len (set-length live-out)))
	 ;; first, we compute the new live-in set
	 [assert (asm) (and (set? live-in) (set? live-out))]
	 (set-union! live-in live-out)
	 ;; now we compute live-out
	 (for-each (lambda (succ)
		      (cond
			 ((asm-oper? succ)
			  (set-union! live-out (asm-oper-live-in succ)))
			 ((asm-label? succ)
			  (set-union! live-out (asm-label-live-in succ)))
			 ((or (null? succ) (asm-instr? succ))
			  #unspecified)
			 (else
			  (error "live-at ::asm-oper"
				 asm
				 (cons (find-runtime-type succ) succ)))))
		   succ)
	 (or (>fx (set-length live-in) live-in-len)
	     (>fx (set-length live-out) live-out-len)))))

;*---------------------------------------------------------------------*/
;*    live-at ...                                                      */
;*    -------------------------------------------------------------    */
;*    Some asm-label instruction don't have live-in and live-out       */
;*    sets. These labels are dummy return labels. Instead of           */
;*    processing these special operations in the initialization stage  */
;*    we check here that live-in and live-out are sets.                */
;*---------------------------------------------------------------------*/
(define-method (live-at asm::asm-label)
   (with-access::asm-label asm (live-in live-out succ)
      (if (and (set? live-in) (set? live-out))
	  (let ((live-in-len  (set-length live-in))
		(live-out-len (set-length live-out)))
	     ;; first, we compute the new live-in set
	     (set-union! live-in live-out)
	     ;; now we compute live-out
	     (for-each (lambda (succ)
			  (cond
			     ((asm-oper? succ)
			      (set-union! live-out (asm-oper-live-in succ)))
			     ((asm-label? succ)
			      (set-union! live-out (asm-label-live-in succ)))
			     ((or (null? succ) (asm-instr? succ))
			      #unspecified)
			     (else
			      (error "live-at ::asm-label"
				     asm
				     (cons (find-runtime-type succ) succ)))))
		       succ)
	     (or (>fx (set-length live-in) live-in-len)
		 (>fx (set-length live-out) live-out-len)))
	  #f)))

;*---------------------------------------------------------------------*/
;*    live-at ::asm-instr ...                                          */
;*---------------------------------------------------------------------*/
(define-method (live-at asm::asm-instr)
   #f)

