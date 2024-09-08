;*=====================================================================*/
;*    serrano/prgm/project/bglstone/src/cgc/bigloo/Iselect/asm.scm     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Feb 13 15:50:00 1998                          */
;*    Last change :  Tue Nov 24 16:05:12 2015 (serrano)                */
;*    -------------------------------------------------------------    */
;*    This module implements classes that represent a virtual          */
;*    assembly language.                                               */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module iselect_asm
   
   (import ast_node
	   ir_node
	   tools_set
	   engine_param)

   (include "Iselect/asm.sch")
   
   (export ;; abstract class that represents a virtual assembly instruction
           (class asm-instr
	      ;; The shape of this instruction
	      shape::bstring)

	   ;; Arithmetic and logical instructions, load and store.
	   (class asm-oper::asm-instr
	      ;; the live in temporaries
	      (live-in (default #unspecified))
	      ;; the live out temporaries
	      (live-out (default #unspecified))
	      ;; trashed out temporaries
	      (trash (default '()))
	      ;; the successor list of this instruction
	      (succ (default '()))
	      ;; The list of registers that are used (sources) by this
	      ;; instruction
	      (use (default '()))
	      ;; The list of registers that are defined (destinations)
	      ;; by this instruction.
	      (def (default '())))

	   ;; A move instruction. We use a subclass in order to ease the
	   ;; recognition of move instruction during the register allocation
	   (class asm-move::asm-oper)

	   ;; Jumps and branches
	   (class asm-jump::asm-oper
	      ;; The label the instruction can go to.
	      asm-label::label)

	   ;; conditional branches
	   (class asm-cjump::asm-jump)
	   
	   ;; Labels. We use a wide class here just for being able
	   ;; to define a new printer for these wide labels.
	   (wide-class asm-label::label
	      ;; the live in temporaries
	      (live-in (default #unspecified))
	      ;; the live out temporaries
	      (live-out (default #unspecified))
	      ;; the next instruction
	      (succ (default '())))

	   ;; Pseudo instructions. A pseudo instruction is either a
	   ;; procedure entry or a procedure exit. It is used to manage
	   ;; stack pushes and stack pops
	   (class asm-pseudo::asm-oper
	      ;; The function declaration this instruction belongs to
	      (framedecl (default #unspecified)))

	   (class proc-entry::asm-pseudo)
	   (class proc-exit::asm-pseudo)))

;*---------------------------------------------------------------------*/
;*    object-display ::asm-instr ...                                   */
;*---------------------------------------------------------------------*/
(define-method (object-display asm::asm-instr . port)
   (with-access::asm-instr asm (shape)
      (let ((oport (if (pair? port) (car port) (current-output-port))))
	 (display #"\t" oport)
	 (display shape oport))))

;*---------------------------------------------------------------------*/
;*    temp-shape ...                                                   */
;*---------------------------------------------------------------------*/
(define (temp-shape temp)
   (cond
      ((ireg? temp)
       (temp-name temp))
      ((ireg? (temp-hardware temp))
       (cons (temp-name temp) (temp-name (temp-hardware temp))))
      ((number? (temp-hardware temp))
       (cons (temp-name temp) (temp-hardware temp)))
      (else
       (temp-name temp))))

;*---------------------------------------------------------------------*/
;*    object-display ::asm-oper ...                                    */
;*    -------------------------------------------------------------    */
;*    We suppose here that the shape is correctly constructed. Thus,   */
;*    when printing we do make any test.                               */
;*---------------------------------------------------------------------*/
(define-method (object-display asm::asm-oper . port)
   (with-access::asm-oper asm (shape use def live-in live-out trash succ)
      (let* ((oport   (if (pair? port) (car port) (current-output-port)))
	     (port    (open-input-string shape))
	     (grammar (regular-grammar ((letter (in (#\a #\z) (#\A #\Z) #\_))
					(digit (in (#\0 #\9) #\-))
					(blank (in #\space #\tab #\, #\( #\))))
			 ((+ (or blank digit letter))
			  (display (the-string))
			  (ignore))
			 ((: "%s" (+ digit))
			  (let* ((str (the-string))
				 (len (the-length))
				 (num (string->number (substring str 2 len)))
				 (dummy [assert (num) (> (length use) num)])
				 (temp (list-ref use num)))
			     (display-temp temp oport)
			     (ignore)))
			 ((: "%d" (+ digit))
			  (let* ((str (the-string))
				 (len (the-length))
				 (num (string->number (substring str 2 len)))
				 (dummy [assert (num) (> (length def) num)])
				 (temp (list-ref def num)))
			     (display-temp temp oport)
			     (ignore))))))
	 (display #"\t" oport)
	 (read/rp grammar port)
	 (close-input-port port)
	 (if (>=fx *verbose* 3)
	     (begin
		(newline)
		(if (>=fx *verbose* 4)
		    (let ((verb *verbose*))
		       (set! *verbose* 0)
		       (print #"\t\t ;; succ: " succ)
		       (set! *verbose* verb)))
		(print #"\t\t ;; use   : "
		       (map temp-shape use))
		(print #"\t\t ;; def   : "
		       (map temp-shape def))
		(if (and (set? live-out) (set? live-in))
		    (begin
		       (print #"\t\t ;; in    : "
			      (map temp-shape (set->list live-in)))
		       (print #"\t\t ;; out   : "
			      (map temp-shape (set->list live-out)))
		       (if (pair? trash)
			   (print #"\t\t ;; trash : "
				  (map temp-shape trash))))))))))

;*---------------------------------------------------------------------*/
;*    object-display ::asm-jump ...                                    */
;*    -------------------------------------------------------------    */
;*    We suppose here that the shape is correctly constructed. Thus,   */
;*    when printing we do make any test.                               */
;*---------------------------------------------------------------------*/
(define-method (object-display asm::asm-jump . port)
   (with-access::asm-jump asm (shape use def live-in live-out trash succ)
      (let* ((oport   (if (pair? port) (car port) (current-output-port)))
	     (port    (open-input-string shape))
	     (grammar (regular-grammar ((letter (in (#\a #\z) (#\A #\Z) #\_))
					(digit (in (#\0 #\9)))
					(blank (in #\space #\tab #\, #\( #\))))
			 ((+ (or blank digit letter))
			  (display (the-string))
			  (ignore))
			 ((: "%s" (+ digit))
			  (let* ((str (the-string))
				 (len (the-length))
				 (num (string->number (substring str 2 len)))
				 (dummy [assert (num) (> (length use) num)])
				 (temp (list-ref use num)))
			     (display-temp temp oport)
			     (ignore))))))
	 (display #"\t" oport)
	 (read/rp grammar port)
	 (close-input-port port)
	 (if (>=fx *verbose* 3)
	     (begin
		(newline)
		(if (>=fx *verbose* 4)
		    (let ((verb *verbose*))
		       (set! *verbose* 0)
		       (print #"\t\t ;; succ: " succ)
		       (set! *verbose* verb)))
		(print #"\t\t ;; use   : "
		       (map temp-shape use))
		(print #"\t\t ;; def   : "
		       (map temp-shape def))
		(if (and (set? live-out) (set? live-in))
		    (begin
		       (print #"\t\t ;; in    : "
			      (map temp-shape
				   (set->list live-in)))
		       (print #"\t\t ;; out   : "
			      (map temp-shape (set->list live-out)))
		       (if (pair? trash)
			   (print #"\t\t ;; trash : "
				  (map temp-shape trash))))))))))

;*---------------------------------------------------------------------*/
;*    object-display ::asm-label ...                                   */
;*---------------------------------------------------------------------*/
(define-method (object-display asm::asm-label . port)
   (with-access::asm-label asm (ident succ live-in live-out)
      (let ((oport (if (pair? port) (car port) (current-output-port))))
	 (display (ident-name ident) oport)
	 (write-char #\: oport)
	 (if (>=fx *verbose* 3)
	     (begin
		(newline)
		(if (>=fx *verbose* 4)
		    (let ((verb *verbose*))
		       (set! *verbose* 0)
		       (print #"\t\t ;; succ: " succ)
		       (set! *verbose* verb)))
		(if (and (set? live-out) (set? live-in))
		    (begin
		       (print #"\t\t ;; in    : "
			      (map temp-shape (set->list live-in)))
		       (print #"\t\t ;; out   : "
			      (map temp-shape (set->list live-out))))))))))

;*---------------------------------------------------------------------*/
;*    display-temp ::temp ...                                          */
;*---------------------------------------------------------------------*/
(define-generic (display-temp temp::temp . port)
   (with-access::temp temp (name hardware)
      (let ((oport (if (pair? port) (car port) (current-output-port))))
	 (if (temp? hardware)
	     (display-temp hardware oport)
	     (display name oport)))))

;*---------------------------------------------------------------------*/
;*    display-temp ::ireg ...                                          */
;*---------------------------------------------------------------------*/
(define-method (display-temp temp::ireg . port)
   (with-access::ireg temp (name)
      (let ((oport (if (pair? port) (car port) (current-output-port))))
	 (display name oport))))
			 
   
		      
		
