(module saw_defs
   (import engine_param
	   tools_shape
	   tools_trace
	   type_type
	   ast_var
	   ast_node
	   saw_lib)
   (export
    ;; Regs
    (final-class rtl_reg type::type var) ; var::(or local #f)
    ;; Functions
    (class rtl_fun (loc (default #f)))
     ; dest = #f and no continuation (last instruction of terminals blocks)
     (class rtl_last::rtl_fun)
      (class rtl_return::rtl_last type::type)
      (class rtl_jumpexit::rtl_last)
      (class rtl_fail::rtl_last)
     ; dest = #f and multiple continuation (last instruction of blocks)
     (class rtl_notseq::rtl_fun)
      (class rtl_if::rtl_notseq)
      (class rtl_select::rtl_notseq type::type patterns)
      (class rtl_switch::rtl_select labels)
      (class rtl_ifeq::rtl_notseq then::block)
      (class rtl_ifne::rtl_notseq then::block)
      (class rtl_go::rtl_notseq to::block)
     ; doesn't make side effects
     (class rtl_pure::rtl_fun)
      (class rtl_nop::rtl_pure)
      (class rtl_mov::rtl_pure)
      (class rtl_loadi::rtl_pure constant::atom)
      (class rtl_loadg::rtl_pure var::global)
      (class rtl_loadfun::rtl_pure var::global)
      (class rtl_globalref::rtl_pure var::global)
      (class rtl_getfield::rtl_pure name::bstring objtype::type type::type)
      (class rtl_valloc::rtl_pure type::type vtype::type)
      (class rtl_vref::rtl_pure type::type vtype::type)
      (class rtl_vlength::rtl_pure type::type)
      (class rtl_isa::rtl_pure type::type)
      (class rtl_makebox::rtl_pure)
      (class rtl_boxref::rtl_pure)
     ; dest = #f and make side-effect
     (class rtl_effect::rtl_fun)
      (class rtl_storeg::rtl_effect var::global)
      (class rtl_setfield::rtl_effect name::bstring objtype::type type::type)
      (class rtl_vset::rtl_effect type::type vtype::type)
      (class rtl_boxset::rtl_effect)
     ; others
     (class rtl_new::rtl_fun type::type constr::pair-nil)
     (class rtl_call::rtl_fun var::global)
     (class rtl_apply::rtl_fun)
     (class rtl_lightfuncall::rtl_fun)
     (class rtl_funcall::rtl_fun)
     (class rtl_pragma::rtl_fun format::bstring)
     (class rtl_cast::rtl_fun type::type)
     (class rtl_cast_null::rtl_fun type::type)
     (class rtl_protect::rtl_fun)
      (class rtl_protected::rtl_fun)

    ;; Instructions
    (final-class rtl_ins
       (loc (default #f))
       (dest (default #f))			; ::(or reg #f)
       (fun::rtl_fun)
       (args::pair-nil) )			; ::(list (or reg ins))

    ;; Block of instructions
    (final-class block
       (label::int (default 0))
       (preds::pair-nil (default '()))		; ::(list block)
       (succs::pair-nil (default '()))		; ::(list block)
       first::pair )				; :: (list ins)
    
    (ins-name ins::rtl_ins)
    (ins-args*::pair-nil ::rtl_ins)
    (fun-name fun::rtl_fun)

    (dump-basic-blocks id v params l)
    (rtl-dump ::obj ::output-port)
    (generic dump ::obj ::output-port ::int)
    ))

(define (ins-name ins::rtl_ins)
   (fun-name (rtl_ins-fun ins)) )

(define (fun-name fun::rtl_fun)
   (if (rtl_call? fun)
       (string-append "call "
		      (symbol->string (variable-id (rtl_call-var fun))) )
       (class-name (object-class fun)) ))

(define (ins-args* ins)
   (let loop ((args (rtl_ins-args ins))
	      (res '()))
      (cond
	 ((null? args)
	  res)
	 ((rtl_reg? (car args))
	  (loop (cdr args) (cons (car args) res)))
	 ((rtl_ins? (car args))
	  (loop (cdr args) (append (ins-args* (car args)) res)))
	 (else
	  (loop (cdr args) res)))))

;*---------------------------------------------------------------------*/
;*    dump-basic-blocks ...                                            */
;*---------------------------------------------------------------------*/
(define (dump-basic-blocks id v params l)
   (fprint *trace-port* "*** " id " " (shape v))
   (display " args:" *trace-port*)
   (map (lambda (a)
	   (display " " *trace-port*)
	   (dump a *trace-port* 0))
	params)
   (newline *trace-port*)
   (fprint *trace-port* " Basic blocks: " )
   (for-each (lambda (b)
		(rtl-dump b *trace-port*)
		(newline *trace-port*))
	     l))

;*---------------------------------------------------------------------*/
;*    rtl-dump ...                                                     */
;*---------------------------------------------------------------------*/
(define (rtl-dump obj port)
   (dump obj port 0)
   (newline port))

;*---------------------------------------------------------------------*/
;*    dump-margin ...                                                  */
;*---------------------------------------------------------------------*/
(define (dump-margin m p)
   (let ((mgs '#("" " " "  " "   " "    " "     " "      " "       ")))
      (if (<fx m (vector-length mgs))
	  (display (vector-ref mgs m) p)
	  (display (make-string m #\space) p))))

;*---------------------------------------------------------------------*/
;*    dump :: ...                                                      */
;*---------------------------------------------------------------------*/
(define-generic (dump o p m)
   (cond
      ((or (string? o) (number? o) (symbol? o))
       (display o p))
      ((pair? o)
       (for-each (lambda (o)
		    (dump o p m)
		    (newline p)
		    (if (>fx m 0)
			(dump-margin m p)
			(newline p)))
		 o))
      (else
       (write o p))))

;*---------------------------------------------------------------------*/
;*    dump* ...                                                        */
;*---------------------------------------------------------------------*/
(define (dump* o p m)
   (cond
      ((null? o)
       #unspecified)
      ((null? (cdr o))
       (dump (car o) p m))
      (else
       (let loop ((o o))
	  (dump (car o) p m)
	  (when (pair? (cdr o))
	     (newline p)
	     (dump-margin m p)
	     (loop (cdr o)))))))

;*---------------------------------------------------------------------*/
;*    dump-args ...                                                    */
;*---------------------------------------------------------------------*/
(define (dump-args args p)
   (let loop ((args args)
	      (sep " "))
      (when (pair? args)
	 (let ((a (car args)))
	    (cond
	       ((rtl_reg? a)
		(display sep p)
		(dump a p 0))
	       ((rtl_ins? a)
		(loop (rtl_ins-args a) " ."))
	       (else
		(display sep p)
		(display a p)))
	    (loop (cdr args) sep)))))

;*---------------------------------------------------------------------*/
;*    dump ::block ...                                                 */
;*---------------------------------------------------------------------*/
(define-method (dump o::block p m)
   (with-access::block o (label first)
      (fprint p "(block " label)
      (dump-margin (+fx m 1) p)
      (dump* first p (+fx m 1))
      (display ")" p)))

;*---------------------------------------------------------------------*/
;*    dump ::rtl_ins ...                                               */
;*---------------------------------------------------------------------*/
(define-method (dump o::rtl_ins p m)
   (with-access::rtl_ins o (fun dest args)
      (when dest
	 (dump dest p m)
	 (display " <- " p))
      (display "(" p)
      (dump-fun fun args p m)
      (display ")" p)))

;*---------------------------------------------------------------------*/
;*    *reg-table* ...                                                  */
;*---------------------------------------------------------------------*/
(define *reg-table* (make-hashtable))

;*---------------------------------------------------------------------*/
;*    dump ::rtl_reg ...                                               */
;*---------------------------------------------------------------------*/
(define-method (dump o::rtl_reg p m)
   (with-access::rtl_reg o (var type)
      (if var
	  (display (shape var) p)
	  (let ((old (hashtable-get *reg-table* o)))
	     (display "$" p)
	     (if old
		 (display old p)
		 (let ((new (gensym)))
		    (hashtable-put! *reg-table* o new)
		    (display new p)))))
      (when *type-shape?*
	 (display "::" p)
	 (display (shape type) p))))

;*---------------------------------------------------------------------*/
;*    show-fun ...                                                     */
;*---------------------------------------------------------------------*/
(define (show-fun o p)
   (let ((c (symbol->string (class-name (object-class o)))))
      (display (substring c 4 (string-length c)) p)))
   
;*---------------------------------------------------------------------*/
;*    dump-fun ...                                                     */
;*---------------------------------------------------------------------*/
(define-generic (dump-fun o::rtl_fun args p m)
   #unspecified)

;*---------------------------------------------------------------------*/
;*    dump-fun ::rtl_fun ...                                           */
;*---------------------------------------------------------------------*/
(define-method (dump-fun o::rtl_fun args p m)
   (show-fun o p)
   (dump-args args p))
   
;*---------------------------------------------------------------------*/
;*    dump-fun ::rtl_loadi ...                                         */
;*---------------------------------------------------------------------*/
(define-method (dump-fun o::rtl_loadi args p m)
   (show-fun o p)
   (display " " p)
   (display (atom-value (rtl_loadi-constant o)) p)
   (dump-args args p))

;*---------------------------------------------------------------------*/
;*    dump-fun ::rtl_loadg ...                                         */
;*---------------------------------------------------------------------*/
(define-method (dump-fun o::rtl_loadg args p m)
   (show-fun o p)
   (display " " p)
   (display (shape (rtl_loadg-var o)) p)
   (dump-args args p))

;*---------------------------------------------------------------------*/
;*    dump-fun ::rtl_loadfun ...                                       */
;*---------------------------------------------------------------------*/
(define-method (dump-fun o::rtl_loadfun args p m)
   (show-fun o p)
   (display " " p)
   (display (shape (rtl_loadfun-var o)) p)
   (dump-args args p))

;*---------------------------------------------------------------------*/
;*    dump-fun ::rtl_globalref ...                                     */
;*---------------------------------------------------------------------*/
(define-method (dump-fun o::rtl_globalref args p m)
   (show-fun o p)
   (display " " p)
   (display (shape (rtl_globalref-var o)) p)
   (dump-args args p))

;*---------------------------------------------------------------------*/
;*    dump-fun ::rtl_ifeq ...                                          */
;*---------------------------------------------------------------------*/
(define-method (dump-fun o::rtl_ifeq args p m)
   (with-access::rtl_ifeq o (then)
      (show-fun o p)
      (dump-args args p)
      (display " " p)
      (display (block-label then) p)))

;*---------------------------------------------------------------------*/
;*    dump-fun ::rtl_ifne ...                                          */
;*---------------------------------------------------------------------*/
(define-method (dump-fun o::rtl_ifne args p m)
   (with-access::rtl_ifne o (then)
      (show-fun o p)
      (dump-args args p)
      (display " " p)
      (display (block-label then) p)))

;*---------------------------------------------------------------------*/
;*    dump-fun ::rtl_go ...                                            */
;*---------------------------------------------------------------------*/
(define-method (dump-fun o::rtl_go args p m)
   (with-access::rtl_go o (to)
      (show-fun o p)
      (display " " p)
      (display (block-label to) p)
      (dump-args args p)))

;*---------------------------------------------------------------------*/
;*    dump-fun ::rtl_call ...                                          */
;*---------------------------------------------------------------------*/
(define-method (dump-fun o::rtl_call args p m)
   (with-access::rtl_call o (var)
      (show-fun o p)
      (display " " p)
      (display (shape var) p)
      (dump-args args p)))

