;*=====================================================================*/
;*    /tmp/bglstone/bglstone/src/cgc/bigloo/Ir/display.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Feb  5 13:59:14 1998                          */
;*    Last change :  Tue Mar  1 08:05:05 2005 (serrano)                */
;*    -------------------------------------------------------------    */
;*    How to display inode...                                          */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module ir_display
   (import engine_param
	   ast_node
	   ir_node)
   (export (set-ir-display-port::output-port ::output-port)))

;*---------------------------------------------------------------------*/
;*    set-ir-display-port ...                                          */
;*---------------------------------------------------------------------*/
(define (set-ir-display-port new-port)
   (let ((old-port *port*))
      (set! *port* new-port)
      old-port))

;*---------------------------------------------------------------------*/
;*    *margin* ...                                                     */
;*---------------------------------------------------------------------*/
(define *margin* 0)

;*---------------------------------------------------------------------*/
;*    *port*                                                           */
;*---------------------------------------------------------------------*/
(define *port* (current-output-port))

;*---------------------------------------------------------------------*/
;*    print/margin ...                                                 */
;*---------------------------------------------------------------------*/
(define (print/margin . args)
   (let ((margin (make-string *margin* #\space)))
      (let ((port *port*))
	 (display margin port)
	 (for-each (lambda (x)
		      (display x port))
		   args)
	 (newline port))))
 
;*---------------------------------------------------------------------*/
;*    object-display ::ir ...                                          */
;*---------------------------------------------------------------------*/
(define-method (object-display ir::ir . port)
   (let ((old-port (set-ir-display-port (if (pair? port) (car port) *port*))))
      (with-access::ir ir (text-segment bss-segment data-segment)
	 (if (not *text-only?*)
	     (fprint *port* ";; text"))
	 (for-each display text-segment)
	 (if (not *text-only?*)
	     (begin
		(fprint *port* ";; bss")
		(fprint *port* bss-segment #" memory words")
		(fprint *port* ";; data")
		(for-each print data-segment))))
      (set-ir-display-port old-port)))

;*---------------------------------------------------------------------*/
;*    object-display ::framedecl ...                                   */
;*---------------------------------------------------------------------*/
(define-method (object-display framedecl::framedecl . port)
   (with-access::framedecl framedecl (id prolog ir-stmt epilogue)
      (print/margin ";; " id)
      (if (not *text-only?*)
	  (print/margin ".prolog " id))
      (print/margin prolog)
      (if (not *text-only?*)
	  (print/margin ";; body"))
      (print/margin ir-stmt)
      (if (not *text-only?*)
	  (print/margin ";; epilogue"))
      (print/margin epilogue)))

;*---------------------------------------------------------------------*/
;*    object-display ::basic-block ...                                 */
;*---------------------------------------------------------------------*/
(define-method (object-display basic-block::basic-block . port)
   (with-access::basic-block basic-block (prolog body epilogue)
      (display "--------------------" *port*)
      (newline *port*)
      (print/margin prolog)
      (for-each display body)
      (print/margin epilogue)))

;*---------------------------------------------------------------------*/
;*    object-display ::ir-const ...                                    */
;*---------------------------------------------------------------------*/
(define-method (object-display const::ir-const . port)
   (with-access::ir-const const (value)
      (print/margin "(ir-const " value ")")))

;*---------------------------------------------------------------------*/
;*    object-display ::name ...                                        */
;*---------------------------------------------------------------------*/
(define-method (object-display name::name . port)
   (with-access::name name (label)
      (with-access::label label (ident)
	 (print/margin "(ir-name " ident ")"))))

;*---------------------------------------------------------------------*/
;*    object-display ::ireg ...                                        */
;*---------------------------------------------------------------------*/
(define-method (object-display ireg::ireg . port)
   (with-access::ireg ireg (name)
      (print/margin "(ir-ireg " name ")")))

;*---------------------------------------------------------------------*/
;*    object-display ::temp ...                                        */
;*---------------------------------------------------------------------*/
(define-method (object-display temp::temp . port)
   (with-access::temp temp (name key)
      (print/margin "(ir-temp " name ")")))

;*---------------------------------------------------------------------*/
;*    object-display ::opfx ...                                        */
;*---------------------------------------------------------------------*/
(define-method (object-display opfx::opfx . port)
   (with-access::opfx opfx (op left right)
      (print/margin "(ir-opfx " op)
      (set! *margin* (+fx *margin* 2))
      (object-display left)
      (object-display right)
      (display ")" *port*)
      (set! *margin* (-fx *margin* 2))))
      
;*---------------------------------------------------------------------*/
;*    object-display ::mem ...                                         */
;*---------------------------------------------------------------------*/
(define-method (object-display mem::mem . port)
   (with-access::mem mem (addr)
      (print/margin "(ir-mem")
      (set! *margin* (+fx *margin* 2))
      (object-display addr)
      (set! *margin* (-fx *margin* 2))
      (display ")" *port*)))

;*---------------------------------------------------------------------*/
;*    object-display ::call ...                                        */
;*---------------------------------------------------------------------*/
(define-method (object-display call::call . port)
   (with-access::call call (framedecl args)
      (print/margin "(ir-call " (ident-name (fundecl-id framedecl)))
      (set! *margin* (+fx *margin* 2))
     (for-each object-display args)
      (set! *margin* (-fx *margin* 2))
      (display ")" *port*)))

;*---------------------------------------------------------------------*/
;*    object-display ::move-temp ...                                   */
;*---------------------------------------------------------------------*/
(define-method (object-display move-temp::move-temp . port)
   (with-access::move-temp move-temp (temp expr)
      (print/margin "(ir-move-temp " (temp-name temp))
      (set! *margin* (+fx *margin* 2))
      (object-display expr)
      (display ")" *port*)
      (set! *margin* (-fx *margin* 2))))

;*---------------------------------------------------------------------*/
;*    object-display ::move-mem ...                                    */
;*---------------------------------------------------------------------*/
(define-method (object-display move-mem::move-mem . port)
   (with-access::move-mem move-mem (addr k expr)
      (print/margin "(ir-move-mem")
      (set! *margin* (+fx *margin* 2))
      (object-display addr)
      (if *display-move-mem-length?*
	  (print/margin k))
      (object-display expr)
      (set! *margin* (-fx *margin* 2))
      (display ")" *port*)))

;*---------------------------------------------------------------------*/
;*    object-display ::estmt ...                                       */
;*---------------------------------------------------------------------*/
(define-method (object-display estmt::estmt . port)
   (with-access::estmt estmt (>expr)
      (print/margin "(ir-estmt")
      (set! *margin* (+fx *margin* 2))
      (object-display >expr)
      (set! *margin* (-fx *margin* 2))
      (display ")" *port*)))

;*---------------------------------------------------------------------*/
;*    object-display ::jump ...                                        */
;*---------------------------------------------------------------------*/
(define-method (object-display jump::jump . port)
   (with-access::jump jump (addr)
      (print/margin "(ir-jump " (label-ident addr) ")")))

;*---------------------------------------------------------------------*/
;*    object-display ::cjump ...                                       */
;*---------------------------------------------------------------------*/
(define-method (object-display cjump::cjump . port)
   (with-access::cjump cjump (op left right true false)
      (print/margin "(ir-cjump " op)
      (set! *margin* (+fx *margin* 2))
      (object-display left)
      (object-display right)
      (print/margin (ident-name (label-ident true)))
      (print/margin (ident-name (label-ident false)))
      (set! *margin* (-fx *margin* 2))
      (display ")" *port*)))

;*---------------------------------------------------------------------*/
;*    object-display ::seq ...                                         */
;*---------------------------------------------------------------------*/
(define-method (object-display seq::seq . port)
   (with-access::seq seq (stmts)
      (print/margin "(ir-seq")
      (set! *margin* (+fx *margin* 2))
      (for-each (lambda (stmt)
		   (object-display stmt))
		stmts)
      (set! *margin* (-fx *margin* 2))
      (display ")" *port*)))

;*---------------------------------------------------------------------*/
;*    object-display ::label ...                                       */
;*---------------------------------------------------------------------*/
(define-method (object-display label::label . port)
   (with-access::label label (ident)
      (display (ident-name ident) *port*)
      (display #\: *port*)))

;*---------------------------------------------------------------------*/
;*    object-display ::pseudo-fundef ...                               */
;*---------------------------------------------------------------------*/
(define-method (object-display pseudo::pseudo-fundef . port)
   (with-access::pseudo-fundef pseudo (name)
      (fprint *port* ".prolog " name)))

;*---------------------------------------------------------------------*/
;*    object-display ::pseudo-return ...                               */
;*---------------------------------------------------------------------*/
(define-method (object-display pseudo::pseudo-return . port)
   (display ".return" *port*))
