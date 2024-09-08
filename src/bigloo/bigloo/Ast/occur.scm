;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Ast/occur.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Jan  6 11:09:14 1995                          */
;*    Last change :  Wed May 21 09:34:21 2003 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The substitution tools module                                    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module ast_occur
   (include "Ast/node.sch")
   (import  tools_error
	    tools_shape
	    ast_sexp
	    ast_env
	    ast_local)
   (export  (occur-var globals)
	    (occur-node-in! ::node ::global)))

;*---------------------------------------------------------------------*/
;*    occur-var ...                                                    */
;*---------------------------------------------------------------------*/
(define (occur-var globals)
   ;; first, we rester the counter.
   (for-each-global! (lambda (global) (global-occurrence-set! global 0)))
   ;; then we recompute the global occurrences.
   (for-each (lambda (global)
		(occur-node-in! (sfun-body (global-value global)) global))
	     globals))

;*---------------------------------------------------------------------*/
;*    occur-node-in! ...                                               */
;*---------------------------------------------------------------------*/
(define (occur-node-in! node global)
   (set! *global* global)
   (occur-node! node))
   
;*---------------------------------------------------------------------*/
;*    *global* ...                                                     */
;*---------------------------------------------------------------------*/
(define *global* #unspecified)

;*---------------------------------------------------------------------*/
;*    occur-node! ...                                                  */
;*---------------------------------------------------------------------*/
(define-generic (occur-node! node::node)
   'done)

;*---------------------------------------------------------------------*/
;*    occur-node! ::var ...                                            */
;*---------------------------------------------------------------------*/
(define-method (occur-node! node::var)
   (let ((v (var-variable node)))
      (let ((value (variable-value v)))
	 (if (and (scnst? value) (node? (scnst-node value)))
	     (occur-node! (scnst-node value))))
      (if (not (eq? v *global*))
	  (variable-occurrence-set! v (+fx (variable-occurrence v) 1)))))

;*---------------------------------------------------------------------*/
;*    occur-node! ::sequence ...                                       */
;*---------------------------------------------------------------------*/
(define-method (occur-node! node::sequence)
   (occur-node*! (sequence-nodes node)))

;*---------------------------------------------------------------------*/
;*    occur-node! ::app ...                                            */
;*---------------------------------------------------------------------*/
(define-method (occur-node! node::app)
   (with-access::app node (fun args)
      (occur-node! fun)
      (occur-node*! args)))

;*---------------------------------------------------------------------*/
;*    occur-node! ::app-ly ...                                         */
;*---------------------------------------------------------------------*/
(define-method (occur-node! node::app-ly)
   (with-access::app-ly node (fun arg)
      (occur-node! fun)
      (occur-node! arg)))

;*---------------------------------------------------------------------*/
;*    occur-node! ::funcall ...                                        */
;*---------------------------------------------------------------------*/
(define-method (occur-node! node::funcall)
   (with-access::funcall node (fun args)
      (occur-node! fun)
      (occur-node*! args)))

;*---------------------------------------------------------------------*/
;*    occur-node! ::extern ...                                         */
;*---------------------------------------------------------------------*/
(define-method (occur-node! node::extern)
   (occur-node*! (extern-expr* node)))

;*---------------------------------------------------------------------*/
;*    occur-node! ::cast ...                                           */
;*---------------------------------------------------------------------*/
(define-method (occur-node! node::cast)
   (occur-node! (cast-arg node)))

;*---------------------------------------------------------------------*/
;*    occur-node! ::setq ...                                           */
;*---------------------------------------------------------------------*/
(define-method (occur-node! node::setq)
   (with-access::setq node (var value)
      (occur-node! var)
      (occur-node! value)))

;*---------------------------------------------------------------------*/
;*    occur-node! ::conditional ...                                    */
;*---------------------------------------------------------------------*/
(define-method (occur-node! node::conditional)
   (with-access::conditional node (test true false)
      (occur-node! test)
      (occur-node! true)
      (occur-node! false)))

;*---------------------------------------------------------------------*/
;*    occur-node! ::fail ...                                           */
;*---------------------------------------------------------------------*/
(define-method (occur-node! node::fail)
   (with-access::fail node (proc msg obj)
      (occur-node! proc)
      (occur-node! msg)
      (occur-node! obj)))

;*---------------------------------------------------------------------*/
;*    occur-node! ::select ...                                         */
;*---------------------------------------------------------------------*/
(define-method (occur-node! node::select)
   (with-access::select node (test clauses)
      (occur-node! test)
      (for-each (lambda (clause)
		   (occur-node! (cdr clause)))
		clauses)))

;*---------------------------------------------------------------------*/
;*    occur-node! ::let-fun ...                                        */
;*---------------------------------------------------------------------*/
(define-method (occur-node! node::let-fun)
   (with-access::let-fun node (body locals)
      (for-each (lambda (local)
		   (local-occurrence-set! local 0)
		   (for-each (lambda (a) (local-occurrence-set! a 1))
			     (sfun-args (local-value local))))
		locals)
      (for-each (lambda (local)
		   (let ((old (local-occurrence local)))
		      (occur-node! (sfun-body (local-value local)))
		      (local-occurrence-set! local old)))
		locals)
      (occur-node! body)))

;*---------------------------------------------------------------------*/
;*    occur-node! ::let-var ...                                        */
;*---------------------------------------------------------------------*/
(define-method (occur-node! node::let-var)
   (with-access::let-var node (body bindings)
      (for-each (lambda (binding)
		   (local-occurrence-set! (car binding) 0)
		   (occur-node! (cdr binding)))
		bindings)
      (occur-node! body)))

;*---------------------------------------------------------------------*/
;*    occur-node! ::set-ex-it ...                                      */
;*---------------------------------------------------------------------*/
(define-method (occur-node! node::set-ex-it)
   (with-access::set-ex-it node (var)
      (local-occurrence-set! (var-variable var) 0)
      (occur-node! (set-ex-it-body node))))

;*---------------------------------------------------------------------*/
;*    occur-node! ::jump-ex-it ...                                     */
;*---------------------------------------------------------------------*/
(define-method (occur-node! node::jump-ex-it)
   (with-access::jump-ex-it node (exit value)
      (occur-node! exit)
      (occur-node! value)))

;*---------------------------------------------------------------------*/
;*    occur-node! ::make-box ...                                       */
;*---------------------------------------------------------------------*/
(define-method (occur-node! node::make-box)
   (occur-node! (make-box-value node)))

;*---------------------------------------------------------------------*/
;*    occur-node! ::box-ref ...                                        */
;*---------------------------------------------------------------------*/
(define-method (occur-node! node::box-ref)
   (occur-node! (box-ref-var node)))

;*---------------------------------------------------------------------*/
;*    occur-node! ::box-set! ...                                       */
;*---------------------------------------------------------------------*/
(define-method (occur-node! node::box-set!)
   (with-access::box-set! node (var value)
      (occur-node! var)
      (occur-node! value)))

;*---------------------------------------------------------------------*/
;*    occur-node*! ...                                                 */
;*---------------------------------------------------------------------*/
(define (occur-node*! node*)
   (for-each occur-node! node*))
