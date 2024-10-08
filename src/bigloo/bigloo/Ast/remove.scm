;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Ast/remove.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Jun  3 08:46:28 1996                          */
;*    Last change :  Fri Nov 12 14:38:01 2004 (serrano)                */
;*    -------------------------------------------------------------    */
;*    This module implements a function which takes a list of          */
;*    global variable and remove all globals which are not             */
;*    reachable.                                                       */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module ast_remove
   (include "Ast/node.sch")
   (import  tools_error
	    tools_speek
	    tools_shape
	    effect_effect
	    engine_param
	    ast_occur)
   (export  (remove-var-from! ::symbol ::variable)
	    (remove-var::obj ::obj ::obj)))

;*---------------------------------------------------------------------*/
;*    remove-var-from! ...                                             */
;*---------------------------------------------------------------------*/
(define (remove-var-from! pass var)
   (let ((old (variable-removable var)))
      (cond
	 ((eq? old 'now)
	  ;; we set the removable field
	  (global-removable-set! var pass))
	 ((eq? old pass)
	  'ok)
	 ((eq? pass 'now)
	  'ok) 
	 (else
	  (internal-error "remove-var-from!"
			  "already removable variable"
			  (string-append (symbol->string old)
					 "/"
					 (symbol->string pass)))))))
   
;*---------------------------------------------------------------------*/
;*    remove-var ...                                                   */
;*---------------------------------------------------------------------*/
(define (remove-var pass globals)
   ;; first we recompute the occurrences
   (occur-var globals)
   ;; then, we do the removal
   (let loop ((globals globals)
	      (res     '()))
      (if (null? globals)
	  ;; it is finished ...
	  (reverse! res)
	  (let ((global (car globals)))
	     (cond
		;; the function is exported, it hence can't be removed
		((eq? (global-import global) 'export)
		 (let ((sfun (global-value global)))
		    (sfun-body-set! sfun (node-remove! (sfun-body sfun))))
		 (loop (cdr globals) (cons global res)))
		;; we can remove this global from now, 
		;; we mark it or remove it.
		((or (eq? (global-removable global) pass)
		     (and (pair? pass)
			  (memq (global-removable global) pass)))
		 (if (<=fx (global-occurrence global) 0)
		     (begin
			;; we remove this function
			(verbose 3 "         removing "
				 (shape global) #\Newline)
			(loop (cdr globals) res))
		     (let ((sfun (global-value global)))
			(global-removable-set! global 'now)
			(sfun-body-set! sfun (node-remove! (sfun-body sfun)))
			(loop (cdr globals) (cons global res)))))
		((and (<=fx (global-occurrence global) 0)
		      (eq? (global-removable global) 'now)
		      (not (eq? pass 'no-remove)))
		 ;; this function is never used, we skip it
		 (verbose 3 "        " (shape global) " removed "
			  " (import: " (global-import global) ")"
			  #\Newline)
		 (loop (cdr globals) res))
		(else
		 (let ((sfun (global-value global)))
		    (sfun-body-set! sfun (node-remove! (sfun-body sfun)))
		    ;; we keep the function
		    (loop (cdr globals) (cons global res)))))))))
   
;*---------------------------------------------------------------------*/
;*    node-remove! ...                                                 */
;*---------------------------------------------------------------------*/
(define-generic (node-remove!::node node::node))

;*---------------------------------------------------------------------*/
;*    node-remove! ::atom ...                                          */
;*---------------------------------------------------------------------*/
(define-method (node-remove! node::atom)
   node)
 
;*---------------------------------------------------------------------*/
;*    node-remove! ::var ...                                           */
;*---------------------------------------------------------------------*/
(define-method (node-remove! node::var)
   node)

;*---------------------------------------------------------------------*/
;*    node-remove! ::kwote ...                                         */
;*---------------------------------------------------------------------*/
(define-method (node-remove! node::kwote)
   node)
       
;*---------------------------------------------------------------------*/
;*    node-remove*! ::sequence ...                                     */
;*---------------------------------------------------------------------*/
(define-method (node-remove! node::sequence)
   (node-remove*! (sequence-nodes node))
   node)

;*---------------------------------------------------------------------*/
;*    node-remove! ::app ...                                           */
;*---------------------------------------------------------------------*/
(define-method (node-remove! node::app)
   (app-fun-set! node (node-remove! (app-fun node)))
   (node-remove*! (app-args node))
   node)

;*---------------------------------------------------------------------*/
;*    node-remove! ::app-ly ...                                        */
;*---------------------------------------------------------------------*/
(define-method (node-remove! node::app-ly)
   (app-ly-fun-set! node (node-remove! (app-ly-fun node)))
   (app-ly-arg-set! node (node-remove! (app-ly-arg node)))
   node)

;*---------------------------------------------------------------------*/
;*    node-remove! ::funcall ...                                       */
;*---------------------------------------------------------------------*/
(define-method (node-remove! node::funcall)
   (funcall-fun-set! node (node-remove! (funcall-fun node)))
   (node-remove*! (funcall-args node))
   node)

;*---------------------------------------------------------------------*/
;*    node-remove! ::extern ...                                        */
;*---------------------------------------------------------------------*/
(define-method (node-remove! node::extern)
   (node-remove*! (extern-expr* node))
   node)

;*---------------------------------------------------------------------*/
;*    node-remove! ::cast ...                                          */
;*---------------------------------------------------------------------*/
(define-method (node-remove! node::cast)
   (node-remove! (cast-arg node))
   node)

;*---------------------------------------------------------------------*/
;*    node-remove! ::setq ...                                          */
;*---------------------------------------------------------------------*/
(define-method (node-remove! node::setq)
   (setq-value-set! node (node-remove! (setq-value node)))
   node)

;*---------------------------------------------------------------------*/
;*    node-remove! ::conditional ...                                   */
;*---------------------------------------------------------------------*/
(define-method (node-remove! node::conditional)
   (conditional-test-set! node (node-remove! (conditional-test node)))
   (conditional-true-set! node (node-remove! (conditional-true node)))
   (conditional-false-set! node (node-remove! (conditional-false node)))
   node)

;*---------------------------------------------------------------------*/
;*    node-remove! ::fail ...                                          */
;*---------------------------------------------------------------------*/
(define-method (node-remove! node::fail)
   (fail-proc-set! node (node-remove! (fail-proc node)))
   (fail-msg-set! node (node-remove! (fail-msg node)))
   (fail-obj-set! node (node-remove! (fail-obj node)))
   node)

;*---------------------------------------------------------------------*/
;*    node-remove! ::select ...                                        */
;*---------------------------------------------------------------------*/
(define-method (node-remove! node::select)
   (select-test-set! node (node-remove! (select-test node)))
   (for-each (lambda (clause)
		(set-cdr! clause (node-remove! (cdr clause))))
	     (select-clauses node))
   node)

;*---------------------------------------------------------------------*/
;*    node-remove! ::make-box ...                                      */
;*---------------------------------------------------------------------*/
(define-method (node-remove! node::make-box)
   (make-box-value-set! node (node-remove! (make-box-value node)))
   node)

;*---------------------------------------------------------------------*/
;*    node-remove! ::box-ref ...                                       */
;*---------------------------------------------------------------------*/
(define-method (node-remove! node::box-ref)
   (box-ref-var-set! node (node-remove! (box-ref-var node)))
   node)

;*---------------------------------------------------------------------*/
;*    node-remove! ::box-set! ...                                      */
;*---------------------------------------------------------------------*/
(define-method (node-remove! node::box-set!)
   (box-set!-var-set! node (node-remove! (box-set!-var node)))
   (box-set!-value-set! node (node-remove! (box-set!-value node)))
   node)

;*---------------------------------------------------------------------*/
;*    node-remove! ::let-fun ...                                       */
;*---------------------------------------------------------------------*/
(define-method (node-remove! node::let-fun)
   (with-access::let-fun node (locals body)
      (set! body (node-remove! body))
      (let loop ((old-locals locals)
		 (new-locals '()))
	 (if (null? old-locals)
	     (if (null? new-locals)
		 body
		 (begin
		    (set! locals (reverse! new-locals))
		    node))
	     (let ((local (car old-locals)))
		(if (<=fx (local-occurrence local) 0)
		    (loop (cdr old-locals) new-locals)
		    (let ((sfun (local-value local)))
		       (sfun-body-set! sfun (node-remove! (sfun-body sfun)))
		       (loop (cdr old-locals) (cons local new-locals)))))))))

;*---------------------------------------------------------------------*/
;*    node-remove! ::let-var ...                                       */
;*---------------------------------------------------------------------*/
(define-method (node-remove! node::let-var)
   (with-access::let-var node (bindings body removable?)
      (set! body (node-remove! body))
      (let loop ((old-bindings bindings)
		 (new-bindings '()))
	 (if (null? old-bindings)
	     (if (null? new-bindings)
		 (if removable? 
		     body
		     (begin
			(set! bindings '())
			node))
		 (begin
		    (set! bindings (reverse! new-bindings))
		    node))
	     (let* ((binding (car old-bindings))
		    (var     (car binding))
		    (val     (node-remove! (cdr binding))))
		(if (and (<=fx (local-occurrence var) 0)
			 (not (side-effect? val))
			 (or (not (local-user? var))
			     (=fx *bdb-debug* 0)))
		    (begin
		       (if (var? val)
			   (let ((v (var-variable val)))
			      (variable-occurrence-set!
			       v
			       (-fx (variable-occurrence v) 1))))
		       (loop (cdr old-bindings) new-bindings))
		    (begin
		       (set-cdr! binding val)
		       (loop (cdr old-bindings)
			     (cons binding new-bindings)))))))))

;*---------------------------------------------------------------------*/
;*    node-remove! ::set-ex-it ...                                     */
;*---------------------------------------------------------------------*/
(define-method (node-remove! node::set-ex-it)
   (set-ex-it-body-set! node (node-remove! (set-ex-it-body node)))
   (if (<=fx (local-occurrence (var-variable (set-ex-it-var node))) 0)
       (set-ex-it-body node)
       node))

;*---------------------------------------------------------------------*/
;*    node-remove! ::jump-ex-it ...                                    */
;*---------------------------------------------------------------------*/
(define-method (node-remove! node::jump-ex-it)
   (jump-ex-it-exit-set! node (node-remove! (jump-ex-it-exit node)))
   (jump-ex-it-value-set! node (node-remove! (jump-ex-it-value node)))
   node)

;*---------------------------------------------------------------------*/
;*    node-remove*! ...                                                */
;*---------------------------------------------------------------------*/
(define (node-remove*! node*)
   (if (null? node*)
       'done
       (begin
	  (set-car! node* (node-remove! (car node*)))
	  (node-remove*! (cdr node*)))))
