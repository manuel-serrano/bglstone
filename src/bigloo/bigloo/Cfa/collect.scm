;*=====================================================================*/
;*    .../prgm/project/bglstone/src/bigloo/bigloo/Cfa/collect.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Apr  5 09:06:26 1995                          */
;*    Last change :  Fri Mar  7 07:43:38 2025 (serrano)                */
;*    Copyright   :  1995-2025 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    We collect all type and alloc approximations                     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module cfa_collect
   (include "Ast/node.sch" "Type/type.sch" "Cfa/cinfo.sch" "Cfa/cinfo2.sch" "Cfa/cinfo3.sch")
   (include "Tools/trace.sch")
   (import  tools_shape
	    tools_error
	    type_type
	    type_cache
	    type_typeof
	    module_module
	    engine_param
	    ast_var
	    ast_node
	    ast_dump
	    ast_env
	    ast_sexp
	    cfa_info
	    cfa_info2
	    cfa_info3
	    cfa_arithmetic
	    cfa_specialize
	    cfa_procedure
	    cfa_vector
	    cfa_struct
	    cfa_box
	    cfa_closure)
   (export  (collect-all-approx! globals)
	    (get-allocs)))

;*---------------------------------------------------------------------*/
;*    collect-all-approx! ...                                          */
;*---------------------------------------------------------------------*/
(define (collect-all-approx! globals)
   (for-each (lambda (global) (collect-sfun! (global-value global) global))
	     globals))

;*---------------------------------------------------------------------*/
;*    collect-sfun! ::sfun ...                                         */
;*---------------------------------------------------------------------*/
(define (collect-sfun! value::sfun global::global)
   (node-collect! (sfun-body value) global))

;*---------------------------------------------------------------------*/
;*    node-collect! ...                                                */
;*---------------------------------------------------------------------*/
(define-generic (node-collect! node::node owner::variable))

;*---------------------------------------------------------------------*/
;*    node-collect! ::atom ...                                         */
;*---------------------------------------------------------------------*/
(define-method (node-collect! node::atom owner)
   #unspecified)

;*---------------------------------------------------------------------*/
;*    node-collect! ::kwote ...                                        */
;*---------------------------------------------------------------------*/
(define-method (node-collect! node::kwote owner)
   (let ((value (kwote-value node)))
      (if (and (vector-optim?) (vector? value))
	  (let* ((warning (let ((wan (bigloo-warning)))
			     (bigloo-warning-set! 0)
			     wan))
		 (pragma? (let ((tgt *pragma?*))
			     (set! *pragma?* #t)
			     tgt))
		 (dummy (top-level-sexp->node
			 `(c-make-vector ,(vector-length value)
					 ,(if (monomorphic-vector? value)
					      (vector-ref value 0)
					      '(pragma::obj "")))
			 #f)))
	     (bigloo-warning-set! warning)
	     (set! *pragma?* pragma?)
	     (widen!::kwote/node node (node dummy))
	     (node-collect! dummy owner)))))
		    
;*---------------------------------------------------------------------*/
;*    monomorphic-vector? ...                                          */
;*---------------------------------------------------------------------*/
(define (monomorphic-vector? vector)
   (define (get-atype value)
      (cond
	 ((fixnum? value)
	  'integer)
	 ((char? value)
	  'char)
	 ((boolean? value)
	  'boolean)
	 ((string? value)
	  'string)
	 ((real? value)
	  'real)
	 (else
	  #f)))
   (let ((len (vector-length vector)))
      (if (=fx len 0)
	  #f 
	  (let ((atype (get-atype (vector-ref vector 0))))
	     (let loop ((i 1))
		(cond
		   ((not atype)
		    #f)
		   ((=fx i len)
		    #t)
		   ((eq? (get-atype (vector-ref vector i)) atype)
		    (loop (+fx i 1)))
		   (else
		    #f)))))))
  
;*---------------------------------------------------------------------*/
;*    node-collect! ::var ...                                          */
;*---------------------------------------------------------------------*/
(define-method (node-collect! node::var owner)
   (let ((v (var-variable node)))
      (if (and (global? v)
	       (eq? (global-module v) *module*)
	       (scnst? (global-value v))
	       (not (used-alloc? (scnst-node (global-value v)))))
	  (begin
	     (trace (cfa 2) "Je collecte une scnt: "
		    (shape v) " " (shape (scnst-node (global-value v)))
		    #\Newline)
	     ;; this variable holds a constant
	     (node-collect! (scnst-node (global-value v)) owner))))
   #unspecified)

;*---------------------------------------------------------------------*/
;*    node-collect! ::sequence ...                                     */
;*---------------------------------------------------------------------*/
(define-method (node-collect! node::sequence owner)
   (node-collect*! (sequence-nodes node) owner))

;*---------------------------------------------------------------------*/
;*    node-collect! ::app ...                                          */
;*---------------------------------------------------------------------*/
(define-method (node-collect! node::app owner)
   (with-access::app node (fun args)
      (node-collect*! args owner)
      (node-collect! fun owner) 
      (let ((v (var-variable fun)))
	 (if (global? v)
	     (if (cfun? (variable-value v))
		 (begin
		    ;; closure tracing is mandatory otherwise the cfa
		    ;; approximation are incorrects (because if we don't
		    ;; trace closure we can possibly never enter
		    ;; some functions).
		    (case (global-id v)
		       ((c-eq?)
			(if *optim-cfa-arithmetic?*
			    (widen!::pre-arithmetic-app node
			       (spec-types (arithmetic-spec-types v)))))
		       ((make-fx-procedure)
			(use-alloc! node)
			(widen!::pre-make-procedure-app node (owner owner)))
		       ((make-va-procedure)
			(use-alloc! node)
			(widen!::pre-make-procedure-app node (owner owner)))
		       ((procedure-ref)
			(widen!::pre-procedure-ref-app node))
		       ((procedure-set!)
			(widen!::pre-procedure-set!-app node)))
		    (if (vector-optim?)
			(case (global-id v)
			   ((c-make-vector)
			    (use-alloc! node)
			    (widen!::pre-make-vector-app node (owner owner)))
			   ((c-vector-ref c-vector-set!
					  c-vector-length
					  c-create-vector)
			    (internal-error "node-collect!"
					    "Illegal foreign application"
					    (global-id v)))))
		    (if (>=fx *optim* 2)
			(case (global-id v)
			   ((c-make-struct)
			    (use-alloc! node)
			    (widen!::pre-make-struct-app node (owner owner)))
			   ((c-struct-ref)
			    (widen!::pre-struct-ref-app node))
			   ((c-struct-set!)
			    (widen!::pre-struct-set!-app node)))))
		 ;; non C function 
		 (if *optim-cfa-arithmetic?*
		     (if (arithmetic-operator? v)
			 (widen!::pre-arithmetic-app node
			    (spec-types (arithmetic-spec-types v))))))))))

;*---------------------------------------------------------------------*/
;*    node-collect! ::valloc ...                                       */
;*---------------------------------------------------------------------*/
(define-method (node-collect! node::valloc owner)
   (with-access::valloc node (expr*)
      (node-collect*! expr* owner)
      (if (vector-optim?)
	  (begin
	     (use-alloc! node)
	     (widen!::pre-valloc/Cinfo node (owner owner)))))) 
   
;*---------------------------------------------------------------------*/
;*    node-collect! ::app-ly ...                                       */
;*---------------------------------------------------------------------*/
(define-method (node-collect! node::app-ly owner)
   (with-access::app-ly node (fun arg)
      (node-collect! fun owner)
      (node-collect! arg owner)))

;*---------------------------------------------------------------------*/
;*    node-collect! ::funcall ...                                      */
;*---------------------------------------------------------------------*/
(define-method (node-collect! node::funcall owner)
   (add-funcall! node)
   (with-access::funcall node (fun args)
      (node-collect! fun owner)
      (node-collect*! args owner)))

;*---------------------------------------------------------------------*/
;*    node-collect! ::extern ...                                       */
;*---------------------------------------------------------------------*/
(define-method (node-collect! node::extern owner)
   (with-access::extern node (expr*)
      (node-collect*! expr* owner)))

;*---------------------------------------------------------------------*/
;*    node-collect! ::cast ...                                         */
;*---------------------------------------------------------------------*/
(define-method (node-collect! node::cast owner)
   (with-access::cast node (arg)
      (node-collect! arg owner)))

;*---------------------------------------------------------------------*/
;*    node-collect! ::setq ...                                         */
;*---------------------------------------------------------------------*/
(define-method (node-collect! node::setq owner)
   (with-access::setq node (type value)
      (node-collect! value owner)))

;*---------------------------------------------------------------------*/
;*    node-collect! ::conditional ...                                  */
;*---------------------------------------------------------------------*/
(define-method (node-collect! node::conditional owner)
   (with-access::conditional node (test true false)
       (node-collect! test owner)
       (node-collect! true owner)
       (node-collect! false owner)))

;*---------------------------------------------------------------------*/
;*    node-collect! ::fail ...                                         */
;*---------------------------------------------------------------------*/
(define-method (node-collect! node::fail owner)
   (with-access::fail node (type proc msg obj)
      (node-collect! proc owner)
      (node-collect! msg owner)
      (node-collect! obj owner)))

;*---------------------------------------------------------------------*/
;*    node-collect! ::select ...                                       */
;*---------------------------------------------------------------------*/
(define-method (node-collect! node::select owner)
   (with-access::select node (clauses test)
      (node-collect! test owner)
      (for-each (lambda (clause)
		   (node-collect! (cdr clause) owner))
		clauses)))

;*---------------------------------------------------------------------*/
;*    node-collect! ::let-fun ...                                      */
;*---------------------------------------------------------------------*/
(define-method (node-collect! node::let-fun owner)
   (with-access::let-fun node (body locals)
      (for-each (lambda (l)
		   (let ((f (local-value l)))
		      (node-collect! (sfun-body f) l)))
		locals)
      (node-collect! body owner)))

;*---------------------------------------------------------------------*/
;*    node-collect! ::let-var ...                                      */
;*---------------------------------------------------------------------*/
(define-method (node-collect! node::let-var owner)
   (with-access::let-var node (body bindings)
      (for-each (lambda (binding)
		   (node-collect! (cdr binding) owner))
		bindings)
      (node-collect! body owner)))

;*---------------------------------------------------------------------*/
;*    node-collect! ::set-ex-it ...                                    */
;*---------------------------------------------------------------------*/
(define-method (node-collect! node::set-ex-it owner)
   (with-access::set-ex-it node (var body)
      (node-collect! body owner)))

;*---------------------------------------------------------------------*/
;*    node-collect! ::jump-ex-it ...                                   */
;*---------------------------------------------------------------------*/
(define-method (node-collect! node::jump-ex-it owner)
   (with-access::jump-ex-it node (exit value)
      (node-collect! exit owner) 
      (node-collect! value owner)))

;*---------------------------------------------------------------------*/
;*    node-collect! ::make-box ...                                     */
;*---------------------------------------------------------------------*/
(define-method (node-collect! node::make-box owner)
   (node-collect! (make-box-value node) owner)
   (if (>=fx *optim* 1)
       (begin
	  (use-alloc! node)
	  (widen!::pre-make-box node))))

;*---------------------------------------------------------------------*/
;*    node-collect! ::box-set! ...                                     */
;*---------------------------------------------------------------------*/
(define-method (node-collect! node::box-set! owner)
   (with-access::box-set! node (var value)
      (node-collect! var owner)
      (node-collect! value owner)))

;*---------------------------------------------------------------------*/
;*    node-collect! ::box-ref ...                                      */
;*---------------------------------------------------------------------*/
(define-method (node-collect! node::box-ref owner)
   (with-access::box-ref node (var)
      (node-collect! var owner)))

;*---------------------------------------------------------------------*/
;*    node-collect*! ...                                               */
;*---------------------------------------------------------------------*/
(define (node-collect*! node* owner)
   (for-each (lambda (node) (node-collect! node owner)) node*))

;*---------------------------------------------------------------------*/
;*    *used-alloc* ...                                                 */
;*---------------------------------------------------------------------*/
(define *used-alloc* '())

;*---------------------------------------------------------------------*/
;*    use-alloc! ...                                                   */
;*---------------------------------------------------------------------*/
(define (use-alloc! alloc)
   (set! *used-alloc* (cons alloc *used-alloc*)))

;*---------------------------------------------------------------------*/
;*    used-alloc? ...                                                  */
;*---------------------------------------------------------------------*/
(define (used-alloc? alloc)
   (memq alloc *used-alloc*))

;*---------------------------------------------------------------------*/
;*    get-allocs ...                                                   */
;*---------------------------------------------------------------------*/
(define (get-allocs)
   *used-alloc*)
