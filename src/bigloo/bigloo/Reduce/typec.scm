;*=====================================================================*/
;*    .../prgm/project/bglstone/src/bigloo/bigloo/Reduce/typec.scm     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jul 13 10:29:17 1995                          */
;*    Last change :  Fri Mar  7 08:25:11 2025 (serrano)                */
;*    Copyright   :  1995-2025 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The reduction of type checks.                                    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module reduce_typec
   (include "Tools/trace.sch")
   (import  tools_shape
	    tools_speek
	    tools_error
	    type_type
	    type_cache
	    tvector_tvector
	    object_class
	    type_typeof
	    type_misc
	    coerce_coerce
	    effect_effect
	    ast_var
	    ast_node
	    ast_lvtype
	    object_class)
   (export  (reduce-type-check! globals)))

;*---------------------------------------------------------------------*/
;*    reduce-type-check! ...                                           */
;*---------------------------------------------------------------------*/
(define (reduce-type-check! globals)
   (verbose 2 #"      type check             ")
   (for-each (lambda (global)
		(let* ((fun  (global-value global))
		       (node (sfun-body fun))) 
		   (sfun-body-set! fun (node-typec! node))
		   #unspecified))
	     globals)
   (verbose 2 "(removed : " *type-checks-removed*
	    ") (remaining : " *type-checks-remaining* #")\n")
   globals)


;*---------------------------------------------------------------------*/
;*    Statitics ...                                                    */
;*---------------------------------------------------------------------*/
(define *type-checks-remaining* 0)
(define *type-checks-removed*   0)

;*---------------------------------------------------------------------*/
;*    node-typec! ...                                                  */
;*---------------------------------------------------------------------*/
(define-generic (node-typec!::node node::node))

;*---------------------------------------------------------------------*/
;*    node-typec! ::atom ...                                           */
;*---------------------------------------------------------------------*/
(define-method (node-typec! node::atom)
   node)

;*---------------------------------------------------------------------*/
;*    node-typec! ::kwote ...                                          */
;*---------------------------------------------------------------------*/
(define-method (node-typec! node::kwote)
   node)

;*---------------------------------------------------------------------*/
;*    node-typec! ::var ...                                            */
;*---------------------------------------------------------------------*/
(define-method (node-typec! node::var)
   node)

;*---------------------------------------------------------------------*/
;*    node-typec! ::closure ...                                        */
;*---------------------------------------------------------------------*/
(define-method (node-typec! node::closure)
   node)

;*---------------------------------------------------------------------*/
;*    node-typec! ::sequence ...                                       */
;*---------------------------------------------------------------------*/
(define-method (node-typec! node::sequence)
   (with-access::sequence node (nodes)
      (node-typec*! nodes)
      node))

;*---------------------------------------------------------------------*/
;*    node-typec! ::app-ly ...                                         */
;*---------------------------------------------------------------------*/
(define-method (node-typec! node::app-ly)
   (with-access::app-ly node (fun arg)
      (set! fun (node-typec! fun))
      (set! arg (node-typec! arg))
      node))

;*---------------------------------------------------------------------*/
;*    node-typec! ::funcall ...                                        */
;*---------------------------------------------------------------------*/
(define-method (node-typec! node::funcall)
   (with-access::funcall node (fun args)
      (set! fun (node-typec! fun))
      (node-typec*! args)
      node))

;*---------------------------------------------------------------------*/
;*    node-typec! ::extern ...                                         */
;*---------------------------------------------------------------------*/
(define-method (node-typec! node::extern)
   (with-access::extern node (expr* type)
      (node-typec*! expr*)
      node))

;*---------------------------------------------------------------------*/
;*    node-typec! ::cast ...                                           */
;*---------------------------------------------------------------------*/
(define-method (node-typec! node::cast)
   (with-access::cast node (arg)
      (node-typec! arg)
      node))

;*---------------------------------------------------------------------*/
;*    node-typec! ::setq ...                                           */
;*---------------------------------------------------------------------*/
(define-method (node-typec! node::setq)
   (with-access::setq node (var value)
      (set! value (node-typec! value))
      (set! var (node-typec! var))
      node))

;*---------------------------------------------------------------------*/
;*    node-typec! ::conditional ...                                    */
;*---------------------------------------------------------------------*/
(define-method (node-typec! node::conditional)
   (with-access::conditional node (test true false)
       (set! test (node-typec! test))
       (set! true (node-typec! true))
       (set! false (node-typec! false))
       node))

;*---------------------------------------------------------------------*/
;*    node-typec! ::fail ...                                           */
;*---------------------------------------------------------------------*/
(define-method (node-typec! node::fail)
   (with-access::fail node (type proc msg obj)
      (set! proc (node-typec! proc))
      (set! msg (node-typec! msg))
      (set! obj (node-typec! obj))
      node))

;*---------------------------------------------------------------------*/
;*    node-typec! ::select ...                                         */
;*---------------------------------------------------------------------*/
(define-method (node-typec! node::select)
   (with-access::select node (clauses test)
      (set! test (node-typec! test))
      (for-each (lambda (clause)
		   (set-cdr! clause (node-typec! (cdr clause))))
		clauses)
      node))

;*---------------------------------------------------------------------*/
;*    node-typec! ::let-fun ...                                        */
;*---------------------------------------------------------------------*/
(define-method (node-typec! node::let-fun)
   (with-access::let-fun node (body locals)
      (for-each (lambda (local)
		   (let ((fun (local-value local)))
		      (sfun-body-set! fun (node-typec! (sfun-body fun)))))
		locals)
      (set! body (node-typec! body))
      node))

;*---------------------------------------------------------------------*/
;*    node-typec! ::let-var ...                                        */
;*---------------------------------------------------------------------*/
(define-method (node-typec! node::let-var)
   (with-access::let-var node (body bindings)
      (for-each (lambda (binding)
		   (set-cdr! binding (node-typec! (cdr binding))))
		bindings)
      (set! body (node-typec! body))
      node))

;*---------------------------------------------------------------------*/
;*    node-typec! ::set-ex-it ...                                      */
;*---------------------------------------------------------------------*/
(define-method (node-typec! node::set-ex-it)
   (with-access::set-ex-it node (var body)
      (set! body (node-typec! body))
      (set! var (node-typec! var))
      node))

;*---------------------------------------------------------------------*/
;*    node-typec! ::jump-ex-it ...                                     */
;*---------------------------------------------------------------------*/
(define-method (node-typec! node::jump-ex-it)
   (with-access::jump-ex-it node (exit value)
      (set! exit (node-typec! exit))
      (set! value (node-typec! value))
      node))

;*---------------------------------------------------------------------*/
;*    node-typec! ::make-box ...                                       */
;*---------------------------------------------------------------------*/
(define-method (node-typec! node::make-box)
   (with-access::make-box node (value)
      (set! value (node-typec! value))
      node))

;*---------------------------------------------------------------------*/
;*    node-typec! ::box-set! ...                                       */
;*---------------------------------------------------------------------*/
(define-method (node-typec! node::box-set!)
   (with-access::box-set! node (var value)
      (set! var (node-typec! var))
      (set! value (node-typec! value))
      node))

;*---------------------------------------------------------------------*/
;*    node-typec! ::box-ref ...                                        */
;*---------------------------------------------------------------------*/
(define-method (node-typec! node::box-ref)
   (with-access::box-ref node (var)
      (set! var (node-typec! var))
      node))

;*---------------------------------------------------------------------*/
;*    node-typec*! ...                                                 */
;*---------------------------------------------------------------------*/
(define (node-typec*! node*)
   (let loop ((node* node*))
      (if (null? node*)
	  'done
	  (begin
	     (set-car! node* (node-typec! (car node*)))
	     (loop (cdr node*))))))

;*---------------------------------------------------------------------*/
;*    node-typec! ::app ...                                            */
;*    -------------------------------------------------------------    */
;*    The subtype relationship between nil, pair, epair and pair-nil   */
;*    is hard coded into that function.                                */
;*    @label pair-nil subtyping@                                       */
;*    @ref ../../runtime/Llib/type.scm:pair-nil subtyping@             */
;*---------------------------------------------------------------------*/
(define-method (node-typec! node::app)
   (with-access::app node (fun args loc)
      (node-typec*! args)
      (let* ((var   (var-variable fun))
	     (typec (fun-predicate-of (variable-value var)))
	     (type  (typeof node)))
	 (if (and (pair? args)
		  (null? (cdr args))
		  (type? typec)
		  (not (side-effect? (car args))))
	     (let ((typea (typeof (car args))))
		(cond
		   ((type-less-specific? typec typea)
		    (set! *type-checks-removed*
			  (+fx 1 *type-checks-removed*))
		    (trace (reduce 3) "typec: reducing: "
			   (shape node) " => #t" #\Newline)
		    (let ((node (coerce! (instantiate::atom
					    (loc loc)
					    (type type)
					    (value #t))
					 #unspecified
					 type)))
		       (lvtype-node! node)
		       node))
		   ((type-disjoint? typec typea)
		    (set! *type-checks-removed*
			  (+fx 1 *type-checks-removed*))
		    (trace (reduce 3) "typec: reducing: "
			   (shape node) " => #f ("
			   (shape typec) " " (shape typea) ")"
			   #\Newline)
		    (let ((node (coerce! (instantiate::atom
					    (loc loc)
					    (type type)
					    (value #f))
					 #unspecified
					 type)))
		       (lvtype-node! node)
		       node))
		   (else
		    (set! *type-checks-remaining*
			  (+fx 1 *type-checks-remaining*))
		    node)))
	     node))))

