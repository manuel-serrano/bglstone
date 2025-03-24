;*=====================================================================*/
;*    .../prgm/project/bglstone/src/bigloo/bigloo/Cfa/tvector.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Apr  5 18:47:23 1995                          */
;*    Last change :  Fri Mar  7 07:49:29 2025 (serrano)                */
;*    Copyright   :  1995-2025 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The `vector->tvector' optimization.                              */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module cfa_tvector
   (include "Ast/node.sch" "Type/type.sch" "Cfa/cinfo.sch" "Cfa/cinfo2.sch" "Cfa/cinfo3.sch" "Object/slots.sch")
   (include "Tools/trace.sch"
	    "Ast/unit.sch"
	    "Tvector/tvector.sch")
   (import  engine_param
	    module_type
	    module_pragma
	    type_type
	    type_cache
	    type_env
	    tvector_tvector
	    tools_shape
	    tools_speek
	    tools_error
	    ast_var
	    ast_node
	    ast_build
	    ast_sexp
	    ast_env
	    cfa_info
	    cfa_info2
	    cfa_info3
	    cfa_cfa
	    cfa_approx
	    cfa_set
	    cfa_type
	    object_slots
	    globalize_walk
	    inline_inline
	    inline_walk)
   (export  (patch-vector-set!)
	    (unpatch-vector-set!)
	    (vector->tvector! globals)
	    (add-make-vector! ::node)
	    (tvector-optimization?)
	    (generic get-vector-item-type::type ::node)))

;*---------------------------------------------------------------------*/
;*    tvector-optimization? ...                                        */
;*    -------------------------------------------------------------    */
;*    Tvectors cannot be used for the library module due to pbm        */
;*    of initialization order (tvector module must have been           */
;*    initialized before any tvector is declared). Hence, we disable   */
;*    the optimization when compiling the library.                     */
;*---------------------------------------------------------------------*/
(define (tvector-optimization?)
   (and (>=fx *optim* 3) (not *lib-mode*)))

;*---------------------------------------------------------------------*/
;*    patch-vector-set! ...                                            */
;*    -------------------------------------------------------------    */
;*    This function is called by (@ compiler engine) at the very       */
;*    beginning of the compilation (just after the heap restoration).  */
;*---------------------------------------------------------------------*/
(define (patch-vector-set!)
   (if (tvector-optimization?)
       (begin
	  (for-each (lambda (set)
		       (let ((g (find-global set)))
			  (if (global? g)
			      (let ((fun (global-value g)))
				 (cond
				    ((cfun? fun)
				     (set-car! (cddr (cfun-args-type fun))
					       (get-default-type)))
				    ((sfun? fun)
				     (local-type-set! (caddr (sfun-args fun))
						      (get-default-type))))))))
		    '(vector-set! c-vector-set! vector-set-ur!))
	  (let ((g (find-global 'c-vector?)))
	     (if (global? g)
		 (let ((f (global-value g)))
		    (set-car! (cfun-args-type f) (get-default-type)))))
	  (let ((g (find-global 'vector?)))
	     (if (global? g)
		 (let ((f (global-value g)))
		    (local-type-set! (car (sfun-args f))
				     (get-default-type))))))))
	
;*---------------------------------------------------------------------*/
;*    unpatch-vector-set! ...                                          */
;*    -------------------------------------------------------------    */
;*    This function is called by (@ compiler engine) at the very       */
;*    beginning of the compilation (just after the heap restoration).  */
;*---------------------------------------------------------------------*/
(define (unpatch-vector-set!)
   (if (tvector-optimization?)
       (begin
	  (for-each (lambda (set)
		       (let ((g (find-global set)))
			  (if (global? g)
			      (let ((fun (global-value g)))
				 (cond
				    ((cfun? fun)
				     (set-car! (cddr (cfun-args-type fun))
					       *obj*))
				    ((sfun? fun)
				     (local-type-set! (caddr (sfun-args fun))
						      *obj*)))))))
		    '(vector-set! c-vector-set! vector-set-ur!))
	  (let ((g (find-global 'c-vector?)))
	     (if (global? g)
		 (let ((f (global-value g)))
		    (set-car! (cfun-args-type f) *obj*))))
	  (let ((g (find-global 'vector?)))
	     (if (global? g)
		 (let ((f (global-value g)))
		    (local-type-set! (car (sfun-args f)) *obj*)))))
       #unspecified))
    
;*---------------------------------------------------------------------*/
;*    vector->tvector! ...                                             */
;*---------------------------------------------------------------------*/
(define (vector->tvector! globals)
   (if (tvector-optimization?)
       (begin
	  (trace cfa
		 "--------------------------------------"
		 #\Newline "tvector-optimization! :" #\Newline
		 (shape *make-vector-list*)
		 #\Newline)
	  ;; we setup the inlining 
	  (inline-setup! 'all)
	  (let ((tvectors (get-tvectors)))
	     (show-tvector tvectors)
	     (trace (cfa 2) "tvectors: " (shape tvectors) #\Newline)
	     (if (pair? tvectors)
		 (let ((add-tree (declare-tvectors tvectors)))
		    (trace (cfa 2)
			   "additional-body: " (shape add-tree) #\Newline)
		    (patch-tree! globals)
		    add-tree)
		 (begin
		    (patch-tree! globals)
		    '()))))
       '()))

;*---------------------------------------------------------------------*/
;*    lists for quick access to vectors                                */
;*---------------------------------------------------------------------*/
(define *make-vector-list* '())

;*---------------------------------------------------------------------*/
;*    add-make-vector! ...                                             */
;*---------------------------------------------------------------------*/
(define (add-make-vector! node)
   (if (tvector-optimization?)
       (set! *make-vector-list* (cons node *make-vector-list*))))

;*---------------------------------------------------------------------*/
;*    get-tvectors ...                                                 */
;*    -------------------------------------------------------------    */
;*    We scan all declared vector to find which of them can be         */
;*    optimized.                                                       */
;*---------------------------------------------------------------------*/
(define (get-tvectors)
   (let loop ((apps *make-vector-list*)
	      (tvectors '()))
      (if (null? apps)
	  tvectors
	  (let* ((app  (car apps))
		 (type (get-vector-item-type app)))
	     (trace (cfa 1)
		    "vector: " (shape app) " type: " (shape type) " "
		    (type-class type) #\Newline)
	     (if (and (not (eq? type *_*))
		      (not (sub-type? type *obj*)))
		 (loop (cdr apps) (cons app tvectors))
		 (loop (cdr apps) tvectors))))))
			   
;*---------------------------------------------------------------------*/
;*    get-vector-item-type ...                                         */
;*---------------------------------------------------------------------*/
(define-generic (get-vector-item-type node::node))
 
;*---------------------------------------------------------------------*/
;*    get-vector-item-type ::make-vector-app ...                       */
;*---------------------------------------------------------------------*/
(define-method (get-vector-item-type app::make-vector-app)
   (with-access::make-vector-app app (value-approx seen?)
      (if (not seen?)
	  *vector*
	  (approx-type value-approx))))

;*---------------------------------------------------------------------*/
;*    get-vector-item-type ::valloc/Cinfo+optim ...                    */
;*---------------------------------------------------------------------*/
(define-method (get-vector-item-type node::valloc/Cinfo+optim)
   (with-access::valloc/Cinfo+optim node (value-approx seen?)
      (if (not seen?)
	  *vector*
	  (approx-type value-approx))))
   
;*---------------------------------------------------------------------*/
;*    show-tvector ...                                                 */
;*---------------------------------------------------------------------*/
(define (show-tvector tvector)
   (verbose 1 "   . Vector -> Tvector" #\newline)
   (for-each (lambda (app)
		(verbose 2
			 "        vector of " (shape *obj*)
			 " -> vector of "
			 (shape (get-vector-item-type app))
			 #\Newline))
	     tvector))

;*---------------------------------------------------------------------*/
;*    declare-tvectors ...                                             */
;*---------------------------------------------------------------------*/
(define (declare-tvectors tvector)
   ;; first we declare the tvectors
   (for-each (lambda (app)
		(let ((type (get-vector-item-type app)))
		   (with-access::type type (tvector id)
		      (if (not (type? tvector))
			  (let ((tv-id (symbol-append 'tv-of- id)))
			     (set! tvector
				   (type-parser `(tvector ,tv-id (,id))
						'())))))))
	     tvector)
   ;; now we just make the new ast for the typed vectors.
   (let ((old-default-type (get-default-type)))
      (set-default-type! *obj*)
      (let ((tvector-unit (tvector-finalizer)))
	 (pragma-finalizer)
	 (let ((res (if (unit? tvector-unit)
			(globalize-walk! (build-ast-sans-remove
					  (list tvector-unit))
					 'no-remove)
			'())))
	    (set-default-type! old-default-type)
	    res))))
 
;*---------------------------------------------------------------------*/
;*    patch-tree! ...                                                  */
;*    -------------------------------------------------------------    */
;*    With replace the vector accessors and creators _and_ we          */
;*    also replace vector? by the result of the predicate and          */
;*    vector-length by the proper function.                            */
;*---------------------------------------------------------------------*/
(define (patch-tree! globals)
   (for-each patch-fun! globals))

;*---------------------------------------------------------------------*/
;*    patch-fun! ...                                                   */
;*---------------------------------------------------------------------*/
(define (patch-fun! variable)
   (let ((fun (variable-value variable)))
      (trace (cfa 4) "Je patch l'arbre de " (shape variable) ": " #\Newline
	     (shape (sfun-body fun)) #\Newline)
      (sfun-body-set! fun (patch! (sfun-body fun)))))

;*---------------------------------------------------------------------*/
;*    patch! ...                                                       */
;*---------------------------------------------------------------------*/
(define-generic (patch! node::node))

;*---------------------------------------------------------------------*/
;*    patch! ::atom ...                                                */
;*---------------------------------------------------------------------*/
(define-method (patch! node::atom)
   node)

;*---------------------------------------------------------------------*/
;*    patch! ::kwote ...                                               */
;*---------------------------------------------------------------------*/
(define-method (patch! node::kwote)
   node)

;*---------------------------------------------------------------------*/
;*    patch! ::kwote/node ...                                          */
;*---------------------------------------------------------------------*/
(define-method (patch! knode::kwote/node)
   (with-access::kwote/node knode (node value)
      (let* ((approx (cfa! node))
	     (tv     (get-approx-type approx)))
	 (if ((@ isa? __object) tv tvec)
	     (let ((knode (shrink! knode)))
		(duplicate::kwote knode (value (a-tvector tv value))))
	     (shrink! knode)))))
		    
;*---------------------------------------------------------------------*/
;*    patch! ::var ...                                                 */
;*---------------------------------------------------------------------*/
(define-method (patch! node::var)
   node)
 
;*---------------------------------------------------------------------*/
;*    patch! ::closure ...                                             */
;*---------------------------------------------------------------------*/
(define-method (patch! node::closure)
   (internal-error "patch!" "Unexpected closure" (shape node)))

;*---------------------------------------------------------------------*/
;*    patch! ::sequence ...                                            */
;*---------------------------------------------------------------------*/
(define-method (patch! node::sequence)
   (with-access::sequence node (nodes)
      (patch*! nodes)
      node))

;*---------------------------------------------------------------------*/
;*    patch! ::app-ly ...                                              */
;*---------------------------------------------------------------------*/
(define-method (patch! node::app-ly)
   (with-access::app-ly node (fun arg)
      (set! fun (patch! fun))
      (set! arg (patch! arg))
      node))

;*---------------------------------------------------------------------*/
;*    patch! ::funcall ...                                             */
;*---------------------------------------------------------------------*/
(define-method (patch! node::funcall)
   (with-access::funcall node (fun args)
      (set! fun (patch! fun))
      (patch*! args)
      node))

;*---------------------------------------------------------------------*/
;*    patch! ::extern ...                                              */
;*---------------------------------------------------------------------*/
(define-method (patch! node::extern)
   (with-access::extern node (expr* type)
      (patch*! expr*)
      node))

;*---------------------------------------------------------------------*/
;*    patch! ::cast ...                                                */
;*---------------------------------------------------------------------*/
(define-method (patch! node::cast)
   (with-access::cast node (arg)
      (patch! arg)
      node))

;*---------------------------------------------------------------------*/
;*    patch! ::setq ...                                                */
;*---------------------------------------------------------------------*/
(define-method (patch! node::setq)
   (with-access::setq node (var value)
      (set! value (patch! value))
      (set! var (patch! var))
      node))

;*---------------------------------------------------------------------*/
;*    patch! ::conditional ...                                         */
;*---------------------------------------------------------------------*/
(define-method (patch! node::conditional)
   (with-access::conditional node (test true false)
       (set! test (patch! test))
       (set! true (patch! true))
       (set! false (patch! false))
       node))

;*---------------------------------------------------------------------*/
;*    patch! ::fail ...                                                */
;*---------------------------------------------------------------------*/
(define-method (patch! node::fail)
   (with-access::fail node (type proc msg obj)
      (set! proc (patch! proc))
      (set! msg (patch! msg))
      (set! obj (patch! obj))
      node))

;*---------------------------------------------------------------------*/
;*    patch! ::select ...                                              */
;*---------------------------------------------------------------------*/
(define-method (patch! node::select)
   (with-access::select node (clauses test)
      (set! test (patch! test))
      (for-each (lambda (clause)
		   (set-cdr! clause (patch! (cdr clause))))
		clauses)
      node))

;*---------------------------------------------------------------------*/
;*    patch! ::let-fun ...                                             */
;*---------------------------------------------------------------------*/
(define-method (patch! node::let-fun)
   (with-access::let-fun node (body locals)
      (for-each patch-fun! locals)
      (set! body (patch! body))
      node))

;*---------------------------------------------------------------------*/
;*    patch! ::let-var ...                                             */
;*---------------------------------------------------------------------*/
(define-method (patch! node::let-var)
   (with-access::let-var node (body bindings)
      (for-each (lambda (binding)
		   (let ((val (cdr binding)))
		      (set-cdr! binding (patch! val))))
		bindings)
      (set! body (patch! body))
      node))

;*---------------------------------------------------------------------*/
;*    patch! ::set-ex-it ...                                           */
;*---------------------------------------------------------------------*/
(define-method (patch! node::set-ex-it)
   (with-access::set-ex-it node (var body)
      (set! body (patch! body))
      (patch! var)
      node))

;*---------------------------------------------------------------------*/
;*    patch! ::jump-ex-it ...                                          */
;*---------------------------------------------------------------------*/
(define-method (patch! node::jump-ex-it)
   (with-access::jump-ex-it node (exit value)
      (set! exit (patch! exit))
      (set! value (patch! value))
      node))

;*---------------------------------------------------------------------*/
;*    patch! ::make-box ...                                            */
;*---------------------------------------------------------------------*/
(define-method (patch! node::make-box)
   (with-access::make-box node (value)
      (set! value (patch! value))
      node))

;*---------------------------------------------------------------------*/
;*    patch! ::box-set! ...                                            */
;*---------------------------------------------------------------------*/
(define-method (patch! node::box-set!)
   (with-access::box-set! node (var value)
      (set! var (patch! var))
      (set! value (patch! value))
      node))

;*---------------------------------------------------------------------*/
;*    patch! ::box-ref ...                                             */
;*---------------------------------------------------------------------*/
(define-method (patch! node::box-ref)
   (with-access::box-ref node (var)
      (set! var (patch! var))
      node))

;*---------------------------------------------------------------------*/
;*    patch*! ...                                                      */
;*---------------------------------------------------------------------*/
(define (patch*! node*)
   (let loop ((node* node*))
      (if (null? node*)
	  'done
	  (begin
	     (set-car! node* (patch! (car node*)))
	     (loop (cdr node*))))))

;*---------------------------------------------------------------------*/
;*    patch! ::app ...                                                 */
;*---------------------------------------------------------------------*/
(define-method (patch! node::app)
   (with-access::app node (fun args)
      (patch*! args)
      (set! fun (patch! fun))
      (let ((v (var-variable fun)))
	 (if (global? v)
	     (if (cfun? (variable-value v))
		 (case (global-id v)
		    ((c-vector?)
		     (patch-vector?! node))
		    (else
		     node))
		 (if (and (eq? (global-id v) 'vector->list)
			  (eq? (global-module v) '__r4_vectors_6_8))
		     (patch-vector->list! node)
		     node))
	     node))))

;*---------------------------------------------------------------------*/
;*    patch! ::vlength ...                                             */
;*---------------------------------------------------------------------*/
(define-method (patch! node::vlength)
   (with-access::vlength/Cinfo node (expr* loc tvector?)
      (patch*! expr*)
      (let* ((approx (cfa! (car expr*)))
	     (tv     (get-approx-type approx)))
	 (if (and ((@ isa? __object) tv tvec) (not tvector?))
	     (let* ((length-tv (symbol-append (type-id tv) '-length))
		    (new-node  (sexp->node `(,length-tv ,(car expr*))
					   '()
					   loc
					   'value)))
;* 		(node-type-set! new-node *int*)                        */
		(inline-node new-node 1 '()))
	     node))))

;*---------------------------------------------------------------------*/
;*    patch-vector?! ...                                               */
;*---------------------------------------------------------------------*/
(define (patch-vector?! node::app)
   (with-access::app node (args loc)
      (patch*! args)
      (let* ((approx (cfa! (car args)))
	     (type   (approx-type approx)))
	 (if (eq? type *vector*)
	     (instantiate::atom
		(loc loc)
		(type *bool*)
		(value #t))
	     node))))

;*---------------------------------------------------------------------*/
;*    patch-vector->list! ...                                          */
;*---------------------------------------------------------------------*/
(define (patch-vector->list! node::app)
   (with-access::app node (args loc)
      (patch*! args)
      (let* ((approx (cfa! (car args)))
	     (tv     (get-approx-type approx)))
	 (if ((@ isa? __object) tv tvec)
	     (let* ((tv->list  (symbol-append (type-id tv) '->list))
		    (new-node  (sexp->node `(,tv->list ,@args)
					   '()
					   loc
					   'value)))
		(node-type-set! new-node (node-type node))
		new-node)
	     node))))

;*---------------------------------------------------------------------*/
;*    patch! ::make-vector-app ...                                     */
;*---------------------------------------------------------------------*/
(define-method (patch! node::make-vector-app)
   (with-access::make-vector-app node (value-approx fun args loc)
      (patch*! args)
      (let* ((type (approx-type value-approx))
	     (tv   (type-tvector type)))
	 (if (type? tv)
	     (let* ((make-tv   (symbol-append 'make- (type-id tv)))
		    (new-node  (sexp->node `(,make-tv ,@args)
					   '()
					   loc
					   'value)))
		(node-type-set! new-node tv)
		(inline-node new-node 1 '()))
	     node))))

;*---------------------------------------------------------------------*/
;*    patch! ::valloc/Cinfo+optim ...                                  */
;*---------------------------------------------------------------------*/
(define-method (patch! node::valloc/Cinfo+optim)
   (with-access::valloc/Cinfo+optim node (value-approx expr* loc)
      (patch*! expr*)
      (let* ((type (approx-type value-approx))
	     (tv   (type-tvector type)))
	 (if (type? tv)
	     (let* ((create-tv (symbol-append 'allocate- (type-id tv)))
		    (new-node  (sexp->node `(,create-tv ,@expr*)
					   '()
					   loc
					   'value)))
		(node-type-set! new-node tv)
		(inline-node new-node 1 '()))
	     node))))
	    
;*---------------------------------------------------------------------*/
;*    patch! ::vref/Cinfo ...                                          */
;*---------------------------------------------------------------------*/
(define-method (patch! node::vref/Cinfo)
   (with-access::vref/Cinfo node (expr* loc tvector?)
      (patch*! expr*)
      (let* ((vec-approx (cfa! (car expr*)))
	     (tv         (get-approx-type vec-approx)))
	 (if (or tvector? (not ((@ isa? __object) tv tvec)))
	     node
	     (let* ((tv-ref   (symbol-append (type-id tv) '-ref))
		    (new-node (sexp->node `(,tv-ref ,@expr*)
 					  '()
					  loc
					  'value)))
		(inline-node new-node 1 '()))))))

;*---------------------------------------------------------------------*/
;*    patch! ::vset!/Cinfo ...                                         */
;*---------------------------------------------------------------------*/
(define-method (patch! node::vset!/Cinfo)
   (with-access::vset!/Cinfo node (expr* loc tvector? ftype)
      (patch*! expr*)
      (let* ((vec-approx (cfa! (car expr*)))
	     (tv         (get-approx-type vec-approx)))
	 (if (or tvector? (not ((@ isa? __object) tv tvec)))
	     node
	     (let* ((tv-set!  (symbol-append (type-id tv) '-set!))
		    (new-node (sexp->node `(,tv-set! ,@expr*)
					  '()
					  loc
					  'value)))
		(inline-node new-node 1 '()))))))
	    






