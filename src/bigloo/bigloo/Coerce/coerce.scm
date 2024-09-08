;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Coerce/coerce.scm           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jan 19 09:57:49 1995                          */
;*    Last change :  Wed Oct  6 15:37:01 2004 (serrano)                */
;*    Copyright   :  1995-2004 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    We coerce an Ast                                                 */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module coerce_coerce
   (include "Tools/trace.sch")
   (import  engine_param
	    tools_shape
	    tools_error
	    type_type
	    type_cache
	    type_coercion
	    type_typeof
	    type_misc
	    object_class
	    ast_var
	    ast_node
	    coerce_pproto
	    coerce_convert
	    coerce_app
	    coerce_apply
	    coerce_funcall)
   (export  (coerce-function! ::variable)
	    (generic coerce!::node ::node ::obj ::type)))

;*---------------------------------------------------------------------*/
;*    coerce-function! ...                                             */
;*---------------------------------------------------------------------*/
(define (coerce-function! variable)
   (trace coerce #"\ncoerce-function!: " (shape variable) #"\n")
   (let* ((fun  (variable-value variable))
	  (body (sfun-body fun))
	  (tres (variable-type variable)))
      (pfunction-proto 3 variable)
      (sfun-body-set! fun (coerce! body variable tres))))

;*---------------------------------------------------------------------*/
;*    coerce! ...                                                      */
;*---------------------------------------------------------------------*/
(define-generic (coerce!::node node::node caller to::type))

;*---------------------------------------------------------------------*/
;*    coerce! ...                                                      */
;*---------------------------------------------------------------------*/
(define-method (coerce! node::atom caller to)
   (convert! node (typeof node) to))
 
;*---------------------------------------------------------------------*/
;*    coerce! ...                                                      */
;*---------------------------------------------------------------------*/
(define-method (coerce! node::kwote caller to)
   (convert! node (typeof node) to))

;*---------------------------------------------------------------------*/
;*    coerce! ...                                                      */
;*---------------------------------------------------------------------*/
(define-method (coerce! node::var caller to)
   (with-access::var node (variable)
      (let ((type (typeof node)))
	 (convert! node type to))))

;*---------------------------------------------------------------------*/
;*    coerce! ::closure ...                                            */
;*---------------------------------------------------------------------*/
(define-method (coerce! node::closure caller to)
   (internal-error "coerce!" "Unexepected `closure' node" (shape node)))

;*---------------------------------------------------------------------*/
;*    coerce! ::sequence ...                                           */
;*---------------------------------------------------------------------*/
(define-method (coerce! node::sequence caller to)
   (with-access::sequence node (nodes)
      (let loop ((hook nodes))
	 (if (null? (cdr hook))
	     (begin
		(set-car! hook (coerce! (car hook) caller to))
		node)
	     (begin
		;; yes, it is strange, we coerce to the type of
		;; the expression !
		(set-car! hook (coerce! (car hook) caller (typeof (car hook))))
		(loop (cdr hook)))))))

;*---------------------------------------------------------------------*/
;*    coerce! ::extern ...                                             */
;*---------------------------------------------------------------------*/
(define-method (coerce! node::extern caller to)
   (with-access::extern node (expr*)
      (let loop ((values expr*))
	 (if (null? values)
	     (convert! node (typeof node) to)
	     (begin
		(set-car! values (coerce! (car values)
					  caller
					  (typeof (car values))))
		(loop (cdr values)))))))

;*---------------------------------------------------------------------*/
;*    coerce! ::getfield ...                                           */
;*---------------------------------------------------------------------*/
(define-method (coerce! node::getfield caller to)
   (with-access::getfield node (expr* ftype otype)
      ;; there is no need to type check the argument because since
      ;; getfield/setfield forms are introduced by the compiler they
      ;; are always type safe
      (set-car! expr* (coerce! (car expr*) caller *obj*))
      (convert! node ftype to)))
      
;*---------------------------------------------------------------------*/
;*    coerce! ::setfield ...                                           */
;*---------------------------------------------------------------------*/
(define-method (coerce! node::setfield caller to)
   (with-access::setfield node (expr* type ftype otype)
      (set-car! expr* (coerce! (car expr*) caller *obj*))
      (set-car! (cdr expr*) (coerce! (cadr expr*) caller ftype))
      (convert! node *unspec* to)))

;*---------------------------------------------------------------------*/
;*    coerce! ::new ...                                                */
;*---------------------------------------------------------------------*/
(define-method (coerce! node::new caller to)
   (with-access::new node (expr* type)
      (let loop ((l expr*))
	 (if (null? l)
	     (convert! node type to)
	     (begin
		(set-car! l (coerce! (car l) caller (typeof (car l))))
		(loop (cdr l)))))
      (convert! node type to)))

;*---------------------------------------------------------------------*/
;*    coerce! ::valloc ...                                             */
;*---------------------------------------------------------------------*/
(define-method (coerce! node::valloc caller to)
   (with-access::valloc node (type otype expr*)
      (set-car! expr* (coerce! (car expr*) caller otype))
      (convert! node type to)))

;*---------------------------------------------------------------------*/
;*    coerce! ::vref ...                                               */
;*---------------------------------------------------------------------*/
(define-method (coerce! node::vref caller to)
   (with-access::vref node (expr* vtype ftype otype)
      ;; when compiling for vector optimization, it may happens
      ;; that the ftype of the node VREF is still *_* (because the cfa
      ;; has not been able to unpatch it). This situation arises for non
      ;; optimized vectors that are accessed by the C function C-VECTOR-REF
      ;; instead of the regular function VECTOR-REF). So we have to make
      ;; a special rule in the typing system.
      (let ((ftype (if (eq? ftype *_*) *obj* ftype)))
	 (set-car! expr* (coerce! (car expr*) caller vtype))
	 (set-car! (cdr expr*) (coerce! (cadr expr*) caller otype))
	 (convert! node ftype to))))
      
;*---------------------------------------------------------------------*/
;*    coerce! ::vset! ...                                              */
;*---------------------------------------------------------------------*/
(define-method (coerce! node::vset! caller to)
   (with-access::vset! node (expr* type vtype ftype otype)
      ;; same remark as for VREF.
      (let ((ftype (if (eq? ftype *_*) *obj* ftype)))
	 (set-car! expr* (coerce! (car expr*) caller vtype))
	 (set-car! (cdr expr*) (coerce! (cadr expr*) caller otype))
	 (set-car! (cddr expr*) (coerce! (caddr expr*) caller ftype))
	 (convert! node type to))))

;*---------------------------------------------------------------------*/
;*    coerce! ::vlength ...                                            */
;*---------------------------------------------------------------------*/
(define-method (coerce! node::vlength caller to)
   (with-access::vlength node (expr* type vtype)
      (set-car! expr* (coerce! (car expr*) caller vtype))
      (convert! node type to)))

;*---------------------------------------------------------------------*/
;*    coerce! ::cast ...                                               */
;*---------------------------------------------------------------------*/
(define-method (coerce! node::cast caller to)
   (with-access::cast node (arg type)
      (trace (coerce 2) "coerce-cast!: " (shape node) " -> " (shape to)
	     #\Newline)
      (set! arg (coerce! arg caller to))
      (convert! node type to)))

;*---------------------------------------------------------------------*/
;*    coerce! ::setq ...                                               */
;*---------------------------------------------------------------------*/
(define-method (coerce! node::setq caller to)
   (with-access::setq node (var value)
      (set! value (coerce! value caller (variable-type (var-variable var))))
      (convert! node *unspec* to)))

;*---------------------------------------------------------------------*/
;*    coerce! ::conditional ...                                        */
;*    -------------------------------------------------------------    */
;*    We play here a special trick in order to avoir reporting         */
;*    "illegal" type errors because of a lack of data flow             */
;*    information propagation. Basically what is done here is          */
;*    the following:                                                   */
;*     - the test is typed.                                            */
;*     - if the test if statically recognize as #t or #f               */
;*          then only on branch is compiled.                           */
;*          else the whole expression is compiled.                     */
;*    Of course, all this is very bad because it makes strong          */
;*    assumption on the shape of the test. If this shape changes,      */
;*    the compiler will, one more time, erroneously fails              */
;*    reporting "false" type errors.                                   */
;*---------------------------------------------------------------------*/
(define-method (coerce! node::conditional caller to)
   (define (test-static-value node)
      (and (let-var? node)
	   (with-access::let-var node (bindings body)
	      (and (pair? bindings)
		   (null? (cdr bindings))
		   (app? body)
		   (let* ((fun (app-fun body))
			  (val (variable-value (var-variable fun)))
			  (typec (fun-predicate-of val))
			  (typep (variable-type (car (car bindings))))
			  (typev (if (eq? typep *obj*)
				     (typeof (cdr (car bindings)))
				     typep)))
		      (cond
			 ((not (type? typec))
			  ;; this is not a predicate
			  #f)
			 ((eq? typev *obj*)
			  ;; we have not idea of the result of the type
			  #f)
			 ((type-less-specific? typec typev)
			  'true)
			 ((type-disjoint? typec typev)
			  'false)
			 (else
			  #f)))))))
   (with-access::conditional node (test true false type)
      (set! test (coerce! test caller *bool*))
      (case (test-static-value test)
	 ((true)
	  (coerce! true caller to))
	 ((false)
	  (coerce! false caller to))
	 (else
	  (set! true (coerce! true caller to))
	  (set! false (coerce! false caller to))
	  (set! type to)
	  node))))

;*---------------------------------------------------------------------*/
;*    coerce! ::fail ...                                               */
;*---------------------------------------------------------------------*/
(define-method (coerce! node::fail caller to)
   (with-access::fail node (proc msg obj)
      (set! proc (coerce! proc caller *obj*))
      (set! msg (coerce! msg caller *obj*))
      (set! obj (coerce! obj caller *obj*)) 
      (convert! node *magic* to)))

;*---------------------------------------------------------------------*/
;*    coerce! ::select ...                                             */
;*---------------------------------------------------------------------*/
(define-method (coerce! node::select caller to)
   (with-access::select node (loc clauses test type)
      (set! type to)
      (let ((clauses        clauses)
	    (test-type      (select-item-type node))
	    (test-node-type (typeof test)))
	 ;; select constructions are normalized: the test should have
	 ;; been placed in a variable. That's why this test below should
	 ;; work. This test may fails (in strange cases that I'm currently
	 ;; ignoring) and then, it may happens that some `correct' select
	 ;; construction could be rejected. These forms are those where the
	 ;; else clause trap objects of different types from the one tested
	 ;; in the clauses.
	 (if (not (coercer-exists? test-node-type test-type))
	     (coerce! (runtime-type-error loc
					  (type-id test-type)
					  test)
		      caller
		      to)
	     (begin
		(select-test-set! node (coerce! test caller test-type))
		(for-each (lambda (clause)
			     (set-cdr! clause (coerce! (cdr clause)
						       caller
						       to)))
			  clauses)
		node)))))
      
;*---------------------------------------------------------------------*/
;*    coerce! ::let-fun ...                                            */
;*---------------------------------------------------------------------*/
(define-method (coerce! node::let-fun caller to)
   (with-access::let-fun node (body locals)
      (inc-ppmarge!)
      (for-each coerce-function! locals)
      (set! body (coerce! body caller to))
      (dec-ppmarge!)
      node))

;*---------------------------------------------------------------------*/
;*    coerce! ::let-var ...                                            */
;*---------------------------------------------------------------------*/
(define-method (coerce! node::let-var caller to)
   (trace (coerce 3) "coercer ::let-var: " (shape node) " -> " (shape to)
	  #\Newline)
   (with-access::let-var node (body bindings)
      (inc-ppmarge!)
      (for-each (lambda (binding)
		   (pvariable-proto 3 (car binding))
		   (set-cdr! binding (coerce! (cdr binding)
					      caller 
					      (local-type (car binding)))))
		bindings)
      (set! body (coerce! body caller to))
      (dec-ppmarge!)
      node))
 
;*---------------------------------------------------------------------*/
;*    coerce! ::set-ex-it ...                                          */
;*---------------------------------------------------------------------*/
(define-method (coerce! node::set-ex-it caller to)
   (with-access::set-ex-it node (var body)
      (set! var (coerce! var caller *exit*))
      (pvariable-proto 3 (var-variable var))
      (set! body (coerce! body caller to))
      node))

;*---------------------------------------------------------------------*/
;*    coerce! ::jump-ex-it ...                                         */
;*---------------------------------------------------------------------*/
(define-method (coerce! node::jump-ex-it caller to)
   (with-access::jump-ex-it node (exit value)
      (set! exit (coerce! exit caller *exit*))
      (set! value (coerce! value caller to))
      node))

;*---------------------------------------------------------------------*/
;*    coerce! ::make-box ...                                           */
;*---------------------------------------------------------------------*/
(define-method (coerce! node::make-box caller to)
   (with-access::make-box node (value)
      (set! value (coerce! value caller *obj*))
      node))

;*---------------------------------------------------------------------*/
;*    coerce! ::box-ref ...                                            */
;*---------------------------------------------------------------------*/
(define-method (coerce! node::box-ref caller to)
   (with-access::box-ref node (var)
      (convert! node *obj* to)))

;*---------------------------------------------------------------------*/
;*    coerce! ::box-set! ...                                           */
;*---------------------------------------------------------------------*/
(define-method (coerce! node::box-set! caller to)
   (with-access::box-set! node (var value)
      (local-type-set! (var-variable var) *obj*)
      (set! value (coerce! value caller *obj*))
      (convert! node *unspec* to)))

