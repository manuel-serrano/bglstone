;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Trace/walk.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Apr 13 13:53:58 1995                          */
;*    Last change :  Fri Nov  5 09:13:20 2004 (serrano)                */
;*    Copyright   :  1995-2004 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The introduction of trace in debugging mode.                     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module trace_walk
   (include "Engine/pass.sch"
	    "Ast/node.sch"
	    "Tools/location.sch")
   (import  tools_shape
	    tools_error
	    tools_misc
	    type_env
	    ast_sexp
	    ast_ident
	    module_module
	    engine_param
	    (mark-symbol-non-user! ast_ident)
	    (find-global ast_env)
	    (find-location tools_location))
   (export  (trace-walk! tree)))

;*---------------------------------------------------------------------*/
;*    trace-walk! ...                                                  */
;*---------------------------------------------------------------------*/
(define (trace-walk! globals)
   (pass-prelude "Trace")
   ;; We make some extra tracing for top level set! expression. Top level
   ;; set! expression that change a global variable are traced. This enables
   ;; tracing of toplevel defined closures. This transformation is applied
   ;; before regular tracing because it only scans [begin] top level forms.
   ;; It stops before any nested expression.
   (let* ((id   'toplevel-init)
	  (glo  (find-global id *module*)))
      (if (global? glo)
	  (with-access::sfun (global-value glo) (body)
	     (set! body (toplevel-trace-node body)))))
   ;; then, we trace all functions (including the toplevel one)
   (for-each (lambda (v) (trace-fun! v '())) globals)
   (pass-postlude globals))

;*---------------------------------------------------------------------*/
;*    trace-id ...                                                     */
;*    -------------------------------------------------------------    */
;*    To get the trace identifier we take the ident of the global      */
;*    function holding the function. There is one exception for        */
;*    the module [toplevel-init] function. Instead of using the        */
;*    rather anonymous [toplevel-init] ident, we use the name of       */
;*    the module.                                                      */
;*---------------------------------------------------------------------*/
(define (trace-id variable)
   (cond
      ((and (global? variable)
	    (eq? (global-id variable) 'toplevel-init))
       (symbol-append (string->symbol "%toplevel@") (global-module variable)))
      ((and (global? variable)
	    (eq? (global-id variable) 'imported-modules-init))
       (symbol-append (string->symbol "%import@") (global-module variable)))
      (else
       (variable-id variable))))

;*---------------------------------------------------------------------*/
;*    trace-fun! ...                                                   */
;*    -------------------------------------------------------------    */
;*    We don't trace predicates. It is useless and make the code       */
;*    much bigger in safe modes.                                       */
;*---------------------------------------------------------------------*/
(define (trace-fun! var stack)
   (let* ((fun  (variable-value var))
	  (body (sfun-body fun))
	  (type (variable-type var))
	  (lloc (if (global? var)
		    (find-location (find-last-sexp (global-src var)))
		    (node-loc (find-last-node body)))))
      (if (and (not (fun-predicate-of fun))
	       (not (memq 'no-trace (sfun-property fun))))
	  (begin
	     (enter-function (trace-id var))
	     (let* ((new-body  (if (or (>=fx *compiler-debug* 3)
				       (and (global? var)
					    (eq? (global-id var)
						 'toplevel-init)))
				   ;; we always goes trough the first level
				   ;; (i.e. not the nested local functions)
				   ;; of the toplevel-init function even
				   ;; if [*compiler-debug* < 3]. That way
				   ;; we are sure that global closures will
				   ;; be correctly traced and not labeled
				   ;; [toplevel-init].
				   (trace-node body (cons var stack))
				   body))
		    (new2-body (make-traced-node new-body
						 type
						 (trace-id var)
						 lloc
						 stack)))
		(sfun-body-set! fun new2-body)
		(leave-function))))))

;*---------------------------------------------------------------------*/
;*    find-last-sexp ...                                               */
;*    -------------------------------------------------------------    */
;*    Find the last sexp embedded in SEXP. The expression we are       */
;*    seeking is a LIST, not an atom. The function we are              */
;*    implementing could also be called something like LAST-LIST.      */
;*---------------------------------------------------------------------*/
(define (find-last-sexp sexp)
   (let loop ((sexp sexp)
	      (res sexp))
      (cond
	 ((not (pair? sexp))
	  res)
	 ((not (pair? (cdr sexp)))
	  (loop (car sexp) sexp))
	 (else
	  (loop (last-pair sexp) sexp)))))

;*---------------------------------------------------------------------*/
;*    find-last-node ::node ...                                        */
;*    -------------------------------------------------------------    */
;*    This function computes the same computation as FIND-LAST-SEXP    */
;*    but on NODEs instead of LISTs.                                   */
;*---------------------------------------------------------------------*/
(define-generic (find-last-node node::node)
   node)

;*---------------------------------------------------------------------*/
;*    find-last-node ::sequence ...                                    */
;*---------------------------------------------------------------------*/
(define-method (find-last-node node::sequence)
   (with-access::sequence node (nodes)
      (if (pair? nodes)
	  (find-last-node (car (last-pair nodes)))
	  node)))

;*---------------------------------------------------------------------*/
;*    find-last-node ::app ...                                         */
;*---------------------------------------------------------------------*/
(define-method (find-last-node node::app)
   (with-access::app node (args)
      (if (pair? args)
	  (find-last-node (car (last-pair args)))
	  node)))

;*---------------------------------------------------------------------*/
;*    find-last-node ::app-ly ...                                      */
;*---------------------------------------------------------------------*/
(define-method (find-last-node node::app-ly)
   (find-last-node (app-ly-arg node)))

;*---------------------------------------------------------------------*/
;*    find-last-node ::funcall ...                                     */
;*---------------------------------------------------------------------*/
(define-method (find-last-node node::funcall)
   (with-access::funcall node (fun args)
      (if (pair? args)
	  (find-last-node (car (last-pair args)))
	  (find-last-node fun))))

;*---------------------------------------------------------------------*/
;*    find-last-node ::extern ...                                      */
;*---------------------------------------------------------------------*/
(define-method (find-last-node node::extern)
   (with-access::extern node (expr*)
      (if (pair? expr*)
	  (find-last-node (car (last-pair expr*)))
	  node)))

;*---------------------------------------------------------------------*/
;*    find-last-node ::setq ...                                        */
;*---------------------------------------------------------------------*/
(define-method (find-last-node node::setq)
   (find-last-node (setq-value node)))

;*---------------------------------------------------------------------*/
;*    find-last-node ::conditional ...                                 */
;*---------------------------------------------------------------------*/
(define-method (find-last-node node::conditional)
   (find-last-node (conditional-false node)))

;*---------------------------------------------------------------------*/
;*    find-last-node ::fail ...                                        */
;*---------------------------------------------------------------------*/
(define-method (find-last-node node::fail)
   (find-last-node (fail-obj node)))

;*---------------------------------------------------------------------*/
;*    find-last-node ::select ...                                      */
;*---------------------------------------------------------------------*/
(define-method (find-last-node node::select)
   (with-access::select node (clauses test)
      (if (pair? clauses)
	  (find-last-node (cdr (last-pair clauses)))
	  (find-last-sexp test))))

;*---------------------------------------------------------------------*/
;*    find-last-node ::let-var ...                                     */
;*---------------------------------------------------------------------*/
(define-method (find-last-node node::let-var)
   (find-last-node (let-var-body node)))

;*---------------------------------------------------------------------*/
;*    find-last-node ::let-fun ...                                     */
;*---------------------------------------------------------------------*/
(define-method (find-last-node node::let-fun)
   (find-last-node (let-fun-body node)))

;*---------------------------------------------------------------------*/
;*    find-last-node ::set-ex-it ...                                   */
;*---------------------------------------------------------------------*/
(define-method (find-last-node node::set-ex-it)
   (find-last-node (set-ex-it-body node)))

;*---------------------------------------------------------------------*/
;*    find-last-node ::jump-ex-it ...                                  */
;*---------------------------------------------------------------------*/
(define-method (find-last-node node::jump-ex-it)
   (find-last-node (jump-ex-it-value node)))

;*---------------------------------------------------------------------*/
;*    make-traced-node ...                                             */
;*---------------------------------------------------------------------*/
(define (make-traced-node::let-var node::node type::type symbol lloc stack)
   (let* ((loc  (node-loc node))
	  (aux  (mark-symbol-non-user! (gensym 'aux)))
	  (taux (make-typed-ident aux (type-id type)))
	  (sym  (mark-symbol-non-user! (gensym 'symbol)))
	  (s+   (if (and (pair? stack) (variable? (car stack)))
		    (symbol-append symbol ': (variable-id (car stack)))
		    symbol))
	  (exp `(let ((,sym ',s+))
		   (let ()
		      ($push-trace ,sym)
		      ;; this dummy `let' and `set!' and used to normalize
		      ;; the code and disable the reduction optimizations
		      ;; on `push-trace' nodes (at least it wont produce
		      ;; any code and then, has no cost).
		      (set! ,sym ,sym)
		      (let ((,taux ,node))
			 ,(if (location? lloc)
			      (econs '$pop-trace '() lloc)
			      '($pop-trace))
			 ,aux))))
	  (node (sexp->node exp '() loc 'value)))
      (if (not (memq *target-language* '(jvm .net)))
	  ;; with the JVM back-end there is no constraint of
	  ;; block declarations
	  (let-var-removable?-set! node #f))
      node))

;*---------------------------------------------------------------------*/
;*    trace-node ...                                                   */
;*---------------------------------------------------------------------*/
(define-generic (trace-node::node node::node stack)
   node)

;*---------------------------------------------------------------------*/
;*    trace-node ::sequence ...                                        */
;*---------------------------------------------------------------------*/
(define-method (trace-node node::sequence stack)
   (trace-node*! (sequence-nodes node) stack)
   node)

;*---------------------------------------------------------------------*/
;*    trace-node ::app ...                                             */
;*---------------------------------------------------------------------*/
(define-method (trace-node node::app stack)
   (trace-node*! (app-args node) stack)
   node)
 
;*---------------------------------------------------------------------*/
;*    trace-node ::app-ly ...                                          */
;*---------------------------------------------------------------------*/
(define-method (trace-node node::app-ly stack)
   (with-access::app-ly node (fun arg)
      (set! fun (trace-node fun stack))
      (set! arg (trace-node arg stack))
      node))

;*---------------------------------------------------------------------*/
;*    trace-node ::funcall ...                                         */
;*---------------------------------------------------------------------*/
(define-method (trace-node node::funcall stack)
   (with-access::funcall node (fun args)
      (set! fun (trace-node fun stack))
      (trace-node*! args stack)
      node))

;*---------------------------------------------------------------------*/
;*    trace-node ::extern ...                                          */
;*---------------------------------------------------------------------*/
(define-method (trace-node node::extern stack)
   (trace-node*! (extern-expr* node) stack)
   node)

;*---------------------------------------------------------------------*/
;*    trace-node ::cast ...                                            */
;*---------------------------------------------------------------------*/
(define-method (trace-node node::cast stack)
   (trace-node (cast-arg node) stack)
   node)

;*---------------------------------------------------------------------*/
;*    trace-node ::setq ...                                            */
;*---------------------------------------------------------------------*/
(define-method (trace-node node::setq stack)
   (setq-value-set! node (trace-node (setq-value node) stack))
   node)

;*---------------------------------------------------------------------*/
;*    trace-node ::conditional ...                                     */
;*---------------------------------------------------------------------*/
(define-method (trace-node node::conditional stack)
   (with-access::conditional node (test true false)
       (set! test (trace-node test stack))
       (set! true (trace-node true stack))
       (set! false (trace-node false stack))
       node))

;*---------------------------------------------------------------------*/
;*    trace-node ::fail ...                                            */
;*---------------------------------------------------------------------*/
(define-method (trace-node node::fail stack)
   (with-access::fail node (proc msg obj)
      (set! proc (trace-node proc stack))
      (set! msg (trace-node msg stack))
      (set! obj (trace-node obj stack))
      node))

;*---------------------------------------------------------------------*/
;*    trace-node ::select ...                                          */
;*---------------------------------------------------------------------*/
(define-method (trace-node node::select stack)
   (with-access::select node (clauses test)
      (set! test (trace-node test stack))
      (for-each (lambda (clause)
		   (set-cdr! clause (trace-node (cdr clause) stack)))
		clauses)
      node))

;*---------------------------------------------------------------------*/
;*    trace-node ::let-fun ...                                         */
;*---------------------------------------------------------------------*/
(define-method (trace-node node::let-fun stack)
   (with-access::let-fun node (body locals)
      (for-each (lambda (v) (trace-fun! v stack)) locals)
      (set! body (trace-node body stack))
      node))

;*---------------------------------------------------------------------*/
;*    trace-node ::let-var ...                                         */
;*---------------------------------------------------------------------*/
(define-method (trace-node node::let-var stack)
   (with-access::let-var node (body bindings)
      (for-each (lambda (binding)
		   (set-cdr! binding (trace-node (cdr binding) stack)))
		bindings)
      (set! body (trace-node body stack))
      node))

;*---------------------------------------------------------------------*/
;*    trace-node ::set-ex-it ...                                       */
;*---------------------------------------------------------------------*/
(define-method (trace-node node::set-ex-it stack)
   (set-ex-it-body-set! node (trace-node (set-ex-it-body node) stack))
   node)

;*---------------------------------------------------------------------*/
;*    trace-node ::jump-ex-it ...                                      */
;*---------------------------------------------------------------------*/
(define-method (trace-node node::jump-ex-it stack)
   (with-access::jump-ex-it node (exit value)
      (set! exit (trace-node exit stack)) 
      (set! value (trace-node value stack))
      node))

;*---------------------------------------------------------------------*/
;*    trace-node ::make-box ...                                        */
;*---------------------------------------------------------------------*/
(define-method (trace-node node::make-box stack)
   (make-box-value-set! node (trace-node (make-box-value node) stack))
   node)

;*---------------------------------------------------------------------*/
;*    trace-node ::box-ref ...                                         */
;*---------------------------------------------------------------------*/
(define-method (trace-node node::box-ref stack)
   (box-ref-var-set! node (trace-node (box-ref-var node) stack))
   node)

;*---------------------------------------------------------------------*/
;*    trace-node ::box-set! ...                                        */
;*---------------------------------------------------------------------*/
(define-method (trace-node node::box-set! stack)
   (with-access::box-set! node (var value)
      (set! var (trace-node var stack))
      (set! value (trace-node value stack))
      node))

;*---------------------------------------------------------------------*/
;*    trace-node*! ...                                                 */
;*---------------------------------------------------------------------*/
(define (trace-node*! node* stack)
   (if (null? node*)
       'done
       (begin
	  (set-car! node* (trace-node (car node*) stack))
	  (trace-node*! (cdr node*) stack))))
   
;*---------------------------------------------------------------------*/
;*    toplevel-trace-node ...                                          */
;*---------------------------------------------------------------------*/
(define-generic (toplevel-trace-node::node node::node)
   node)

;*---------------------------------------------------------------------*/
;*    toplevel-trace-node ::sequence ...                               */
;*---------------------------------------------------------------------*/
(define-method (toplevel-trace-node node::sequence)
   (toplevel-trace-node*! (sequence-nodes node))
   node)

;*---------------------------------------------------------------------*/
;*    toplevel-trace-node ::setq ...                                   */
;*    -------------------------------------------------------------    */
;*    This method instruments global variable affections provided      */
;*    those affections are:                                            */
;*      - top level                                                    */
;*      - they set composed values (i.e. non trivial values).          */
;*---------------------------------------------------------------------*/
(define-method (toplevel-trace-node node::setq)
   (with-access::setq node (var value loc)
      (with-access::var var (variable)
	 (if (and (global? variable)
		  (not (or (atom? value)
			   (var? value)
			   (kwote? value)
			   (pragma? value))))
	     (let* ((id    (global-id variable))
		    (type  (global-type variable))
		    (trace (make-traced-node value type id loc '())))
		(set! value trace)))))
   node)

;*---------------------------------------------------------------------*/
;*    toplevel-trace-node*! ...                                        */
;*---------------------------------------------------------------------*/
(define (toplevel-trace-node*! node*)
   (if (null? node*)
       'done
       (begin
	  (set-car! node* (toplevel-trace-node (car node*)))
	  (toplevel-trace-node*! (cdr node*)))))
   




