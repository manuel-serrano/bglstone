;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Cgen/cgen.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Jul  2 13:17:04 1996                          */
;*    Last change :  Wed Jan 19 11:29:52 2005 (serrano)                */
;*    Copyright   :  1996-2005 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The C production code.                                           */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module cgen_cgen
   (include "Tools/trace.sch"
	    "Tools/location.sch")
   (import  tools_error
	    tools_shape
	    engine_param
	    type_type
	    type_tools
	    type_cache
	    type_typeof
	    object_class
	    object_slots
	    ast_var
	    ast_node
	    ast_local
	    effect_effect
	    cgen_cop
	    cgen_emit-cop
	    cgen_capp
	    backend_c_emit
	    backend_c_prototype
	    backend_backend
	    backend_cplib)
   (export  (cgen-function ::global)
	    (node-setq::setq variable::variable value::node)
	    (generic node->cop::cop ::node ::procedure)
	    (make-local-svar/name::local ::symbol ::type)
	    (bdb-let-var::cop ::cop loc)
	    *the-global*
	    *return-kont*
	    *id-kont*
	    (block-kont cop loc)
	    *stop-kont*))
  
;*---------------------------------------------------------------------*/
;*    cgen-function ...                                                */
;*---------------------------------------------------------------------*/
(define (cgen-function global::global)
   (enter-function (global-id global))
   (trace cgen "cgen global: " (shape global) #\Newline)
   (trace (cgen 2) "*void-kont*: " *void-kont* #\Newline)
   (trace (cgen 2) "*return-kont*: " *return-kont* #\Newline)
   (set! *the-global* global)
   (let ((sh (shape global)))
      [assert (sh) (string? (global-name global))])
   (let* ((sfun (widen!::sfun/C (global-value global)
		   (label (instantiate::clabel
			     (loc (sfun-loc (global-value global)))
			     (name (global-name global))))
		   (integrated #t)))
	  (loc  (sfun-loc sfun))
	  (cop  (node->cop (sfun-body sfun)
			   (if (eq? (global-type global) *void*)
			       *void-kont*
			       *return-kont*))))
      (reset-bdb-loc!)
      (newline *c-port*)
      (newline *c-port*)
      (display "/* " *c-port*)
      (display (shape global) *c-port*)
      (display " */" *c-port*)
      ;; we have to emit a dummy location otherwise gdb get confused
      ;; with the function arguments! Thus all functions looks like
      ;; starting at the module definition site instead of there correct
      ;; definition site
      (emit-bdb-loc #f)
      ;; we have to reset the loc because we must not emit location
      ;; before the first c expression otherwise gdb get confused
      (reset-bdb-loc!)
      (clabel-body-set! (sfun/C-label sfun) cop)
      (sfun->c sfun global)
      (let ((cop (block-kont (sfun/C-label sfun) loc)))
	 ;; we have to remove first location otherwise gdb get confused
 	 '(if (>fx *bdb-debug* 0)
	     (let loop ((cop cop))
		(cond
		   ((block? cop)
		    (cop-loc-set! cop #unspecified)
		    (loop (block-body cop)))
		   ((csequence? cop)
		    (with-access::csequence cop (c-exp? cops)
		       (if (and (not c-exp?) (pair? cops))
			   (begin
			      (cop-loc-set! (car cops) #unspecified)
			      (loop (car cops))))))
		   ((clabel? cop)
		    (with-access::clabel cop (used? body loc)
		       (if (not used?)
			   (begin
			      (set! loc #unspecified)
			      (loop body))))))))
	 ;; we define a local variable that acts as a temporary variable
	 (display "{ AN_OBJECT;" *c-port*)
	 ;; when compiling for debugging, we have to insert a dummy
	 ;; statement otherwise gdb get confused
	 (if (and (> *bdb-debug* 0) (location? loc))
	     (begin
		(emit-bdb-loc loc)
		(display "{ obj_t ___ = BUNSPEC; } /* bdb dummy init stmt */"
			 *c-port*)))
	 ;; we now may emit the body
	 (emit-cop cop)
	 ;; emit the current location before the closing bracket
	 (emit-bdb-loc (get-current-bdb-loc))
	 ;; and then clause the function body
	 (fprint *c-port* "}"))
      (no-bdb-newline)
      (leave-function)))
  
;*---------------------------------------------------------------------*/
;*    *the-global* ...                                                 */
;*    -------------------------------------------------------------    */
;*    This variable is use to implement global tail calls.             */
;*---------------------------------------------------------------------*/
(define *the-global* #unspecified)

;*---------------------------------------------------------------------*/
;*    sfun->c ...                                                      */
;*---------------------------------------------------------------------*/
(define (sfun->c sfun variable)
   (with-access::global variable (type id name import)
      (if (eq? import 'export)
	  (display "BGL_EXPORTED_DEF " *c-port*))
      (display (make-typed-declaration
		type
		(string-append
		 name
		 (if (null? (sfun-args sfun))
		     "()"
		     (string-append
		      "("
		      (let loop ((args (sfun-args sfun)))
			 (if (null? (cdr args))
			     (with-access::local (car args) (name id type)
				(string-append
				 (make-typed-declaration type name) ")"))
			     (with-access::local (car args) (name id type)
				(string-append
				 (make-typed-declaration type name)
				 ", "
				 (loop (cdr args))))))))))
	       *c-port*))
   (no-bdb-newline))

;*---------------------------------------------------------------------*/
;*    *return-kont* ...                                                */
;*---------------------------------------------------------------------*/
(define *return-kont*
   (lambda (cop)
      (instantiate::creturn
	 (loc   (cop-loc cop))
	 (value (cond
		   ((csetq? cop)
		    (instantiate::csequence
		       (loc    (cop-loc cop))
		       (c-exp? #t)
		       (cops (list cop (instantiate::catom
					  (value #unspecified))))))
		   (else
		    cop))))))

;*---------------------------------------------------------------------*/
;*    *id-kont* ...                                                    */
;*---------------------------------------------------------------------*/
(define *id-kont* (lambda (cop) cop))
      
;*---------------------------------------------------------------------*/
;*    *void-kont* ...                                                  */
;*---------------------------------------------------------------------*/
(define *void-kont*
   (lambda (cop)
      (instantiate::cvoid (value cop))))

;*---------------------------------------------------------------------*/
;*    *stop-kont* ...                                                  */
;*---------------------------------------------------------------------*/
(define *stop-kont*
   (lambda (cop)
      (instantiate::stop (value cop))))

;*---------------------------------------------------------------------*/
;*    block-kont ...                                                   */
;*---------------------------------------------------------------------*/
(define (block-kont cop loc)
   (cond
      ((block? cop)
       cop)
      (else
       (instantiate::block
	  (body cop)
	  (loc  loc)))))

;*---------------------------------------------------------------------*/
;*    *fail-kont* ...                                                  */
;*---------------------------------------------------------------------*/
(define *fail-kont* (lambda (cop) cop))

;*---------------------------------------------------------------------*/
;*    *exit-kont* ...                                                  */
;*---------------------------------------------------------------------*/
(define *exit-kont* (lambda (cop) cop))

;*---------------------------------------------------------------------*/
;*    make-setq-kont ...                                               */
;*---------------------------------------------------------------------*/
(define (make-setq-kont var loc kont)
   (lambda (cop)
      (if (cfail? cop)
	  cop
	  (kont (instantiate::csetq
		   (var (instantiate::varc (variable var)))
		   (value (cond
			     ((csetq? cop)
			      (instantiate::csequence
				 (loc (cop-loc cop))
				 (c-exp? #t)
				 (cops (list cop
					       (instantiate::catom
						  (loc (cop-loc cop))
						  (value #unspecified))))))
			     (else
			      cop)))
		   (loc loc))))))
			  
;*---------------------------------------------------------------------*/
;*    node->cop ...                                                    */
;*---------------------------------------------------------------------*/
(define-generic (node->cop::cop node::node kont::procedure))

;*---------------------------------------------------------------------*/
;*    node->cop ...                                                    */
;*---------------------------------------------------------------------*/
(define-method (node->cop node::atom kont)
   (trace (cgen 3)
	  "(node->cop node::atom kont): " (shape node) #\Newline
	  "  kont: " kont #\Newline)
   (with-access::atom node (value loc)
      (kont (instantiate::catom
	       (value value)
	       (loc   loc)))))

;*---------------------------------------------------------------------*/
;*    node->cop ...                                                    */
;*---------------------------------------------------------------------*/
(define-method (node->cop node::kwote kont)
   (trace (cgen 3)
	  "(node->cop node::kwote kont): " (shape node) #\Newline
	  "  kont: " kont #\Newline)
   (internal-error "node->cop" "Unexpected `kwote' node" (shape node)))

;*---------------------------------------------------------------------*/
;*    node->cop ...                                                    */
;*---------------------------------------------------------------------*/
(define-method (node->cop node::var kont)
   (trace (cgen 3)
	  "(node->cop node::var kont): " (shape node) #\Newline
	  "  kont: " kont #\Newline)
   (trace (cgen 4) "(node->cop node::var kont): " (shape node) #\Newline
	  "  var-variable-name: " (variable-name (var-variable node))
	  #\Newline)
   (with-access::var node (variable loc)
      (kont (instantiate::varc
	       (variable variable)
	       (loc      loc)))))

;*---------------------------------------------------------------------*/
;*    node->cop ::closure ...                                          */
;*---------------------------------------------------------------------*/
(define-method (node->cop node::closure kont)
   (trace (cgen 3)
	  "(node->cop node::closure kont): " (shape node) #\Newline
	  "  kont: " kont #\Newline)
   (internal-error "node->cop" "Unexpected `closure' node" (shape node)))

;*---------------------------------------------------------------------*/
;*    node->cop ::sequence ...                                         */
;*---------------------------------------------------------------------*/
(define-method (node->cop node::sequence kont)
   (trace (cgen 3)
	  "(node->cop node::sequence kont): " (shape node) #\Newline
	  "  kont: " kont #\Newline)
   (with-access::sequence node (nodes loc)
      (let ((exp nodes))
	 (cond
	    ((null? exp)
	     (kont (instantiate::nop
		      (loc loc))))
	    ((null? (cdr exp))
	     (let ((cop (node->cop (car exp) kont)))
		(instantiate::stop (value cop))))
	    (else
	     (let loop ((exp exp)
			(new '()))
		(if (null? (cdr exp))
		    (let ((cop (node->cop (car exp) kont)))
		       (instantiate::csequence
			  (loc  (cop-loc cop))
			  (cops (reverse! (cons cop new)))))
		    (begin
		       (if (not (side-effect? (car exp)))
			   (loop (cdr exp) new)
			   (loop (cdr exp)
				 (cons (node->cop (car exp) *stop-kont*)
				       new)))))))))))

;*---------------------------------------------------------------------*/
;*    extern->cop ...                                                  */
;*---------------------------------------------------------------------*/
(define (extern->cop format::bstring args-safe node::extern kont)
   (trace (cgen 3)
	  "(extern->cop node::extern kont): " (shape node) #\Newline
	  "  kont: " kont #\Newline)
   (with-access::extern node (expr* loc)
      (node-args->cop expr*
		      args-safe
		      loc
		      (lambda (new-args)
			 (kont (instantiate::cpragma
				  (loc loc)
				  (format format)
				  (args new-args)))))))
   
;*---------------------------------------------------------------------*/
;*    node->cop ::pragma ...                                           */
;*---------------------------------------------------------------------*/
(define-method (node->cop node::pragma kont)
   (trace (cgen 3)
	  "(node->cop node::pragma kont): " (shape node) #\Newline
	  "  kont: " kont #\Newline)
   (with-access::pragma node (format)
      (extern->cop format #f node kont)))

;*---------------------------------------------------------------------*/
;*    node->cop ::getfield ...                                         */
;*---------------------------------------------------------------------*/
(define-method (node->cop node::getfield kont)
   (trace (cgen 3)
	  "(node->cop node::getfield kont): " (shape node) #\Newline
	  "  kont: " kont #\Newline)
   (with-access::getfield node (fname otype)
      (let ((fmt (string-append "(((" (type-name otype) ")CREF($1))->"
				fname ")")))
	 (extern->cop fmt #t node kont))))
   
;*---------------------------------------------------------------------*/
;*    node->cop ::setfield ...                                         */
;*---------------------------------------------------------------------*/
(define-method (node->cop node::setfield kont)
   (trace (cgen 3)
	  "(node->cop node::setfield kont): " (shape node) #\Newline
	  "  kont: " kont #\Newline)
   (with-access::setfield node (fname ftype otype)
      (let ((fmt (string-append "((((" (type-name otype) ")CREF($1))->"
				fname ") = ((" (type-name ftype)
				")$2), BUNSPEC)")))
	 (extern->cop fmt #t node kont))))

;*---------------------------------------------------------------------*/
;*    node->cop ::vlength ...                                          */
;*---------------------------------------------------------------------*/
(define-method (node->cop node::vlength kont)
   (with-access::vlength node (c-format)
      (trace (cgen 3)
	     "(node->cop node::vlength kont): " (shape node) #\Newline
	     "  kont: " kont " " c-format #\Newline)
      (extern->cop c-format #t node kont)))
   
;*---------------------------------------------------------------------*/
;*    node->cop ::vref ...                                             */
;*---------------------------------------------------------------------*/
(define-method (node->cop node::vref kont)
   (with-access::vref node (c-format)
      (trace (cgen 3)
	     "(node->cop node::vref kont): " (shape node) #\Newline
	     "  kont: " kont " " c-format #\Newline)
      (extern->cop c-format #t node kont)))
   
;*---------------------------------------------------------------------*/
;*    node->cop ::vset! ...                                            */
;*---------------------------------------------------------------------*/
(define-method (node->cop node::vset! kont)
   (with-access::vset! node (c-format)
      (trace (cgen 3)
	     "(node->cop node::vset! kont): " (shape node) #\Newline
	     "  kont: " kont " " c-format #\Newline)
      (extern->cop c-format #t node kont)))
   
;*---------------------------------------------------------------------*/
;*    node->cop ::valloc ...                                           */
;*---------------------------------------------------------------------*/
(define-method (node->cop node::valloc kont)
   (with-access::valloc node (c-heap-format)
      (trace (cgen 3)
	     "(node->cop node::vallocate kont): " (shape node) #\Newline
	     "  kont: " kont " " c-heap-format " " #\Newline) 
      (extern->cop c-heap-format #t node kont)))
   
;*---------------------------------------------------------------------*/
;*    node->cop ::cast ...                                             */
;*---------------------------------------------------------------------*/
(define-method (node->cop node::cast kont)
   (trace (cgen 3)
	  "(node->cop node::cast kont): " (shape node) #\Newline
	  "  kont: " kont #\Newline)
   (with-access::cast node (arg type loc)
      (node-args->cop (list arg)
		      #t
		      loc
		      (lambda (new-args)
			 (kont (instantiate::ccast
				  (type type)
				  (loc loc)
				  (arg (car new-args))))))))

;*---------------------------------------------------------------------*/
;*    node->cop ::setq ...                                             */
;*---------------------------------------------------------------------*/
(define-method (node->cop node::setq kont)
   (trace (cgen 3)
	  "(node->cop node::setq kont): " (shape node) #\Newline
	  "  kont: " kont #\Newline)
   (with-access::setq node (value loc)
      (let ((var (var-variable (setq-var node))))
	 (if (and (var? value) (eq? var (var-variable value)))
	     (kont (*void-kont* (instantiate::catom
				   (loc loc)
				   (value #unspecified))))
	     (node->cop value (make-setq-kont var loc kont))))))

;*---------------------------------------------------------------------*/
;*    node->cop ::conditional ...                                      */
;*---------------------------------------------------------------------*/
(define-method (node->cop node::conditional kont)
   (trace (cgen 3)
	  "(node->cop node::conditional kont): " (shape node) #\Newline
	  "  loc: " (node-loc node) #\Newline
	  "  kont: " kont #\Newline)
   (with-access::conditional node (test true false loc)
      (let* ((aux   (make-local-svar/name 'test *bool*))
	     (ctest (node->cop (node-setq aux test) *id-kont*)))
	 (if (and (csetq? ctest) (eq? (varc-variable (csetq-var ctest)) aux))
	     (instantiate::cif
		(test (csetq-value ctest))
		(true (block-kont (node->cop true kont) loc))
		(false (block-kont (node->cop false kont) loc))
		(loc   loc))
	     (instantiate::block
		(loc loc)
		(body
		 (instantiate::csequence
		    (loc loc)
		    (cops
		     (list
		      (instantiate::local-var
			 (vars (list aux))
			 (loc  loc))
		      ctest
		      (instantiate::cif
			 (test (instantiate::varc
				  (variable aux)
				  (loc loc)))
			 (false (block-kont (node->cop false kont) loc))
			 (true (block-kont (node->cop true kont) loc))
			 (loc  loc)))))))))))

;*---------------------------------------------------------------------*/
;*    node->cop ::fail ...                                             */
;*---------------------------------------------------------------------*/
(define-method (node->cop node::fail kont)
   (trace (cgen 3)
	  "(node->cop node::fail kont): " (shape node) #\Newline
	  "  kont: " kont #\Newline)
   (with-access::fail node (proc msg obj loc)
      (node-args->cop (list proc msg obj)
		      #f
		      loc
		      (lambda (new-args)
			 (*fail-kont*
			  (instantiate::cfail
			     (loc loc)
			     (proc (car new-args))
			     (msg (cadr new-args))
			     (obj (caddr new-args))))))))

;*---------------------------------------------------------------------*/
;*    node->cop ::select ...                                           */
;*---------------------------------------------------------------------*/
(define-method (node->cop node::select kont)
   (trace (cgen 3)
	  "(node->cop node::select kont): " (shape node) #\Newline
	  "  kont: " kont #\Newline)
   (with-access::select node (clauses test item-type loc)
      (for-each (lambda (clause)
		   (set-cdr! clause (node->cop (cdr clause) kont)))
		clauses)
      (let ((aux  (make-local-svar/name 'aux item-type)))
	 (let ((cop (node->cop (node-setq aux test) *id-kont*)))
	    (if (and (csetq? cop)
		     (eq? (varc-variable (csetq-var cop)) aux))
		(instantiate::cswitch
		   (loc  loc)
		   (test (csetq-value cop))
		   (clauses clauses))
		(instantiate::block
		   (loc loc)
		   (body (instantiate::csequence
			    (loc loc)
			    (cops (list (instantiate::local-var
					   (loc  loc)
					   (vars (list aux)))
					cop
					(instantiate::cswitch
					   (loc  loc)
					   (test (instantiate::varc
						    (loc loc)
						    (variable aux)))
					   (clauses clauses))))))))))))

;*---------------------------------------------------------------------*/
;*    node->cop ::let-fun ...                                          */
;*---------------------------------------------------------------------*/
(define-method (node->cop node::let-fun kont)
   (trace (cgen 3)
	  "(node->cop node::let-fun kont): " (shape node) #\Newline
	  "  kont: " kont #\Newline)
   (with-access::let-fun node (body locals loc)
      ;; local function are open-coded of their first call site.
      ;; So, the compilation of `let-fun' construction is just
      ;; the declaration of all local functions' formals (and
      ;; a initialization mark in local function to express the
      ;; need of integration of the first call site).
      (let loop ((locals      locals)
		 (all-formals '()))
	 (if (null? locals)
	     (block-kont
	      (bdb-let-var
	       (instantiate::csequence
		  (loc loc)
		  (cops (list (instantiate::local-var
				 (loc  loc)
				 (vars all-formals))
			      (node->cop body kont))))
	       loc)
	      #f)
	     (let ((local (car locals)))
		(set-variable-name! local)
		(let* ((fun (widen!::sfun/C (local-value local)
			       (label (instantiate::clabel
					 (loc  (sfun-loc (local-value local)))
					 (name (local-name local))))
			       (integrated #f)))
		       (formals (sfun-args fun)))
		   (for-each set-variable-name! formals)
		   (loop (cdr locals) (append formals all-formals))))))))

;*---------------------------------------------------------------------*/
;*    node->cop ::let-var ...                                          */
;*---------------------------------------------------------------------*/
(define-method (node->cop node::let-var kont)
   (trace (cgen 3)
	  "(node->cop node::let-var kont): " (shape node) #\Newline
	  "  kont: " kont #\Newline)
   (with-access::let-var node (body bindings loc)
      (let ((decls (instantiate::local-var
		      (loc loc)
		      (vars (map car bindings))))
	    (sets  (map (lambda (x)
			   (set-variable-name! (car x))
			   (node->cop (node-setq (car x) (cdr x)) *stop-kont*))
			bindings))
	    (body  (let ((cop (node->cop body kont)))
		      (instantiate::stop
			 (value cop)))))
	 (block-kont
	  (bdb-let-var
	   (instantiate::csequence
	      (loc loc)
	      (cops (cons decls (append sets (list body)))))
	   loc)
	  loc))))

;*---------------------------------------------------------------------*/
;*    bdb-let-var ...                                                  */
;*---------------------------------------------------------------------*/
(define (bdb-let-var cop loc)
   (if (and (>fx *bdb-debug* 0) (location? loc))
       (instantiate::bdb-block
	  (body cop)
	  (loc  loc))
       cop))
   
;*---------------------------------------------------------------------*/
;*    node->cop ::set-ex-it ...                                        */
;*---------------------------------------------------------------------*/
(define-method (node->cop node::set-ex-it kont)
   (trace (cgen 3)
	  "(node->cop node::set-ex-it kont): " (shape node) #\Newline
	  "  kont: " kont #\Newline)
   (with-access::set-ex-it node (var body loc)
      (let ((exit (var-variable var)))
	 (set-variable-name! exit)
	 (instantiate::csequence
	    (loc loc)
	    (cops (list
		   (instantiate::cpragma
		      (loc    loc)
		      (format "jmp_buf jmpbuf")
		      (args '()))
		   (instantiate::local-var
		      (loc loc)
		      (vars (list (var-variable var))))
		   (instantiate::cset-ex-it
		      (loc loc)
		      (exit (instantiate::varc
			       (loc loc)
			       (variable exit)))
		      (jump-value (node->cop (instantiate::pragma
						(loc loc)
						(type *_*)
						(format "_exit_value_")
						(expr* '()))
					     kont))
		      (body (instantiate::csequence
			       (loc loc)
			       (cops
				(list
				 (node->cop
				  (node-setq
				   exit
				   (instantiate::pragma
				      (loc loc)
				      (type *_*)
				      (format
				       (string-append
					"("
					(string-sans-$
					 (type-name (local-type exit)))
					")jmpbuf"))
				      (expr* '())))
				  *id-kont*)
				 (node->cop body kont))))))))))))

;*---------------------------------------------------------------------*/
;*    node->cop ::jump-ex-it ...                                       */
;*---------------------------------------------------------------------*/
(define-method (node->cop node::jump-ex-it kont)
   (trace (cgen 3)
	  "(node->cop node::jump-ex-it kont): " (shape node) #\Newline
	  "  kont: " kont #\Newline)
   (with-access::jump-ex-it node (exit value loc)
      (let* ((vaux  (make-local-svar/name 'aux *obj*))
	     (vcop  (node->cop (node-setq vaux value) *id-kont*))
	     (exit  exit)
	     (eaux  (make-local-svar/name 'exit *procedure*))
	     (ecop  (node->cop (node-setq eaux exit) *id-kont*)))
	 (cond
	    ((and (csetq? vcop) (eq? (varc-variable (csetq-var vcop)) vaux)
		  (csetq? ecop) (eq? (varc-variable (csetq-var ecop)) eaux))
	     (*exit-kont*
	      (instantiate::cjump-ex-it
		 (loc loc)
		 (exit (csetq-value ecop))
		 (value (csetq-value vcop)))))
	    ((and (csetq? vcop) (eq? (varc-variable (csetq-var vcop)) vaux))
	     (instantiate::block
		(loc loc)
		(body (instantiate::csequence
			 (loc loc)
			 (cops (list (instantiate::local-var
					(loc loc)
					(vars (list eaux)))
				       (instantiate::csequence
					  (loc loc)
					  (cops (list ecop)))
				       (*exit-kont*
					(instantiate::cjump-ex-it
					   (loc loc)
					   (exit (instantiate::varc
						    (loc loc)
						    (variable eaux)))
					   (value (csetq-value vcop))))))))))
	    ((and (csetq? ecop) (eq? (varc-variable (csetq-var ecop)) eaux))
	     (instantiate::block
		(loc loc)
		(body (instantiate::csequence
			 (loc loc)
			 (cops (list (instantiate::local-var
					(loc loc)
					(vars (list vaux)))
				     (instantiate::csequence
					(loc loc)
					(cops (list vcop)))
				     (*exit-kont*
				      (instantiate::cjump-ex-it
					 (loc loc)
					 (exit (csetq-value ecop))
					 (value (instantiate::varc
						   (loc loc)
						   (variable vaux)))))))))))
	    (else
	     (instantiate::block
		(loc loc)
		(body (instantiate::csequence
			 (loc loc)
			 (cops
			  (list
			   (instantiate::local-var
			      (loc loc)
			      (vars (list eaux vaux)))
			   (instantiate::csequence
			      (loc loc)
			      (cops (list ecop vcop)))
			   (*exit-kont*
			    (instantiate::cjump-ex-it
			       (loc loc)
			       (exit (instantiate::varc
					(variable eaux)))
			       (value (instantiate::varc
					 (variable vaux)))))))))))))))

;*---------------------------------------------------------------------*/
;*    node->cop ::make-box ...                                         */
;*---------------------------------------------------------------------*/
(define-method (node->cop node::make-box kont)
   (trace (cgen 3)
	  "(node->cop node::make-box kont): " (shape node) #\Newline
	  "  kont: " kont #\Newline)
   (with-access::make-box node (value loc)
      (if (or (var? value) (atom? value) (kwote? value))
	  (node->cop value
		     (lambda (v) (kont (instantiate::cmake-box
					  (loc loc)
					  (value v)))))
	  (let* ((aux  (make-local-svar/name 'cellval *obj*))
		 (cval (node->cop (node-setq aux value) *id-kont*)))
	     (instantiate::block
		(loc loc)
		(body (instantiate::csequence
			 (loc loc)
			 (cops (list
				(instantiate::local-var
				   (loc loc)
				   (vars (list aux)))
				cval
				(kont
				 (instantiate::cmake-box
				    (loc loc)
				    (value (instantiate::varc
					      (loc loc)
					      (variable aux))))))))))))))

;*---------------------------------------------------------------------*/
;*    node->cop ::box-ref ...                                          */
;*---------------------------------------------------------------------*/
(define-method (node->cop node::box-ref kont)
   (trace (cgen 3)
	  "(node->cop node::box-ref kont): " (shape node) #\Newline
	  "  kont: " kont #\Newline)
   (with-access::box-ref node (var loc)
      (kont (node->cop var (lambda (v) (instantiate::cbox-ref
					  (loc loc)
					  (var v)))))))

;*---------------------------------------------------------------------*/
;*    node->cop ::box-set! ...                                         */
;*---------------------------------------------------------------------*/
(define-method (node->cop node::box-set! kont)
   (trace (cgen 3)
	  "(node->cop node::box-set! kont): " (shape node) #\Newline
	  "  kont: " kont #\Newline)
   (with-access::box-set! node (var value loc)
      (let ((v (var-variable var)))
	 (node->cop value
		    (lambda (vl) (kont (instantiate::cbox-set!
					  (loc loc)
					  (var (instantiate::varc
						  (loc loc)
						  (variable v)))
					  (value vl))))))))

;*---------------------------------------------------------------------*/
;*    node-setq ...                                                    */
;*---------------------------------------------------------------------*/
(define (node-setq::setq variable::variable value::node)
   (instantiate::setq
      (loc (node-loc value))
      (type *unspec*)
      (var (instantiate::var
	      (loc #f)
	      (type (variable-type variable))
	      (variable variable)))
      (value value)))

;*---------------------------------------------------------------------*/
;*    make-local-svar/name ...                                         */
;*---------------------------------------------------------------------*/
(define (make-local-svar/name::local id::symbol type::type)
   (let ((local (make-local-svar id type)))
      (if (not (string? (local-name local)))
	  (error "make-local-svar/name" "Illegal local name" local))
      local))

;*---------------------------------------------------------------------*/
;*    no-bdb-newline ...                                               */
;*    -------------------------------------------------------------    */
;*    Emit a newline only if not compiling for bdb.                    */
;*---------------------------------------------------------------------*/
(define (no-bdb-newline)
   (if (eq? *bdb-debug* 0)
       (newline *c-port*)))

;*---------------------------------------------------------------------*/
;*    node-args->cop ...                                               */
;*---------------------------------------------------------------------*/
(define (node-args->cop args args-safe loc kont)
   (let loop ((old-actuals  args)
	      (new-actuals  '())
	      (aux          (make-local-svar/name 'aux *obj*))
	      (auxs         '())
	      (exps         '()))
      (if (null? old-actuals)
	  (if (null? auxs)
	      (kont (reverse! new-actuals))
	      (instantiate::block
		 (body (instantiate::csequence
			  (loc loc)
			  (cops (list
				 (instantiate::local-var
				    (vars auxs)
				    (loc  loc))
				 (instantiate::csequence (cops exps))
				 (kont (reverse! new-actuals))))))))
	  (let ((cop (node->cop (node-setq aux (car old-actuals)) *id-kont*)))
	     (if (and (csetq? cop)
		      (eq? (varc-variable (csetq-var cop)) aux)
		      (or args-safe
			  (catom? (csetq-value cop))
			  (varc? (csetq-value cop))
			  (cpragma? (csetq-value cop))))
		 (loop (cdr old-actuals)
		       (cons (csetq-value cop) new-actuals)
		       aux
		       auxs
		       exps)
		 (begin
		    (local-type-set! aux (typeof (car old-actuals)))
		    (loop (cdr old-actuals)
			  (cons (instantiate::varc
				   (variable aux)
				   (loc      loc))
				new-actuals)
			  (make-local-svar/name 'aux *obj*)
			  (cons aux auxs)
			  (cons cop exps))))))))
   
   
