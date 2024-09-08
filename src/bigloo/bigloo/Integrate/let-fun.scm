;*=====================================================================*/
;*    .../prgm/project/bigloo2.3/comptime/Integrate/let-fun.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Mar 16 09:38:46 1995                          */
;*    Last change :  Thu Jul 13 11:09:38 2000 (serrano)                */
;*    Copyright   :  1995-2000 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    This module implements a function which remove displaced         */
;*    local functions and which adds the integrated ones.              */
;*=====================================================================*/
 
;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module integrate_let-fun
   (include "Tools/trace.sch")
   (import  tools_shape
	    tools_error
	    type_type
	    type_cache
	    ast_var
	    ast_node
	    integrate_info)
   (export  (displace-let-fun! <variable>)))

;*---------------------------------------------------------------------*/
;*    *stamp* ...                                                      */
;*---------------------------------------------------------------------*/
(define *stamp* 0)

;*---------------------------------------------------------------------*/
;*    bind-fun! ...                                                    */
;*---------------------------------------------------------------------*/
(define (bind-fun! var)
   (trace (integrate 4) "bind-fun!(" (shape var) ", " *stamp* ")" #\Newline)
   (if (local? var)
       (sfun/Iinfo-istamp-set! (local-value var) *stamp*)))

;*---------------------------------------------------------------------*/
;*    free-fun? ...                                                    */
;*---------------------------------------------------------------------*/
(define (free-fun? local)
   (trace (integrate 4) "free-fun?( " (shape local) ", " *stamp* ")" #\Newline)
   (not (eq? (sfun/Iinfo-istamp (local-value local)) *stamp*)))

;*---------------------------------------------------------------------*/
;*    displace-let-fun! ...                                            */
;*---------------------------------------------------------------------*/
(define (displace-let-fun! var)
   (trace (integrate 2)
	  "   displace-let-fun!: " (shape var) #\Newline
	  "                 Led: " (shape (sfun/Iinfo-Led
					   (variable-value var)))
	  #\Newline)
   (set! *stamp* (+fx 1 *stamp*))
   (displace-let-fun-node! (sfun-body (variable-value var)) var)
   ;; we scan all local functions in order to remove from addition
   ;; functions nested in already integrated functions. In the
   ;; following example we don't integrate f3.
   ;;   (define (foo x)
   ;;      (labels ((f1 (a) (labels ((f2 (b) (f2 b)))
   ;;                          (f2 a))))
   ;;         (labels ((f3 (c) (f3 (f3 (f3 (f1 c))))))
   ;;            (f3 4))))
   (let ((Led (sfun/Iinfo-Led (variable-value var))))
      (for-each (lambda (l)
		   (trace (integrate 3)
			  "  free-fun?( " (shape l) "): "
			  (free-fun? l)
			  #\Newline)
		   (if (free-fun? l)
		       ;; we _absolutely can't_ mark function as seen
		       ;; otherwise all this computation will be wrong
		       ;; but we can test if the function is nested.
		       ;; If the function is, the predicate `free-fun?'
		       ;; will be false.
		       (displace-let-fun-node! (sfun-body (variable-value l))
					       var)))
		Led)
      (let loop ((Led   Led)
		 (added '()))
	 (cond
	    ((null? Led)
	     ;; we set the new body.
	     (if (pair? added)
		 (let* ((old-body (sfun-body (variable-value var)))
			(new-body (instantiate::let-fun
				     (loc (node-loc old-body))
				     (type *_*)
				     (locals added)
				     (body old-body))))
		    (trace (integrate 2)
			   "    j'ajoute les fonctions: " (shape added)
			   #\Newline)
		    (sfun-body-set! (variable-value var) new-body)
		    new-body)))
	    ((free-fun? (car Led))
	     ;; we have to add this local function.
	     (loop (cdr Led) (cons (car Led) added)))
	    (else
	     ;; this function is already defined in var
	     (loop (cdr Led) added))))))

;*---------------------------------------------------------------------*/
;*    displace-let-fun-node! ...                                       */
;*    -------------------------------------------------------------    */
;*    This function only modify the list of the `let-fun'              */
;*    constructions. So, we don't need to perform mutation             */
;*    during all the pass. We just realize side-effects when           */
;*    managing a `let-fun' node.                                       */
;*---------------------------------------------------------------------*/
(define-generic (displace-let-fun-node! node::node variable::variable))

;*---------------------------------------------------------------------*/
;*    displace-let-fun-node! ::atom ...                                */
;*---------------------------------------------------------------------*/
(define-method (displace-let-fun-node! node::atom host)
   #unspecified)

;*---------------------------------------------------------------------*/
;*    displace-let-fun-node! ::kwote ...                               */
;*---------------------------------------------------------------------*/
(define-method (displace-let-fun-node! node::kwote host)
   #unspecified)

;*---------------------------------------------------------------------*/
;*    displace-let-fun-node! ::var ...                                 */
;*---------------------------------------------------------------------*/
(define-method (displace-let-fun-node! node::var host)
   #unspecified)

;*---------------------------------------------------------------------*/
;*    displace-let-fun-node! ::closure ...                             */
;*---------------------------------------------------------------------*/
(define-method (displace-let-fun-node! node::closure host)
   (internal-error "displace-let-fun-node" "Unexpected closure" (shape node)))

;*---------------------------------------------------------------------*/
;*    displace-let-fun-node! ::sequence ...                            */
;*---------------------------------------------------------------------*/
(define-method (displace-let-fun-node! node::sequence host)
   (with-access::sequence node (nodes)
      (for-each (lambda (node) (displace-let-fun-node! node host)) nodes)))

;*---------------------------------------------------------------------*/
;*    displace-let-fun-node! ::app ...                                 */
;*---------------------------------------------------------------------*/
(define-method (displace-let-fun-node! node::app host)
   (let liip ((args (app-args node)))
      (if (null? args)
	  #unspecified
	  (begin
	     (displace-let-fun-node! (car args) host)
	     (liip (cdr args))))))

;*---------------------------------------------------------------------*/
;*    displace-let-fun-node! ::app-ly ...                              */
;*---------------------------------------------------------------------*/
(define-method (displace-let-fun-node! node::app-ly host)
   (with-access::app-ly node (fun arg)
      (displace-let-fun-node! fun host)
      (displace-let-fun-node! arg host)))

;*---------------------------------------------------------------------*/
;*    displace-let-fun-node! ::funcall ...                             */
;*---------------------------------------------------------------------*/
(define-method (displace-let-fun-node! node::funcall host)
   (with-access::funcall node (fun args)
      (let liip ((asts args))
	 (if (null? asts)
	     (displace-let-fun-node! fun host)
	     (begin
		(displace-let-fun-node! (car asts) host)
		(liip (cdr asts)))))))

;*---------------------------------------------------------------------*/
;*    displace-let-fun-node! ::extern ...                              */
;*---------------------------------------------------------------------*/
(define-method (displace-let-fun-node! node::extern host)
   (with-access::extern node (expr*)
      (for-each (lambda (node) (displace-let-fun-node! node host)) expr*)))

;*---------------------------------------------------------------------*/
;*    displace-let-fun-node! ::cast ...                                */
;*---------------------------------------------------------------------*/
(define-method (displace-let-fun-node! node::cast host)
   (with-access::cast node (arg)
      (displace-let-fun-node! arg host)))

;*---------------------------------------------------------------------*/
;*    displace-let-fun-node! ::setq ...                                */
;*---------------------------------------------------------------------*/
(define-method (displace-let-fun-node! node::setq host)
   (with-access::setq node (var value)
      (displace-let-fun-node! value host)))

;*---------------------------------------------------------------------*/
;*    displace-let-fun-node! ::conditional ...                         */
;*---------------------------------------------------------------------*/
(define-method (displace-let-fun-node! node::conditional host)
   (with-access::conditional node (test true false)
      (displace-let-fun-node! test host)
      (displace-let-fun-node! true host)
      (displace-let-fun-node! false host)))

;*---------------------------------------------------------------------*/
;*    displace-let-fun-node! ::fail ...                                */
;*---------------------------------------------------------------------*/
(define-method (displace-let-fun-node! node::fail host)
   (with-access::fail node (proc msg obj)
      (displace-let-fun-node! proc host)
      (displace-let-fun-node! msg host)
      (displace-let-fun-node! obj host)))

;*---------------------------------------------------------------------*/
;*    displace-let-fun-node! ::select ...                              */
;*---------------------------------------------------------------------*/
(define-method (displace-let-fun-node! node::select host)
   (with-access::select node (test)
      (let liip ((clauses (select-clauses node)))
	 (if (null? clauses)
	     (displace-let-fun-node! test host)
	     (begin
		(displace-let-fun-node! (cdr (car clauses)) host)
		(liip (cdr clauses)))))))

;*---------------------------------------------------------------------*/
;*    displace-let-fun-node! ::let-fun ...                             */
;*---------------------------------------------------------------------*/
(define-method (displace-let-fun-node! node::let-fun host)
   (trace (integrate 4)
	  "displace-let-fun-node(let-fun): " (shape node) #\Newline)
   (with-access::let-fun node (body)
      (let liip ((old (let-fun-locals node))
		 (new '()))
	 (if (not (null? old))
	     (trace (globalize 4)
		    "display-let-fun-node!(let-fun):"
		    (shape (car old)) "): "
		    "  host: " (shape host)
		    "  Iinfo-L: "
		    (shape (sfun/Iinfo-L (local-value (car old))))
		    #\Newline))
	 (cond
	    ((null? old)
	     (let-fun-locals-set! node new)
	     (displace-let-fun-node! body host))
	    ((eq? (sfun/Iinfo-L (local-value (car old))) host)
	     (let ((l (car old)))
		(bind-fun! l)
		(displace-let-fun-node! (sfun-body (local-value l)) host)
		(liip (cdr old) (cons l new))))
	    (else
	     (liip (cdr old) new))))))

;*---------------------------------------------------------------------*/
;*    displace-let-fun-node! ::let-var ...                             */
;*---------------------------------------------------------------------*/
(define-method (displace-let-fun-node! node::let-var host)
   (with-access::let-var node (body)
      (let liip ((bindings (let-var-bindings node)))
	 (if (null? bindings)
	     (displace-let-fun-node! body host)
	     (let* ((binding (car bindings))
		    (var (car binding))
		    (val (cdr binding)))
		(displace-let-fun-node! val host)
		(liip (cdr bindings)))))))

;*---------------------------------------------------------------------*/
;*    displace-let-fun-node! ::set-ex-it ...                           */
;*---------------------------------------------------------------------*/
(define-method (displace-let-fun-node! node::set-ex-it host)
   (with-access::set-ex-it node (body)
      (displace-let-fun-node! body host)))

;*---------------------------------------------------------------------*/
;*    displace-let-fun-node! ::jump-ex-it ...                          */
;*---------------------------------------------------------------------*/
(define-method (displace-let-fun-node! node::jump-ex-it host)
   (with-access::jump-ex-it node (exit value)
      (displace-let-fun-node! exit host)
      (displace-let-fun-node! value host)))

;*---------------------------------------------------------------------*/
;*    displace-let-fun-node! ::make-box ...                            */
;*---------------------------------------------------------------------*/
(define-method (displace-let-fun-node! node::make-box host)
   (with-access::make-box node (value)
      (displace-let-fun-node! value host)))

;*---------------------------------------------------------------------*/
;*    displace-let-fun-node! ::box-set! ...                            */
;*---------------------------------------------------------------------*/
(define-method (displace-let-fun-node! node::box-set! host)
   #unspecified)

;*---------------------------------------------------------------------*/
;*    displace-let-fun-node! ::box-ref ...                             */
;*---------------------------------------------------------------------*/
(define-method (displace-let-fun-node! node::box-ref host)
   #unspecified)

