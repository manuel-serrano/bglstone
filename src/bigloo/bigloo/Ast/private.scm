;*=====================================================================*/
;*    .../prgm/project/bglstone/src/bigloo/bigloo/Ast/private.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jul 13 14:11:36 2000                          */
;*    Last change :  Thu Mar  6 15:59:35 2025 (serrano)                */
;*    Copyright   :  2000-25 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Private constructino of the AST.                                 */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module ast_private
   (include "Ast/node.sch")
   (import engine_param
	   ast_node
	   type_type
	   type_cache
	   type_env
	   ast_sexp
	   ast_var)
   (export (private->node ::pair ::obj ::obj ::symbol)
	   (private-sexp?::bool ::pair)
	   (make-private-sexp::pair ::symbol ::symbol . objs)))

;*---------------------------------------------------------------------*/
;*    *private-stamp* ...                                              */
;*    -------------------------------------------------------------    */
;*    This is a constant. It cannot be changed without bootstrapping   */
;*    because the heap refers to it.                                   */
;*---------------------------------------------------------------------*/
(define *private-stamp* '___bgl_private_stamp_mark)

;*---------------------------------------------------------------------*/
;*    private-sexp? ...                                                */
;*---------------------------------------------------------------------*/
(define (private-sexp?::bool sexp)
   (eq? (car sexp) *private-stamp*))

;*---------------------------------------------------------------------*/
;*    private->node ...                                                */
;*---------------------------------------------------------------------*/
(define (private->node sexp::pair stack loc site)
   (define (bigloodemangle f)
      (if (bigloo-mangled? f)
	  (bigloo-demangle f)
	  f ))
   (match-case sexp
      ((?- getfield ?ftype ?otype ?field-name ?obj)
       (let ((ftype (use-type! ftype loc))
	     (otype (use-type! otype loc))
	     (tid (symbol-append otype '-
				 (string->symbol
				  (bigloodemangle field-name)))))
	  (instantiate::getfield
	     (loc loc)
	     (type ftype)
	     (otype otype)
	     (fname field-name)
	     (ftype ftype)
	     (side-effect? #f)
	     (expr* (list (sexp->node obj stack loc site)))
	     (effect (instantiate::feffect
			(read (list tid)))))))
      ((?- setfield ?ftype ?otype ?field-name . ?rest)
       (let ((otype (use-type! otype loc))
	     (ftype (use-type! ftype loc))
	     (tid (symbol-append otype '-
				 (string->symbol
				  (bigloodemangle field-name)))))
	  (instantiate::setfield
	     (loc loc)
	     (type *unspec*)
	     (otype otype)
	     (fname field-name)
	     (ftype ftype)
	     (side-effect? #t)
	     (expr* (sexp*->node rest stack loc site))
	     (effect (instantiate::feffect
			(write (list tid)))))))
      ((?- new ?type)
       (if *saw*
	   (instantiate::new
	      (loc loc)
	      (type (use-type! type loc))
	      (side-effect? #t))
	   (instantiate::new
	      (loc loc)
	      (type (use-type! type loc))
	      (side-effect? #t))))
      ((?- new ?type (quote ?args-type) . ?rest)
       (if *saw*
	   (if (null? rest)
	       ;; not an external class
	       (instantiate::new
		  (loc loc)
		  (type (use-type! type loc))
		  (args-type (map (lambda (t) (use-type! t loc)) args-type))
		  (side-effect? #t))
	       (instantiate::new
		  (loc loc)
		  (type (use-type! type loc))
		  (args-type (map (lambda (t) (use-type! t loc)) args-type))
		  (expr* (if (null? rest)
			     '()
			     (sexp*->node rest stack loc site)))
		  (side-effect? #t)))
	   (instantiate::new
	      (loc loc)
	      (type (use-type! type loc))
	      (args-type (map (lambda (t) (use-type! t loc)) args-type))
	      (side-effect? #t))))
      ((?- cast ?type ?exp)
       (instantiate::cast
	  (loc loc)
	  (type (use-type! type loc))
	  (arg (sexp->node exp stack loc site))))
      ((?- cast-null ?type)
       (instantiate::cast-null
	  (loc loc)
	  (type (use-type! type loc))))
      ((?- isa ?type ?exp)
       (instantiate::isa
	  (loc loc)
	  (type *bool*)
	  (class (use-type! type loc))
	  (expr* (list (sexp->node exp stack loc site)))
	  (effect (instantiate::feffect))))
      ((?- vlength ?vtype ?ftype ?otype (and (? string?) ?c-fmt) ?exp)
       (let ((vtype (use-type! vtype loc))
	     (otype (use-type! otype loc))
	     (ftype (use-type! ftype loc)))
	  (instantiate::vlength
	     (loc loc)
	     (type otype)
	     (vtype vtype)
	     (c-format c-fmt)
	     (expr* (list (sexp->node exp stack loc site)))
	     (effect (instantiate::feffect)))))
      ((?- vref ?vtype ?ftype ?otype (and (? string?) ?c-fmt) . ?rest)
       (let ((ftype (use-type! ftype loc))
	     (vtype (use-type! vtype loc))
	     (otype (use-type! otype loc)))
	  (instantiate::vref
	     (loc loc)
	     (type ftype)
	     (ftype ftype)
	     (otype otype)
	     (vtype vtype)
	     (c-format c-fmt)
	     (expr* (sexp*->node rest stack loc site))
	     (effect (instantiate::feffect
			(read (list (type-id ftype))))))))
      ((?- vset! ?vtype ?ftype ?otype (and (? string?) ?c-fmt) . ?rest)
       (let ((ftype (use-type! ftype loc))
	     (vtype (use-type! vtype loc))
	     (otype (use-type! otype loc)))
	  (instantiate::vset!
	     (loc loc)
	     (type *unspec*)
	     (ftype ftype)
	     (otype otype)
	     (vtype vtype)
	     (c-format c-fmt)
	     (expr* (sexp*->node rest stack loc site))
	     (effect (instantiate::feffect
			(write (list (type-id ftype))))))))
      ((?- valloc ?vtype ?ftype ?otype
	   (and (? string?) ?c-heap-fmt)
	   (and (? string?) ?c-stack-fmt)
	   (and (? boolean?) ?stack?) . ?rest)
       (let ((ftype (use-type! ftype loc))
	     (vtype (use-type! vtype loc))
	     (otype (use-type! otype loc)))
	  (instantiate::valloc
	     (loc loc)
	     (type vtype)
	     (ftype ftype)
	     (otype otype)
	     (c-heap-format c-heap-fmt)
	     (expr* (sexp*->node rest stack loc site)))))
      (else
       (if (pair? (cdr sexp))
	   (error "private->node" "Illegal private kind" (cadr sexp))
	   (error "private->node" "Illegal private kind" sexp)))))

;*---------------------------------------------------------------------*/
;*    make-private-sexp ...                                            */
;*    -------------------------------------------------------------    */
;*    Build an private sexp. That is a sexp that can be processed by   */
;*    SEXP->NODE but that is not accessible from the user source code. */
;*    -------------------------------------------------------------    */
;*    the private stamp can't be gensymed because it has to traverse   */
;*    heap files.                                                      */
;*---------------------------------------------------------------------*/
(define (make-private-sexp::pair kind::symbol type-id::symbol . objs)
   [assert (kind) (memq kind '(getfield setfield new cast cast-null isa
		 			vlength vref vset! valloc))]
   (cons* *private-stamp* kind type-id objs))

