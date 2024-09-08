;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Cgen/capp.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Jul  3 07:50:47 1996                          */
;*    Last change :  Wed Apr 14 12:16:34 2004 (serrano)                */
;*    Copyright   :  1996-2004 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The C production for application (apply, funcall, app) nodes.    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module cgen_capp
   (include "Tools/trace.sch")
   (import  tools_error
	    tools_shape
	    type_type
	    type_tools
	    type_cache
	    ast_var
	    ast_node
	    ast_local
	    effect_effect
	    backend_c_emit
	    cgen_cop
	    cgen_cgen))

;*---------------------------------------------------------------------*/
;*    node->cop ::app-ly ...                                           */
;*---------------------------------------------------------------------*/
(define-method (node->cop node::app-ly kont)
   (trace (cgen 3) "(node->cop node::app-ly kont): " (shape node) #\Newline
	  "  kont: " kont #\Newline)
   (with-access::app-ly node (fun arg loc)
      (let* ((value arg)
	     (vaux  (make-local-svar/name 'aux *obj*))
	     (vcop  (node->cop (node-setq vaux value) *id-kont*))
	     (fun   fun)
	     (faux  (make-local-svar/name 'fun *procedure*))
	     (fcop  (node->cop (node-setq faux fun) *id-kont*)))
	 (cond
	    ((and (csetq? vcop) (eq? (varc-variable (csetq-var vcop)) vaux)
		  (csetq? fcop) (eq? (varc-variable (csetq-var fcop)) faux))
	     (kont (instantiate::capply
		      (loc loc)
		      (fun (csetq-value fcop))
		      (arg (csetq-value vcop)))))
	    ((and (csetq? vcop) (eq? (varc-variable (csetq-var vcop)) vaux))
	     (instantiate::block
		(loc  loc)
		(body (instantiate::csequence
			 (loc loc)
			 (cops
			  (list
			   (instantiate::local-var
			      (loc loc)
			      (vars (list faux)))
			   (instantiate::csequence
			      (loc loc)
			      (cops (list fcop)))
			   (kont (instantiate::capply
				    (loc loc)
				    (fun (instantiate::varc
					    (loc loc)
					    (variable faux)))
				    (arg (csetq-value vcop))))))))))
	    ((and (csetq? fcop) (eq? (varc-variable (csetq-var fcop)) faux))
	     (instantiate::block
		(loc loc)
		(body (instantiate::csequence
			 (loc loc)
			 (cops
			  (list
			   (instantiate::local-var
			      (loc loc)
			      (vars (list vaux)))
			   (instantiate::csequence
			      (loc loc)
			      (cops (list vcop)))
			   (kont (instantiate::capply
				    (loc loc)
				    (fun (csetq-value fcop))
				    (arg (instantiate::varc
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
			      (vars (list faux vaux)))
			   (instantiate::csequence
			      (loc loc)
			      (cops (list fcop vcop)))
			   (kont (instantiate::capply
				    (loc loc)
				    (fun (instantiate::varc
					    (loc loc)
					    (variable faux)))
				    (arg (instantiate::varc
					    (loc loc)
					    (variable vaux)))))))))))))))

;*---------------------------------------------------------------------*/
;*    node->cop ::funcall ...                                          */
;*---------------------------------------------------------------------*/
(define-method (node->cop node::funcall kont)
   (trace (cgen 3) "(node->cop node::funcall kont): " (shape node) #\Newline
	  "  kont: " kont #\Newline)
   (with-access::funcall node (fun args strength loc)
      (let loop ((old-actuals  args)
		 (new-actuals  '())
		 (aux          (make-local-svar/name 'aux *obj*))
		 (auxs         '())
		 (exps         '()))
	 (if (null? old-actuals)
	     (let* ((aux (make-local-svar/name 'aux *obj*))
		    (cop (node->cop (node-setq aux fun) *id-kont*)))
		(if (and (csetq? cop)
			 (var? fun)
			 (eq? (varc-variable (csetq-var cop)) aux))
		    (let ((cfun (csetq-value cop)))
		       (if (null? auxs)
			   (kont (instantiate::cfuncall
				    (loc loc)
				    (fun cfun)
				    (args (reverse! new-actuals))
				    (strength strength)))
			   (instantiate::block
			      (loc  loc)
			      (body (instantiate::csequence
				       (loc loc)
				       (cops
					(list
					 (instantiate::local-var
					    (loc loc)
					    (vars auxs))
					 (instantiate::csequence
					    (loc loc)
					    (cops exps))
					 (kont (instantiate::cfuncall
						  (loc loc)
						  (fun cfun)
						  (args (reverse! new-actuals))
						  (strength strength))))))))))
		    (let ((cfun cop))
		       (instantiate::block
			  (loc  loc)
			  (body (instantiate::csequence
				   (loc loc)
				   (cops
				    (list
				     (instantiate::local-var
					(loc  loc)
					(vars (cons aux auxs)))
				     (instantiate::csequence
					(loc loc)
					(cops (cons cfun exps)))
				     (kont (instantiate::cfuncall
					      (loc loc)
					      (fun (instantiate::varc
						      (loc loc)
						      (variable aux)))
					      (args (reverse! new-actuals))
					      (strength strength)))))))))))
	     (let ((cop (node->cop (node-setq aux (car old-actuals))
				   *id-kont*)))
		(if (and (csetq? cop)
			 (eq? (varc-variable (csetq-var cop)) aux))
		    ;; the local is useless, we ignore it
		    (loop (cdr old-actuals)
			  (cons (csetq-value cop) new-actuals)
			  aux
			  auxs
			  exps)
		    (begin
		       (loop (cdr old-actuals)
			     (cons (instantiate::varc
				      (loc (node-loc (car old-actuals)))
				      (variable aux))
				   new-actuals)
			     (make-local-svar/name 'aux *obj*)
			     (cons aux auxs)
			     (cons cop exps)))))))))
		
;*---------------------------------------------------------------------*/
;*    node->cop ::app ...                                              */
;*---------------------------------------------------------------------*/
(define-method (node->cop node::app kont)
   (trace (cgen 3) "(node->cop node::app kont): " (shape node) #\Newline
	  "  kont: " kont #\Newline)
   (with-access::app node (fun)
      (let ((var (var-variable fun)))
	 (if (and (global? var)
		  (or (not (eq? var *the-global*))
		      (not (eq? kont *return-kont*)))) 
	     (node-non-tail-app->cop var node kont)
	     (node-tail-app->cop var node kont)))))

;*---------------------------------------------------------------------*/
;*    node-non-tail-app->cop ...                                       */
;*---------------------------------------------------------------------*/
(define (node-non-tail-app->cop var node kont)
   (trace (cgen 2) "node-non-tail-app->cop: "
	  (shape var) " " 
	  (shape node) " "
	  "kont: " kont
	  #\Newline)
   (if (sfun? (variable-value var))
       (node-sfun-non-tail-app->cop var node kont)
       (node-cfun-non-tail-app->cop var node kont)))

;*---------------------------------------------------------------------*/
;*    node-sfun-non-tail-app->cop ...                                  */
;*---------------------------------------------------------------------*/
(define (node-sfun-non-tail-app->cop var node kont)
   (let* ((args      (sfun-args (variable-value var)))
	  (args-type (cond
			((null? args)
			 '())
			((local? (car args))
			 (map local-type args))
			(else
			 args)))
	  (useless?  (lambda (cop aux)
			(and (csetq? cop)
			     (eq? (varc-variable (csetq-var cop)) aux)))))
      (let loop ((old-actuals  (app-args node))
		 (args-type    args-type)
		 (new-actuals  '())
		 (aux          (make-local-svar/name 'aux *obj*))
		 (auxs         '())
		 (exps         '()))
	 (if (null? old-actuals)
	     (if (null? auxs)
		 (kont (instantiate::capp
			  (loc (node-loc node))
			  (fun (node->cop (app-fun node) *id-kont*))
			  (args (reverse! new-actuals))))
		 ;; when this function call uses arguments we have to take
		 ;; care where to emit soruce line information. We have to
		 ;; do it at the beginning of the lexical block that will bind
		 ;; the actual parameter and that's it. nothing more.
		 (let ((loc (app-loc node)))
		    (instantiate::block
		       (loc  loc)
		       (body (instantiate::csequence
				(loc loc)
				(cops
				 (list
				  (instantiate::local-var
				     (loc  loc)
				     (vars auxs))
				  (instantiate::csequence
				     (loc loc)
				     (cops exps))
				  (kont (instantiate::capp
					   (loc loc)
					   (fun (node->cop (app-fun node)
							   *id-kont*))
					   (args (reverse! new-actuals)))))))))))
	     (let ((cop (node->cop (node-setq aux (car old-actuals)) *id-kont*)))
		(if (useless? cop aux)
		    (loop (cdr old-actuals)
			  (cdr args-type)
			  (cons (csetq-value cop) new-actuals)
			  aux
			  auxs
			  exps)
		    (begin
		       (local-type-set! aux (car args-type))
		       (loop (cdr old-actuals)
			     (cdr args-type)
			     (cons (instantiate::varc
				      (loc (node-loc (car old-actuals)))
				      (variable aux))
				   new-actuals)
			     (make-local-svar/name 'aux (car args-type))
			     (cons aux auxs)
			     (cons cop exps)))))))))

;*---------------------------------------------------------------------*/
;*    node-cfun-non-tail-app->cop ...                                  */
;*    -------------------------------------------------------------    */
;*    cfun differ from sfun because we have to take care to the        */
;*    n-ary functions.                                                 */
;*---------------------------------------------------------------------*/
(define (node-cfun-non-tail-app->cop var node kont)
   (let ((args-type (cfun-args-type (variable-value var)))
	 (useless? (lambda (cop aux)
		      (and (csetq? cop)
			   (eq? (varc-variable (csetq-var cop)) aux)
			   (or (global-args-safe? var)
			       (catom? (csetq-value cop))
			       (varc? (csetq-value cop)))))))
      (let loop ((old-actuals  (app-args node))
		 (args-type    args-type)
		 (new-actuals  '())
		 (aux          (make-local-svar/name 'aux *obj*))
		 (auxs         '())
		 (exps         '()))
	 (if (null? old-actuals)
	     (if (null? auxs)
		 (kont (instantiate::capp
			  (loc (node-loc node))
			  (fun (node->cop (app-fun node) *id-kont*))
			  (args (reverse! new-actuals))))
		 ;; when this function call uses arguments we have to take
		 ;; care where to emit source line information. We have to
		 ;; do it at the beginning of the lexical block that will bind
		 ;; the actual parameter and that's it. nothing more.
		 (let ((loc (app-loc node)))
		    (instantiate::block
		       (loc  loc)
		       (body (instantiate::csequence
				(loc loc)
				(cops
				 (list
		 		  (instantiate::local-var
				     (loc  loc)
				     (vars auxs))
				  (instantiate::csequence
				     (loc loc)
				     (cops exps))
				  (kont (instantiate::capp
					   (loc loc)
					   (fun (node->cop (app-fun node)
							   *id-kont*))
					   (args (reverse! new-actuals)))))))))))
	     (let ((cop (node->cop (node-setq aux (car old-actuals)) *id-kont*)))
		(if (useless? cop aux)
		    (loop (cdr old-actuals)
			  (if (null? (cdr args-type))
			      args-type
			      (cdr args-type))
			  (cons (csetq-value cop) new-actuals)
			  aux
			  auxs
			  exps)
		    (begin
		       (local-type-set! aux (car args-type))
		       (loop (cdr old-actuals)
			     (if (null? (cdr args-type))
				 args-type
				 (cdr args-type))
			     (cons (instantiate::varc
				      (loc (node-loc (car old-actuals)))
				      (variable aux))
				   new-actuals)
			     (make-local-svar/name 'aux (car args-type))
			     (cons aux auxs)
			     (cons cop exps)))))))))

;*---------------------------------------------------------------------*/
;*    node-tail-app->cop ...                                           */
;*    -------------------------------------------------------------    */
;*    For local functions, the first time we see it, we have           */
;*    to expand their body. So we check if the function is             */
;*    already expanded, then we jump to the definition. Otherwise      */
;*    we expand the body and don't produce jump.                       */
;*---------------------------------------------------------------------*/
(define (node-tail-app->cop var node kont)
   (let ((label (sfun/C-label (variable-value var)))
	 (args  (sfun-args (variable-value var)))
	 (loc   (node-loc node)))
      (if (not (sfun/C-integrated (variable-value var)))
	  (begin
	     (sfun/C-integrated-set! (variable-value var) #t)
	     (let ((body (node->cop (sfun-body (local-value var)) kont)))
		(clabel-body-set! label body)
		(if (null? args)
		    label
		    (let loop ((formals args)
			       (actuals (app-args node))
			       (seq     '()))
		       (if (null? formals)
			   (instantiate::csequence
			      (loc loc)
			      (cops (reverse! (cons label seq))))
			   (loop (cdr formals)
				 (cdr actuals)
				 (cons (node->cop (node-setq (car formals)
							     (car actuals))
						  *stop-kont*)
				       seq)))))))
	  ;; before branching, we create local variable to hold
	  ;; new formal cops.
	  (if (null? args)
	      (begin
		 (clabel-used?-set! label #t)
		 (instantiate::cgoto
		    (loc   loc)
		    (label label)))
	      (let loop ((args    args) 
			 (actuals (app-args node))
			 (auxs    '())
			 (seq1    '())
			 (seq2    (list (begin
					   (clabel-used?-set! label #t)
					   (instantiate::cgoto
					      (loc   loc)
					      (label label))))))
		 (if (null? args)
		     (begin
			(if (null? seq1)
			    (set! seq1 seq2)
			    (begin
			       (set! seq1 (reverse! seq1))
			       (set-cdr! (last-pair seq1) seq2)))
			(block-kont
			 (bdb-let-var
			  (instantiate::csequence
			     (loc loc)
			     (cops (cons (instantiate::local-var
					    (loc  loc)
					    (vars auxs))
					 seq1)))
			  loc)
			 #f))
		     (let ((arg (car args))
			   (act (car actuals)))
			;; we look for this special case in order to avoid
			;; local variable creations for constant parameters
			;; (as kaptured ones).
			(if (and (var? act) (eq? arg (var-variable act)))
			    (loop (cdr args)
				  (cdr actuals)
				  auxs
				  seq1
				  seq2)
			    (let ((aux (make-local-svar/name
					(local-id arg)
					(local-type arg))))
			       (loop (cdr args)
				     (cdr actuals)
				     (cons aux auxs)
				     (cons (node->cop (node-setq aux act)
						      *stop-kont*)
					   seq1)
				     (cons
				      (instantiate::stop
					 (loc (node-loc act))
					 (value
					  (instantiate::csetq
					     (loc loc)
					     (var (instantiate::varc
						     (variable arg)))
					     (value (instantiate::varc
						       (loc (node-loc act))
						       (variable aux))))))
				      seq2)))))))))))



