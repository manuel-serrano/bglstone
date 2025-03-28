;*=====================================================================*/
;*    serrano/prgm/project/bglstone/src/bigloo/bigloo/Cfa/cfa.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Feb 23 14:21:20 1995                          */
;*    Last change :  Fri Mar  7 07:46:33 2025 (serrano)                */
;*    Copyright   :  1995-2025 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The `control flow analysis': the walk down the ast               */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module cfa_cfa
   (include "Ast/node.sch" "Type/type.sch" "Cfa/cinfo.sch" "Cfa/cinfo2.sch" "Cfa/cinfo3.sch")
   (include "Tools/trace.sch")
   (import  tools_shape
	    tools_error
	    type_type
	    type_cache
	    ast_var
	    ast_node
	    cfa_info
	    cfa_info2
	    cfa_info3
	    cfa_loose
	    cfa_approx
	    cfa_app
	    cfa_funcall)
   (export  (generic cfa!::approx ::node)
	    (generic cfa-variable-value-approx value::value)))

;*---------------------------------------------------------------------*/
;*    cfa! ...                                                         */
;*---------------------------------------------------------------------*/
(define-generic (cfa!::approx node::node)
   (internal-error "cfa!:no method for this ast" node (shape node)))
   
;*---------------------------------------------------------------------*/
;*    cfa! ::atom/Cinfo ...                                            */
;*---------------------------------------------------------------------*/
(define-method (cfa! node::atom/Cinfo)
   (with-access::atom/Cinfo node (approx)
      approx))

;*---------------------------------------------------------------------*/
;*    cfa! ::kwote/Cinfo ...                                           */
;*---------------------------------------------------------------------*/
(define-method (cfa! node::kwote/Cinfo)
   (with-access::kwote/Cinfo node (approx)
      approx))
 
;*---------------------------------------------------------------------*/
;*    cfa! ::kwote/node ...                                            */
;*---------------------------------------------------------------------*/
(define-method (cfa! knode::kwote/node)
   (with-access::kwote/node knode (node)
      (trace (cfa 3) "kwote/node: " (shape knode) " -> " (shape node)
	     #\Newline)
      (cfa! node)))
		    
;*---------------------------------------------------------------------*/
;*    cfa! ::var ...                                                   */
;*---------------------------------------------------------------------*/
(define-method (cfa! node::var)
   (with-access::var node (variable)
      (cfa-variable-value-approx (variable-value variable))))

;*---------------------------------------------------------------------*/
;*    cfa! ::closure ...                                               */
;*---------------------------------------------------------------------*/
(define-method (cfa! node::closure)
   (internal-error "cfa!" "Unexpected closure" (shape node)))

;*---------------------------------------------------------------------*/
;*    cfa-variable-value-approx ...                                    */
;*---------------------------------------------------------------------*/
(define-generic (cfa-variable-value-approx value::value))

;*---------------------------------------------------------------------*/
;*    cfa-variable-value-approx ::svar/Cinfo ...                       */
;*---------------------------------------------------------------------*/
(define-method (cfa-variable-value-approx value::svar/Cinfo)
   (with-access::svar/Cinfo value (approx)
      approx))
   
;*---------------------------------------------------------------------*/
;*    cfa-variable-value-approx ::scnst/Cinfo ...                      */
;*---------------------------------------------------------------------*/
(define-method (cfa-variable-value-approx value::scnst/Cinfo)
   (with-access::scnst/Cinfo value (approx)
      approx))
   
;*---------------------------------------------------------------------*/
;*    cfa-variable-value-approx ::cvar/Cinfo ...                       */
;*---------------------------------------------------------------------*/
(define-method (cfa-variable-value-approx value::cvar/Cinfo)
    (with-access::cvar/Cinfo value (approx)
      approx))
				     
;*---------------------------------------------------------------------*/
;*    cfa-variable-value-approx ::sexit/Cinfo ...                      */
;*---------------------------------------------------------------------*/
(define-method (cfa-variable-value-approx value::sexit/Cinfo)
   (with-access::sexit/Cinfo value (approx)
      approx))
   
;*---------------------------------------------------------------------*/
;*    cfa-variable-value-approx ::intern-sfun/Cinfo ...                */
;*    -------------------------------------------------------------    */
;*    We reach this method each time a `make-procedure' is scanned.    */
;*    This node has no effect. Its value is never used. It can be      */
;*    ignore but for the typing system, it as to be an `approx'.       */
;*---------------------------------------------------------------------*/
(define-method (cfa-variable-value-approx value::intern-sfun/Cinfo)
    (with-access::intern-sfun/Cinfo value (approx)
      approx))
				     
;*---------------------------------------------------------------------*/
;*    cfa! ::sequence ...                                              */
;*---------------------------------------------------------------------*/
(define-method (cfa! node::sequence)
   (with-access::sequence node (nodes)
      (let loop ((n      nodes)
		 (approx #unspecified))
	 (if (null? n)
	     approx
	     (loop (cdr n) (cfa! (car n)))))))

;*---------------------------------------------------------------------*/
;*    cfa! ::app-ly ...                                                */
;*---------------------------------------------------------------------*/
(define-method (cfa! node::app-ly/Cinfo)
   (with-access::app-ly/Cinfo node (fun arg approx) 
      (loose! (cfa! arg) 'all)
      (loose! (cfa! fun) 'all)
      approx))

;*---------------------------------------------------------------------*/
;*    cfa! ::pragma/Cinfo ...                                          */
;*---------------------------------------------------------------------*/
(define-method (cfa! node::pragma/Cinfo)
   (with-access::pragma/Cinfo node (approx expr*)
      (for-each (lambda (a) (loose! (cfa! a) 'all)) expr*)
      approx))

;*---------------------------------------------------------------------*/
;*    cfa! ::getfield/Cinfo ...                                        */
;*---------------------------------------------------------------------*/
(define-method (cfa! node::getfield/Cinfo)
   (with-access::getfield/Cinfo node (approx expr*)
      (for-each (lambda (a) (loose! (cfa! a) 'all)) expr*)
      approx))

;*---------------------------------------------------------------------*/
;*    cfa! ::setfield/Cinfo ...                                        */
;*---------------------------------------------------------------------*/
(define-method (cfa! node::setfield/Cinfo)
   (with-access::setfield/Cinfo node (approx expr*)
      (for-each (lambda (a) (loose! (cfa! a) 'all)) expr*)
      approx))

;*---------------------------------------------------------------------*/
;*    cfa! ::new/Cinfo ...                                             */
;*---------------------------------------------------------------------*/
(define-method (cfa! node::new/Cinfo)
   (with-access::new/Cinfo node (approx expr*)
      (for-each (lambda (a) (loose! (cfa! a) 'all)) expr*)
      approx))

;*---------------------------------------------------------------------*/
;*    cfa! ::isa/Cinfo ...                                             */
;*---------------------------------------------------------------------*/
(define-method (cfa! node::isa/Cinfo)
   (with-access::isa/Cinfo node (approx expr*)
      approx))

;*---------------------------------------------------------------------*/
;*    cfa! ::cast-null/Cinfo ...                                       */
;*---------------------------------------------------------------------*/
(define-method (cfa! node::cast-null/Cinfo)
   (with-access::cast-null/Cinfo node (approx expr*)
      approx))

;*---------------------------------------------------------------------*/
;*    cfa! ::cast ...                                                  */
;*---------------------------------------------------------------------*/
(define-method (cfa! node::cast)
   (with-access::cast node (arg)
      (cfa! arg)))

;*---------------------------------------------------------------------*/
;*    cfa! ::setq/Cinfo ...                                            */
;*---------------------------------------------------------------------*/
(define-method (cfa! node::setq/Cinfo)
   (with-access::setq/Cinfo node (approx var value)
      (let* ((var-approx (cfa! var))
	     (val-approx (cfa! value))
	     (v          (var-variable var)))
	 (trace (cfa 3) "*** " (shape node) " <- " (shape val-approx)
		#\Newline)
	 (union-approx! var-approx val-approx)
	 (if (global? v) (global-loose! v var-approx))
	 approx)))

;*---------------------------------------------------------------------*/
;*    cfa! ::conditional/Cinfo ...                                     */
;*---------------------------------------------------------------------*/
(define-method (cfa! node::conditional/Cinfo)
   (with-access::conditional/Cinfo node (approx test true false)
       (cfa! test)
       (let ((then-approx (cfa! true))
	     (else-approx (cfa! false)))
	  (union-approx! approx then-approx)
	  (union-approx! approx else-approx)
	  approx)))

;*---------------------------------------------------------------------*/
;*    cfa! ::fail/Cinfo ...                                            */
;*---------------------------------------------------------------------*/
(define-method (cfa! node::fail/Cinfo)
   (with-access::fail/Cinfo node (approx proc msg obj)
      (loose! (cfa! proc) 'all)
      (loose! (cfa! msg) 'all)
      (loose! (cfa! obj) 'all)
      approx))

;*---------------------------------------------------------------------*/
;*    cfa! ::select/Cinfo ...                                          */
;*---------------------------------------------------------------------*/
(define-method (cfa! node::select/Cinfo)
   (with-access::select/Cinfo node (approx clauses test)
      (cfa! test)
      (let ((res-approx approx))
	 (let loop ((cls clauses))
	    (if (null? cls)
		res-approx
		(let ((new-approx (cfa! (cdr (car cls)))))
		   (union-approx! res-approx new-approx)
		   (loop (cdr cls))))))))

;*---------------------------------------------------------------------*/
;*    cfa! ::let-fun ...                                               */
;*---------------------------------------------------------------------*/
(define-method (cfa! node::let-fun)
   (with-access::let-fun node (body)
      (cfa! body)))

;*---------------------------------------------------------------------*/
;*    cfa! ::let-var ...                                               */
;*---------------------------------------------------------------------*/
(define-method (cfa! node::let-var)
   (with-access::let-var node (body bindings)
      (trace (cfa 3) "cfa! ::let-var: " (shape node) #\Newline)
      (for-each (lambda (binding)
		   (let* ((var        (car binding))
			  (var-approx (svar/Cinfo-approx (variable-value var)))
			  (val-approx (cfa! (cdr binding))))
		      (let ((vtype (variable-type var))
			    (atype (approx-type val-approx)))
			 (union-approx! var-approx val-approx)
			 (trace (cfa 4)
				"   binding: " (shape binding) #\Newline
				"       var: " (shape var-approx) #\Newline
				"       val: " (shape val-approx) #\Newline
				"     vtype: " (shape vtype) #\Newline
				"     atype: " (shape atype) #\Newline)
			 ;; Here we explicitly check for type errors.
			 ;; When one is encountered, top is propagated
			 (if (and (not (eq? vtype *_*))
				  (not (eq? vtype *obj*))
				  (not (eq? atype *_*))
				  (not (eq? atype vtype)))
			     (begin
				(approx-set-top! val-approx)
				(loose! val-approx 'all))))))
		bindings)
      (cfa! body)))

;*---------------------------------------------------------------------*/
;*    cfa! ::set-ex-it/Cinfo ...                                       */
;*---------------------------------------------------------------------*/
(define-method (cfa! node::set-ex-it/Cinfo)
   (with-access::set-ex-it/Cinfo node (approx body)
      (loose! (cfa! body) 'all)
      approx))

;*---------------------------------------------------------------------*/
;*    cfa! ::jump-ex-it/Cinfo ...                                      */
;*---------------------------------------------------------------------*/
(define-method (cfa! node::jump-ex-it/Cinfo)
   (with-access::jump-ex-it/Cinfo node (approx exit value)
      (cfa! exit)
      (let ((val-approx (cfa! value)))
	 (loose! val-approx 'all)
	 approx)))

;*---------------------------------------------------------------------*/
;*    cfa! ::make-box/Cinfo ...                                        */
;*---------------------------------------------------------------------*/
(define-method (cfa! node::make-box/Cinfo)
   (with-access::make-box/Cinfo node (approx value)
      (trace (cfa 3) "cfa!(make-box): " (shape value) #\Newline)
      (loose! (cfa! value) 'all)
      approx))

;*---------------------------------------------------------------------*/
;*    cfa! ::box-set!/Cinfo ...                                        */
;*---------------------------------------------------------------------*/
(define-method (cfa! node::box-set!/Cinfo)
   (with-access::box-set!/Cinfo node (approx var value)
      (cfa! var)
      (loose! (cfa! value) 'all)
      approx))

;*---------------------------------------------------------------------*/
;*    cfa! ::box-ref ...                                               */
;*---------------------------------------------------------------------*/
(define-method (cfa! node::box-ref/Cinfo)
   (with-access::box-ref/Cinfo node (approx var)
      (cfa! var)
      approx))
