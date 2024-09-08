;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Cgen/ccond.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Apr  2 08:36:44 2003                          */
;*    Last change :  Wed Apr  2 15:10:09 2003 (serrano)                */
;*    Copyright   :  2003 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    The C production of conditional nodes.                           */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module cgen_ccond
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
	    cgen_emit
	    cgen_cop
	    cgen_cgen))

;*---------------------------------------------------------------------*/
;*    node->cop ::conditional ...                                      */
;*    -------------------------------------------------------------    */
;*    Some compilers does not support a very high number of nested     */
;*    blocks (no comments!). Such C patterns may occurs when compiling */
;*    large Scheme COND constructions. The C back-end tries to detect  */
;*    such a sequence in order to compile them with explicit           */
;*    C label+goto.                                                    */
;*---------------------------------------------------------------------*/
(define-method (node->cop node::conditional kont)
   (trace (cgen 3)
	  "(node->cop node::conditional kont): " (shape node) #\Newline
	  "  loc: " (node-loc node) #\Newline
	  "  kont: " kont #\Newline)
   (with-access::conditional node (test true false loc)
      (let* ((aux   (make-local-svar/name 'test *bool*))
	     (ctest (node->cop (node-setq aux test) *id-kont*))
	     (depth (conditional-nesting-depth node)))
	 (if (and (csetq? ctest) (eq? (varc-variable (csetq-var ctest)) aux))
	     (if (> depth *cc-conditional-nesting-max*)
		 (let ((lbl (symbol->string (gensym 'cond)))
		       (cthen (block-kont (node->cop true kont) loc))
		       (condi (instantiate::cif
				 (test (csetq-value ctest))
				 (true (instantiate::csequence
					  (cops (list cthen
						      (instantiate::cgoto
							 (label lbl))))))
				 (false (block-kont (node->cop false kont) loc))
				 (loc   loc)))
		       (otherw (block-kont (node->cop false kont) loc)))
		    (instantiate::csequence
		       (cops (list condi
				   (instantiate::cgoto
				      (label lbl))
				   otherw
				   (instantiate::clabel
		       
		 (instantiate::cif
		    (test (csetq-value ctest))
		    (true (block-kont (node->cop true kont) loc))
		    (false (block-kont (node->cop false kont) loc))
		    (loc   loc)))
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
;*    conditional-nesting-depth ...                                    */
;*    -------------------------------------------------------------    */
;*    Returns the depth of nesting conditional. A nested conditional   */
;*    is defined as:                                                   */
;*                                                                     */
;*      1: (if test1 then1 (if test2 then2 ...))                       */
;*      2: (if test1 then1 (let ... (if test2 then2 ...)))             */
;*      3: (if test1 then1 (begin ... (if test2 then2 ...)))           */
;*---------------------------------------------------------------------*/
(define (conditional-nesting-depth node::conditional)
   (with-access::conditional node (test true false loc)
      (let loop ((d 0)
		 (then false))
	 (if (> d *cc-conditional-nesting-max*)
	     d
	     (let ((next-then (conditional-nesting-then then)))
		(if (node? next-then)
		    (loop (+fx d 1) next-then)
		    d))))))

;*---------------------------------------------------------------------*/
;*    conditional-nesting-then :: ...                                  */
;*---------------------------------------------------------------------*/
(define-generic (conditional-nesting-then node::node)
   #f)

;*---------------------------------------------------------------------*/
;*    conditional-nesting-then ::conditional ...                       */
;*---------------------------------------------------------------------*/
(define-method (conditional-nesting-then node::conditional)
   (with-access::conditional node (false)
      false))

;*---------------------------------------------------------------------*/
;*    conditional-nesting-then ::let-var ...                           */
;*---------------------------------------------------------------------*/
(define-method (conditional-nesting-then node::let-var)
   (with-access::let-var node (body)
      (conditional-nesting-then body)))

;*---------------------------------------------------------------------*/
;*    conditional-nesting-then ::sequence ...                          */
;*---------------------------------------------------------------------*/
(define-method (conditional-nesting-then node::sequence)
   (with-access::sequence node (nodes)
      (and (pair? nodes) (conditional-nesting-then (car (last-pair nodes))))))
      

   
