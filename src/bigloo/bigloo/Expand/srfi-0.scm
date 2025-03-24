;*=====================================================================*/
;*    .../project/bglstone/src/bigloo/bigloo/Expand/srfi-0.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Feb 24 15:25:03 1999                          */
;*    Last change :  Thu Mar  6 16:41:07 2025 (serrano)                */
;*    Copyright   :  1999-2025 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The expander for srfi forms.                                     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module expand_srfi-0
   (include "Ast/node.sch" "Type/type.sch")
   (import engine_param
	   tools_misc)
   (export (expand-cond-expand x e)
	   (add-supported-srfi! ::symbol))
   (eval   (export register-srfi!)))

;*---------------------------------------------------------------------*/
;*    *srfi-list* ...                                                  */
;*    -------------------------------------------------------------    */
;*    The list of supported srfi                                       */
;*    -------------------------------------------------------------    */
;*    The four initial supported srfi are:                             */
;*       - srfi-0                                                      */
;*       - bigloo                                                      */
;*       - bigloo<major-num>                                           */
;*       - bigloo<major-num><minor-num>                                */
;*    -------------------------------------------------------------    */
;*    When a library is used for compiling the name of that library    */
;*    is added to the supported srfis.                                 */
;*    -------------------------------------------------------------    */
;*    @label srfi-list@                                                */
;*    This list mirrors the one defined in                             */
;*    runtime/Eval/expdsrfi0.scm.                                      */
;*    @ref ../../runtime/Eval/expdsrfi0.scm:srfi-eval-list@            */
;*---------------------------------------------------------------------*/
(define *srfi-list*
   (let ((bgl-major (string-append
		     "bigloo"
		     (substring *bigloo-version*
				0
				(-fx (string-length *bigloo-version*) 1))))
	 (bgl-version (string-append "bigloo" *bigloo-version*)))
      (list (string->symbol bgl-major)
	    (string->symbol bgl-version)
	    'bigloo
	    'srfi-0
	    'srfi-2
	    'srfi-6
	    'srfi-8
	    'srfi-9
	    'srfi-22
	    'srfi-28
	    'srfi-30)))

;*---------------------------------------------------------------------*/
;*    add-supported-srfi! ...                                          */
;*---------------------------------------------------------------------*/
(define (add-supported-srfi! srfi::symbol)
   (set! *srfi-list* (cons srfi *srfi-list*)))

;*---------------------------------------------------------------------*/
;*    register-srfi! ...                                               */
;*---------------------------------------------------------------------*/
(define (register-srfi! srfi)
   (if (symbol? srfi)
       (add-supported-srfi! srfi)
       (error "register-srfi!" "Illegal srfi argument" srfi)))

;*---------------------------------------------------------------------*/
;*    expand-cond-expand ...                                           */
;*---------------------------------------------------------------------*/

(define (expand-cond-expand x e)
   (match-case x
      ((cond-expand)
       (error "cond-expand" "Illegal form" x))
      ((?- ?clause . ?else)
       (match-case clause
	  (((kwote else) . ?body)
	   (if (null? else)
	       (e (epairify `(begin ,@body) x) e)
	       (error "cond-expand" "Illegal form" x)))
	  ((((kwote and)) . ?body)
	   (e (epairify `(begin ,@body) x) e))
	  ((((kwote and) ?req1) . ?body)
	   (e (epairify `(cond-expand
			    (,req1 ,@body)
			    ,@else)
			x)
	      e))
	  ((((kwote and) ?req1 ?req2 . ?reqs) . ?body)
	   (expand-cond-expand-and x e req1 req2 reqs body else))
	  ((((kwote or)) . ?body)
	   (e (epairify `(cond-expand ,@else) x) e))
	  ((((kwote or) ?req1) . ?body)
	   (e (epairify `(cond-expand
			    (,req1 ,@body)
			    ,@else)
			x)
	      e))
	  ((((kwote or) ?req1 ?req2 . ?reqs) . ?body)
	   (expand-cond-expand-or x e req1 req2 reqs body else))
	  ((((kwote not) ?req) . ?body)
	   (e (epairify `(cond-expand
			    (,req (cond-expand ,@else))
			    (else ,@body))
			x)
	      e))
	  (((and (? symbol?) ?feature) . ?body)
	   (e (epairify (if (memq feature *srfi-list*)
			    `(begin ,@body)
			    `(cond-expand ,@else))
			x)
	      e))
	  (else
	   (error "cond-expand" "Illegal form" x))))
      (else
       (error "cond-expand" "Illegal form" x))))

;*---------------------------------------------------------------------*/
;*    expand-cond-expand-and ...                                       */
;*---------------------------------------------------------------------*/
(define (expand-cond-expand-and x e req1 req2 reqs body else)
   (e (epairify `(cond-expand
		    (,req1 (cond-expand
			      ((and ,req2 ,@reqs) ,@body)
			      ,@else))
		    ,@else)
		x)
      e))

;*---------------------------------------------------------------------*/
;*    expand-cond-expand-or ...                                        */
;*---------------------------------------------------------------------*/
(define (expand-cond-expand-or x e req1 req2 reqs body else)
   (let ((bd (gensym 'body)))
      (e (epairify `(let ((,bd ,(epairify `(begin ,@body) body)))
		       (cond-expand
			  (,req1 ,bd)
			  (else
			   (cond-expand
			      ((or ,req2 ,@reqs)
			       ,bd)
			      ,@else))))
		   x)
	 e)))
