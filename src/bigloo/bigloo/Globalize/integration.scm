;*=====================================================================*/
;*    .../prgm/project/bigloo/comptime/Globalize/integration.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jan 26 17:10:12 1995                          */
;*    Last change :  Thu May 22 09:23:46 2003 (serrano)                */
;*    Copyright   :  1995-2003 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The L property, L(f,g) stands for `f be integrated in g?'        */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module globalize_integration
   (include "Tools/trace.sch")
   (import  engine_param
	    tools_shape
	    type_type
	    ast_var
	    ast_node
	    globalize_ginfo
	    globalize_globalize
	    (union globalize_kapture)
	    tools_error)
   (export  (set-integration!)))

;*---------------------------------------------------------------------*/
;*    set-integration! ...                                             */
;*---------------------------------------------------------------------*/
(define (set-integration!)
   (for-each (lambda (f)
		(let ((g (find-integrator f)))
		   (if (variable? g)
		       (integrate-in! f g)
		       (force-globalize! f))))
	     *G1*))

;*---------------------------------------------------------------------*/
;*    integrate-in! ...                                                */
;*---------------------------------------------------------------------*/
(define (integrate-in! f g)
   (with-access::local f (value)
      (trace (globalize 2)
	     "   -> integrating " (shape f) " inside " (shape g) #\Newline)
      (sfun/Ginfo-integrator-set! value g)
      (sfun/Ginfo-G?-set! value #f)
      (with-access::sfun/Ginfo (variable-value g) (integrated)
	 (set! integrated (cons f integrated)))))

;*---------------------------------------------------------------------*/
;*    force-globalize! ...                                             */
;*---------------------------------------------------------------------*/
(define (force-globalize! f)
   (trace (globalize 2)
	  "   -> globalizing " (shape f) #\Newline)
   (sfun/Ginfo-integrator-set! (local-value f) #f))

;*---------------------------------------------------------------------*/
;*    find-integrator ...                                              */
;*---------------------------------------------------------------------*/
(define (find-integrator f::local)
   (with-access::sfun/Ginfo (local-value f) (integrator imark owner cfrom cto)
      (trace (globalize 3)
	     "      find integrator f=" (shape f) #\Newline
	     "                      integrator=" (shape integrator) #\Newline
	     "                      imark=" (shape imark) #\Newline
	     "                      owner=" (shape owner) #\Newline
	     "                      cfrom="
	     (shape (sfun/Ginfo-cfrom (local-value f))) #\Newline
	     "                      cto="
	     (shape (sfun/Ginfo-cto (local-value f))) #\Newline)
      (and *optim-integrate?*
	   (let ((guess (find-integrator* f)))
	      (cond
		 ((null? guess)
		  ;; an internal error because all the functions send to
		  ;; FIND-INTEGRATOR belong to G1 which is the set of functions
		  ;; called by at least one global or escape function
		  (internal-error 'find-integrator "empty integrator*" f)
		  #f)
		 ((pair? (cdr guess))
		  ;; cannot integrate it because it is called
		  ;; from two globalized functions
		  #f)
		 ((integrate? f (car guess))
		  ;; it can be integrated
		  (car guess))
		 (else
		  #f))))))

;*---------------------------------------------------------------------*/
;*    integrator? ...                                                  */
;*---------------------------------------------------------------------*/
(define (integrator? g)
   (or (global? g)
       (and (local? g) (local/Ginfo-escape? g))))
   
;*---------------------------------------------------------------------*/
;*    *escape-mark* ...                                                */
;*---------------------------------------------------------------------*/
(define *escape-mark* 0)

;*---------------------------------------------------------------------*/
;*    find-integrator* ...                                             */
;*---------------------------------------------------------------------*/
(define (find-integrator* f)
   (set! *escape-mark* (+fx 1 *escape-mark*))
   (let loop ((cfrom (sfun/Ginfo-cfrom (local-value f)))
	      (escape* '()))
      (if (null? cfrom)
	  escape*
	  (let ((f (car cfrom)))
	     (if (eq? (sfun/Ginfo-cfrom* (variable-value f)) *escape-mark*)
		 (loop (cdr cfrom) escape*)
		 (begin 
		    (sfun/Ginfo-cfrom*-set! (variable-value f) *escape-mark*)
		    (cond
		       ((integrator? f)
			(loop (cdr cfrom) (cons f escape*)))
		       (else
			(loop (cdr cfrom)
			      (loop (sfun/Ginfo-cfrom (local-value f))
				    escape*))))))))))

;*---------------------------------------------------------------------*/
;*    *integrate-mark* ...                                             */
;*---------------------------------------------------------------------*/
(define *integrate-mark* 0)

;*---------------------------------------------------------------------*/
;*    integrate? ...                                                   */
;*    -------------------------------------------------------------    */
;*    It is possible to integrate f in g                               */
;*---------------------------------------------------------------------*/
(define (integrate? f::variable g::local)
   [assert (g) (integrator? g)]
   (trace (globalize 4) "         +++ integrate? " (shape f) " " (shape g)
	  #\Newline)
   (set! *integrate-mark* (+fx *integrate-mark* 1))
   (define (visible? h)
      ;; is h visible from g?
      (or (eq? h g)
	  (and (not (global? h))
	       (not (local/Ginfo-escape? h))
	       (integrate? h))))
   (define (integrate? f)
      ;; is f integrable in g?
      (or (eq? f g)
	  (and (local? g)
	       (let ((v (local-value f)))
		  (with-access::sfun/Ginfo v (cfrom cto imark)
		     (and (fixnum? imark)
			  (or (=fx imark *integrate-mark*)
			      (and (<fx imark *integrate-mark*)
				   (begin
				      (set! imark *integrate-mark*)
				      (let ((r (and (every? visible? cfrom)
						    (every? visible? cto))))
					 (if (not r) (set! imark #f))
					 r))))))))))
   (let ((r (integrate? f)))
      (trace (globalize 4) "         >>> integrate? "
	     (shape f) " " (shape g) " -> " r
	     " imark=" (shape (sfun/Ginfo-imark (local-value f))) #\Newline)
      r))

      
       
