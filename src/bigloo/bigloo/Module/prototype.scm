;*=====================================================================*/
;*    .../project/bglstone/src/bigloo/bigloo/Module/prototype.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Jun  4 14:27:58 1996                          */
;*    Last change :  Thu Mar  6 16:26:23 2025 (serrano)                */
;*    Copyright   :  1996-2025 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The prototype management                                         */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module module_prototype
   (include "Ast/node.sch")
   (import tools_error
	   tools_dsssl
	   tools_misc
	   type_type
	   ast_ident
	   module_eval
	   (find-location/loc tools_location)
	   (find-location tools_location)
	   type_cache)
   (export (parse-prototype prototype)))

;*---------------------------------------------------------------------*/
;*    parse-prototype ...                                              */
;*---------------------------------------------------------------------*/
(define (parse-prototype proto)
   (match-case proto
      (((and ?class (or class final-class wide-class abstract-class)) . ?-)
       (parse-class class (cdr proto)))
      ((generic . ?-)
       (parse-function-prototype (cdr proto) 'sgfun))
      ((inline . ?-)
       (parse-function-prototype (cdr proto) 'sifun))
      ((?id . ?-)
       (if (or (not *all-export-mutable?*)
	       (memq id '(main module-initialization))
	       (not (eq? (get-default-type)
			 (type-of-id id (find-location proto)))))
	   (parse-function-prototype proto 'sfun)
	   (parse-variable-prototype id)))
      (else
       (parse-variable-prototype proto))))

;*---------------------------------------------------------------------*/
;*    parse-function-prototype ...                                     */
;*---------------------------------------------------------------------*/
(define (parse-function-prototype proto class)
   (match-case proto
      (((and ?id (? symbol?)) . ?the-args)
       (let loop ((args the-args))
	  (cond
	     ((null? args)
	      (list class id (dsssl-formals-skeleton the-args)))
	     ((symbol? args)
	      (list class id the-args))
	     ((and (pair? args)
		   (or (symbol? (car args))
		       (dsssl-named-constant? (car args))))
	      (loop (cdr args)))
	     (else
	      #f))))
      (else
       #f)))

;*---------------------------------------------------------------------*/
;*    parse-variable-prototype ...                                     */
;*---------------------------------------------------------------------*/
(define (parse-variable-prototype proto)
   (if (symbol? proto)
       (list 'svar proto) 
       #f))

;*---------------------------------------------------------------------*/
;*    parse-class ...                                                  */
;*---------------------------------------------------------------------*/
(define (parse-class class class-def)
   (let ((loc (find-location class-def)))
      (define (parse-class-slots slots)
	 (let loop ((slots slots)
		    (res   '()))
	    (cond
	       ((null? slots)
		(reverse! res))
	       ((not (pair? slots))
		#f)
	       (else
		(let ((slot (parse-class-slot (car slots) loc)))
		   (if (not slot)
		       (user-error "Parse error"
				   "Illegal class field definition"
				   (if (pair? (car slots))
				       (car slots)
				       class-def))
		       (loop (cdr slots)
			     (cons slot res))))))))
      (match-case class-def
	 (((and ?name (? symbol?)) (?constructor) . ?slots)
	  (cons* class name constructor (parse-class-slots slots)))
	 (((and ?name (? symbol?)) . ?slots)
	  (cons* class name #f (parse-class-slots slots)))
	 (else
	  #f))))

;*---------------------------------------------------------------------*/
;*    parse-class-slot ...                                             */
;*---------------------------------------------------------------------*/
(define (parse-class-slot slot loc)
   (let ((loc (find-location/loc slot loc)))
      (match-case slot
	 ((? symbol?)
	  (epairify `(id ,(parse-id slot loc)) slot))
	 ((* (and ?id (? symbol?)) . ?att)
	  (if (correct-attribut? att)
	      (epairify `(* (id ,(parse-id id loc)) ,@att) slot)
	      #f))
	 (((and ?id (? symbol?)) . ?att)
	  (if (correct-attribut? att)
	      (epairify `((id ,(parse-id id loc)) ,@att) slot)
	      #f))
	 (else
	  #f))))

;*---------------------------------------------------------------------*/
;*    correct-attribut? ...                                            */
;*---------------------------------------------------------------------*/
(define (correct-attribut? attribut)
   (let loop ((attribut attribut))
      (cond
         ((null? attribut)
          #t)
         ((memq (car attribut) '(read-only))
          (loop (cdr attribut)))
         (else
          (match-case (car attribut)
	     ((get ?-)
	      (loop (cdr attribut)))
	     ((set ?-)
	      (loop (cdr attribut)))
             ((default ?-)
              (loop (cdr attribut)))
	     ((info ?-)
	      (loop (cdr attribut)))
             (else
              #f))))))
