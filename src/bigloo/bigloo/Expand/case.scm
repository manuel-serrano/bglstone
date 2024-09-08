;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Expand/case.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Jul  3 10:13:16 1992                          */
;*    Last change :  Tue Sep 16 16:38:06 2003 (serrano)                */
;*    Copyright   :  1992-2003 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    On macro-expanse ce satane `case'                                */
;*=====================================================================*/
 
;*---------------------------------------------------------------------*/
;*    Le module                                                        */
;*---------------------------------------------------------------------*/
(module expand_case
   (include "Tools/trace.sch")
   (import  tools_progn
	    tools_error
	    engine_param
	    type_type
	    ast_ident)
   (export  (expand-case ::obj ::procedure)))
	   
;*---------------------------------------------------------------------*/
;*    expand-case ...                                                  */
;*    -------------------------------------------------------------    */
;*    Le case des constantes a ete rajoute en partie pour la           */
;*    compilation de ML car je ne pense pas que lors d'une compilation */
;*    Scheme cela serve beaucoup. Neanmoins, je n'ai pas voulu trop    */
;*    cabler que les caracteres sont des constantes, c'est pourquoi    */
;*    j'ai fait un effort pour laisser le case des char (meme s'il     */
;*    pourrait etre inclus dans celui des constantes).                 */
;*---------------------------------------------------------------------*/
(define (expand-case x e)
   (trace expand "expand-case: " x #\Newline)
   (match-case x
      ((?- ?value . ?clauses)
       (case (case-type x clauses)
	  ((integer)
	   (trace expand "expand-case [integer]" #\Newline)
	   (do-typed-case 'long value clauses e))
	  ((char)
	   (trace expand "expand-case [char]" #\Newline)
	   (do-typed-case 'char value clauses e))
	  ((cnst)
	   (trace expand "expand-case [cnst]" #\Newline)
	   (do-cnst-case value clauses e))
	  (else
	   (trace expand "expand-case [else]" #\Newline)
	   (do-generic-case value clauses e))))
   (else
    (error "case" "Illegal `case' form" x))))

;*---------------------------------------------------------------------*/
;*    case-type ...                                                    */
;*    < datum+ x sexp+ >+ --> integer @ char @ symbol @ etherogeneous  */
;*    -------------------------------------------------------------    */
;*    On cherche a savoir si on va poouvoir coder ce case comme un     */
;*    `switch' ou s'il va falloir le coder comme un `if'. On ne peut   */
;*    utiliser un `switch' que si tous les datums sont des constantes. */
;*    -------------------------------------------------------------    */
;*    On profite de cette fonction pour s'assurer que chacune des      */
;*    clauses a la bonne syntaxe.                                      */
;*---------------------------------------------------------------------*/
(define (case-type x clauses)
   (labels ((type-match? (type1 type2) (or (null? type1)
					   (null? type2)
					   (and (not (eq? type1 'fail-type))
						(or (eq? type1 type2)
						    (and (eq? type1 'cnst)
							 (eq? type2 'char))
						    (and (eq? type1 'char)
							 (eq? type2 'cnst))))))
	    (general     (type1 type2)  (cond
					   ((eq? type1 type2)
					    type1)
					   ((eq? type1 'cnst)
					    type1)
					   ((null? type2)
					    type1)
					   (else
					    type2)))
	    (one-type    (datum) (cond
				    ((fixnum? datum)
				     'integer)
				    ((char? datum)
				     'char)
				    ((cnst? datum)
				     'cnst)
				    (else
				     'fail-type)))
	    (datum-type  (datums) (let loop ((datums datums)
					     (type   '()))
				     (cond
					((null? datums)
					 type)
					((not (pair? datums))
					 (error "case"
						"Illegal `case' form"
						x)
					 #f)
					(else
					 (let ((dtype (one-type (car datums))))
					    (if (type-match? dtype type)
						(loop (cdr datums)
						      (general dtype type))
						'fail-type)))))))
      (let loop ((clauses clauses)
		 (type    '()))
	 (if (null? clauses)
	     type
	     (match-case (car clauses)
		((else . ?exps)
		 (if (or (not (null? (cdr clauses)))
			 (null? exps))
		     (error "case" "Illegal `case' form" x)
		     type))
		(((and (not ()) ?datum) . ?exps)
		 (if (null? exps)
		     (error "case" "Illegal `case' form" x)
		     (let ((dtype (datum-type datum)))
			(if (type-match? dtype type)
			    (loop (cdr clauses) (general dtype type))
			    'etherogeneous))))
		(else
		 (error "case" "Illegal `case' form" x)))))))

;*---------------------------------------------------------------------*/
;*    do-typed-case ...                                                */
;*    type x sexp x < datum+ x sexp+ >+ x (sexp x sexp --> sexp)       */    
;*---------------------------------------------------------------------*/
(define (do-typed-case type value clauses e)
   (let* ((else-body (let loop ((clauses clauses))
			(if (null? clauses)
			    (list #unspecified)
			    (match-case (car clauses)
					(()
					 #unspecified)
					((else . ?body)
					 (map (lambda (x) (e x e)) body))
					(else
					 (loop (cdr clauses)))))))
	  (else-name (mark-symbol-non-user! (gensym "case_else")))
	  (aux       (mark-symbol-non-user! (gensym 'aux))))
      (let ((case `(case ,aux
		      ,@(let loop ((clauses clauses))
			   (if (null? clauses)
			       (begin
				  `((else ,#unspecified)))
			       (match-case (car clauses)
				  (() 
				   `((else #unspecified)))
				  ((else . ?body)
				   `((else (,else-name))))
				  ((?datums . ?body)
				   (if (null? body)
				       (error "case"
					      "Illegal `case' clause"
					      (car clauses))
				       (cons `(,datums
					       ,@(map (lambda (x) (e x e))
						      body))
					     (loop (cdr clauses)))))
				  (else
				   (error "case"
					  "Illegal `case' form"
					  clauses))))))))
	 (type-test aux type value case else-body else-name e))))

;*---------------------------------------------------------------------*/
;*    do-cnst-case ...                                                 */
;*    sexp x < datum+ x sexp+ >+ x (sexp x sexp --> sexp)              */    
;*    -------------------------------------------------------------    */
;*    On transforme un case sur des constantes en case sur des         */
;*    entiers.                                                         */
;*---------------------------------------------------------------------*/
(define (do-cnst-case value clauses e)
   (let* ((aux   (mark-symbol-non-user! (gensym 'aux)))
	  (value `(let ((,aux ,value))
		    (if (cnst? ,aux)
			(cnst->integer ,aux)
			;; on met -1 car les constantes ne peuvent
			;; pas avoir des valeurs negatives.
			-1))))
      (let loop ((c clauses))
	 (if (null? c)
	     (do-typed-case 'long value clauses e)
	     (let ((clause (car c)))
		(if (not (eq? (car clause) 'else))
		    (set-car! clause (map cnst->integer (car clause))))
		(loop (cdr c)))))))

;*---------------------------------------------------------------------*/
;*    type-test ...                                                    */
;*---------------------------------------------------------------------*/
(define (type-test aux type value case else-body else-name e)
   (cond
      ((eq? type 'char)
       `(labels ((,else-name () ,@else-body))
	   (let ((,aux ,(e value e)))
	      (if (c-char? ,aux)
		  ,case
		  (,else-name)))))
      ((eq? type 'long)
       `(labels ((,else-name () ,@else-body))
	   (let ((,aux ,(e value e)))
	      (if (c-fixnum? ,aux)
		  ,case
		  (,else-name)))))
      (else
       (error "type-test" "Unknown `case' type" type))))

;*---------------------------------------------------------------------*/
;*    do-generic-case ...                                              */
;*    sexp x < datum+ x sexp+ >+ x (sexp x sexp --> sexp)              */  
;*---------------------------------------------------------------------*/
(define (do-generic-case value clauses e)
   (e `(let ((case-value ,value))
	  ,(let loop ((clauses clauses))
	      (if (null? clauses)
		  #unspecified
		  (match-case (car clauses)
		     (()
		      #unspecified)
		     ((else . ?body)
		      (if (null? body)
			  #f
			  (normalize-progn body)))
		     (((and ?datums (?- . (?- ???-))) . ?body)
		      (if (null? body)
			  (error "case" "Illegal `case' clause" (car clauses))
			  `(if (memv case-value ',datums)
			       ,(normalize-progn body)
			       ,(loop (cdr clauses)))))
		     (((?datums) . ?body)
		      (if (null? body)
			  (error "case" "Illegal `case' clause" (car clauses))
			  `(if (eqv? case-value ',datums)
			       ,(normalize-progn body)
			       ,(loop (cdr clauses)))))))))
      e))
