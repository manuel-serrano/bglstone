;*=====================================================================*/
;*    .../prgm/project/bglstone/src/cgc/bigloo/Parser/lexer.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Nov 24 11:36:25 1995                          */
;*    Last change :  Wed Oct 15 11:46:42 2014 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The cgc lexer                                                    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module parser_lexer
   (import engine_param
	   ast_ident
	   ast_location)
   (export *c-lexer*)) 

;*---------------------------------------------------------------------*/
;*    *keyword-list*                                                   */
;*---------------------------------------------------------------------*/
(define *keyword-list*
   '("if" "return" "typedef" "struct" "else" "while" "break"))

;*---------------------------------------------------------------------*/
;*    The keyword initialization                                       */
;*---------------------------------------------------------------------*/
(for-each (lambda (word)
	     (putprop! (string->symbol word) 'reserved #t))
	  *keyword-list*)

;*---------------------------------------------------------------------*/
;*    *c-lexer* ...                                                    */
;*---------------------------------------------------------------------*/
(define *c-lexer*
   (regular-grammar ()
      
      ;; blank
      ((+ (in #\space #\newline #\tab #a012))
       (ignore))
      
      ;; comment
      ((or (: "/*" (* (or (out #\*) (: (+ #\*) (out #\/ #\*)))) (+ #\*) "/")
	   (: "//" (* all)))
       (ignore))
      
      ;; comma
      (#\,
       (list 'COMMA (the-port)))
      
      ;; semi-comma
      (#\;
       (list 'SEMI-COMMA
	     (the-port)
	     (make-location (input-port-name (the-port))
			    (input-port-position (the-port)))))
      
      ;; bracket
      (#\{
       (list 'BRA-OPEN (the-port)))
      (#\}
       (list 'BRA-CLO (the-port)))
      
      ;; angle
      (#\[
       (list 'ANGLE-OPEN (the-port)))
      (#\]
       (list 'ANGLE-CLO (the-port)))
      
      ;; parenthesis
      (#\(
       (list 'PAR-OPEN (the-port)))
      (#\)
       (list 'PAR-CLO (the-port)))
      
      ;; integer constant
      ((+ digit)
       (list 'CONSTANT
	     (the-port)
	     (the-fixnum)
	     (make-location (input-port-name (the-port))
			    (input-port-position (the-port)))))
      
      ;; string constant
      ((or (: #\" (* (or (out #\\ #\") (: #\\ all))) #\")
	   (+ (: (: #\" (* (or (out #\\ #\") (: #\\ all))) #\")
		 (* (in #\space #\tab #\Newline)))))
       (list 'CONSTANT
	     (the-port)
	     (the-string)
	     (make-location (input-port-name (the-port))
			    (input-port-position (the-port)))))
      
      ;; binary operators
      ((or (in "*+-/<>") "<=" ">=" "==" "!=")
       (list 'BINARY-OP
	     (the-port)
	     (instantiate::ident
		(name  (the-string))
		(location (make-location
			   (input-port-name (the-port))
			   (input-port-position (the-port)))))))
      
      ;; setq
      (#\=
       (list (the-symbol)
	     (the-port)
	     (make-location
	      (input-port-name (the-port))
	      (input-port-position (the-port)))))
      
      ;; record access
      (#\.
       (list 'DOT (the-port)))
      
      ;; identifier
      ((: alpha (* (or #\_ alpha digit)))
       (let* ((string  (the-string))
	      (symbol  (string->symbol string)))
	  (cond
	     ((eq? symbol 'nil)
	      (list 'CONSTANT
		    (the-port)
		    'nil
		    (make-location (input-port-name (the-port))
				   (input-port-position (the-port)))))
	     ((getprop symbol 'reserved)
	      ;; this is a keyword
	      (list (string->symbol (string-upcase string))
		    (the-port)
		    (make-location
		     (input-port-name (the-port))
		     (input-port-position (the-port)))))
	     (else
	      ;; this is a regular identifier
	      (list 'IDENT
		    (the-port)
		    (instantiate::ident
		       (name  string)
		       (location (make-location
				  (input-port-name (the-port))
				  (input-port-position (the-port))))))))))
      
      ;; error
      (else
       (let ((c (the-failure)))
	  (if (eof-object? c)
	      c
	      (list 'ERROR (the-port) c))))))

