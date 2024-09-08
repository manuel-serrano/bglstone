;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Expand/initial.scm          */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Dec 28 15:41:05 1994                          */
;*    Last change :  Thu Jan 13 09:47:44 2005 (serrano)                */
;*    Copyright   :  1994-2005 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    Initial compiler expanders.                                      */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module expand_install
   (include "Tools/location.sch")
   (import  expand_if
	    expand_lambda
	    expand_define
	    expand_expander
	    expand_exit
	    expand_garithmetique
	    expand_iarithmetique
	    expand_farithmetique
	    expand_let
	    expand_case
	    expand_struct
	    expand_map
	    expand_assert
	    expand_object
	    tools_progn
	    tools_misc
	    tools_location
	    tools_error
	    engine_param
	    expand_expander
	    expand_srfi-0
	    type_type
	    ast_ident)
   (export  (install-initial-expander)))

;*---------------------------------------------------------------------*/
;*    install-initial-expander ...                                     */
;*---------------------------------------------------------------------*/
(define (install-initial-expander)
   ;; In order to be able to install O-macros,
   ;; we first of all, we perform Oenv initialization
   (initialize-Oenv!)
   (initialize-Genv!)
   
   ;; if
   (install-compiler-expander 'if expand-if)
   
   ;; or
   (install-compiler-expander 'or expand-or)
   
   ;; and
   (install-compiler-expander 'and expand-and)
   
   ;; not
   (install-compiler-expander 'not expand-not)
   
   ;; lambda
   (install-compiler-expander 'lambda expand-lambda)
   
   ;; define
   (install-compiler-expander 'define expand-define)
   
   ;; define-inline
   (install-compiler-expander 'define-inline expand-inline)
   
   ;; define-generic
   (install-compiler-expander 'define-generic expand-generic)
   
   ;; define-method
   (install-compiler-expander 'define-method expand-method)
   
   ;; define-struct
   (install-compiler-expander 'define-struct expand-struct)
   
   ;; set!
   (install-compiler-expander 'set! expand-set!)
   
   ;; set-exit
   (install-compiler-expander 'set-exit expand-set-exit)
   
   ;; jump-exit
   (install-compiler-expander 'jump-exit expand-jump-exit)
   
   ;; bind-exit
   (install-compiler-expander 'bind-exit expand-bind-exit)
   
   ;; unwind-protect
   (install-compiler-expander 'unwind-protect expand-unwind-protect)

   ;; error 
   (install-O-comptime-expander
    'error
    (lambda (x::obj e::procedure)
       (let ((loc (find-location x)))
	  (if (and (location? loc) *error-localization*)
	      (match-case x
		 ((?- ?l1 ?l2 ?l3)
		  `(begin
		      (error/c-location ,(e l1 e)
					,(e l2 e)
					,(e l3 e)
					,(location-fname loc)
					,(location-pos loc))
		      (error #f #f #f)))
		 ((?- . ?list)
		  `(error ,@(map (lambda (l) (e l e)) list))))
	      `(error ,@(map (lambda (l) (e l e)) (cdr x)))))))
   
   ;; warning
   (install-O-comptime-expander
    'warning
    (lambda (x::obj e::procedure)
       (let ((loc (find-location x)))
	  (if (and (location? loc) *error-localization*)
	      `(warning/c-location ,(location-fname loc)
				   ,(location-pos loc)
				   ,@(map (lambda (l) (e l e))
					  (cdr x)))
	      `(warning ,@(map (lambda (l) (e l e)) (cdr x)))))))
   
   ;; append
   (install-O-comptime-expander
    'append
    (lambda (x::obj e::procedure)
       (match-case x
	  ((?- ?l1 ?l2)
	   `(append-2 ,(e l1 e) ,(e l2 e)))
	  ((?- . ?lists)
	   `(append
	     ,@(map (lambda (l) (e l e)) lists)))
	  (else
	   (error #f "Illegal `append' form" x)))))
   (install-G-comptime-expander
    'append
    (lambda (x::obj e::procedure)
       (call-check x 'list? "list" e)))
   
   ;; eappend
   (install-O-comptime-expander
    'eappend
    (lambda (x::obj e::procedure)
       (match-case x
	  ((?- ?l1 ?l2)
	   `(eappend-2 ,(e l1 e) ,(e l2 e)))
	  ((?- . ?lists)
	   `(eappend ,@(map (lambda (l) (e l e)) lists)))
	  (else
	   (error #f "Illegal `eappend' form" x)))))
   
   ;; string-length
   (install-O-comptime-expander
    'string-length
    (lambda (x::obj e::procedure)
       (match-case x
	  ((?- ?s)
	   (if (string? s)
	       (string-length s)
	       `(string-length ,(e s e))))
	  (else
	   (error #f "Illegal 'string-length' form" x)))))
   
   ;; cons
   (install-O-comptime-expander
    'cons
    (lambda (x::obj e::procedure)
       (match-case x
	  ((?fun ?a ?d)
	   `(c-cons ,(e a e) ,(e d e)))
	  (else
	   (error #f "Illegal `cons' form" x)))))
   
   ;; map
   (install-O-comptime-expander 'map expand-map)
   (install-G-comptime-expander 'map
				(lambda (x::obj e::procedure)
				   (map-check-non-null x e ''())))
   
   ;; for-each
   (install-O-comptime-expander 'for-each expand-for-each)
   (install-G-comptime-expander 'for-each
				(lambda (x::obj e::procedure)
				   (map-check-non-null x e #unspecified)))

   ;; filter and filter!
   (install-G-comptime-expander 'filter
				(lambda (x::obj e::procedure)
				   (map-check x e)))
   (install-G-comptime-expander 'filter!
				(lambda (x::obj e::procedure)
				   (map-check x e)))
   
   ;; any? / every?
   (install-O-comptime-expander 'any? expand-any?)
   (install-G-comptime-expander 'any?
				(lambda (x::obj e::procedure)
				   (map-check x e)))
   (install-O-comptime-expander 'every? expand-every?)
   (install-G-comptime-expander 'every?
				(lambda (x::obj e::procedure)
				   (map-check x e)))
   
   ;; any / every
   (install-G-comptime-expander 'any
				(lambda (x::obj e::procedure)
				   (map-check x e)))
   (install-G-comptime-expander 'every
				(lambda (x::obj e::procedure)
				   (map-check x e)))
   
   ;; equal?
   (install-O-comptime-expander
    'equal?
    (lambda (x::obj e::procedure)
       (match-case x
	  ((?- ?a1 ?a2)
	   `(,(if (or (fixnum? a1)
		      (fixnum? a2)
		      (and (pair? a1)
			   (eq? (car a1) 'quote)
			   (symbol? (cadr a1)))
		      (and (pair? a2)
			   (eq? (car a2) 'quote)
			   (symbol? (cadr a2))))
		  'eq?
		  'equal?)
	     ,(e a1 e)
	     ,(e a2 e)))
	  (else
	   (error #f "Illegal `equal?' form" x)))))
   
   ;; les procedures arithmetiques
   (if *genericity*
       (begin
	  ;; +
	  (install-O-comptime-expander '+ expand-g+)
	  ;; *
	  (install-O-comptime-expander '* expand-g*)
	  ;; /
	  (install-O-comptime-expander '/ expand-g/)
	  ;; -
	  (install-O-comptime-expander '- expand-g-)
	  ;; =
	  (install-O-comptime-expander '= expand-g=)
	  ;; <
	  (install-O-comptime-expander '< expand-g<)
	  ;; >
	  (install-O-comptime-expander '> expand-g>)
	  ;; <=
	  (install-O-comptime-expander '<= expand-g<=)
	  ;; >=
	  (install-O-comptime-expander '>= expand-g>=))
       (begin
	  ;; +
	  (install-O-comptime-expander '+ expand-i+)
	  ;; *
	  (install-O-comptime-expander '* expand-i*)
	  ;; /
	  (install-O-comptime-expander '/ expand-i/)
	  ;; -
	  (install-O-comptime-expander '- expand-i-)
	  ;; =
	  (install-O-comptime-expander '= expand-i=)
	  ;; <
	  (install-O-comptime-expander '< expand-i<)
	  ;; >
	  (install-O-comptime-expander '> expand-i>)
	  ;; <=
	  (install-O-comptime-expander '<= expand-i<=)
	  ;; >=
	  (install-O-comptime-expander '>= expand-i>=)))
   
   (install-G-comptime-expander '+ (lambda (x::obj e::procedure)
				      (call-check x 'number? "number" e)))
   (install-G-comptime-expander '* (lambda (x::obj e::procedure)
				      (call-check x 'number? "number" e)))
   (install-G-comptime-expander '/ (lambda (x::obj e::procedure)
				      (call-check x 'number? "number" e)))
   (install-G-comptime-expander '- (lambda (x::obj e::procedure)
				      (call-check x 'number? "number" e)))
   (install-G-comptime-expander '= (lambda (x::obj e::procedure)
				      (call-check x 'number? "number" e)))
   (install-G-comptime-expander '> (lambda (x::obj e::procedure)
				      (call-check x 'number? "number" e)))
   (install-G-comptime-expander '< (lambda (x::obj e::procedure)
				      (call-check x 'number? "number" e)))
   (install-G-comptime-expander '>= (lambda (x::obj e::procedure)
				      (call-check x 'number? "number" e)))
   (install-G-comptime-expander '<= (lambda (x::obj e::procedure)
				       (call-check x 'number? "number" e)))
   (install-G-comptime-expander 'cos (lambda (x::obj e::procedure)
					(call-check x 'number? "number" e)))
   (install-G-comptime-expander 'sin (lambda (x::obj e::procedure)
					(call-check x 'number? "number" e)))
   (install-G-comptime-expander 'min (lambda (x::obj e::procedure)
					(call-check x 'number? "number" e)))
   (install-G-comptime-expander 'max (lambda (x::obj e::procedure)
					(call-check x 'number? "number" e)))

   ;; eq?
   (install-O-comptime-expander 'eq? expand-eq?)
   
   ;; +fx
   (install-O-comptime-expander '+fx expand-+fx)
   
   ;; -fx
   (install-O-comptime-expander '-fx expand--fx)
   
   ;; maxfl
   (install-O-comptime-expander 'maxfl expand-fmax)
   
   ;; minfl
   (install-O-comptime-expander 'minfl expand-fmin)
   
   ;; atanfl
   (install-O-comptime-expander 'atanfl expand-fatan)
   
   ;; sqrtfl
   (install-O-comptime-expander
    'sqrtfl
    (lambda (x::obj e::procedure)
       (match-case x
	  ((?- ?n)
	   (if *unsafe-range*
	       `(sqrtfl-ur ,(e n e))
	       `(sqrtfl ,(e n e))))
	  (else
	   (error #f "Illegal `sqrtfl' call" x)))))
   ;; atan-2fl
   (install-O-comptime-expander
    'atan-2fl
    (lambda (x::obj e::procedure)
       (match-case x
	  ((?- ?n ?m)
	   (if *unsafe-range*
	       `(atan-2fl-ur ,(e n e) ,(e m e))
	       `(atan-2fl ,(e n e) ,(e m e))))
	  (else
	   (error #f "Illegal `atan-2fl' call" x)))))
   ;; let*
   (install-compiler-expander 'let* expand-let*)
   
   ;; let
   (install-compiler-expander 'let expand-let)
   
   ;; letrec
   (install-compiler-expander 'letrec expand-letrec)
   
   ;; labels
   (install-compiler-expander 'labels expand-labels)
   
   ;; case
   (install-compiler-expander 'case expand-case)
   
   ;; read
   (install-O-comptime-expander
    'read
    (lambda (x::obj e::procedure)
       (match-case x
	  ((?- ?port)
	   `(read ,(e port e)))
	  ((?- ?port ?value)
	   `(read ,(e port e) ,(e value e)))
	  ((?-)
	   `(read ((@ current-input-port __r4_ports_6_10_1))))
	  (else
	   (error #f "Illegal `read' form" x)))))
   
   ;; read/rp
   (install-O-comptime-expander
    'read/rp
    (lambda (x::obj e::procedure)
       (match-case x
	  ((?- . ?opts)
	   `(read/rp ,@(map (lambda (x) (e x e)) opts)))
	  (else
	   (error #f "Illegal `read/rp' form" x)))))
   
   ;; vector
   (install-O-comptime-expander
    'vector
    (lambda (x::obj e::procedure)
       (let ((args (cdr x))
	     (v    (mark-symbol-non-user!
		    (gensym 'v))))
	  (e `(let ((,v (c-create-vector
			 ,(length args))))
		 ,@(let loop ((i    0)
			      (args args)
			      (res  '()))
		      (if (null? args)
			  res
			  (loop (+fx i 1)
				(cdr args)
				(cons
				 (epairify
				  `(vector-set-ur! ,v ,i ,(car args)) x)
				 res))))
		 ,v)
	     e))))
   
   ;; make-vector
   (install-O-comptime-expander
    'make-vector
    (lambda (x::obj e::procedure)
       (match-case x
	  ((?- ?n)
	   `(c-make-vector ,(e n e) ,(e '(unspecified) e)))
	  ((?- ?n ?init)
	   `(c-make-vector ,(e n e) ,(e init e)))
	  (else
	   (map (lambda (x) (e x e)) x)))))
   ;; vector-set!
   (install-O-comptime-expander
    'vector-set!
    (lambda (x::obj e::procedure)
       (match-case x
	  ((?- ?vec ?k ?obj)
	   (let ((evec (e vec e))
		 (ek   (e k e))
		 (eobj (e obj e)))
	      (if *unsafe-range*
		  `(vector-set-ur! ,evec ,ek ,eobj)
		  `(vector-set! ,evec ,ek ,eobj))))
	  (else
	   (error #f "Illegal `vector-set!' form" x)))))
   (install-G-comptime-expander
    'vector-set!
    (lambda (x::obj e::procedure)
       (bound-check x 'vector-length 'vector-bound-check? e)))
   ;; vector-ref
   (install-O-comptime-expander
    'vector-ref
    (lambda (x::obj e::procedure)
       (match-case x
	  ((?- ?vec ?k)
	   (let ((evec (e vec e))
		 (ek   (e k e)))
	      (if *unsafe-range*
		  `(vector-ref-ur ,evec ,ek)
		  `(vector-ref ,evec ,ek))))
	  (else
	   (error #f "Illegal `vector-ref' form" x)))))
   (install-G-comptime-expander
    'vector-ref
    (lambda (x::obj e::procedure)
       (bound-check x 'vector-length 'vector-bound-check? e)))

   ;; string-append
   (install-O-comptime-expander
    'string-append
    (lambda (x::obj e::procedure)
       (match-case x
	  ((?- ?str1)
	   (e str1 e))
	  ((?- ?str1 ?str2)
	   `(c-string-append ,(e str1 e) ,(e str2 e)))
	  ((?- ?str1 ?str2 ?str3)
	   `(c-string-append-3 ,(e str1 e) ,(e str2 e) ,(e str3 e)))
	  (else
	   (map (lambda (x) (e x e)) x)))))
   (install-G-comptime-expander
    'string-append
    (lambda (x::obj e::procedure)
       (call-check x 'string? "string" e)))
   (install-G-comptime-expander
    'symbol-append
    (lambda (x::obj e::procedure)
       (call-check x 'symbol? "symbol" e)))
   
   ;; substring
   (install-O-comptime-expander
    'substring
    (lambda (x::obj e::procedure)
       (match-case x
	  ((?- ?s ?min ?max)
	   (let ((s   (e s e))
		 (min (e min e))
		 (max (e max e)))
	      (if *unsafe-range*
		  `(substring-ur ,s ,min ,max)
		  `(substring ,s ,min ,max))))
	  (else
	   (map (lambda (x) (e x e)) x)))))
   
   ;; string-set!
   (install-O-comptime-expander
    'string-set!
    (lambda (x::obj e::procedure)
       (match-case x
	  ((?- ?vec ?k ?obj)
	   (let ((evec (e vec e))
		 (ek   (e k e))
		 (eobj (e obj e)))
	      (if *unsafe-range*
		  `(string-set-ur! ,evec ,ek ,eobj)
		  `(string-set! ,evec ,ek ,eobj))))
	  (else
	   (error #f "Illegal `string-set!' form" x)))))
   (install-G-comptime-expander
    'string-set!
    (lambda (x::obj e::procedure)
       (bound-check x 'string-length 'string-bound-check? e)))
   
   
   ;; string-ref
   (install-O-comptime-expander
    'string-ref
    (lambda (x::obj e::procedure)
       (match-case x
	  ((?- ?vec ?k)
	   (let ((evec (e vec e))
		 (ek   (e k e)))
	      (if *unsafe-range*
		  `(string-ref-ur ,evec ,ek)
		  `(string-ref ,evec ,ek))))
	  (else
	   (error #f "Illegal `string-ref' form" x)))))
   (install-G-comptime-expander
    'string-ref
    (lambda (x::obj e::procedure)
       (bound-check x 'string-length 'string-bound-check? e)))
   
   ;; blit-string!
   (install-O-comptime-expander
    'blit-string!
    (lambda (x::obj e::procedure)
       (match-case x
	  ((?- ?s1 ?o1 ?s2 ?o2 ?l)
	   (let ((s1 (e s1 e))
		 (o1 (e o1 e))
		 (s2 (e s2 e))
		 (o2 (e o2 e))
		 (l  (e l e)))
	      (if *unsafe-range*
		  `(blit-string-ur! ,s1
				    ,o1
				    ,s2
				    ,o2
				    ,l)
		  `(blit-string! ,s1
				 ,o1
				 ,s2
				 ,o2
				 ,l))))
	  (else
	   (error #f "Illegal `blit-string!' form" x)))))
   ;; integer->char
   (install-O-comptime-expander
    'integer->char
    (lambda (x::obj e::procedure)
       (match-case x
	  ((?- ?n)
	   (if *unsafe-range*
	       `(integer->char-ur ,(e n e))
	       `(integer->char ,(e n e))))
	  (else
	   (error #f
		  "Illegal `integer->char' call"
		  x)))))
   
   ;; cons*
   (install-O-comptime-expander
    'cons*
    (lambda (x::obj e::procedure)
       (match-case x
	  ((?- ?x1)
	   (e x1 e))
	  ((?- ?x1 ?x2)
	   `(c-cons ,(e x1 e) ,(e x2 e)))
	  ((?- ?x1 ?x2 . ?rest)
	   `(c-cons ,(e x1 e)
		    ,(e (epairify
			 `(cons* ,x2 ,@rest)
			 x)
			e)))
	  (else
	   (map (lambda (x) (e x e)) x)))))

   ;; apply
   (install-O-comptime-expander
    'apply
    (lambda (x::obj e::procedure)
       (match-case x
	  ((?- ?function ?one-arg)
	   `(apply ,(e function e)
		   ,(e one-arg e)))
	  ((?- ?function . ?args)
	   `(apply ,(e function e)
		   ,(e (epairify
			`((@ cons*
			     __r4_pairs_and_lists_6_3)
			  ,@args)
			x)
		       e)))
	  (else
	   (error #f
		  "Illegal `apply' form"
		  x)))))

   ;; newline
   (install-O-comptime-expander
    'newline
    (lambda (x::obj e::procedure)
       (match-case x
	  ((?-)
	   `((@ newline-1 __r4_output_6_10_3)
	     ((@ current-output-port
		 __r4_ports_6_10_1))))
	  ((?- ?port)
	   `((@ newline-1 __r4_output_6_10_3)
	     ,(e port e)))
	  (else
	   (map (lambda (x) (e x e)) x)))))
   ;; display
   (install-O-comptime-expander
    'display
    (lambda (x::obj e::procedure)
       (match-case x
	  ((?- ?o)
	   `(,(disp o) ,(e o e) ((@ current-output-port __r4_ports_6_10_1))))
	  ((?- ?o ?port)
	   `(,(disp o) ,(e o e) ,(e port e)))
	  (else
	   (map (lambda (x) (e x e)) x)))))
   ;; write-char
   (install-O-comptime-expander
    'write-char
    (lambda (x::obj e::procedure)
       (match-case x
	  ((?- ?obj)
	   `((@ write-char-2 __r4_output_6_10_3)
	     ,(e obj e)
	     ((@ current-output-port __r4_ports_6_10_1))))
	  ((?- ?obj ?port)
	   `((@ write-char-2 __r4_output_6_10_3) ,(e obj e) ,(e port e)))
	  (else
	   (map (lambda (x) (e x e)) x)))))

   ;; print
   (install-O-comptime-expander
    'print
    (lambda (x::obj e::procedure)
       (match-case x
	  ((?-)
	   `((@ newline-1 __r4_output_6_10_3)
	     ((@ current-output-port __r4_ports_6_10_1))))
	  ((?- . ?obj)
	   (let ((p (mark-symbol-non-user! (gensym 'port))))
	      (e `(let ((,p ((@ current-output-port __r4_ports_6_10_1))))
		     ,@(map
			(lambda (y) (epairify `(,(disp y) ,y ,p) x))
			obj)
		     ((@ newline-1 __r4_output_6_10_3) ,p))
		 e))))))
   ;; fprint
   (install-O-comptime-expander
    'fprint
    (lambda (x::obj e::procedure)
       (match-case x
	  ((?- ?port)
	   (e `((@ newline-1 __r4_output_6_10_3)
		,port) e))
	  ((?- ?port . ?obj)
	   (let ((aux (mark-symbol-non-user! (gensym 'port))))
	      (e `(let ((,aux ,port))
		     ,@(map (lambda (y)
			       (epairify `(,(disp y) ,y ,aux) x))
			    obj)
		     ((@ newline-1 __r4_output_6_10_3) ,aux))
		 e)))
	  (else
	   (map (lambda (x) (e x e)) x)))))

   ;; inexact->exact
   (install-G-comptime-expander
    'inexact->exact
    (lambda (x::obj e::procedure)
       (call-check x 'number? "number" e)))
   
   ;; exact->inexact
   (install-G-comptime-expander
    'exact->inexact
    (lambda (x::obj e::procedure)
       (call-check x 'number? "number" e)))
   
   ;; values
   (install-O-comptime-expander
    'values
    (lambda (x e)
       (match-case x
	  ((?-)
	   '((@ %set-mvalues-number! __r5_control_features_6_4) 0))
	  ((?- ?val0)
	   (let ((g0 (mark-symbol-non-user! (gensym 'val0_))))
	      `(let ((,g0 ,(e val0 e)))
		  ((@ %set-mvalues-number! __r5_control_features_6_4) 1)
		  ,g0)))
	  ((?- ?val0 ?val1)
	   (let ((g0 (mark-symbol-non-user! (gensym 'val0_)))
		 (g1 (mark-symbol-non-user! (gensym 'val1_))))
	      `(let ((,g0 ,(e val0 e))
		     (,g1 ,(e val1 e)))
		  ((@ %set-mvalues-number! __r5_control_features_6_4) 2)
		  ((@ %set-mvalues-val! __r5_control_features_6_4) 1 ,g1)
		  ,g0)))
	  ((?- ?val0 ?val1 ?val2)
	   (let ((g0 (mark-symbol-non-user! (gensym 'val0_)))
		 (g1 (mark-symbol-non-user! (gensym 'val1_)))
		 (g2 (mark-symbol-non-user! (gensym 'val2_))))
	      `(let ((,g0 ,(e val0 e))
		     (,g1 ,(e val1 e))
		     (,g2 ,(e val2 e)))
		  ((@ %set-mvalues-number! __r5_control_features_6_4) 3)
		  ((@ %set-mvalues-val! __r5_control_features_6_4) 1 ,g1)
		  ((@ %set-mvalues-val! __r5_control_features_6_4) 2 ,g2)
		  ,g0)))
	  ((?- ?val0 ?val1 ?val2 ?val3)
	   (let ((g0 (mark-symbol-non-user! (gensym 'val0_)))
		 (g1 (mark-symbol-non-user! (gensym 'val1_)))
		 (g2 (mark-symbol-non-user! (gensym 'val2_)))
		 (g3 (mark-symbol-non-user! (gensym 'val3_))))
	      `(let ((,g0 ,(e val0 e))
		     (,g1 ,(e val1 e))
		     (,g2 ,(e val2 e))
		     (,g3 ,(e val3 e)))
		  ((@ %set-mvalues-number! __r5_control_features_6_4) 4)
		  ((@ %set-mvalues-val! __r5_control_features_6_4) 1 ,g1)
		  ((@ %set-mvalues-val! __r5_control_features_6_4) 2 ,g2)
		  ((@ %set-mvalues-val! __r5_control_features_6_4) 3 ,g3)
		  ,g0)))
	  ((?- ?val0 ?val1 ?val2 ?val3 ?val4)
	   (let ((g0 (mark-symbol-non-user! (gensym 'val0_)))
		 (g1 (mark-symbol-non-user! (gensym 'val1_)))
		 (g2 (mark-symbol-non-user! (gensym 'val2_)))
		 (g3 (mark-symbol-non-user! (gensym 'val3_)))
		 (g4 (mark-symbol-non-user! (gensym 'val4_))))
	      `(let ((,g0 ,(e val0 e))
		     (,g1 ,(e val1 e))
		     (,g2 ,(e val2 e))
		     (,g3 ,(e val3 e))
		     (,g4 ,(e val4 e)))
		  ((@ %set-mvalues-number! __r5_control_features_6_4) 5)
		  ((@ %set-mvalues-val! __r5_control_features_6_4) 1 ,g1)
		  ((@ %set-mvalues-val! __r5_control_features_6_4) 2 ,g2)
		  ((@ %set-mvalues-val! __r5_control_features_6_4) 3 ,g3)
		  ((@ %set-mvalues-val! __r5_control_features_6_4) 4 ,g4)
		  ,g0)))
	  ((?- ?val0 ?val1 ?val2 ?val3 ?val4 ?val5)
	   (let ((g0 (mark-symbol-non-user! (gensym 'val0_)))
		 (g1 (mark-symbol-non-user! (gensym 'val1_)))
		 (g2 (mark-symbol-non-user! (gensym 'val2_)))
		 (g3 (mark-symbol-non-user! (gensym 'val3_)))
		 (g4 (mark-symbol-non-user! (gensym 'val4_)))
		 (g5 (mark-symbol-non-user! (gensym 'val5_))))
	      `(let ((,g0 ,(e val0 e))
		     (,g1 ,(e val1 e))
		     (,g2 ,(e val2 e))
		     (,g3 ,(e val3 e))
		     (,g4 ,(e val4 e))
		     (,g5 ,(e val5 e)))
		  ((@ %set-mvalues-number! __r5_control_features_6_4) 6)
		  ((@ %set-mvalues-val! __r5_control_features_6_4) 1 ,g1)
		  ((@ %set-mvalues-val! __r5_control_features_6_4) 2 ,g2)
		  ((@ %set-mvalues-val! __r5_control_features_6_4) 3 ,g3)
		  ((@ %set-mvalues-val! __r5_control_features_6_4) 4 ,g4)
		  ((@ %set-mvalues-val! __r5_control_features_6_4) 5 ,g5)
		  ,g0)))
	  ((?- ?val0 ?val1 ?val2 ?val3 ?val4 ?val5 ?val6)
	   (let ((g0 (mark-symbol-non-user! (gensym 'val0_)))
		 (g1 (mark-symbol-non-user! (gensym 'val1_)))
		 (g2 (mark-symbol-non-user! (gensym 'val2_)))
		 (g3 (mark-symbol-non-user! (gensym 'val3_)))
		 (g4 (mark-symbol-non-user! (gensym 'val4_)))
		 (g5 (mark-symbol-non-user! (gensym 'val5_)))
		 (g6 (mark-symbol-non-user! (gensym 'val6_))))
	      `(let ((,g0 ,(e val0 e))
		     (,g1 ,(e val1 e))
		     (,g2 ,(e val2 e))
		     (,g3 ,(e val3 e))
		     (,g4 ,(e val4 e))
		     (,g5 ,(e val5 e))
		     (,g6 ,(e val6 e)))
		  ((@ %set-mvalues-number! __r5_control_features_6_4) 7)
		  ((@ %set-mvalues-val! __r5_control_features_6_4) 1 ,g1)
		  ((@ %set-mvalues-val! __r5_control_features_6_4) 2 ,g2)
		  ((@ %set-mvalues-val! __r5_control_features_6_4) 3 ,g3)
		  ((@ %set-mvalues-val! __r5_control_features_6_4) 4 ,g4)
		  ((@ %set-mvalues-val! __r5_control_features_6_4) 5 ,g5)
		  ((@ %set-mvalues-val! __r5_control_features_6_4) 6 ,g6)
		  ,g0)))
	  ((?- ?val0 ?val1 ?val2 ?val3 ?val4 ?val5 ?val6 ?val7)
	   (let ((g0 (mark-symbol-non-user! (gensym 'val0_)))
		 (g1 (mark-symbol-non-user! (gensym 'val1_)))
		 (g2 (mark-symbol-non-user! (gensym 'val2_)))
		 (g3 (mark-symbol-non-user! (gensym 'val3_)))
		 (g4 (mark-symbol-non-user! (gensym 'val4_)))
		 (g5 (mark-symbol-non-user! (gensym 'val5_)))
		 (g6 (mark-symbol-non-user! (gensym 'val6_)))
		 (g7 (mark-symbol-non-user! (gensym 'val7_))))
	      `(let ((,g0 ,(e val0 e))
		     (,g1 ,(e val1 e))
		     (,g2 ,(e val2 e))
		     (,g3 ,(e val3 e))
		     (,g4 ,(e val4 e))
		     (,g5 ,(e val5 e))
		     (,g6 ,(e val6 e))
		     (,g7 ,(e val7 e)))
		  ((@ %set-mvalues-number! __r5_control_features_6_4) 8)
		  ((@ %set-mvalues-val! __r5_control_features_6_4) 1 ,g1)
		  ((@ %set-mvalues-val! __r5_control_features_6_4) 2 ,g2)
		  ((@ %set-mvalues-val! __r5_control_features_6_4) 3 ,g3)
		  ((@ %set-mvalues-val! __r5_control_features_6_4) 4 ,g4)
		  ((@ %set-mvalues-val! __r5_control_features_6_4) 5 ,g5)
		  ((@ %set-mvalues-val! __r5_control_features_6_4) 6 ,g6)
		  ((@ %set-mvalues-val! __r5_control_features_6_4) 7 ,g7)
		  ,g0)))
	  (else
	   (let ((g0 (mark-symbol-non-user! (gensym 'val_))))
	      `(let ((,g0 (list ,@(map (lambda (x) (e x e)) (cdr x)))))
		  ((@ %set-mvalues-number! __r5_control_features_6_4) -1)
		  ,g0))))))
   
   ;; call-with-values
   (install-O-comptime-expander
    'call-with-values
    (lambda (x e)
       (e (match-case x
	     ((?- ?producer (lambda () . ?body))
	      `(begin
		  ,(match-case producer
		      ((lambda () . ?prod)
		       `(begin ,@prod))
		      (else
		       `(,producer)))
		  ,@body))
	     ((?- ?producer (lambda (?v0) . ?body))
	      `(let ((,v0 ,(match-case producer
			      ((lambda () . ?prod)
			       `(begin ,@prod))
			      (else
			       `(,producer)))))
		  ,@body))
	     ((?- ?producer (lambda (?v0 ?v1) . ?body))
	      `(let ((,v0 ,(match-case producer
			      ((lambda () . ?prod)
			       `(begin ,@prod))
			      (else
			       `(,producer)))))
		  (let ((,v1 ((@ %get-mvalues-val __r5_control_features_6_4) 1)))
		     ,@body)))
	     ((?- ?producer (lambda (?v0 ?v1 ?v2) . ?body))
	      `(let ((,v0 ,(match-case producer
			      ((lambda () . ?prod)
			       `(begin ,@prod))
			      (else
			       `(,producer)))))
		  (let ((,v1 ((@ %get-mvalues-val __r5_control_features_6_4) 1))
			(,v2 ((@ %get-mvalues-val __r5_control_features_6_4) 2)))
		     ,@body)))
	     ((?- ?producer (lambda (?v0 ?v1 ?v2 ?v3) . ?body))
	      `(let ((,v0 ,(match-case producer
			      ((lambda () . ?prod)
			       `(begin ,@prod))
			      (else
			       `(,producer)))))
		  (let ((,v1 ((@ %get-mvalues-val __r5_control_features_6_4) 1))
			(,v2 ((@ %get-mvalues-val __r5_control_features_6_4) 2))
			(,v3 ((@ %get-mvalues-val __r5_control_features_6_4) 3)))
		     ,@body)))
	     ((?- ?producer (lambda (?v0 ?v1 ?v2 ?v3 ?v4) . ?body))
	      `(let ((,v0 ,(match-case producer
			      ((lambda () . ?prod)
			       `(begin ,@prod))
			      (else
			       `(,producer)))))
		  (let ((,v1 ((@ %get-mvalues-val __r5_control_features_6_4) 1))
			(,v2 ((@ %get-mvalues-val __r5_control_features_6_4) 2))
			(,v3 ((@ %get-mvalues-val __r5_control_features_6_4) 3))
			(,v4 ((@ %get-mvalues-val __r5_control_features_6_4) 4)))
		     ,@body)))
	     ((?- ?producer (lambda (?v0 ?v1 ?v2 ?v3 ?v4 ?v5) . ?body))
	      `(let ((,v0 ,(match-case producer
			      ((lambda () . ?prod)
			       `(begin ,@prod))
			      (else
			       `(,producer)))))
		  (let ((,v1 ((@ %get-mvalues-val __r5_control_features_6_4) 1))
			(,v2 ((@ %get-mvalues-val __r5_control_features_6_4) 2))
			(,v3 ((@ %get-mvalues-val __r5_control_features_6_4) 3))
			(,v4 ((@ %get-mvalues-val __r5_control_features_6_4) 4))
			(,v5 ((@ %get-mvalues-val __r5_control_features_6_4) 5)))
		     ,@body)))
	     ((?- ?producer (lambda (?v0 ?v1 ?v2 ?v3 ?v4 ?v5 ?v6) . ?body))
	      `(let ((,v0 ,(match-case producer
			      ((lambda () . ?prod)
			       `(begin ,@prod))
			      (else
			       `(,producer)))))
		  (let ((,v1 ((@ %get-mvalues-val __r5_control_features_6_4) 1))
			(,v2 ((@ %get-mvalues-val __r5_control_features_6_4) 2))
			(,v3 ((@ %get-mvalues-val __r5_control_features_6_4) 3))
			(,v4 ((@ %get-mvalues-val __r5_control_features_6_4) 4))
			(,v5 ((@ %get-mvalues-val __r5_control_features_6_4) 5))
			(,v6 ((@ %get-mvalues-val __r5_control_features_6_4) 6)))
		     ,@body)))
	     ((?- ?producer (lambda (?v0 ?v1 ?v2 ?v3 ?v4 ?v5 ?v6 ?v7) . ?body))
	      `(let ((,v0 ,(match-case producer
			      ((lambda () . ?prod)
			       `(begin ,@prod))
			      (else
			       `(,producer)))))
		  (let ((,v1 ((@ %get-mvalues-val __r5_control_features_6_4) 1))
			(,v2 ((@ %get-mvalues-val __r5_control_features_6_4) 2))
			(,v3 ((@ %get-mvalues-val __r5_control_features_6_4) 3))
			(,v4 ((@ %get-mvalues-val __r5_control_features_6_4) 4))
			(,v5 ((@ %get-mvalues-val __r5_control_features_6_4) 5))
			(,v6 ((@ %get-mvalues-val __r5_control_features_6_4) 6))
			(,v7 ((@ %get-mvalues-val __r5_control_features_6_4) 7)))
		     ,@body)))
	     (else
	      `((@ call-with-values __r5_control_features_6_4) ,@(cdr x))))
	  e)))
   
   ;; assert
   (install-compiler-expander 'assert expand-assert)
   
   ;; with-access
   (install-compiler-expander 'with-access expand-with-access)
   
   ;; instantiate
   (install-compiler-expander 'instantiate expand-instantiate)
   
   ;; co-instantiate
   (install-compiler-expander 'co-instantiate expand-co-instantiate)
   
   ;; duplicate
   (install-compiler-expander 'duplicate expand-duplicate)
   
   ;; widen!
   (install-compiler-expander 'widen! expand-widen!)
   
   ;; shrink!
   (install-compiler-expander 'shrink! expand-shrink!)
   
   ;; cond-expand
   (install-compiler-expander 'cond-expand expand-cond-expand)
   
   ;; profile
   (install-compiler-expander
    'profile
    (lambda (x e)
       (match-case x
	  ((?- (and (? symbol?) ?lbl) . ?exprs)
	   (if (or (not (number? *profile-mode*)) (= *profile-mode* 0))
	       (e `(begin ,@exprs) e)
	       (let* ((la  `(lambda () ,@exprs))
		      (lam (if (epair? x)
			       (econs (car la) (cdr la) (cer x))
			       la))
		      (val (let ((sym (gensym 'value)))
			      (mark-symbol-non-user! sym)
			      sym))
		      (aux `(let ((,lbl ,lam))
			       (GC-profile-push ,(symbol->string lbl) ,lbl)
			       (let ((,val (,lbl)))
				  (GC-profile-pop)
				  ,val)))
		      (res (if (epair? x)
			       (econs (car aux) (cdr aux) (cer x))
			       aux)))
		  (e aux e))))
	  (else
	   (error "profile" "Illegal form" x)))))

   ;; profile
   (install-compiler-expander
    'profile/gc
    (lambda (x e)
       (match-case x
	  ((?- (and (? symbol?) ?lbl) . ?exprs)
	   (if (or (not (number? *profile-mode*)) (= *profile-mode* 0))
	       (e `(begin ,@exprs) e)
	       (let* ((la  `(lambda () ,@exprs))
		      (lam (if (epair? x)
			       (econs (car la) (cdr la) (cer x))
			       la))
		      (val (let ((sym (gensym 'value)))
			      (mark-symbol-non-user! sym)
			      sym))
		      (aux `(let ((,lbl ,lam))
			       (GC-collect-profile-push ,(symbol->string lbl)
							,lbl)
			       (let ((,val (,lbl)))
				  (GC-profile-pop)
				  ,val)))
		      (res (if (epair? x)
			       (econs (car aux) (cdr aux) (cer x))
			       aux)))
		  (e aux e))))
	  (else
	   (error "profile" "Illegal form" x))))))
			 
;*---------------------------------------------------------------------*/
;*    call-check ...                                                   */
;*---------------------------------------------------------------------*/
(define (call-check x pred tname e)
   (let* ((fun (car x))
	  (actuals (cdr x))
	  (formals (map (lambda (x) (mark-symbol-non-user! (gensym))) actuals))
	  (msg (mark-symbol-non-user! (gensym))))
      `(let (,@(map (lambda (f v) (list f (e v e))) formals actuals)
	     (,msg ,(string-append (symbol->string fun) ": argument not a "
				   tname)))
	  ,(let loop ((args formals))
	      (if (null? args)
		  (cons fun formals)
		  `(if (,pred ,(car args))
		       ,(loop (cdr args))
		       (error #f ,msg ,(car args))))))))

;*---------------------------------------------------------------------*/
;*    bound-check ...                                                  */
;*---------------------------------------------------------------------*/
(define (bound-check x flen pred e)
   (match-case x
      ((?fun ?aobj ?aoff . ?rest)
       (let ((fobj (mark-symbol-non-user! (gensym)))
	     (foff (mark-symbol-non-user! (gensym)))
	     (len (mark-symbol-non-user! (gensym))))
	  `(let ((,fobj ,(e aobj e))
		 (,foff ,(e aoff e)))
	      (let ((,len (,flen ,fobj)))
		 (if (,pred ,foff ,len)
		     (,fun ,fobj ,foff ,@(map (lambda (x) (e x e)) rest))
		     (error #f
			    ,(string-append (symbol->string fun)
					    ": index out of bound")
			    ,foff))))))
      (else
       (error #f "Illegal expression" x))))

;*---------------------------------------------------------------------*/
;*    map-check-non-null ...                                           */
;*---------------------------------------------------------------------*/
(define (map-check-non-null x e null-val)
   (match-case x
      ((?name ?-)
       (user-warning name "used with only two arguments" x)
       null-val)
      (else
       (map-check x e))))

;*---------------------------------------------------------------------*/
;*    map-check ...                                                    */
;*---------------------------------------------------------------------*/
(define (map-check x e)
   (match-case x
      ((?op ?fun . ?actuals)
       (let ((formals (map (lambda (x) (mark-symbol-non-user! (gensym)))
			   actuals))
	     (lformals (map (lambda (x) (mark-symbol-non-user! (gensym)))
			    actuals))
	     (ufun (mark-symbol-non-user! (gensym)))
	     (msg-list (mark-symbol-non-user! (gensym))))
	  `(let (,@(map (lambda (f v) (list f (e v e))) formals actuals)
		   (,ufun ,(e fun e))
		   (,msg-list ,(string-append (symbol->string op)
					      ": argument not a list")))
	      (if (correct-arity? ,ufun ,(length actuals))
		  ,(let loop ((args formals))
		      (if (null? args)
			  (if (>fx (length actuals) 1)
			      `(let ,(map (lambda (lf f)
					     `(,lf (length ,f)))
					  lformals formals)
				  (if (= ,@lformals)
				      (,op ,ufun ,@formals)
				      (error #f ,(string-append
						  (symbol->string op)
						  ": various lists length")
					     (list ,@lformals))))
			      `(,op ,ufun ,@formals))
			  `(if (list? ,(car args))
			       ,(loop (cdr args))
			       (error #f ,msg-list ,(car args)))))
		  (error #f
			 ,(string-append (symbol->string op)
					 ": incorrect function arity")
			 ,(length actuals))))))
      (else
       (error #f (car x) "Illegal form"))))

;*---------------------------------------------------------------------*/
;*    disp ...                                                         */
;*---------------------------------------------------------------------*/
(define (disp obj)
   (cond
      ((string? obj)
       '(@ display-string foreign))
      ((and (pair? obj)
	    (eq? (car obj) 'quote)
	    (symbol? (cadr obj)))
       '(@ display-symbol foreign))
      ((fixnum? obj)
       '(@ display-fixnum foreign))
      ((flonum? obj)
       '(@ display-flonum foreign))
      ((char? obj)
       '(@ display-char foreign))
      (else
       '(@ display-2 __r4_output_6_10_3))))
