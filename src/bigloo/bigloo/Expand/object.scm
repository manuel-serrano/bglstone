;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Expand/object.scm           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri May  3 10:13:58 1996                          */
;*    Last change :  Mon Nov 29 13:01:25 2004 (serrano)                */
;*    Copyright   :  1996-2004 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The Object expanders                                             */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module expand_object
   (import  tools_args
	    tools_progn
	    tools_misc
	    tools_location
	    type_type
	    type_env
	    expand_eps
	    engine_param
	    ast_var
	    ast_ident
	    ast_private
	    object_class
	    object_slots)
   (export  (expand-with-access    ::obj ::procedure)
	    (expand-instantiate    ::obj ::procedure)
	    (expand-co-instantiate ::obj ::procedure)
	    (expand-duplicate      ::obj ::procedure)
	    (expand-widen!         ::obj ::procedure)
	    (expand-shrink!        ::obj ::procedure)))

;*---------------------------------------------------------------------*/
;*    expand-with-access ...                                           */
;*---------------------------------------------------------------------*/
(define (expand-with-access x e)
   (match-case x
      ((?with-access ?instance (and (? pair?) ?slots) . (and (? pair?) ?body))
       (let* ((loc   (find-location x))
	      (class (type-of-id with-access loc)))
	  (cond
	     ((not (tclass? class))
	      (error #f "Illegal `with-access' form" x))
	     (else
	      (let loop ((s slots))
		 (cond
		    ((null? s)
		     (let* ((aux (mark-symbol-non-user! (gensym 'instance)))
			    (mark aux)
			    (instance (e instance e))
			    (class-id (type-id class)))
			(multiple-value-bind (direct indexed)
			   (class-split-slots x class slots)
			   (replace! x
				     (with-lexical
				      slots
				      mark
				      (find-location x)
				      (lambda () 
					 (let ((e (with-access-expander
						   e
						   mark
						   aux
						   class-id
						   direct)))
					    (make-with-access-body aux
								   instance
								   class-id
								   indexed
								   body
								   e))))))))
		    ((not (pair? s))
		     (error #f "Illegal `with-access' form" x))
		    ((symbol? (car s))
		     (loop (cdr s)))
		    (else
		     (error #f
			    (string-append
			     "Illegal `with-access' form. Illegal slot identifier `"
			     (with-output-to-string
				(lambda () (display (car s))))
			     "'")
			    x))))))))
      (else
       (error #f "Illegal `with-access' form" x))))

;*---------------------------------------------------------------------*/
;*    class-split-slots ...                                            */
;*    -------------------------------------------------------------    */
;*    Split a slot list in two lists: the direct slots and the         */
;*    indexed slots.                                                   */
;*---------------------------------------------------------------------*/
(define (class-split-slots x class slots)
   (let ((mark (gensym 'with-access)))
      (for-each (lambda (slot)
		   (with-access::slot slot (id)
		      (putprop! id mark slot)))
		(tclass-all-slots class))
      (let loop ((slots slots)
		 (direct '())
		 (indexed '()))
	 (if (null? slots)
	     (values direct indexed)
	     (let ((slot (getprop (car slots) mark)))
		(if (not (slot? slot))
		    (error #f
			   (string-append
			    "Illegal `with-access' form. Unknown slot `"
			    (symbol->string (car slots))
			    "'")
			   x)
		    (if (slot-indexed slot)
			(loop (cdr slots)
			      direct
			      (cons slot indexed))
			(loop (cdr slots)
			      (cons (car slots) direct)
			      indexed))))))))

;*---------------------------------------------------------------------*/
;*    make-with-access-body ...                                        */
;*---------------------------------------------------------------------*/
(define (make-with-access-body aux instance class-id indexed body e)
   (define (make-indexed-body)
      (let loop ((indexed indexed))
	 (cond
	    ((null? indexed)
	     (e (normalize-progn body) e))
	    ((slot-read-only? (car indexed))
	     (let* ((slot (car indexed))
		    (sref (symbol-append (slot-id slot) '-ref))
		    (full-sref (symbol-append class-id '- sref))
		    (slen (symbol-append (slot-id slot) '-len))
		    (full-slen (symbol-append class-id '- slen))
		    (i (mark-symbol-non-user! (gensym 'index))))
		`(let ((,slen ,(e `(,full-slen ,aux) e)))
		    (labels ((,sref (,i) (,full-sref ,aux ,i)))
		       ,(loop (cdr indexed))))))
	    (else
	     (let* ((slot (car indexed))
		    (sref (symbol-append (slot-id slot) '-ref))
		    (full-sref (symbol-append class-id '- sref))
		    (sset (symbol-append (slot-id slot) '-set!))
		    (full-sset (symbol-append class-id '- sset))
		    (slen (symbol-append (slot-id slot) '-len))
		    (full-slen (symbol-append class-id '- slen))
		    (i (mark-symbol-non-user! (gensym 'index)))
		    (v (mark-symbol-non-user! (gensym 'value))))
		`(let ((,slen ,(e `(,full-slen ,aux) e)))
		    (labels ((,sref (,i) (,full-sref ,aux ,i))
			     (,sset (,i ,v) (,full-sset ,aux ,i ,v)))
		       ,(loop (cdr indexed)))))))))
   `(let ((,aux ,instance))
       ,(make-indexed-body)))

;*---------------------------------------------------------------------*/
;*    with-access-expander ...                                         */
;*---------------------------------------------------------------------*/
(define (with-access-expander olde mark instance class slots)
   (lambda (x e)
      (match-case x
	 ((and ?var (? symbol?))
	  (if (and (memq var slots)
		   (let ((cell (assq var (lexical-stack))))
		      (and (pair? cell) (eq? (cdr cell) mark))))
	      (olde `(,(symbol-append class '- var) ,instance) olde)
	      (olde var olde)))
	 ((set! (and (? symbol?) ?var) ?val)
	  (let ((val (e val e)))
	     (if (and (memq var slots)
		      (let ((cell (assq var (lexical-stack))))
			 (and (pair? cell) (eq? (cdr cell) mark))))
		 (object-epairify
		  (olde `(,(symbol-append class '- var '-set!) ,instance ,val)
			olde)
		  x)
		 (begin
		    (set-car! (cddr x) val)
		    (olde x olde)))))
	 (else
	  (olde x e)))))

;*---------------------------------------------------------------------*/
;*    expand-instantiate ...                                           */
;*---------------------------------------------------------------------*/
(define (expand-instantiate x e)
   (match-case x
      ((?instantiate . ?-)
       (let ((class (type-of-id instantiate (find-location x))))
	  (cond
	     ((not (tclass? class))
	      (error #f "Illegal `instantiate' form" x))
	     ((tclass-abstract? class)
	      (error #f "Abstract classes can't be instantiated" x))
	     (else
	      (replace! x (instantiate->make x class e))))))
      (else
       (error #f "Illegal `instantiate' form" x))))

;*---------------------------------------------------------------------*/
;*    instantiate->make ...                                            */
;*---------------------------------------------------------------------*/
(define (instantiate->make form class e)
   (let ((make-name (class-make class)))
      (instantiate->fill form
			 class
			 (lambda (largs)
			    (if (epair? form)
				(econs make-name largs (cer form))
				(cons make-name largs)))
			 e)))

;*---------------------------------------------------------------------*/
;*    instantiate->fill ...                                            */
;*---------------------------------------------------------------------*/
(define (instantiate->fill form class alloc e)
   (let* ((slots    (tclass-all-slots class))
	  (len      (length slots))
	  (vargs    (make-vector (length slots)))
	  (new      (gensym 'new))
	  (provided (cdr form)))    
      ;; we collect the default values
      (let loop ((i     0)
		 (slots slots))
	 (if (null? slots)
	     'done
	     (let ((s (car slots)))
		(cond
		   ((slot-default? s)
		    (vector-set! vargs i (cons #t (slot-default-value s))))
		   (else
		    (vector-set! vargs i (cons #f #unspecified))))
		(loop (+fx i 1)
		      (cdr slots)))))
      ;; we collect the provided values
      (let loop ((provided provided))
	 (if (null? provided)
	     'done
	     (let ((p (car provided)))
		(match-case p
		   (((and (? symbol?) ?s-name) ?value)
		    ;; plain slot
		    (let ((pval (vector-ref vargs
					    (find-slot-offset slots
							      s-name
							      "instantiate"
							      p))))
		       (set-car! pval #t)
		       (set-cdr! pval (object-epairify value p))))
		   (((and (? symbol?) ?s-name) ?len ?value)
		    ;; indexed slot
		    (let* ((snum (find-slot-offset slots
						   s-name
						   "instantiate"
						   p))
			   (slot (list-ref slots snum)))
		       (if (not (slot-indexed slot))
			   (error #f "Illegal `instantiate' form" form)
			   (let* ((pval (vector-ref vargs snum))
				  (plen (vector-ref vargs (-fx snum 1))))
			      (set-car! pval #t)
			      (set-cdr! pval (object-epairify value p))
			      (set-car! plen #t)
			      (set-cdr! plen len)))))
		   (else
		    (error #f "Illegal `instantiate' form" form)))
		(loop (cdr provided)))))
      ;; we check that we have a value for all formals
      (let loop ((i 0)
		 (s slots))
	 (cond
	    ((=fx i len)
	     'ok)
	    ((and (not (car (vector-ref vargs i)))
		  (not (slot-virtual? (car s))))
	     ;; no, this is not correct, at least one argument is missing
	     (error #f
		    (string-append
		     "Illegal `instantiate' form (missing arguments for slot "
		     (symbol->string (slot-id (car s)))
		     ")")
		    form))
	    (else
	     (loop (+fx i 1) (cdr s)))))
      ;; we just have now to build the make call
      (let loop ((i        0)
		 (slots    slots)
		 (largs    '())
		 (virtuals '()))
	 (if (=fx i len)
	     (if (null? virtuals)
		 (alloc (reverse! largs))
		 `(let ((,new ,(alloc (reverse! largs))))
		     ,@(reverse! virtuals)
		     ,new))
	     (let ((value (e (cdr (vector-ref vargs i)) e)))
		(cond
		   ((slot-virtual? (car slots))
		    (loop (+fx i 1)
			  (cdr slots)
			  largs
			  (if (eq? value #unspecified)
			      virtuals
			      (if (epair? value)
				  (econs (make-virtual-set (car slots)
							   value
							   new
							   class
							   form
							   e
							   "instantiate")
					 virtuals
					 (cer value))
				  (cons (make-virtual-set (car slots)
							  value
							  new
							  class
							  form
							  e
							  "instantiate")
					virtuals)))))
		   (else
		    (loop (+fx i 1)
			  (cdr slots)
			  (if (epair? value)
			      (econs value largs (cer value))
			      (cons value largs))
			  virtuals))))))))
   
;*---------------------------------------------------------------------*/
;*    expand-co-instantiate ...                                        */
;*---------------------------------------------------------------------*/
(define (expand-co-instantiate x e)
   (match-case x
      ((co-instantiate ?bindings . ?body)
       (replace! x (co-instantiate->let bindings body x e)))
      (else
       (error #f "Illegal `co-instantiate' form" x))))

;*---------------------------------------------------------------------*/
;*    co-instantiate->let ...                                          */
;*---------------------------------------------------------------------*/
(define (co-instantiate->let bindings body x e)
   (define (find-instantiate-class expr bdg loc)
      (match-case expr
	 ((?instantiate . ?body)
	  (let* ((loc (find-location/loc body loc))
		 (id.type (parse-id instantiate loc))
		 (kclass (cdr id.type)))
	     (cond
		((not (eq? (car id.type) 'instantiate))
		 (error #f
			"co-instantiate:Illegal binding"
			bdg))
		((not (tclass? kclass))
		 (error #f
			"co-instantiate:Illegal class"
			bdg))
		((tclass-abstract? kclass)
		 (error #f
			"co-instantiate:Abstract classes can't be instantiated"
			bdg))
		(else
		 kclass))))
	 (else
	  (error #f
		 "co-instantiate:Illegal binding"
		 bdg))))
   (let ((loc (find-location x)))
      (let loop ((bindings bindings)
		 (user-variables '())
		 (private-variables '())
		 (rewrite-bindings '())
		 (classes '())
		 (instantiates '()))
	 (if (null? bindings)
	     (letrec ((e1 (lambda (x _)
			     (if (symbol? x)
				 (let ((cell (assq x rewrite-bindings)))
				    (if (pair? cell)
					(cdr cell)
					x))
				 (match-case x
				    ((begin ?x)
				     (e1 x e))
				    (else
				     (e x e)))))))
		`(let ,(map (lambda (id class)
			       (let* ((cid (tclass-id class))
				      (tv (make-typed-ident id cid)))
				  `(,tv (,(symbol-append '%allocate- cid)))))
			    private-variables
			    classes)
		    (let ,(reverse!
			   (map (lambda (user private class instantiate)
				   (let* ((cid (tclass-id class))
					  (fill (symbol-append 'fill- cid '!))
					  (alloc (lambda (args)
						    `(,fill ,private ,@args))))
				      `(,user ,(instantiate->fill instantiate
								  class
								  alloc
								  e1))))
				user-variables
				private-variables
				classes
				instantiates))
		       ,(e `(begin ,@body) e))))
	     (match-case (car bindings)
		(((and ?var (? symbol?)) ?expr)
		 (let* ((bdg (car bindings))
			(loc (find-location/loc bdg x))
			(id.type (parse-id var loc))
			(id (car id.type))
			(t (cdr id.type))
			(priv (gensym))
			(kclass (find-instantiate-class expr bdg loc)))
		    (loop (cdr bindings)
			  (cons var user-variables)
			  (cons priv private-variables)
			  (cons (cons id priv) rewrite-bindings)
			  (cons kclass classes)
			  (cons expr instantiates))))
		(else
		 (error #f
			"co-instantiate:Illegal binding"
			(car bindings))))))))

;*---------------------------------------------------------------------*/
;*    expand-duplicate ...                                             */
;*---------------------------------------------------------------------*/
(define (expand-duplicate  x e)
   (match-case x
      ((?duplicate ?dup . ?prov)
       (let* ((id.type (parse-id duplicate (find-location x)))
	      (id      (car id.type))
	      (class   (cdr id.type)))
	  (cond
	     ((not (tclass? class))
	      (error #f
		     (string-append "duplicate:Illegal class type:"
				    (symbol->string id))
		     x))
	     ((tclass-abstract? class)
	      (error #f "Abstract classes can't be duplicated" x))
	     (else
	      (replace! x (e (duplicate->make x class dup prov e) e))))))
      (else
       (error #f "Illegal `duplicate' form" x))))

;*---------------------------------------------------------------------*/
;*    duplicate->make ...                                              */
;*    -------------------------------------------------------------    */
;*    In contrast with instantiate this macro does not check at        */
;*    compile time that all values are provided. All the missing       */
;*    values (the ones not provided explicitly) are picked from        */
;*    the duplicated object (which hence, is supposed to have the      */
;*    proper fields).                                                  */
;*---------------------------------------------------------------------*/
(define (duplicate->make form class duplicated provided e)
   (let* ((slots            (tclass-all-slots class))
	  (len              (length slots))
	  (dup-var          (mark-symbol-non-user! (gensym 'duplicated)))
	  (dup-var-typed    (make-typed-ident dup-var (type-id class)))
	  (new              (gensym 'new))
	  (vargs            (make-vector (length slots))))
      ;; we collect the provided values
      (let loop ((provided provided))
	 (if (null? provided)
	     'done
	     (let ((p (car provided)))
		(match-case p
		   (((and (? symbol?) ?s-name) ?value)
		    ;; plain slot
		    (vector-set! vargs
				 (find-slot-offset slots
						   s-name
						   "duplicate"
						   p)
				 (cons #t (object-epairify value p))))
		   (((and (? symbol?) ?s-name) ?len ?value)
		    ;; index slot
		    (let* ((snum (find-slot-offset slots
						   s-name
						   "duplicate"
						   p))
			   (slot (list-ref slots snum)))
		       (if (not (slot-indexed slot))
			   (error #f "Illegal `duplicate' form" form)
			   (begin
			      (vector-set! vargs
					   snum
					   (cons #t (object-epairify value p)))
			      (vector-set! vargs (-fx snum 1)
					   (cons #f len))))))
		   (else
		    (error #f "Illegal `duplicate' form" form)))
		(loop (cdr provided)))))
      ;; we collect the duplicated values
      (let loop ((i     0)
		 (slots slots))
	 (if (null? slots)
	     'done
	     (let ((value (vector-ref vargs i)))
		(if (pair? value)
		    ;; a value is already provided for this object
		    'nothing
		    (let ((slot (car slots)))
		       (if (slot-indexed slot)
			   ;; for indexed slot, we pick the first value
			   ;; (which is suposed to be existing).
			   (let ((a-name (symbol-append (type-id class)
							'-
							(slot-id slot)
							'-ref)))
			      (vector-set! vargs
					   i
					   (cons #t `(,a-name ,dup-var 0))))
			   ;; no value is provided for this object we pick
			   ;; one from this duplicated object.
			   (let ((a-name (symbol-append (type-id class)
							'-
							(slot-id slot))))
			      (vector-set! vargs
					   i
					   (cons #t `(,a-name ,dup-var)))))))
		(loop (+fx i 1)
		      (cdr slots)))))
      ;; we just have now to build the make call
      (let loop ((i        0)
		 (slots    slots)
		 (largs    '())
		 (virtuals '()))
	 (if (=fx i len)
	     (let* ((make-name (symbol-append 'make- (type-id class)))
		    (alloc     `(,make-name ,@(reverse! largs))))
		`(let* ((,dup-var-typed ,duplicated)
			(,new           ,alloc))
		    ,@(reverse! virtuals)
		    ,new))
	     (let ((value (cdr (vector-ref vargs i))))
		(cond
		   ((slot-virtual? (car slots))
		    (loop (+fx i 1)
			  (cdr slots)
			  largs
			  (cons (make-virtual-set (car slots)
						  value
						  new
						  class
						  form
						  e
						  "duplicate")
				virtuals)))
		   (else
		    (loop (+fx i 1)
			  (cdr slots)
			  (cons value largs)
			  virtuals))))))))
   
;*---------------------------------------------------------------------*/
;*    expand-widen! ...                                                */
;*---------------------------------------------------------------------*/
(define (expand-widen! x e)
   (match-case x
      ((?widen! ?obj . ?provided)
       (let ((class (type-of-id widen! (find-location x))))
	  (if (and (tclass? class) (tclass-widening class))
	      (replace! x (e (make-widening x class obj provided e) e))
	      (error #f
		     (string-append "widen!:Illegal class type:"
				    (symbol->string (type-id class)))
		     x))))
      (else
       (error #f "Illegal `widen!' form" x))))

;*---------------------------------------------------------------------*/
;*    make-widening ...                                                */
;*---------------------------------------------------------------------*/
(define (make-widening form class obj provided e)
   (let* ((slots         (tclass-slots class))
	  (holder        (tclass-holder class))
	  (len           (length slots))
	  (vargs         (make-vector (length slots)))
	  (dup-var       (mark-symbol-non-user! (gensym 'obj)))
	  (tid           (type-id class))
	  (tsid          (type-id (tclass-its-super class)))
	  (dup-var-typed (make-typed-ident dup-var tsid))
	  (cast          (make-private-sexp 'cast tsid obj)))
      ;; we collect the default values
      (let loop ((i     0)
		 (slots slots))
	 (if (null? slots)
	     'done
	     (let ((s (car slots)))
		(if (slot-default? s)
		    (vector-set! vargs i (cons #t (slot-default-value s)))
		    (vector-set! vargs i (cons #f #unspecified)))
		(loop (+fx i 1)
		      (cdr slots)))))
      ;; we collect the provided values
      (let loop ((provided provided))
	 (if (null? provided)
	     'done
	     (let ((p (car provided)))
		(match-case p
		   (((and (? symbol?) ?s-name) ?value)
		    (vector-set! vargs
				 (find-slot-offset slots
						   s-name
						   "widen!"
						   p)
				 (cons #t (object-epairify value p))))
		   (((and (? symbol?) ?s-name) ?len ?value)
		    (let* ((snum (find-slot-offset slots
						   s-name
						   "widen!"
						   p))
			   (slot (list-ref slots snum)))
		       (if (not (slot-indexed slot))
			   (error #f "Illegal `widen!' form" form)
			   (begin
			      (vector-set! vargs
					   snum
					   (cons #t (object-epairify value p)))
			      (vector-set! vargs (-fx snum 1)
					   (cons #f len))))))
		   (else
		    (error #f "Illegal `widen!' form" form)))
		(loop (cdr provided)))))
      ;; we check that we have a value for all formals
      (let loop ((i 0)
		 (s slots))
	 (cond
	    ((=fx i len)
	     'ok)
	    ((and (not (car (vector-ref vargs i)))
		  (not (slot-virtual? (car s))))
	     ;; no, this is not correct, at least one argument is missing
	     (error #f
		    (string-append
		     "Illegal `widening!' form (missing arguments for slot "
		     (symbol->string (slot-id (car s)))
		     ")")
		    form))
	    (else
	     (loop (+fx i 1) (cdr s)))))
      ;; we just have now to build the make call
      (let loop ((i        0)
		 (slots    slots)
		 (largs    '())
		 (virtuals '()))
	 (if (=fx i len)
	     (let ((widening (symbol-append (tclass-widening class)
					    '-
					    (type-id class))))
		(if *unsafe-type*
		    `(let ((,dup-var-typed ,cast))
			(object-widening-set!
			 ,dup-var
			 (,widening ,@(reverse! largs)))
			;; the new class must be set after initialization
			;; otherwise type errors are possibles
			((@ object-class-num-set! __object)
			 ,dup-var
			 ((@ class-num __object)
			  (@ ,(global-id holder) ,(global-module holder))))
			;; in backtracing mode (-gbdb2) we mark the allocation
			((@ object-mark-producer! __object) ,dup-var)
			,@(reverse! virtuals)
			,dup-var)
		    `(let ((,dup-var-typed ,cast))
			(if (object-widening ,dup-var)
			    (shrink! ,dup-var))
			(if (eq? ((@ class-super __object)
				  (@ ,(global-id holder)
				     ,(global-module holder)))
				 ((@ object-class __object) ,dup-var))
			    (begin
			       (object-widening-set!
				,dup-var
				(,widening ,@(reverse! largs)))
			       ;; same remark as below
			       ((@ object-class-num-set! __object)
				,dup-var
				((@ class-num __object)
				 (@ ,(global-id holder)
				    ,(global-module holder))))
			       ;; in backtracing mode, we mark the allocation
			       ((@ object-mark-producer! __object) ,dup-var)
			       ,@(reverse! virtuals)
			       ,dup-var)
			    (error
			     "widen!"
			     "This object can't be widened to the wanted class"
			     ,dup-var)))))
	     (let ((value (cdr (vector-ref vargs i))))
		(cond
		   ((slot-virtual? (car slots))
		    (loop (+fx i 1)
			  (cdr slots)
			  largs
			  (if (eq? value #unspecified)
			      virtuals
			      (cons (make-virtual-set (car slots)
						      value
						      dup-var
						      class
						      form
						      e
						      "widen!")
				    virtuals))))
		   (else
		    (loop (+fx i 1)
			  (cdr slots)
			  (cons value largs)
			  virtuals))))))))
   
;*---------------------------------------------------------------------*/
;*    expand-shrink! ...                                               */
;*---------------------------------------------------------------------*/
(define (expand-shrink! x e)
   (match-case x
      ((shrink! ?o)
       (replace! x (make-a-shrink! e o)))
      (else
       (error #f "Illegal `shrink!' form" x))))
		 
;*---------------------------------------------------------------------*/
;*    make-a-shrink! ...                                               */
;*---------------------------------------------------------------------*/
(define (make-a-shrink! e o)
   (let ((newo (mark-symbol-non-user! (gensym 'o))))
      (if *unsafe-type*
	  `(let ((,newo ,(e o e)))
	      ((@ object-class-num-set! __object)
	       ,newo
	       ((@ class-num __object)
		((@ class-super __object)
		 ((@ object-class __object) ,newo))))
	      (object-widening-set! ,newo #f)
	      ,newo)
	  `(let ((,newo ,(e o e)))
	      (if (object? ,newo)
		  (if (object-widening ,newo)
		      (begin
			 ((@ object-class-num-set! __object)
			  ,newo
			  ((@ class-num __object)
			   ((@ class-super __object)
			    ((@ object-class __object) ,newo))))
			 (object-widening-set! ,newo #f)
			 ,newo)
		      (error "shrink!" "Not a wide object" ,newo))
		  (error "shrink!" "Not a wide object" ,newo))))))

;*---------------------------------------------------------------------*/
;*    make-virtual-set ...                                             */
;*---------------------------------------------------------------------*/
(define (make-virtual-set slot value var class form e alloc-kind)
   (if (slot-read-only? slot)
       (error #f
	      (string-append "Illegal `" alloc-kind "' form (read-only slot "
			     (symbol->string (slot-id slot))
			     ")")
	      form)
       (object-epairify
	(e `(,(symbol-append (tclass-id class) '- (slot-id slot) '-set!)
	     ,var
	     ,value)
	   e)
	value)))

;*---------------------------------------------------------------------*/
;*    object-epairify ...                                              */
;*---------------------------------------------------------------------*/
(define (object-epairify obj epair)
   (if (epair? epair)
       (if (pair? obj)
	   (econs (car obj) (cdr obj) (cer epair))
	   (object-epairify `(begin ,obj) epair))
       obj))

;*---------------------------------------------------------------------*/
;*    find-slot-offset ...                                             */
;*---------------------------------------------------------------------*/
(define (find-slot-offset slots::pair-nil name::symbol form::bstring sexp)
   (let loop ((slots slots)
	      (i 0))
      (cond
	 ((null? slots)
	  (error #f
		 (string-append "Illegal `" form "' form (unknown slot "
				(symbol->string name)
				")")
		 sexp))
	 ((eq? (slot-id (car slots)) name)
	  i)
	 (else   
	  (loop (cdr slots) (+fx i 1))))))
