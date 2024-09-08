(module saw_jvm_names
   (import type_type ast_var ast_node
	   ast_env
	   type_env
	   object_class
	   object_slots
	   tvector_tvector
	   foreign_jtype
	   read_jvm
	   backend_backend
	   backend_cplib
	   )
   (export (names-initialization me::jvm)
	   (wide-class jvmbasic::type) ))

;;
;; Entry point
;;
(define (names-initialization me::jvm)
   (reset-jvmstd-type!)
;*    (for-each-type! reset-type!)                                     */
   (for-each-global! reset-global!)
   (for-each-type! saw_jvm-set-type-names!) )

;;
;; Creation of standard jvm types
;;
(define (reset-jvmstd-type!)
   ;; Basic types with id=name
   (for-each (lambda (x) (type-name-set! (widen!::jvmbasic (find-type x)) x))
	     '(void short int float double) )
   ;; Basic types with specific names
   (for-each (lambda (x s) (type-name-set! (widen!::jvmbasic (find-type x)) s))
	     '(bool    char ucs2 long uchar llong elong)
	     '(boolean byte char int  int   long  long ) )
   ;; Upgrade some types to vectors
   (for-each (lambda (v i)
		(widen!::tvec (find-type v) (item-type (find-type i))) )
	     '(bstring string ucs2string vector cnst* procedure-el)
	     '(char    char   ucs2       obj    obj   obj ))
   ;; Set some names by hand
   (for-each (lambda (s) (type-name-set! (find-type (car s)) (cdr s)))
	     '((obj           . obj)
	       (magic         . obj)
	       (pair-nil      . obj)
	       (procedure-el1 . obj)
	       (void*         . obj)
	       (tvector       . obj)
	       ; Not here in mklib mode
;	       (object        . object)
	       (output-port   . output-port)
	       (input-port    . input-port)
	       (binary-port   . binary-port)
	       (epair         . extended_pair)
	       (procedure     . procedure) )))

;;
;; Associate jvmtype to a type
;;
(define (saw_jvm-set-type-names! type::type)
  (get-jvmtype type) )

(define (get-jvmtype type::type)
   (let ( (name (type-name type)) )
      (if (symbol? name)
	  name
	  (let ( (jtype (build-type-name type)) )
	     (type-name-set! type jtype)
	     jtype ))))

(define (build-type-name type::type)
   (cond
      ((tclass? type)
       (if (eq? (type-id type) 'object)
	   'object
	   (qualified-tclass-name type) ))
      ((wclass? type)
       (qualified-wclass-name type) )
      ((jclass? type) (qualified-jclass-name type))
      ((tvec? type)
       (get-jvmtype (tvec-item-type type))
       "Zector" )
      ((jarray? type)
       (get-jvmtype (jarray-item-type type))
       "Zector" )
      (else  (qualified-type-name type)) ))

;;
;; Specific methods for subtyping
;;
(define-method (backend-subtype? b::jvm t1::type t2::type)
   (or (eq? t1 t2)
       (eq? (type-id t1) (type-id t2))
       (eq? (type-name t2) 'java.lang.Object)
       (eq? (type-name t1) (type-name t2)) ))
