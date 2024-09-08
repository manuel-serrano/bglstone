(module msil_type
   (import type_type ast_var ast_node
	   backend_backend
	   msil_names
	   )
   (export (typeSize::int type::type)) )

;;
;; Size
;;
(define **long-types** '(elong llong double))

(define (typeSize::int type::type)
   (let ( (id (type-id type)) )
      (cond ((memq id **long-types**) 2)
	    ((eq? id 'void) 0)
	    (else 1) )))

;;
;; Specific methods for subtyping
;;
(define-method (backend-subtype? b::dotnet t1::type t2::type)
   (or (eq? t1 t2)
       (eq? (type-id t1) (type-id t2))
       (eq? (type-id t2) 'obj)
       (string=? (type-name t2) "class System.Object")
       (string=? (type-name t1) (type-name t2)) ))
