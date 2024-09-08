(module saw_jvm_type
   (import type_type ast_var ast_node
	   backend_backend
	   saw_jvm_names
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
