(module jsm_compile
   (import module_module
	   type_type
	   type_cache
	   type_env
	   ast_node
	   ast_var
	   ast_env
	   object_class
	   object_slots
	   cnst_node
	   cnst_alloc
	   tvector_tvector
	   tools_error
	   read_jvm
	   backend_backend
	   backend_jsm
	   backend_lib
	   backend_cplib
	   saw_defs
	   saw_node2rtl
	   saw_woodcutter
	   saw_expr
	   )
   (export (jsm-compile ::jsm ::output-port)) )

;;
;; Main function
;;
(define (jsm-compile me::jsm out::output-port)
   'notyet )
