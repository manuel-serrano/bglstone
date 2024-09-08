;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Ast/glo-decl.scm            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Jun  3 09:17:44 1996                          */
;*    Last change :  Tue Apr  1 16:22:53 2003 (serrano)                */
;*    -------------------------------------------------------------    */
;*    This module implement the functions used to declare a global     */
;*    variable (i.e. in the module language compilation). Global       */
;*    function definitions are managed in ast_glob-def                 */
;*    (glo-def.scm).                                                   */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module ast_glo-decl
   (include "Tools/trace.sch")
   (import  tools_error
	    tools_args
	    tools_shape
	    tools_dsssl
	    tools_location
	    engine_param
	    type_type
	    type_cache
	    ast_var
	    ast_env
	    ast_ident
	    ast_node
	    type_env
	    (module-initialization-id module_module)
	    module_module)
   (export  (declare-global-sfun!::global  id::symbol
					   args::obj
					   module::symbol
					   import::symbol
					   class::symbol
					   src::obj
					   src-import::obj)
	    (declare-global-svar!::global  id::symbol
					   module::symbol
					   import::symbol
					   src::obj
					   src-import)
	    (declare-global-scnst!::global id::symbol
					   module::symbol
					   import::symbol
					   node
					   class::symbol
					   loc)
	    (declare-global-cfun!::global  id::symbol
					   name::bstring
					   type-res::symbol
					   type-args::obj
					   infix?::bool
					   macro::bool
					   src-exp::obj
					   src-import)
	    (declare-global-cvar!::global  id::symbol
					   name::bstring
					   type-id::symbol
					   macro?::bool
					   src::obj
					   src-import)))

;*---------------------------------------------------------------------*/
;*    declare-global-sfun! ...                                         */
;*    -------------------------------------------------------------    */
;*    This function declare a global sfunction. It is used only when   */
;*    compiling module clauses. When a function is defined, this       */
;*    function is _not_used.                                           */
;*---------------------------------------------------------------------*/
(define (declare-global-sfun! id args module import class src-exp src-import)
   (trace (ast 3) "declare-global-sfun!: "
	  (shape id) " " (shape args) #\newline)
   (let* ((arity     (arity args))
	  (args      (args*->args-list args))
	  (export?   (or (not (eq? import 'static))
			 (>=fx *bdb-debug* 3)))
	  (import    (if (and (eq? import 'static)
			      (>=fx *bdb-debug* 3))
			 'export
			 import))
	  (loc       (find-location src-exp))
	  (loci      (find-location/loc src-import loc))
	  (keywords  '())
	  (args-type (let loop ((args   args)
				(res    '())
				(sgfun? (eq? class 'sgfun)))
			(cond
			 ((null? args)
			  (if (>=fx arity 0)
			      (reverse! res) 
			      (let ((type (car res)))
				 (cond
				    ((eq? type *obj*)
				     (reverse! res))
				    ((eq? type *_*)
				     (reverse! (cons *obj* (cdr res))))
				    (else
				     (user-error id
						 "Illegal nary argument type"
						 (shape type)))))))
			 ((dsssl-named-constant? (car args))
			  (set! keywords (dsssl-formals-encoding args))
			  (reverse! (cons *obj* res)))
			 (else
			  (let ((type (let ((t (type-of-id/import-location
						(car args) loc loci)))
					 (if (and (eq? t *_*)
						  (or export? sgfun?))
					     *obj*
					     t))))
			     (loop (cdr args)
				   (cons type res)
				   #f))))))
	  (id.type   (parse-id/import-location id loc loci))
	  (type-res  (cdr id.type))
	  (id        (car id.type))
	  (sfun      (instantiate::sfun (arity arity)
					(args  args-type)
					(dsssl-keywords keywords)
					(class class)))
	  (old       (find-global id))
	  (global    (bind-global! id module sfun import src-exp)))
      (trace (ast 3) "*** declare-global-sfun!: src-exp: " src-exp #\Newline)
      (trace (ast 3) "*** declare-global-sfun!: loc: " (find-location src-exp)
	     #\Newline)
      (trace (ast 4) "   declare-global-sfun!: (instantiate "
	     (shape arity) " " (shape args-type) " " (shape class) #\Newline)
      ;; we set the type of the function
      (cond
	 ((not (eq? type-res *_*))
	  (global-type-set! global type-res))
	 (export?
	  (global-type-set! global *obj*))
	 (else
	  (global-type-set! global type-res)))
      ;; and we return the global
      global))

;*---------------------------------------------------------------------*/
;*    declare-global-svar! ...                                         */
;*---------------------------------------------------------------------*/
(define (declare-global-svar! id module import src-exp src-import)
   (let* ((loc       (find-location src-exp))
	  (loci      (find-location/loc src-import loc))
	  (id.type   (parse-id/import-location id loc loci))
	  (type      (let ((type (cdr id.type)))
			;; we check that global exported variable are defined
			;; without type or with the obj type.
			(cond
			   ((not (eq? (type-class type) 'bigloo))
			    (user-error id
					"Illegal type for global variable"
					(shape type)
					*_*))
			   ((and (eq? type *_*)
				 (or (memq import '(export import))
				     (>fx *bdb-debug* 0)))
			    *obj*)
			   (else
			    type))))
	  (import    (if (and (eq? import 'static)
			      (>=fx *bdb-debug* 3))
			 'export
			 import))
	  (id        (car id.type))
	  (svar      (instantiate::svar))
	  (old       (find-global id))
	  (global    (bind-global! id module svar import src-exp)))
      ;; we set the type of the variable
      (global-type-set! global type)
      ;; we now set the access slot
      (global-access-set! global (if (eq? import 'static) 'read 'write))
      ;; we return the global
      global))

;*---------------------------------------------------------------------*/
;*    declare-global-scnst! ...                                        */
;*---------------------------------------------------------------------*/
(define (declare-global-scnst! id module import node class loc)
   (let* ((id.type   (parse-id id loc))
	  (type      (let ((type (cdr id.type)))
			;; we check that global exported variable are defined
			;; without type or with the obj type.
			(cond
			   ((eq? type *_*)
			    (internal-error id
					    "Illegal type for global variable"
					    (shape type)))
			   (else
			    type))))
	  (id        (car id.type))    
	  (scnst     (instantiate::scnst
			(class class)
			(node node)))
	  (global    (bind-global! id module scnst import 'a-cnst)))
      ;; we set the type of the variable
      (global-type-set! global type)
      ;; we now set the access slot 
      (global-access-set! global 'read)
      ;; we return the global
      global))

;*---------------------------------------------------------------------*/
;*    declare-global-cfun! ...                                         */
;*---------------------------------------------------------------------*/
(define (declare-global-cfun! id name tres-id targs-id infix? macro? src-exp src-import)
   (let* ((arity     (arity targs-id))
	  (loc       (find-location src-exp))
	  (loci      (find-location/loc src-import loc))
	  (type-res  (use-foreign-type/import-loc! tres-id loc loci))
	  (type-args (map (lambda (t)
			     (use-foreign-type/import-loc! t loc loci))
			  (args*->args-list targs-id)))
	  (cfun      (instantiate::cfun (arity arity)
		 			(args-type type-args)
					(macro? macro?)
					(infix? infix?)))
	  (global (bind-global! id 'foreign cfun 'foreign src-exp)))
      ;; we set the name of the global
      (global-name-set! global name)
      ;; we set the type of the variable
      (global-type-set! global type-res)
      ;; foreign variable can be evaluated
      (global-evaluable?-set! global #f)
      ;; we return the global
      global))
   
;*---------------------------------------------------------------------*/
;*    declare-global-cvar! ...                                         */
;*---------------------------------------------------------------------*/
(define (declare-global-cvar! id name type-id macro? src-exp src-import)
   (let* ((loc    (find-location src-exp))
	  (loci   (find-location/loc src-import loc))
	  (type   (use-foreign-type/import-loc! type-id loc loci))
	  (cvar   (instantiate::cvar (macro? macro?)))
	  (global (bind-global! id 'foreign cvar 'foreign src-exp)))
      ;; we set the name of the global
      (global-name-set! global name)
      ;; we set the type of the variable
      (global-type-set! global type)
      ;; foreign variable can't be evaluated
      (global-evaluable?-set! global #f)
      ;; we return the global
      global))


   

