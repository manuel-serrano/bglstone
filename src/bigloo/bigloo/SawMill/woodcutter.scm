(module saw_woodcutter
   (import type_type         ; type
	   type_cache        ; get-object-type
	   ast_env           ; for-each-global!
	   ast_var           ; global
	   ast_node          ; for instr.scm
	   object_class      ; tclass
	   engine_param      ; *src-file*
	   module_module     ; *module*
	   tools_shape
	   backend_backend
	   saw_defs
	   saw_node2rtl
	   saw_collapse
	   saw_remove
	   saw_inline_return
	   saw_blockorder
	   saw_gotos
	   saw_cast
	   saw_register-allocation
    )
   (export
    (global->blocks::pair-nil b::backend v::global))
   )

;; CARE rebuild the backend object
(define *backend* #f)

(define *collapse* #t)
(define *remove* #t)
(define *saw_inline_returns* #t)
(define *cast* #t)

;;
;; Standard transformations
;;
(define (global->blocks::pair-nil back::backend v::global) ;(list block)
   ;; Cannot do it here since it's a inter-procedural correction
   ; (saw-extratype v)
   ;; widen all the formal parameters
   
   (let ( (b (global->rtl v))
	  (args (map local->reg! (sfun-args (global-value v)))) )
      (if *collapse* (collapse b))
      (if *remove* (set! b (remove b)))
      (if *saw_inline_returns* (inline-returns b))
      (let ( (l (block-ordering b)) )
	 (let mark ( (i 0) (l l) )
	    (if (pair? l)
		(begin (block-label-set! (car l) i)
		       (mark (+fx i 1) (cdr l)) )))
	 (add-gotos l)
	 (if *cast* (add-casts back l))
	 l )))

;;
;; Main entry point
;;
;(define (woodcutter globals)
;   globals )

;(define (treecutter v::global)
;   v )

;(define (compile-all globals)
;   (let ( (mill (make_mill globals (classes))) )
;      (mill-funs-set! mill
;	 (map (lambda (g) (bas-compile-global g)) globals) )
;      (mill-vars-set! mill (vars))
;      mill ))

;(define (classes)
;   (filter
;    (lambda (c) (not (eq? (global-import (tclass-holder c)) 'import)))
;    (get-class-list) ))

;(define (vars)
;   (let ( (r '()) )
;      (for-each-global!
;       (lambda (global)
;	  (let ( (value (global-value global)) )
;	     (if (and (eq? (global-module global) *module*)
;		      (or (svar? value) (cvar? value) (scnst? value)) )
;		 (set! r (cons global r)) ))))
;      r ))
