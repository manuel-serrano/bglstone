;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Ast/venv.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Dec 25 11:32:49 1994                          */
;*    Last change :  Fri Nov  5 16:07:43 2004 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The global environment manipulation                              */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module ast_env
   (import  tools_shape
	    engine_param
	    tools_error
	    type_type
	    type_cache
	    type_env
	    ast_var
	    ast_node
	    ast_hrtype
	    read_jvm
	    module_module
	    tools_location)
   (export  (initialize-Genv!)
	    (set-genv! <Genv>)
	    (add-genv! <Genv>)
	    (get-genv) 
	    (find-global ::symbol . <symbol>)
	    (find-global/module ::symbol ::symbol)
	    (bind-global!::global   ::symbol ::symbol ::value ::symbol ::obj)
	    (unbind-global! ::symbol ::symbol)
	    (for-each-global! ::procedure)
	    (global-bucket-position ::symbol ::symbol)
	    (restore-global! new)
	    (additional-heap-restore-globals!)
	    (already-restored? fun)))

;*---------------------------------------------------------------------*/
;*    *Genv* ...                                                       */
;*    -------------------------------------------------------------    */
;*    The Global environment (for global variable definitions).        */
;*---------------------------------------------------------------------*/
(define *Genv* 'the-global-environment)

;*---------------------------------------------------------------------*/
;*    set-genv! ...                                                    */
;*---------------------------------------------------------------------*/
(define (set-genv! Genv)
   (set! *Genv* Genv))
		 
;*---------------------------------------------------------------------*/
;*    add-genv! ...                                                    */
;*    -------------------------------------------------------------    */
;*    When adding a new environment we have to mark that all global    */
;*    bindings are library ones.                                       */
;*---------------------------------------------------------------------*/
(define (add-genv! Genv)
   (hashtable-for-each
    Genv
    (lambda (k bucket)
       (for-each (lambda (new)
		    (delay-restore-global! new)
		    (let* ((module (global-module new))
			   (id     (global-id new))
			   (bucket (hashtable-get *Genv* id)))
		       (cond
			  ((not (pair? bucket))
			   (hashtable-put! *Genv* id (list id new)))
			  ((or (eq? module *module*)
			       (not (eq? *module*
					 (global-module (cadr bucket)))))
			   ;; we add the new global in first position if:
			   ;;   - we are binding a variable of the current
			   ;;     module
			   ;;   - the first global already bound is not owned
			   ;;     by the current module
			   (let ((new-bucket (cons new (cdr bucket))))
			      (set-cdr! bucket new-bucket)))
			  (else
			   (set-cdr! (cdr bucket) (cons new (cddr bucket)))))))
		 (cdr bucket))))
   (set! *restored* '()))

;*---------------------------------------------------------------------*/
;*    *delayed-restored-global* ...                                    */
;*---------------------------------------------------------------------*/
(define *delayed-restored-global* '())

;*---------------------------------------------------------------------*/
;*    delay-restore-global! ...                                        */
;*---------------------------------------------------------------------*/
(define (delay-restore-global! g)
   (set! *delayed-restored-global* (cons g *delayed-restored-global*)))

;*---------------------------------------------------------------------*/
;*    additional-heap-restore-globals! ...                             */
;*---------------------------------------------------------------------*/
(define (additional-heap-restore-globals!)
   (for-each restore-global! *delayed-restored-global*)
   #t)

;*---------------------------------------------------------------------*/
;*    restore-global! ...                                              */
;*---------------------------------------------------------------------*/
(define (restore-global! new)
   ;; we mark that the current global has been restored
   (mark-restored! new)
   (let* ((id      (global-id new))
	  (type    (global-type new))
	  (value   (global-value new))
	  (type-id (type-id type)))
      ;; we mark the global variable
      (global-library?-set! new #t)
      ;; we restore type result
      (global-type-set! new (find-type type-id))
      ;; the parameters type
      (restore-value-types! value)
      ;; we restore the jvm qualified type name
      (if (and (memq *target-language* '(jvm .net))
	       (not (eq? (global-module new) 'foreign)))
	  (add-qualified-type! (global-module new)
			       (global-jvm-type-name new)
			       (shape new)))))

;*---------------------------------------------------------------------*/
;*    *restored* ...                                                   */
;*---------------------------------------------------------------------*/
(define *restored* '())

;*---------------------------------------------------------------------*/
;*    mark-restored! ...                                               */
;*---------------------------------------------------------------------*/
(define (mark-restored! fun)
   (set! *restored* (cons fun *restored*)))

;*---------------------------------------------------------------------*/
;*    already-restored? ...                                            */
;*    -------------------------------------------------------------    */
;*    This function is used only once:                                 */
;*      @ref hrtype.scm:already-restored@                              */
;*---------------------------------------------------------------------*/
(define (already-restored? fun)
   (memq fun *restored*))

;*---------------------------------------------------------------------*/
;*    restore-value-types! ...                                         */
;*---------------------------------------------------------------------*/
(define-generic (restore-value-types! value::value)
   #unspecified)

;*---------------------------------------------------------------------*/
;*    restore-value-types! ::sfun ...                                  */
;*---------------------------------------------------------------------*/
(define-method (restore-value-types! value::sfun)
   (with-access::sfun value (args)
      (let loop ((args args))
	 (cond
	    ((pair? args)
	     (let ((arg (car args)))
		(cond
		   ((type? arg)
		    (set-car! args (find-type (type-id arg))))
		   ((local? arg)
		    (let ((new-type (find-type (type-id (local-type arg)))))
		       (local-type-set! arg new-type)))
		   (else
		    (error "restore-value-types(sfun)"
			   "Illegal argument"
			   (shape arg))))
		(loop (cdr args))))
	    ((null? args)
	     (let ((body (sfun-body value)))
		;; we still have to restore the body types
		(if (node? body)
		    (let ((tres (node-type body)))
		       (if (type? tres)
			   (begin
			      (hrtype-node! body)
			      (node-type-set! body
					      (find-type (type-id tres)))))))))
	    (else
	     (error "restore-value-types"
		    "Illegal non pair argument"
		    (shape args)))))))

;*---------------------------------------------------------------------*/
;*    restore-value-types! ::cfun ...                                  */
;*---------------------------------------------------------------------*/
(define-method (restore-value-types! value::cfun)
   (with-access::cfun value (args-type)
      (let loop ((args args-type))
	 (if (pair? args)
	     (begin
		(set-car! args (find-type (type-id (car args))))
		(loop (cdr args)))))))
   
;*---------------------------------------------------------------------*/
;*    get-genv ...                                                     */
;*---------------------------------------------------------------------*/
(define (get-genv)
   *Genv*)

;*---------------------------------------------------------------------*/
;*    initialize-Genv! ...                                             */
;*---------------------------------------------------------------------*/
(define (initialize-Genv!)
   (set! *Genv* (make-hashtable)))

;*---------------------------------------------------------------------*/
;*    find-global ...                                                  */
;*---------------------------------------------------------------------*/
(define (find-global id::symbol . module)
   [assert (module) (or (null? module) (symbol? (car module)))]
   (let ((bucket (hashtable-get *Genv* id))
	 (module (if (null? module) '() (car module))))
      (cond
	 ((not (pair? bucket))
	  #f)
	 ((null? (cdr bucket))
	  #f)
	 ((null? module)
	  (cadr bucket))
	 (else
	  (let loop ((globals (cdr bucket)))
	     (cond
		((null? globals)
		 #f)
		((eq? (global-module (car globals)) module)
		 (car globals))
		(else
		 (loop (cdr globals)))))))))

;*---------------------------------------------------------------------*/
;*    find-global/module ...                                           */
;*---------------------------------------------------------------------*/
(define (find-global/module id::symbol module)
   (let ((bucket (hashtable-get *Genv* id)))
      (cond
	 ((not (pair? bucket))
	  #f)
	 ((null? (cdr bucket))
	  #f)
	 ((null? module)
	  (cadr bucket))
	 (else
	  (let loop ((globals (cdr bucket)))
	     (cond
		((null? globals)
		 #f)
		((eq? (global-module (car globals)) module)
		 (car globals))
		(else
		 (loop (cdr globals)))))))))

;*---------------------------------------------------------------------*/
;*    warning-override-global! ...                                     */
;*---------------------------------------------------------------------*/
(define (warning-override-global! n o . msg)
   (if (>fx (bigloo-warning) 0)
       (with-access::global o (id module src)
	  (let ((loc (find-location src))
		(old `(@ ,id ,module))
		(new `(@ ,(global-id n) ,(global-module n)))
		(msg (if (pair? msg) (car msg) "Variable overridden by")))
	     (if loc
		 (user-warning/location loc old msg new)
		 (user-warning old msg new))))))

;*---------------------------------------------------------------------*/
;*    error-rebind-global! ...                                         */
;*---------------------------------------------------------------------*/
(define (error-rebind-global! o src)
   (with-access::global o (id module)
      (let ((loc (find-location src))
	    (msg "Duplicate definition"))
	 (if loc
	     (user-error/location loc module msg (shape o))
	     (user-error module msg (shape o))))))

;*---------------------------------------------------------------------*/
;*    bind-global! ...                                                 */
;*    -------------------------------------------------------------    */
;*    This function binds a new global variable in the global table.   */
;*    When several variables share the identifier the following rules  */
;*    are observed:                                                    */
;*       1- if the two declarations concerns the same module, an error */
;*          is notified.                                               */
;*       2- the variable defined in the current module has the highest */
;*          priority (it override the others definitions).             */
;*       3- imported variables have a higher priority than library     */
;*          variables.                                                 */
;*       4- user library variables have a higher priority than system  */
;*          library variables.                                         */
;*---------------------------------------------------------------------*/
(define (bind-global!::global id::symbol
			      module::symbol
			      value::value
			      import::symbol
			      src::obj)
   (let ((old (find-global id module)))
      (if (global? old)
	  (if *lib-mode*
	      old
	      (error-rebind-global! old src))
	  (let ((bucket (hashtable-get *Genv* id))
		(new (instantiate::global
			(module module)
			(jvm-type-name (if (eq? import 'eval)
					   "eval"
					   (module->qualified-type module)))
			(id id)
			(value value)
			(src src) 
			(import import))))
	     (cond
		((or (not (pair? bucket)) (null? (cdr bucket)))
		 ;; this is the firt time we see this identifier
		 (hashtable-put! *Genv* id (list id new))
		 new)
		(else
		 (let* ((old* (cdr bucket))
			(mid (module-initialization-id
			      (global-module (car old*)))))
		    (cond
		       ((eq? (global-module (car old*)) *module*)
			;; hidden by a local variable
			(if (and (not (eq? id mid))
				 (not *lib-mode*))
			    (warning-override-global! (car old*) new))
			(set-cdr! (cdr bucket) (cons new (cddr bucket)))
			new)
		       (else
			(let ((new* (cons new old*)))
			   (if (and (not (eq? id mid))
				    (not *lib-mode*))
			       (warning-override-global! new (car old*)))
			   (set-cdr! bucket new*)
			   new))))))))))

;*---------------------------------------------------------------------*/
;*    unbind-global! ...                                               */
;*---------------------------------------------------------------------*/
(define (unbind-global! id::symbol module::symbol)
   (let ((global (find-global id module)))
      (if (not (global? global))
	  (user-error "unbind-global!" "Can't find global" `(@ ,id ,module))
	  (let ((bucket (hashtable-get *Genv* id)))
	     (let loop ((cur  (cdr bucket))
			(prev bucket))
		(if (eq? (car cur) global)
		    (set-cdr! prev (cdr cur))
		    (loop (cdr cur) (cdr prev))))))))
   
;*---------------------------------------------------------------------*/
;*    for-each-global! ...                                             */
;*---------------------------------------------------------------------*/
(define (for-each-global! proc::procedure)
   (hashtable-for-each *Genv*
		       (lambda (k bucket) (for-each proc (cdr bucket)))))
   
;*---------------------------------------------------------------------*/
;*    global-bucket-position                                           */
;*---------------------------------------------------------------------*/
(define (global-bucket-position id module)
   (let ((bucket (hashtable-get *Genv* id)))
      (if (not (pair? bucket))
	  -1
	  (let loop ((globals (cdr bucket))
		     (pos     0))
	     (cond
		((null? globals)
		 -1)
		((eq? (global-module (car globals)) module)
		 pos)
		(else
		 (loop (cdr globals)
		       (+fx pos 1))))))))
   
