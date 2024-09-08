;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Ast/var.scm                 */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu May 30 15:12:51 1996                          */
;*    Last change :  Mon Sep 29 15:57:42 2003 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The variable class definition                                    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module ast_var

   (import engine_param
	   type_type)

   (export (class value)
	   
	   (class variable::object
	      ;; the variable identifier
	      (id::symbol read-only)
	      ;; the target name
	      (name (default #f))
	      ;; the variable type or the function type result
	      (type::type (default *_*))
	      ;; its type
	      value::value
	      ;; its access mode (should be either READ, WRITE
	      ;; CELL-CALLCC CELL-GOBALIZE CELL-INTEGRATE)
	      (access (default 'read))
	      ;; a slot for fast alpha conversion
	      (fast-alpha (default #unspecified))
	      ;; does this variable can be removed
	      (removable (default 'now))
	      ;; the variable number of occurrences
	      (occurrence::long (default 0)))
	   
	   (final-class global::variable
	      ;; the global's module. this variable is changed only
	      ;; in one place: the module foreign_library
	      ;;    @ref ../Foreign/library.scm:module change@
	      module::symbol
	      ;; the global's importation (is mutated in make_heap@heap_make)
	      ;; should be STATIC, EXPORT, EVAL or FOREIGN.
	      import
	      ;; is this global can be known by eval ?
	      (evaluable?::bool (default #t))
	      ;; is it a library variable ?
	      (library?::bool (default #f))
	      ;; May 6 2003
	      ;; (library?::bool (default *lib-mode*))
	      ;; does this global variable belongs to the user ? this field
	      ;; is used mostly for default entry point of generic functions
	      ;; and unit initialization functions. the user? field is only
	      ;; used when compiling for -gbdb mode.
	      (user?::bool (default #t))
	      ;; a user pragma about the variable
	      (pragma::obj (default '()))
	      ;; declaration source
	      src::obj
	      ;; the qualified type the global belongs to
	      ;; (for JVM compilation only)
	      jvm-type-name::bstring)

	   (final-class local::variable
	      ;; does this local variable belongs to the user ?
	      (user?::bool (default #f))
	      ;; the local's identification key
	      (key::long read-only))

	   (class fun::value
	      ;; the function arity
	      (arity::long read-only)
	      ;; side effect field
	      (side-effect? (default #unspecified))
	      ;; if this function is a predicate, the type tested
	      (predicate-of (default #f))
	      ;; an associated stack allocator
	      (stack-allocator (default #f))
	      ;; is this function `top' its arguments (for the cfa)
	      (top?::bool (default #t))
	      ;; the associated closure
	      (the-closure (default #unspecified))
	      ;; the effect of this function
	      (effect (default #unspecified)))

	   (final-class sfun::fun
	      ;; a property list
	      (property::obj (default '()))
	      ;; the formals parameters
	      (args (default '()))
	      ;; the body
	      (body (default #unspecified))
	      ;; a class (should be SFUN, SIFUN, SNIFUN, SGFUN or SMFUN)
	      class
	      ;; dsssl keyword arguments encoding
	      (dsssl-keywords (default '()))
	      ;; the location, in the source file, of the function declaration.
	      ;; this field is not read-only because it will be set when
	      ;; encountering the global define which will occur after the
	      ;; global declaration (the module clause).
	      (loc (default #unspecified)))

	   (final-class cfun::fun
	      ;; the formal parameters' type
	      (args-type read-only)
	      ;; is it a macro function
	      (macro?::bool read-only)
	      ;; is it an infix macro ?
	      (infix?::bool (default #f))
	      ;; when compiling for the JVM, the kind of method this foreign is
	      (method::pair-nil (default '())))

	   (final-class svar::value
	      ;; the location, in the source file, of the variable declaration
	      (loc (default #unspecified)))

	   (final-class scnst::value
	      ;; a possible variable value
	      (node read-only)
	      ;; a class (should be SGFUN, SFUN, SSTRING, SREAL, STVECTOR)
	      class
	      ;; the location, in the source file, of the variable declaration
	      (loc (default #unspecified)))

	   (final-class cvar::value
	      ;; is it a macro variable
	      (macro?::bool read-only))

	   (final-class sexit::value
	      ;; the associated handling function
	      handler::obj
	      ;; is the handler detached (after globalize)
	      (detached?::bool (default #f)))

	   (class feffect
	      (read (default '()))
	      (write (default '())))

	   (global-read-only?::bool ::global)
	   (global-set-read-only! ::global)
	   (global-args-safe?::bool ::global)))

;*---------------------------------------------------------------------*/
;*    global-read-only? ...                                            */
;*---------------------------------------------------------------------*/
(define (global-read-only? global::global)
   (with-access::global global (pragma)
      (pair? (memq 'read-only pragma))))

;*---------------------------------------------------------------------*/
;*    global-set-read-only! ...                                        */
;*---------------------------------------------------------------------*/
(define (global-set-read-only! global::global)
   (with-access::global global (pragma)
      (set! pragma (cons 'read-only pragma))))

;*---------------------------------------------------------------------*/
;*    global-args-safe? ...                                            */
;*---------------------------------------------------------------------*/
(define (global-args-safe? global::global)
   (with-access::global global (value pragma)
      (or (not (cfun? value))
	  (not (cfun-macro? value))
	  (memq 'args-safe pragma))))

