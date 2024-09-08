(module saw_jvm_out
   (import type_type ast_var ast_node
	   engine_param
	   module_module
	   type_env
	   foreign_jtype
	   object_class
	   object_slots
	   tvector_tvector
	   read_jvm
	   backend_backend
	   backend_cplib
	   saw_defs
	   saw_jvm_names 
	   tools_shape)
   (export (open-class me::jvm class::type super)
	   (close-class me::jvm class::type)
	   (open-module me::jvm)
	   (close-module me::jvm)
	   (declare-global me::jvm var::global)
	   (compile-slot me::jvm field::slot)
	   (compile-global me::jvm var::global)
	   (open-global-method me::jvm var::global)
	   (close-method me::jvm)
	   (declare-locals me::jvm p l)
	   (localvar me::jvm r b e id)
	   (code! me::jvm ins)
	   (push-num me::jvm n type)
	   (push-int me::jvm n)
	   (push-string me::jvm s::bstring)
	   
	   (call-global me::jvm var::global)
	   (newobj me::jvm type::type gen types)
	   (load-field me::jvm type::type owner::type name::bstring)
	   (store-field me::jvm type::type owner::type name::bstring)
	   (label me::jvm lab)
	   (branch me::jvm cop lab)
	   (open-lib-method me::jvm id)
	   (compile-type me::jvm type::type)
	   *perso*))

(define *perso* #f)

;;
;; Classes & Modules
;;
(define (declare-class me::jvm name::symbol);
   (let ( (id (symbol-append 'c_ name)) )
      ;; CARE modifiers is needed only for the current class. Change Jas.
      (with-access::jvm me (declarations)
	 (unless (assq id declarations)
	    (set! declarations
		  (cons `(,id (class () ,(symbol->string name)))
			declarations )))
	 id )))

(define (open-class me::jvm class::type super);
   (with-access::jvm me (declarations fields methods)
      (define (super-name)
	 (if (eq? (type-name super) 'object)
	     "bigloo.object"
	     (if (eq? (type-name super) 'obj)
		 "java.lang.Object"
		 (type-name super) )))
      ;; CARE Why we don't have private classes.
      (set! declarations
	    (reverse (cons*
		      `(me (class (public) ,(type-name class)))
		      `(super (class () ,(super-name)))
		      (declare-lib) )))
      (set! fields '())
      (set! methods '()) ))

(define (close-class me::jvm class::type);
   (define (unqualified name)
      (let ( (u (suffix name)) )
	 (if (string=? u "") 
	     (string->symbol name)
	     (string->symbol u) )))
   (with-access::jvm me (classes declarations fields methods)
      (let ( (file (car *src-files*)) (name (type-name class)) )
	 (let ( (bfile (basename file)) )
	    (set! classes (cons `((class ,(unqualified name)) me super ()
					 (declare ,@(reverse! declarations))
					 (fields ,@fields)
					 (sourcefile ,bfile)
					 (sde ,bfile ,file)
					 ,@methods )
				classes ))))))

(define (declare-module me::jvm module::symbol)
   (if (eq? module *module*)
       'me
       (declare-class me (string->symbol (module->qualified-type module))) ))

(define (open-module me::jvm);
   (with-access::jvm me (declarations fields methods qname)
      (set! declarations
	    (reverse (cons*
		      `(me (class (public) ,(symbol->string qname)))
		      `(super (class () "bigloo.procedure"))
     		      (declare-lib) )))
      (set! fields '())
      (set! methods '()) ))

(define (close-module me::jvm);
   (with-access::jvm me (classes declarations fields methods)
      (let ( (file (car *src-files*)) )
	 (let ( (bfile (basename file)) )
	    ;; CARE May be some cleanup of me (decl fields methods classes)
	    `((module me procedure ()
		      (declare ,@(reverse! declarations))
		      (fields myname ,@fields)
		      (sourcefile ,bfile)
		      (sde ,bfile ,file)
		      ,@(reverse! methods) )
	      ,@classes )))))

(define (declare-lib)
   '(;; Java lib classes/methods
     (obj (class () "java.lang.Object"))
     (string (class () "java.lang.String"))
     (getbytes (method string () (vector byte) "getBytes"))
     (throwable (class () "java.lang.Throwable"))
     (runtimeexception (class () "java.lang.RuntimeException"))
     ;; Fields/Methods owned by the current class
     (myname (field me (public static) string "__the_module_name__"))
     (init (method me (public) void "<init>"))
     (super-init (method super (public) void "<init>"))
     (clinit (method me (public static) void "<clinit>"))
     (funcall0 (method me (public) obj "funcall0"))
     (funcall1 (method me (public) obj "funcall1" obj))
     (funcall2 (method me (public) obj "funcall2" obj obj))
     (funcall3 (method me (public) obj "funcall3" obj obj obj))
     (funcall4 (method me (public) obj "funcall4" obj obj obj obj))
     (apply (method me (public) obj "apply" obj))
     (main (method me (public static) void "main" (vector string)))
     (dlopen (method me (public static) void "BGL_DYNAMIC_LOAD_INIT"))
     ;; Fields/Methods of bigloo.procedure
     (procedure (class () "bigloo.procedure"))
     (procenv (field procedure () (vector obj) "env"))
     (procarity (field procedure () int "arity"))
     (procindex (field procedure () int "index"))
     (pfuncall4 (method procedure () obj "funcall4" obj obj obj obj))
     (pfuncall3 (method procedure () obj "funcall3" obj obj obj))
     (pfuncall2 (method procedure () obj "funcall2" obj obj))
     (pfuncall1 (method procedure () obj "funcall1" obj))
     (pfuncall0 (method procedure () obj "funcall0"))
     (papply (method procedure (public) obj "apply" obj))
     ;; Fields/Methods of bigloo.unspecified
     (unspecified (class () "bigloo.unspecified"))
     (*unspecified* (field unspecified () unspecified "unspecified"))
     ;; Fields/Methods of bigloo.nil
     (nil (class () "bigloo.nil"))
     (*nil* (field nil (static) nil "nil"))
     ;; Fields/Methods of bigloo.eol
     (eof (class () "bigloo.eof"))
     (*eof* (field eof () eof "eof"))
     ;; Fields/Methods of bigloo.optional
     (optional (class () "bigloo.optional"))
     (*optional* (field optional () optional "optional"))
     ;; Fields/Methods of bigloo.rest
     (rest (class () "bigloo.rest"))
     (*rest* (field rest () rest "rest"))
     ;; Fields/Methods of bigloo.nil
     (key (class () "bigloo.key"))
     (*key* (field key () key "key"))
     ;; Fields/Methods of bigloo.bbool
     (bbool (class () "bigloo.bbool"))
     (faux (field bbool (static) bbool "faux"))
     (vrai (field bbool (static) bbool "vrai"))
     ;; Fields/Methods of bigloo.cell
     (cell (class () "bigloo.cell"))
     (init_cell (method cell () void "<init>" obj))
     (ccar (field cell () obj "car"))
     ;; Fields/Methods of bigloo.pair
     (pair (class () "bigloo.pair"))
     (cdr (field pair () obj "cdr"))
     (car (field pair () obj "car"))
     (init_pair (method pair () void "<init>" obj obj))
     (cons (method pair () pair "cons" obj obj))
     ;; Fields/Methods of bigloo.extended_pair
     (extended_pair (class () "bigloo.extended_pair"))
     (cer (field extended_pair () obj "cer"))
     (init_extended_pair (method extended_pair () void "<init>"))
     ;; Fields/Methods of bigloo.bchar
     (bchar (class () "bigloo.bchar"))
     (bchar_value (field bchar () byte "value"))
     (bchar_allocated (field bchar () (vector bchar) "allocated"))
     ;; Fields/Methods of bigloo.bint
     (bint (class () "bigloo.bint"))
     (bint_value (field bint () int "value"))
     ;; Fields/Methods of bigloo.bucs
     (bucs2 (class () "bigloo.bucs2"))
     (init_bucs2 (method bucs2 () void "<init>" char))
     ;; Fields/Methods of bigloo.bllong
     (bllong (class () "bigloo.bllong"))
;     (make_llong (method bllong () bllong "make_llong" string))
     ;; Fields/Methods of bigloo.belong
     (belong (class () "bigloo.belong"))
;     (make_elong (method belong () belong "make_elong" string))
     ;; Fields/Methods of bigloo.real
     (real (class () "bigloo.real"))
     (real_value (field real () double "value"))
;   (init_real (method j_real () void "<init>" double))
;   (double_to_real (method j_foreign (static) j_real "DOUBLE_TO_REAL" double))
     ;; Fields/Methods of bigloo.bexception
     (bexception (class () "bigloo.bexception"))
     ;; Fields/Methods of bigloo.symbol
     (symbol (class () "bigloo.symbol"))
     (symbol_string (field symbol () (vector byte) "string"))
     ;; Fields/Methods of bigloo.exit
     (exit (class () "bigloo.exit"))
     (object (class () "bigloo.object"))
     (widening (field object () obj "widening"))
     (header (field object () int "header"))
     ;; Just for changing names
     (output-port (class () "bigloo.output_port"))
     (input-port (class () "bigloo.input_port"))
     (binary-port (class () "bigloo.binary_port"))
     ;; Fields/Methods of bigloo.foreign
     (foreign (class () "bigloo.foreign"))
     (listargv (method foreign () obj "listargv" (vector string)))
     (fail (method foreign () runtimeexception "fail" obj obj obj))
     (internalerror (method foreign () void "internalerror" throwable))
     (double_to_real (method foreign () real "DOUBLE_TO_REAL" double))
     (elong_to_belong (method foreign () belong "ELONG_TO_BELONG" long))
     (llong_to_bllong (method foreign () bllong "LLONG_TO_BLLONG" long))
     (jumpexit (method foreign () obj "jumpexit" obj obj))
     (debug_handler (method foreign () obj "debug_handler" bexception exit))
     (setexit (method foreign () obj "setexit"))
     ))

;   (math (class () "java.lang.Math"))
;   (bucs2_value (field j_bucs2 () char "value"))
;   (j_llong (class () "bigloo.bllong"))
;   (make_llong (method j_llong (static) j_llong "make_llong" string))
;   (j_cnst (class () "bigloo.cnst"))
;   (cnst_value (field j_cnst () int "value"))
;   (key_string (field j_keyword () (vector byte) "string"))
;   (j_procedure (class () "bigloo.procedure"))
;   (j_keyword (class () "bigloo.keyword"))
;   (j_bexception (class () "bigloo.bexception"))
;   (j_exit (class () "bigloo.exit"))
;   (j_struct (class () "bigloo.struct"))
;   (struct_key (field j_struct () obj "key"))
;   (struct_values (field j_struct () (vector obj) "values"))
;   (j_input (class () "bigloo.input_port"))
;   (io_name (field j_input () string "name"))
;   (io_filepos (field j_input () int "filepos"))
;   (io_bufsiz (field j_input () int "bufsiz"))
;   (io_other_eof (field j_input () boolean "other_eof"))
;   (io_start (field j_input () int "matchstart"))
;   (io_stop (field j_input () int "matchstop"))
;   (io_forward (field j_input () int "forward"))
;   (io_lastchar (field j_input () byte "lastchar"))
;   (io_abufsiz (field j_input () int "abufsiz"))
;   (io_buffer (field j_input () (vector byte) "buffer"))
;   (print (method j_foreign (static) void "print" string))
;   (eqbint (method j_foreign (static) boolean "eqbint" obj obj))
;   (internalerror (method j_foreign (static) void "internalerror" jthrowable))
;   (listargv (method j_foreign (static) obj "listargv" (vector string)))
;   (getbytes (method string () (vector byte) "getBytes"))
;   (concat (method string () string "concat" string))
;   (floor (method math (static) double "floor" double))
;   (ceil (method math (static) double "ceil" double))
;   (exp (method math (static) double "exp" double))
;   (log (method math (static) double "log" double))
;   (sin (method math (static) double "sin" double))
;   (cos (method math (static) double "cos" double))
;   (tan (method math (static) double "tan" double))
;   (asin (method math (static) double "asin" double))
;   (acos (method math (static) double "acos" double))
;   (atan (method math (static) double "atan" double))
;   (atan2 (method math (static) double "atan2" double double))
;   (sqrt (method math (static) double "sqrt" double))
;   (pow (method math (static) double "pow" double double))
;   (jumpexit (method j_foreign (static) obj "jumpexit" obj obj))
;   (setexit (method j_foreign (static) obj "setexit"))

		 

;;
;; Types
;;
(define (compile-type me::jvm type::type)
   (cond
      ((jvmbasic? type) (type-name type))
      ((tvec? type)
       `(vector ,(compile-type me (tvec-item-type type))) )
      ((jarray? type)
       `(vector ,(compile-type me (jarray-item-type type))) )
      (else (let ( (name (type-name type)) )
	       (if (symbol? name)
		   name
		   (declare-class me (string->symbol name)) )))))

(define (compile-bad-type me::jvm type)
   (cond
      ((local? type)
       (compile-type me (local-type type)) )
      ((type? type)
       (compile-type me type) )
      (else (error 'compile-bad-type "unknown type" type)) ))

(define (id-type type)
   (if (pair? type)
       (string-append "*" (id-type (cadr type)))
       (symbol->string type) ))

;;
;; Fields & Globals
;;
(define (declare-field::symbol me::jvm owner::symbol mod t name::bstring);
   (let ((id (symbol-append 'f_ owner '_ (string->symbol name))) )
      (with-access::jvm me (declarations)
	 (unless (assq id declarations)
	    (set! declarations
		  (cons `(,id (field ,owner ,mod ,(compile-type me t) ,name))
			declarations ))))
      id ))

(define (declare-global me::jvm var::global);
   (with-access::global var (name module type)
      (declare-field me (declare-module me module) '() type name) ))


(define (add-field me::jvm id::symbol);
   (jvm-fields-set! me (cons id (jvm-fields me)))
   id )

(define (compile-slot me::jvm field::slot);
   ;; CARE Why we don't have private field.
   (add-field me (declare-field me 'me '(public)
				(get-field-type field)
				(slot-name field) )))

(define (compile-global me::jvm var::global);
   (add-field me (declare-field me 'me
				(cons (if (eq? (global-import var) 'export)
					  'public
					  'private )
				      '(static) )
				(global-type var)
				(global-name var) )))

;;
;; Methods
;;
(define (declare-method me::jvm id owner mod type name args);
   (with-access::jvm me (declarations)
      (unless (assq id declarations)
	 (set! declarations
	       (cons `(,id (method ,owner ,mod ,type ,name ,@args))
		     declarations ))))
   id )

(define (declare-global-method me::jvm var::global);
   (define (args var)
      (let ( (fun (global-value var)) )
	 (if (cfun? fun)
	     (if (memq 'static (cfun-method fun))
		 (cfun-args-type fun)
		 (cdr (cfun-args-type fun)) )
	     (sfun-args fun) )))
   (define (modifier var)
      (let ( (import (global-import var)) )
	(case import
	   ((import foreign) '(static))
	   ((static) '(private static))
	   ((export) '(public static))
	   (else (error "modifier" "unknown modifier" import)) )))
   (with-access::global var (id module type name)
      (let ( (id (symbol-append 'm_ module '_ id)) )
	 (declare-method me id (declare-module me module)
			 (modifier var)
			 (compile-type me type)
			 name
			 (map (lambda (a) (compile-bad-type me a))
			      (args var) )))))

(define (open-lib-method me::jvm id)
   (with-access::jvm me (current-method)
      (set! current-method id) ))

(define (open-global-method me::jvm var::global)
   (with-access::jvm me (current-method)
      (set! current-method (declare-global-method me var)) ))

(define (close-method me::jvm);
   (with-access::jvm me (current-method methods code)
      (set! methods (cons `(method ,current-method ,@(reverse! code))
			  methods ))
      (set! code '()) ))

;;
;; Local variables
;;
(define (declare-locals me::jvm p l);
   (with-access::jvm me (code)
      (set! code (cons* l p '())) ))

(define (localvar me::jvm r b e id);
   (with-access::rtl_reg r (type var)
      (when (and (local? var) (local-user? var))
	 (let ( (user-name (symbol->string (local-id var)))
		(type (compile-type me type)) )
	    (code! me `(localvar ,b ,e ,user-name ,type ,id)) ))))

;;
;; Simple instructions
;;
(define (code! me::jvm ins);
   (with-access::jvm me (code)
      (set! code (cons ins code)) ))

;;
;; Constants
;;
(define (push-num me::jvm n type);
   (code! me
	  (case type
	     ((float)
	      (cond ((= n 0.0) '(fconst_0))
		    ((= n 1.0) '(fconst_1))
		    ((= n 2.0) '(fconst_2))
		    (else `(ldc ,n)) ))
	     ((double)
	      (cond ((= n 0.0) '(dconst_0))
		    ((= n 1.0) '(dconst_1))
		    (else `(ldc2_w ,n)) ))
	     ((long elong llong)
	      (cond ((= n 0) '(lconst_0))
		    ((= n 1) '(lconst_1))
		    (else `(ldc2_w ,n)) ))
	     (else ;(boolean byte char short int)
	      (case n
		 ((-1) '(iconst_m1))
		 ((0)  '(iconst_0))
		 ((1)  '(iconst_1))
		 ((2)  '(iconst_2))
		 ((3)  '(iconst_3))
		 ((4)  '(iconst_4))
		 ((5)  '(iconst_5))
		 (else
		  (cond ((and (> n -129) (< n 128)) `(bipush ,n))
			((and (> n -32769) (< n 32768)) `(sipush ,n))
			(else `(ldc ,n)) )))))))

(define (push-int me::jvm n);
   (push-num me n 'int) )

(define (push-string me::jvm s::bstring);
   (code! me `(ldc ,s)) )

;(define (create-string env str)
;   (split-string env str 0 (string-length str))
;   (_invokevirtual env (jlib-declare env 'getbytes) '(ad) 'ad) )

;(define (split-string env str i n)
;   (if (< n 65536)
;       (_push env 'string (if (= i 0) str (substring str i (+ i n))))
;       (begin
;	  (_push env 'string (substring str i (+ i 65535)))
;(split-string env str (+ i 65535) (- n 65535))
;	  (_invokevirtual env (jlib-declare env 'concat) '(ad ad) 'ad) )))

;;
;; Method call
;;
(define (call-constructor me::jvm owner::symbol params)
   (let ( (types (map (lambda (t) (compile-type me t)) params)) )
      (let ( (s (apply string-append (map id-type types))) )
	 (let ( (id (symbol-append 'c_ owner '_ (string->symbol s))) )
	    (code! me `(invokespecial
			,(declare-method me id owner
					 '(public static)
					 'void "<init>" types )))))))

(define (call-global me::jvm var::global);
   (define (callop v)
      (let ( (fun (variable-value v)) )
	 (if (cfun? fun)
	     (let ( (modifiers (cfun-method fun)) )
		(cond
		   ((memq 'static modifiers) 'invokestatic)
		   ((memq 'abstract modifiers) 'invokeinterface)
		   ((memq 'final modifiers) 'invokespecial)
		   ((memq 'native modifiers) 'invokespecial)
		   (else 'invokevirtual) ))
	     'invokestatic )))
   (code! me `(,(callop var) ,(declare-global-method me var))) )

;;
;; Instructions on objects
;;
(define (newobj me::jvm type::type gen params);
   (let ( (c (compile-type me type)) )
      (code! me `(new ,c))
      (code! me '(dup))
      (gen)
      (call-constructor me c params) ))

(define (load-field me::jvm type::type owner::type name::bstring);
   (let ( (o (compile-type me owner)) )
      (code! me `(getfield ,(declare-field me o '() type name))) ))

(define (store-field me::jvm type::type owner::type name::bstring);
   (let ( (o (compile-type me owner)) )
      (code! me `(putfield ,(declare-field me o '() type name))) ))

;;
;; Branch instructions
;;
(define (check-label lab)
   (cond
      ((symbol? lab) lab)
      ((string? lab) (string->symbol lab))
      ((integer? lab)
       (string->symbol (string-append "L" (integer->string lab))) )
      (else (error 'label "wrong name" lab)) ))

(define (label me::jvm lab);
   (code! me (check-label lab)) )

(define (branch me::jvm cop lab)
   (code! me `(,cop ,(check-label lab))) )

