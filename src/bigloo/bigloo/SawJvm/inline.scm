(module saw_jvm_inline
   (import type_type ast_var ast_node
	   engine_param
	   type_env
	   object_class
	   object_slots
	   module_module
	   cnst_alloc
	   read_jvm
	   backend_backend
	   saw_defs
	   saw_jvm_out
	   saw_jvm_code
	   )
   (export (inline-call? me::jvm var::global)
	   (inline-predicate? me::jvm var::global on? lab)
	   (inline-call-with-args? me::jvm var::global args)) )

;;
(define (inline-call? me::jvm var::global)
   (let ( (fun (getprop (global-id var) 'saw_jvm_inline_function)) )
      (if fun
	  (fun me)
	  (let ( (name (global-name var)) (id (global-id var)) )
;* 	     (when (and (string=? name (string-upcase name))           */
;* 			(not (memq id *too-hard*)) )                   */
;* 		(print "DOINLINE " name " " id) )                      */
	     'not-inlined ))))

(define *too-hard* '(%exit long->bint c-cons c-write-char))

(define-macro (define-inline-call name . body)
   `(putprop! ',name 'saw_jvm_inline_function
	      (lambda (me) ,@body) ))

;;
(define (inline-call-with-args? me::jvm var::global args)
   (let ( (fun (getprop (global-id var) 'saw_jvm_inline_function_args)) )
      (if fun
	  (fun me args)
	  'not-inlined )))

(define-macro (define-inline-call-args name . body)
   `(putprop! ',name 'saw_jvm_inline_function_args
	      (lambda (me args) ,@body) ))

;;
(define (inline-predicate? me::jvm var::global on? lab)
   (let ( (fun (getprop (global-id var) 'saw_jvm_inline_predicate)) )
      (if fun
	  (fun me on? lab)
	  'not-inlined )))

(define-macro (define-inline-predicate name . body)
   `(putprop! ',name 'saw_jvm_inline_predicate
	      (lambda (me on? lab) ,@body) ))

;;
(define (skip-mov arg)
   (if (rtl_ins? arg)
       (let ( (fun (rtl_ins-fun arg)) )
	  (if (rtl_mov? fun)
	      (skip-mov (car (rtl_ins-args arg)))
	      fun ))
       arg ))

;;;
;; Mandatory
;;;
(define-inline-call-args cnst-table-ref
   (code! me `(getstatic ,(declare-global me (get-cnst-table))))
   (gen-expr me (car args))
   (code! me '(aaload)) )

(define-inline-call-args cnst-table-set!
   (code! me `(getstatic ,(declare-global me (get-cnst-table))))
   (gen-expr me (car args))
   (gen-expr me (cadr args))
   (code! me '(aastore))
   'no-value )

(define-inline-call-args make-fx-procedure
   (code! me '(new me))
   (code! me '(dup))
   (code! me '(invokespecial init))
   (code! me '(dup))
   (gen-expr me (car args))
   (code! me '(putfield procindex))
   (code! me '(dup))
   (gen-expr me (cadr args))
   (code! me '(putfield procarity))
   (code! me '(dup))
   (gen-expr me (caddr args))
   (code! me '(anewarray obj))
   (code! me '(putfield procenv)) )

(define-inline-call-args make-va-procedure
   (code! me '(new me))
   (code! me '(dup))
   (code! me '(invokespecial init))
   (code! me '(dup))
   (gen-expr me (car args))
   (code! me '(putfield procindex))
   (code! me '(dup))
   (gen-expr me (cadr args))
   (code! me '(putfield procarity))
   (code! me '(dup))
   (gen-expr me (caddr args))
   (code! me '(anewarray obj))
   (code! me '(putfield procenv)) )

(define-inline-call-args make-l-procedure
   (code! me '(new me))
   (code! me '(dup))
   (code! me '(invokespecial init))
   (code! me '(dup))
   (gen-expr me (car args))
   (code! me '(putfield procindex))
   (code! me '(dup))
   (gen-expr me (cadr args))
   (code! me '(anewarray obj))
   (code! me '(putfield procenv)) )

(define-inline-call make-el-procedure
   (code! me '(anewarray obj)) )

(define-inline-call procedure-el-ref
   (code! me '(aaload)) )

(define-inline-call procedure-el-set!
   (code! me '(aastore))
   'no-value )

(define-inline-call-args make-el-procedure-1
   (code! me '(aconst_null)) )

(define-inline-call procedure-1-el-ref
   (code! me '(pop)) )

;(define-inline-call procedure-1-el-set!) done in node2rtl

;
;;;
;;; BOOLEAN
;;;
(define-inline-call c-boolean? ; "INTEGERP"
   (code! me '(instanceof bbool)) )

(define-inline-predicate obj->bool ; "CBOOL"
   (code! me '(getstatic faux))
   (branch me (if on? 'if_acmpne 'if_acmpeq) lab) )

(define-inline-call-args bool->bbool ; "BBOOL"
   (let ( (fun (skip-mov (car args))) )
      (if (rtl_loadi? fun)
	  (if (atom-value (rtl_loadi-constant fun))
	      (code! me '(getstatic vrai))
	      (code! me '(getstatic faux)) )
	  (let ( (l1 (gensym "I")) (l2 (gensym "I")) )
	     (gen-expr me (car args))
	     (code! me `(ifeq ,l1))
	     (code! me '(getstatic vrai))
	     (code! me `(goto ,l2))
	     (code! me l1)
	     (code! me '(getstatic faux))
	     (code! me l2) ))))
;
;(define-inline-call ("BOXED_EQ" env)
;   (if_acmp-return-boolean-for env 'eq) )
;

(define-inline-predicate c-boxed-eq? ; "BOXED_EQ"
   (branch me (if on? 'if_acmpeq 'if_acmpne) lab) )

(define-inline-predicate c-eq? ; "EQ"
   (if *optim-jvm-fasteq*
       (branch me (if on? 'if_acmpeq 'if_acmpne) lab)
       'not-inlined ))

;;;
;;; CHARACTER
;;;
(define-inline-call c-char? ; "CHARP"
   (code! me '(instanceof bchar)) )
;
(define-inline-call-args uchar->bchar ; "BCHAR"
   (code! me `(getstatic bchar_allocated))
   (gen-expr me (car args))
   (code! me '(sipush 255))
   (code! me '(iand))
   (code! me '(aaload)) )

(define-inline-call bchar->char ; "CCHAR"
   (code! me '(getfield bchar_value)) )

(define-inline-call bchar->uchar ; "BCHAR_TO_UCHAR"
   (code! me `(getfield bchar_value)) )

(define-inline-call char->uchar ; "CHAR_TO_UCHAR"
   (code! me '(sipush 255))
   (code! me '(iand)) )

(define-inline-call uchar->char ; "UCHAR_TO_CHAR"
   (code! me '(i2b)) )

(define-inline-predicate c-char=? ; "CHAR_EQ"
   (branch me (if on? 'if_icmpeq 'if_icmpne) lab) )

(define-inline-predicate c-char<? ; "CHAR_LT"
   (branch me (if on? 'if_icmplt 'if_icmpge) lab) )

(define-inline-predicate c-char<=? ; "CHAR_LE"
   (branch me (if on? 'if_icmple 'if_icmpgt) lab) )

(define-inline-predicate c-char>? ; "CHAR_GT"
   (branch me (if on? 'if_icmpgt 'if_icmple) lab) )

(define-inline-predicate c-char>=? ; "CHAR_GE"
   (branch me (if on? 'if_icmpge 'if_icmplt) lab) )

;(define-inline-call ("CHAR_OR" env)
;   (_ior env) )
;
;(define-inline-call ("CHAR_AND" env)
;   (_iand env) )
;
;(define-inline-call ("CHAR_NOT" env)
;   (_push env 'int -1)
;   (_ixor env)
;   (_i2b env) )

;;;
;;; INTEGER
;;;
(define-inline-call c-fixnum? ; "INTEGERP"
   (code! me '(instanceof bint)) )

(define-inline-call c-elong? ; "ELONGP"
   (code! me '(instanceof belong)) )

;(define-inline-call ("LLONGP" env)
;   (_instanceof env (jlib-declare env 'j_llong)) )

(define-inline-call c-char->integer ; "CHAR_TO_INT"
   (code! me '(sipush 255))
   (code! me '(iand)) )

;(define-inline-call ("INT_TO_CHAR" env)
;   (if *longislong* (_l2i env)) )

(define-inline-call int->short ; "INT_TO_SHORT"
   (code! me '(i2s)) )

(define-inline-call short->int ; "SHORT_TO_INT"
   'ok )

;   public static int SHORT_TO_LONG(short n)
;   {
;      return n;
;   }

(define-inline-call bint->long ; "BINT_TO_LONG"
   (code! me '(getfield bint_value)) )

;(define-inline-call ("BINT_TO_ULONG" env)
;   (bint-value env) )

(define-inline-call int->long ; "INT_TO_LONG"
   'ok )

(define-inline-call long->int ; "LONG_TO_INT"
   'ok )

;   public static int ULONG_TO_INT(int n)
;   {
;      return n;
;   }

;;; public static BLLONG LLONG_TO_BLLONG(long n)
;
;   public static long LONG_TO_LLONG(int n)
;   {
;      return n;
;   }
;
;   public static long BLLONG_TO_LLONG(bllong n)
;   {
;      return n.value;
;   }
;;; public static BELONG LONG_TO_BELONG(int n)

;   public static long LONG_TO_ELONG(int n)
;   {
;      return n;
;   }
;
;   public static long BELONG_TO_LONG(belong n)
;   {
;      return n.value;
;   }
;
;   public static long BELONG_TO_ELONG(belong n)
;   {
;      return n.value;
;   }
;
;   public static belong ELONG_TO_BELONG(long n)
;   {
;      return new belong(n);
;   }
;;;; public static BLLONG LONG_TO_BLLONG(int n)

;; public static BINT BINT(long v)
;; public static BINT BINT(int v)

(define-inline-call bint->int ; "CINT"
   (code! me '(getfield bint_value)) )

;   public static boolean EQ_ELONG(long n1, long n2)
;   {
;      return (n1 == n2);
;   }
;
;   public static boolean EQ_LLONG(long n1, long n2)
;   {
;      return (n1 == n2);
;   }

(define (compute-boolean me cop)
   ;; CARE check if inversing the predicate may generate better code.
   (let ( (l1 (gensym "I")) (l2 (gensym "I")) )
      (code! me `(,cop ,l1))
      (code! me '(iconst_0))
      (code! me `(goto ,l2))
      (code! me l1)
      (code! me '(iconst_1))
      (code! me l2) ))

(define-inline-call c-=fx ; "EQ_FX"
   (compute-boolean me 'if_icmpeq) )

(define-inline-call c-<fx ; "LT_FX"
   (compute-boolean me 'if_icmplt) )

(define-inline-call c-<=fx ; "LE_FX"
   (compute-boolean me 'if_icmple) )

(define-inline-call c->fx ; "GT_FX"
   (compute-boolean me 'if_icmpgt) )

(define-inline-call c->=fx ; "GE_FX"
   (compute-boolean me 'if_icmpge) )

(define-inline-predicate c-=fx ; "EQ_FX"
   (branch me (if on? 'if_icmpeq 'if_icmpne) lab) )

(define-inline-predicate c-<fx ; "LT_FX"
   (branch me (if on? 'if_icmplt 'if_icmpge) lab) )

(define-inline-predicate c-<=fx ; "LE_FX"
   (branch me (if on? 'if_icmple 'if_icmpgt) lab) )

(define-inline-predicate c->fx ; "GT_FX"
   (branch me (if on? 'if_icmpgt 'if_icmple) lab) )

(define-inline-predicate c->=fx ; "GE_FX"
   (branch me (if on? 'if_icmpge 'if_icmplt) lab) )

(define-inline-predicate c-even? ; "EVENP_FX"
   (code! me '(iconst_1))
   (code! me '(iand))
   (branch me (if on? 'ifeq 'ifne) lab) )

(define-inline-predicate c-odd? ; "ODDP_FX"
   (code! me '(iconst_1))
   (code! me '(iand))
   (branch me (if on? 'ifne 'ifeq) lab) )

(define-inline-call c-+fx (code! me '(iadd))) ; "PLUS_FX"
(define-inline-call c--fx (code! me '(isub))) ; "MINUS_FX"
(define-inline-call c-*fx (code! me '(imul))) ; "MUL_FX"
(define-inline-call c-/fx (code! me '(idiv))) ; "DIV_FX"
;(define-inline-call ("NEG_FX"       env) (if *longislong* (_lneg  env) (_ineg  env)))
(define-inline-call c-quotient (code! me '(idiv))) ; "QUOTIENT_FX"
(define-inline-call c-remainder (code! me '(irem))) ; "REMAINDER_FX"
(define-inline-call c-bitor (code! me '(ior))) ; "BITOR"
(define-inline-call c-bitand (code! me '(iand))) ;"BITAND"
(define-inline-call c-bitxor (code! me '(ixor))) ; "BITXOR"
;(define-inline-call ("BITRSH"       env) (if *longislong* (_lshr  env) (_ishr  env)))
;(define-inline-call ("BITURSH"      env) (if *longislong* (_lushr env) (_iushr env)))
;(define-inline-call ("BITLSH"       env) (if *longislong* (_lshl  env) (_ishl  env)))

(define-inline-call c-bitnot ; "BITNOT"
   (code! me '(iconst_1))
   (code! me '(ineg))
   (code! me '(ixor)) )

;;;
;;; FLOAT
;;;
(define-inline-call c-flonum? ; "REALP"
   (code! me '(instanceof real)) )

(define-inline-call real->double ; "REAL_TO_DOUBLE"
   (code! me '(getfield real_value)) )

;(define-inline-call ("REAL_TO_FLOAT" env)
;   (_getfield env 'double (jlib-declare env 'real_value))
;   (_d2f env) )

;(define-inline-call-args ("DOUBLE_TO_REAL" v args env)
;   (_new env (jlib-declare env 'j_real))
;   (_dup env)
;   (compile-expr (car args) env)
;   (if *purify*
;       (_invokespecial env (jlib-declare env 'init_real) '(ad double) 'void)
;       (_putfield env (jlib-declare env 'real_value)) ))

;(define-inline-call-args ("FLOAT_TO_REAL" v args env)
;   (_new env (jlib-declare env 'j_real))
;   (_dup env)
;   (compile-expr (car args) env)
;   (_f2d env)
;   (if *purify*
;       (_invokespecial env (jlib-declare env 'init_real) '(ad double) 'void)
;       (_putfield env (jlib-declare env 'real_value)) ))

;(define-inline-call ("DOUBLE_TO_FLOAT" env)
;   (_d2f env) )

;(define-inline-call ("FLOAT_TO_DOUBLE" env)
;   (_f2d env) )

(define-inline-call c-fixnum->flonum ; "FIXNUM_TO_FLONUM"
   (code! me '(i2d)) )

(define-inline-call c-flonum->fixnum ; "FLONUM_TO_FIXNUM"
   (code! me '(d2i)) )

(define-inline-call  c-elong->flonum; "ELONG_TO_FLONUM"
   (code! me '(l2d)) )

(define-inline-call  c-flonum->elong ; "FLONUM_TO_ELONG"
   (code! me '(d2l)) )

(define-inline-call  c-llong->flonum; "LLONG_TO_FLONUM"
   (code! me '(l2d)) )

(define-inline-call  c-flonum->llong ; "FLONUM_TO_LLONG"
   (code! me '(d2l)) )

(define-inline-predicate c-=fl ; "EQ_FL"
   (code! me '(dcmpg))
   (branch me (if on? 'ifeq 'ifne) lab) )

(define-inline-predicate c-<fl ; "LT_FL"
   (code! me '(dcmpl))
   (branch me (if on? 'iflt 'ifge) lab) )

(define-inline-predicate c-<=fl ; "LE_FL"
   (code! me '(dcmpl))
   (branch me (if on? 'ifle 'ifgt) lab) )

(define-inline-predicate c->fl ; "GT_FL"
   (code! me '(dcmpg))
   (branch me (if on? 'ifgt 'ifle) lab) )

(define-inline-predicate c->=fl ; "GE_FL"
   (code! me '(dcmpg))
   (branch me (if on? 'iflt 'ifge) lab) )

(define-inline-call c-+fl (code! me '(dadd))) ; "PLUS_FL"
(define-inline-call c--fl (code! me '(dsub))) ; "MINUS_FL"
(define-inline-call c-*fl (code! me '(dmul))) ; "MUL_FL"
(define-inline-call c-/fl (code! me '(ddiv))) ; "DIV_FL"
(define-inline-call c-negfl (code! me '(dneg))) ; "NEG_FL"

;(define-inline-call ("fmod"     env) (_drem env))

;(define-inline-call ("floor" env) (dop1 env 'floor))
;(define-inline-call ("ceil"  env) (dop1 env 'ceil))
;(define-inline-call ("exp"   env) (dop1 env 'exp))
;(define-inline-call ("log"   env) (dop1 env 'log))
;(define-inline-call ("sin"   env) (dop1 env 'sin))
;(define-inline-call ("cos"   env) (dop1 env 'cos))
;(define-inline-call ("tan"   env) (dop1 env 'tan))
;(define-inline-call ("asin"  env) (dop1 env 'asin))
;(define-inline-call ("acos"  env) (dop1 env 'acos))
;(define-inline-call ("atan"  env) (dop1 env 'atan))
;(define-inline-call ("atan2" env) (dop2 env 'atan2))
;(define-inline-call ("sqrt"  env) (dop1 env 'sqrt))
;(define-inline-call ("pow"   env) (dop2 env 'pow))
;
;(define (dop1 env name)
;   (_invokestatic env (jlib-declare env name) '(double) 'double) )
;
;(define (dop2 env name)
;   (_invokestatic env (jlib-declare env name) '(double double) 'double) )
;
;;;
;;; CONSTANTS
;;;
;(define-inline-call ("EOF_OBJECTP" env)
;   (_instanceof env (jlib-declare env 'j_eof)) )

(define-inline-call c-null? ; "NULLP"
   (code! me '(getstatic *nil*))
   (compute-boolean me 'if_acmpeq) )

(define-inline-predicate c-null? ; "NULLP"
   (code! me '(getstatic *nil*))
   (branch me (if on? 'if_acmpeq 'if_acmpne) lab) )

;(define-inline-call ("CCNST" env)
;   (if *purify* (_checkcast env 'j_cnst))
;   (_getfield env 'int (jlib-declare env 'cnst_value))
;   (if *longislong* (_i2l env)) )
;
;;; public static CNST BCNST(int v)
;;; public static boolean CNSTP(Object o)
;
;(define-inline-call-args ("POINTERP" v args env)
;   (compile-effect (car args) env)
;   (_push env 'int 1) )
;
;(define-inline-call-args ("OPAQUEP" v args env)
;   (compile-effect (car args) env)
;   (_push env 'int 0) )
;
;(define-inline-call ("BGL_OBJECTP" env)
;   (_instanceof env (jlib-declare env 'j_object)) )
;
;;;
;;; Unicode characters
;;;
;
;;; Delayed until write corrected and C ucs2->integer fixed
;
;;;
;;; Unicode strings
;;;
;
;;; Also delayed
;
;;;
;;; PROCESS
;;;
;
;;; Delayed
;
;;;
;;; STRING
;;;
(define-inline-call c-string? ; "STRINGP"
   (code! me '(instanceof (vector byte))) )

(define-inline-call string->bstring ; "string_to_bstring"
   'ok )

(define-inline-call bstring->string ; "BSTRING_TO_STRING"
   'ok )

(define-inline-call c-string-ref ; "STRING_REF"
   (code! me '(baload))
   (code! me '(sipush 255))
   (code! me '(iand)) )

(define-inline-call c-string-set! ; "STRING_SET"
   (code! me '(bastore))
   'no-value )

(define-inline-call c-string-length ; "STRING_LENGTH"
   (code! me '(arraylength)) )

(define-inline-call c-make-string-wo/fill ; "make_string_sans_fill"
   (code! me '(newarray byte)) )

;;;
;;; KEYWORD
;;;
;(define-inline-call ("KEYWORDP" env)
;   (_instanceof env (jlib-declare env 'j_keyword)) )
;
;(define-inline-call ("KEYWORD_TO_STRING" env)
;   (_getfield env 'ad (jlib-declare env 'key_string)) )
;
;;;public static KEYWORD string_to_keyword(byte[] s) {
;;;	return(KEYWORD.make_keyword(s));
;;;    }
;
;;;
;;; SYMBOL
;;;
(define-inline-call c-symbol? ; "SYMBOLP"
   (code! me '(instanceof symbol)) )

(define-inline-call c-symbol->string ; "SYMBOL_TO_STRING"
   (code! me '(getfield symbol_string)) )

;;;
;;; CELL
;;;
;(define-inline-call ("CELLP" env)
;   (_instanceof env (jlib-declare env 'j_cell)) )
;
;(define-inline-call-args ("MAKE_CELL" v args env)
;   (_new env 'j_cell)
;   (_dup env)
;   (compile-expr (car args) env)
;   (if *purify*
;       (_invokespecial env (jlib-declare env 'init_cell) '(ad ad) 'void)
;       (_putfield env (jlib-declare env 'ccar)) ))

(define-inline-call cell-set! ; "CELL_SET"
   (code! me '(putfield ccar))
   'no-value )

(define-inline-call cell-ref ; "CELL_REF"
   (code! me '(getfield ccar)) )

;(define-inline-call-args ("_EVMEANING_ADDRESS" v args env)
;   (_new env 'j_cell)
;   (_dup env)
;   (compile-expr (car args) env)
;   (if *purify*
;       (_invokespecial env (jlib-declare env 'init_cell) '(ad) 'void)
;       (_putfield env (jlib-declare env 'ccar)) ))
;
;(define-inline-call ("_EVMEANING_ADDRESS_REF" env)
;   (_getfield env 'jobject (jlib-declare env 'ccar)) )
;
;(define-inline-call ("_EVMEANING_ADDRESS_SET" env)
;   (_putfield env (jlib-declare env 'ccar))
;   (_getstatic env (jlib-declare env 'unspecified)) )

;;;
;;; FOREIGN
;;;
;;; Delayed
;
;;;
;;; CUSTOM
;;;
;;; Delayed
;
;;;
;;; PAIR
;;;
(define-inline-call c-pair? ; "PAIRP"
   (code! me '(instanceof pair)) )

;(define-inline-call-args c-cons ; "MAKE_PAIR"
;   (code! me '(new pair))
;   (code! me '(dup))
;   (gen-expr me (car args))
;   (gen-expr me (cadr args))
;   (code! me '(invokespecial init_pair)) )

(define-inline-call c-car ; "CAR"
   (code! me '(getfield car)) )

(define-inline-call c-cdr ; "CDR"
   (code! me '(getfield cdr)) )

(define-inline-call c-set-car! ; "SET_CAR"
   (code! me '(putfield car))
   'no-value )

(define-inline-call c-set-cdr! ; "SET_CDR"
   (code! me '(putfield cdr))
   'no-value )

;;;
;;; EXTENDED PAIR
;;;
(define-inline-call c-epair? ; "EXTENDED_PAIRP"
   (code! me '(instanceof extended_pair)) )
;
;(define-inline-call-args ("MAKE_EXTENDED_PAIR" v args env)
;   (_new env 'j_extended_pair)
;   (if *purify*
;       (begin (_dup env)
;	      (_invokespecial env (jlib-declare env 'init_extended_pair) '(ad) 'void) ))
;   (_dup env)
;   (compile-expr (car args) env)
;   (_putfield env (jlib-declare env 'car))
;   (_dup env)
;   (compile-expr (cadr args) env)
;   (_putfield env (jlib-declare env 'cdr))
;   (_dup env)
;   (compile-expr (caddr args) env)
;   (_putfield env (jlib-declare env 'cer)) )

(define-inline-call c-cer ; "CER"
   (code! me '(getfield cer)) )

(define-inline-call c-set-cer! ; "SET_CER"
   (code! me '(putfield cer))
   'no-value )

;;;
;;; VECTOR
;;;
(define-inline-call c-vector? ; "VECTORP"
   (code! me '(instanceof (vector obj))) )

;(define-inline-call ("VECTOR_LENGTH" env)
;   (_arraylength env) )
;
;(define-inline-call ("VECTOR_REF" env)
;   (_aload env 'jobject) )
;
;(define-inline-call ("VECTOR_SET" env)
;   (_astore env 'jobject)
;   (_getstatic env (jlib-declare env 'unspecified)) )
;
;(define-inline-call ("BOUND_CHECK" env)
;   (if_icmp-return-boolean env 'lt) )
;
;(define-inline-predicate ("BOUND_CHECK" lt lf env)
;   (icompare 'ge lt lf env) )
;
;(define-inline-call ("VECTOR_TAG_SET" env)
;   (_pop env 'int)
;   (_pop env '(vector jobject))
;   (_getstatic env (jlib-declare env 'unspecified)) )
;
;(define-inline-call ("VECTOR_TAG" env)
;   (_pop env '(vector jobject))
;   (_push env 'int 0) )
;
;(define-inline-call ("create_vector" env)
;   (_newarray env 'jobject) )
;
;;;
;;; TVECTOR
;;;
;
;;;
;;; STRUCT
;;;
;(define-inline-call ("STRUCTP" env)
;   (_instanceof env (jlib-declare env 'j_struct)) )
;
;(define-inline-call ("STRUCT_KEY" env)
;   (_getfield env 'jobject (jlib-declare env 'struct_key)) )
;
;(define-inline-call ("STRUCT_KEY_SET" env)
;   (_putfield env (jlib-declare env 'struct_key))
;   (_getstatic env (jlib-declare env 'unspecified)) )
;
;(define-inline-call ("STRUCT_LENGTH" env)
;   (_getfield env 'jobject (jlib-declare env 'struct_values))
;   (_arraylength env) )
;
;(define-inline-call-args ("STRUCT_REF" v args env)
;   (compile-expr (car args) env)
;   (if *purify* (_checkcast env 'j_struct))
;   (_getfield env 'jobject (jlib-declare env 'struct_values))
;   (compile-expr (cadr args) env)
;   (_aload env 'jobject) )
;
;(define-inline-call-args ("STRUCT_SET" v args env)
;   (compile-expr (car args) env)
;   (if *purify* (_checkcast env 'j_struct))
;   (_getfield env 'jobject (jlib-declare env 'struct_values))
;   (compile-expr (cadr args) env)
;   (compile-expr (caddr args) env)
;   (_astore env 'jobject)
;   (_getstatic env (jlib-declare env 'unspecified)) )
;
;(define-inline-call-args ("UNSAFE_STRUCT_REF" v args env)
;   (compile-expr (car args) env)
;   (if *purify* (_checkcast env 'j_struct))
;   (_getfield env 'jobject (jlib-declare env 'struct_values))
;   (compile-expr (cadr args) env)
;   (_aload env 'jobject) )
;
;(define-inline-call-args ("UNSAFE_STRUCT_SET" v args env)
;   (compile-expr (car args) env)
;   (if *purify* (_checkcast env 'j_struct))
;   (_getfield env 'jobject (jlib-declare env 'struct_values))
;   (compile-expr (cadr args) env)
;   (compile-expr (caddr args) env)
;   (_astore env 'jobject)
;   (_getstatic env (jlib-declare env 'unspecified)) )
;
;;; public static STRUCT create_struct(SYMBOL key, int size)
;;; public static STRUCT make_struct(SYMBOL key, int size, Object o)
;
;;;
;;; OBJECT
;;;
;(define-inline-call ("BGL_OBJECT_WIDENING_SET" env)
;   (_putfield env (jlib-declare env 'widening))
;   (_getstatic env (jlib-declare env 'unspecified)) )
;
;(define-inline-call ("BGL_OBJECT_WIDENING" env)
;   (_getfield env 'jobject (jlib-declare env 'widening)) )
;
;(define-inline-call ("BGL_OBJECT_CLASS_NUM" env)
;   (_getfield env 'int (jlib-declare env 'header)) )
;
;(define-inline-call ("BGL_OBJECT_CLASS_NUM_SET" env)
;   (_putfield env (jlib-declare env 'header))
;   (_getstatic env (jlib-declare env 'unspecified)) )
;
;(define-inline-call ("BGL_HEAP_DEBUG_MARK_OBJ" env)
;   #t )
;
;;;
;;; PROCEDURE
;;;
(define-inline-call c-procedure? ; "PROCEDUREP"
   (code! me '(instanceof procedure)) )

;(define-inline-call ("PROCEDURE_ARITY" env)
;   (_getfield env 'int (jlib-declare env 'procarity)) )
;
(define-inline-call-args procedure-set! ; "PROCEDURE_SET"
   (gen-expr me (car args))
   (code! me '(getfield procenv))
   (gen-expr me (cadr args))
   (gen-expr me (caddr args))
   (code! me '(aastore))
   'no-value )

(define-inline-call-args procedure-ref ; "PROCEDURE_REF"
   (gen-expr me (car args))
   (code! me '(getfield procenv))
   (gen-expr me (cadr args))
   (code! me '(aaload)) )

;(define-inline-call-args ("PROCEDURE_EL_SET" v args env)
;   (compile-expr (car args) env)
;   (if *purify* (_checkcast env 'j_procedure))
;   (_getfield env 'jobject (jlib-declare env 'procenv))
;   (compile-expr (cadr args) env)
;   (compile-expr (caddr args) env)
;   (_astore env 'jobject)
;   (_getstatic env (jlib-declare env 'unspecified)) )
;
;(define-inline-call-args ("PROCEDURE_EL_REF" v args env)
;   (compile-expr (car args) env)
;   (if *purify* (_checkcast env 'j_procedure))
;   (_getfield env 'jobject (jlib-declare env 'procenv))
;   (compile-expr (cadr args) env)
;   (_aload env 'jobject) )
;
;(define-inline-call-args ("PROCEDURE_1_EL_SET" v args env)
;   (compile-expr (car args) env)
;   (if *purify* (_checkcast env 'j_procedure))
;   (_getfield env 'jobject (jlib-declare env 'procenv))
;   (compile-expr (cadr args) env)
;   (compile-expr (caddr args) env)
;   (_astore env 'jobject)
;   (_getstatic env (jlib-declare env 'unspecified)) )
;
;(define-inline-call-args ("PROCEDURE_1_EL_REF" v args env)
;   (compile-expr (car args) env)
;   (if *purify* (_checkcast env 'j_procedure))
;   (_getfield env 'jobject (jlib-declare env 'procenv))
;   (compile-expr (cadr args) env)
;   (_aload env 'jobject) )
;
;(define-inline-call-args ("PROCEDURE_L_SET" v args env)
;   (compile-expr (car args) env)
;   (if *purify* (_checkcast env 'j_procedure))
;   (_getfield env 'jobject (jlib-declare env 'procenv))
;   (compile-expr (cadr args) env)
;   (compile-expr (caddr args) env)
;   (_astore env 'jobject)
;   (_getstatic env (jlib-declare env 'unspecified)) )
;
;(define-inline-call-args ("PROCEDURE_L_REF" v args env)
;   (compile-expr (car args) env)
;   (if *purify* (_checkcast env 'j_procedure))
;   (_getfield env 'jobject (jlib-declare env 'procenv))
;   (compile-expr (cadr args) env)
;   (_aload env 'jobject) )
;
;(define-inline-call-args ("PUSH_BEFORE" v args env)
;   (_getstatic env (jlib-declare env 'unspecified)) )
;
;(define-inline-call-args ("POP_BEFORE" v args env)
;   (_getstatic env (jlib-declare env 'unspecified)) )
;
;;;
;;; EXCEPTION
;;;
;
;;;
;;; EVAL
;;;
;(define-inline-call ("__EVMEANING_ADDRESS_REF" env)
;   (if *purify* (_checkcast env 'j_procedure))
;   (_invokevirtual env (jlib-declare env 'funcall0) '(ad) 'ad) )
;
;(define-inline-call-args ("__EVMEANING_ADDRESS_SET" v args env)
;   (compile-expr (car args) env)
;   (if *purify* (_checkcast env 'j_procedure))
;   (compile-expr (cadr args) env)
;   (_invokevirtual env (jlib-declare env 'funcall1) '(ad ad) 'ad) )
;
;;;
;;; FILE/SYSTEM/OS
;;; 
;
;;;
;;; SOCKET
;;;
;
;;;
;;; INPUT
;;;
;(define-inline-call ("CLOSED_RGC_BUFFER" env)
;   (if *purify* (_checkcast env 'j_input))
;   (_getfield env 'boolean (jlib-declare env 'io_other_eof)) )
;
;(define-inline-call ("INPUT_PORT_FILEPOS" env)
;   (_getfield env 'int (jlib-declare env 'io_filepos)))
;
;(define-inline-call ("INPUT_PORT_NAME" env)
;   (_getfield env 'jstring (jlib-declare env 'io_name))
;   (_invokevirtual env (jlib-declare env 'getbytes) '(ad) 'ad) )

;(define-inline-call ("RGC_BUFFER_POSITION" env)
;   (_STORE_NAME env 'reg1 'j_input)
;   (_LOAD_NAME env 'reg1 'j_input)
;   (_GETFIELD env 'int (jlib-declare env 'io_forward))
;   (_LOAD_NAME env 'reg1 'j_input)
;   (_GETFIELD env 'int (jlib-declare env 'io_start))
;   (_ISUB env) )

;(define-inline-call ("RGC_BUFFER_GET_CHAR" env)
;   (_STORE_NAME env 'reg1 'j_input)
;   (_LOAD_NAME env 'reg1 'j_input)
;   (_GETFIELD env '(vector byte) (jlib-declare env 'io_buffer))
;   (_LOAD_NAME env 'reg1 'j_input)
;   (_GETFIELD env 'int (jlib-declare env 'io_forward))
;   (_DUP env)
;   (_LOAD_NAME env 'reg1 'j_input)
;   (_SWAP env)
;   (_PUSH env 'int 1)
;   (_IADD env)
;   (_PUTFIELD env (jlib-declare env 'io_forward))
;   (_ALOAD env 'byte)
;   (_PUSH env 'int 255)
;   (_IAND env) )

;(define-inline-call ("RGC_START_MATCH" env)
;   (_STORE_NAME env 'reg1 'j_input)
;   (_LOAD_NAME env 'reg1 'j_input)
;   (_GETFIELD env 'int (jlib-declare env 'io_stop))
;   (_STORE_NAME env 'reg2 'int)
;   (_LOAD_NAME env 'reg1 'j_input)
;   (_LOAD_NAME env 'reg2 'int)
;   (_PUTFIELD env (jlib-declare env 'io_start))
;   (_LOAD_NAME env 'reg1 'j_input)
;   (_LOAD_NAME env 'reg2 'int)
;   (_PUTFIELD env (jlib-declare env 'io_forward))
;   (_LOAD_NAME env 'reg2 'int) )

;(define-inline-call ("RGC_STOP_MATCH" env)
;   (_STORE_NAME env 'reg1 'j_input)
;   (_LOAD_NAME env 'reg1 'j_input)
;   (_GETFIELD env 'int (jlib-declare env 'io_forward))
;   (_STORE_NAME env 'reg2 'int)
;   (_LOAD_NAME env 'reg1 'j_input)
;   (_LOAD_NAME env 'reg2 'int)
;   (_PUTFIELD env (jlib-declare env 'io_stop))
;   (_LOAD_NAME env 'reg2 'int) )

;(define-inline-call ("RGC_SET_FILEPOS" env)
;   (_STORE_NAME env 'reg1 'j_input)
;   (_LOAD_NAME env 'reg1 'j_input)
;   (_LOAD_NAME env 'reg1 'j_input)
;   (_GETFIELD env 'int (jlib-declare env 'io_filepos))
;   (_LOAD_NAME env 'reg1 'j_input)
;   (_GETFIELD env 'int (jlib-declare env 'io_stop))
;   (_LOAD_NAME env 'reg1 'j_input)
;   (_GETFIELD env 'int (jlib-declare env 'io_start))
;   (_ISUB env)
;   (_STORE_NAME env 'reg2 'int)
;   (_LOAD_NAME env 'reg2 'int)
;   (_I2L env)
;   (_LADD env)
;   (_PUTFIELD env (jlib-declare env 'io_filepos))
;   (_LOAD_NAME env 'reg2 'int) )

;(define-inline-call ("RGC_BUFFER_LENGTH" env)
;   (_STORE_NAME env 'reg1 'j_input)
;   (_LOAD_NAME env 'reg1 'j_input)
;   (_GETFIELD env 'int (jlib-declare env 'io_stop))
;   (_LOAD_NAME env 'reg1 'j_input)
;   (_GETFIELD env 'int (jlib-declare env 'io_start))
;   (_ISUB env) )

;(define-inline-call ("RGC_BUFFER_EMPTY" env)
;   (_STORE_NAME env 'reg1 'j_input)
;   (_LOAD_NAME env 'reg1 'j_input)
;   (_GETFIELD env 'int (jlib-declare env 'io_forward))
;   (_LOAD_NAME env 'reg1 'j_input)
;   (_GETFIELD env 'int (jlib-declare env 'io_abufsiz))
;   (if_icmp-return-boolean env 'EQ) )

;(define-inline-predicate ("RGC_BUFFER_EMPTY" lt lf env)
;   (_STORE_NAME env 'reg1 'j_input)
;   (_LOAD_NAME env 'reg1 'j_input)
;   (_GETFIELD env 'int (jlib-declare env 'io_forward))
;   (_LOAD_NAME env 'reg1 'j_input)
;   (_GETFIELD env 'int (jlib-declare env 'io_abufsiz))
;   (icompare 'NE lt lf env) )
