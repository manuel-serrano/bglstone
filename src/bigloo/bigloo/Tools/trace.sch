;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Tools/trace.sch             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Mar 31 08:56:22 1993                          */
;*    Last change :  Fri Feb  4 09:29:45 2005 (serrano)                */
;*    Copyright   :  1993-2005 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The tracing macro.                                               */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    Les imports indispensables pour pouvoir tracer                   */
;*---------------------------------------------------------------------*/
(directives
   (import tools_trace))

;*---------------------------------------------------------------------*/
;*    trace ...                                                        */
;*---------------------------------------------------------------------*/
(define-macro (trace mask . forms)
   (let ((*debug-mode* #t))
      (let ((*pass-names* '(ast heap inline inline+ cfa cc effect expand
			    globalize integrate coerce cnst cgen reduce
			    reduce- reduce+ recovery egen jvmas)))
	 (if *debug-mode*
	     (match-case mask
		((? symbol?)
		 (if (eq? mask 'get-pass-names)
		     `',*pass-names*
		     (if (memq mask *pass-names*)
			 `(if (trace-satisfy? ',mask 0)
			      (print-trace ,@forms))
			 (error #f "Illegal `trace' expression" mask))))
		(((and ?pass (? symbol?)) (and ?level (? integer?)))
		 (if (memq pass *pass-names*)
		     `(if (trace-satisfy? ',pass ,level)
			  (print-trace ,@forms))
		     (error #f "Illegal `trace' expression" mask)))
		(else
		 (error #f "Illegal `trace' expression" mask)))
	     ''()))))

;*---------------------------------------------------------------------*/
;*    on-trace ...                                                     */
;*---------------------------------------------------------------------*/
(define-macro (on-trace mask . forms)
   (let ((*debug-mode* #t))
      (let ((*pass-names* '(ast heap inline inline+ cfa cc effect expand
			    globalize integrate coerce cnst cgen reduce
			    reduce- reduce+ recovery egen jvmas)))
	 (if *debug-mode*
	     (match-case mask
		((? symbol?)
		 (if (eq? mask 'get-pass-names)
		     `',*pass-names*
		     (if (memq mask *pass-names*)
			 `(if (trace-satisfy? ',mask 0)
			      (begin ,@forms))
			 (error #f "Illegal `trace' expression" mask))))
		(((and ?pass (? symbol?)) (and ?level (? integer?)))
		 (if (memq pass *pass-names*)
		     `(if (trace-satisfy? ',pass ,level)
			  (begin ,@forms))
		     (error #f "Illegal `trace' expression" mask)))
		(else
		 (error #f "Illegal `trace' expression" mask)))
	     ''()))))

