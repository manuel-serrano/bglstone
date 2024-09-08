;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Module/alibrary.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Feb 28 10:20:55 1998                          */
;*    Last change :  Fri Mar 12 06:32:42 2004 (serrano)                */
;*    Copyright   :  1998-2004 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The compiler library clause compilation                          */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module module_alibrary
   (include "Ast/unit.sch")
   (import  module_module
	    tools_error
	    engine_param
	    expand_srfi-0
	    (setup-library-values init_setrc))
   (export  (make-alibrary-compiler)
	    (make-library-name ::obj)
	    (use-library! library mode)))

;*---------------------------------------------------------------------*/
;*    use-library! ...                                                 */
;*---------------------------------------------------------------------*/
(define (use-library! library mode)
   (let ((lib-name (make-library-name library)))
      (if (not (member lib-name *additional-bigloo-libraries*))
	  (begin
	     (set! *additional-bigloo-libraries*
		   (cons lib-name *additional-bigloo-libraries*))
	     (let ((heap-name (string-append lib-name
					     (case *target-language*
						((jvm .net) ".jheap")
						(else ".heap")))))
		(add-supported-srfi! (string->symbol (string-upcase lib-name)))
		(add-supported-srfi! (string->symbol lib-name))
		;; when use-library is called from the argument parsing we have
		;; to delay the library initialization until all arguments have
		;; been parsed. In consequence setup-library-values is called
		;; only when use-library is not called from argument parsing
		;; (which is denotes by the 'now mode value)
		(if (eq? mode 'now) (setup-library-values lib-name))
		(set! *additional-heap-names*
		      (cons heap-name *additional-heap-names*))
		lib-name)))))

;*---------------------------------------------------------------------*/
;*    make-alibrary-compiler ...                                       */
;*---------------------------------------------------------------------*/
(define (make-alibrary-compiler)
   (instantiate::ccomp
      (id 'library)
      (producer alibrary-producer)))

;*---------------------------------------------------------------------*/
;*    alibrary-producer ...                                            */
;*---------------------------------------------------------------------*/
(define (alibrary-producer clause)
   (match-case clause
      ((?- . ?protos)
       (for-each (lambda (x) (use-library! x 'now)) protos)
       '())
      (else
       (user-error "Parse error" "Illegal `library' clause" clause '()))))

;*---------------------------------------------------------------------*/
;*    make-library-name ...                                            */
;*---------------------------------------------------------------------*/
(define (make-library-name libname)
   (cond
      ((symbol? libname)
       (string-downcase (symbol->string libname)))
      ((string? libname)
       (let ((new-name (string-downcase libname)))
	  (if (not (string=? new-name libname))
	      (user-error "Parse error"
			  "Library name must be downcase"
			  libname '())
	      new-name)))
      (else
       (user-error "Parse error" "Illegal library" libname '()))))
       
