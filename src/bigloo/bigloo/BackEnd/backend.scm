;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/BackEnd/backend.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Aug  4 14:08:50 2003                          */
;*    Last change :  Wed Jan 19 12:11:15 2005 (serrano)                */
;*    Copyright   :  2003-05 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The declaration of the backend structure.                        */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module backend_backend
   (import type_type)
   (export (abstract-class backend
	      (language::symbol (default 'none))
	      (name::bstring (default "dummy"))
	      (extern-variables (default '()))
	      (extern-functions (default '()))
	      (extern-types (default '()))
	      (variables (default '()))
	      (functions (default '()))
	      (types (default '())))
	   
	   (abstract-class cvm::backend)
	   (class sawc::cvm)
	   (class cgen::cvm)
	   (class jvm::backend
	      (qname::symbol (default 'unamed))
	      (classes (default '()))
	      (current-class (default #f))
	      (declarations (default '()))
	      (fields (default '()))
	      (methods (default '()))
	      (current-method (default #f))
	      (code (default '()))	      )
	   (class dotnet::backend
	      (out::output-port (default (open-output-string)))
	      (qname::bstring (default "")))
	   (class jsm::backend
	      (qname::bstring (default "")) )
	   
	   (generic backend-compile b::backend)
	   (generic backend-link b::backend result) 
	   (generic backend-subtype? b::backend t1::type t2::type)
	   (generic cvm-compile-functions b::cvm)))

;*---------------------------------------------------------------------*/
;*    backend-compile ...                                              */
;*---------------------------------------------------------------------*/
(define-generic (backend-compile b::backend))

;*---------------------------------------------------------------------*/
;*    backend-link ...                                                 */
;*---------------------------------------------------------------------*/
(define-generic (backend-link b::backend result))

;*---------------------------------------------------------------------*/
;*    backend-subtype? ...                                             */
;*---------------------------------------------------------------------*/
(define-generic (backend-subtype? b::backend t1::type t2::type))
   
;*---------------------------------------------------------------------*/
;*    cvm-compile-functions ...                                        */
;*---------------------------------------------------------------------*/
(define-generic (cvm-compile-functions b::cvm))
