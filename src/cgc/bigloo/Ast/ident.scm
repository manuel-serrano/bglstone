;*=====================================================================*/
;*    serrano/prgm/project/bglstone/src/cgc/bigloo/Ast/ident.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  SERRANO Manuel                                    */
;*    Creation    :  Wed Aug 27 09:33:23 1997                          */
;*    Last change :  Tue Nov 24 16:01:29 2015 (serrano)                */
;*    -------------------------------------------------------------    */
;*    We manage hash tables for identifier (mostly for function        */
;*    identifier). This module handles these tables and provides       */
;*    with some facilities about identifier bindings.                  */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module ast_ident
   (import engine_param)
   (include "Ast/ident.sch")
   (export (class ident
	      name::bstring
	      (location (default #unspecified)))
	   (string->ident::ident ::bstring)
	   (ident=?::bool ::ident ::ident)
	   (ident-is?::bool ::ident ::bstring)))
   
;*---------------------------------------------------------------------*/
;*    string->ident ...                                                */
;*---------------------------------------------------------------------*/
(define (string->ident name)
   (instantiate::ident
      (name name)))

;*---------------------------------------------------------------------*/
;*    object-display ::alias ...                                       */
;*---------------------------------------------------------------------*/
(define-method (object-display ident::ident . port)
   (with-access::ident ident (name)
      (let ((port (if (pair? port) (car port) (current-output-port))))
	 (display name port))))

;*---------------------------------------------------------------------*/
;*    ident=? ...                                                      */
;*---------------------------------------------------------------------*/
(define (ident=? id1 id2)
   (string=? (ident-name id1) (ident-name id2)))

;*---------------------------------------------------------------------*/
;*    ident-is? ...                                                    */
;*---------------------------------------------------------------------*/
(define (ident-is? id name)
   (string=? (ident-name id) name))
