;*=====================================================================*/
;*    .../prgm/project/bglstone/src/bigloo/bigloo/Cnst/cache.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Feb 19 10:35:59 1995                          */
;*    Last change :  Fri Mar  7 07:39:39 2025 (serrano)                */
;*    Copyright   :  1995-2025 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    A cache to be able to recognize function call very fast.         */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module cnst_cache
   (include "Ast/node.sch" "Type/type.sch")
   (import  type_type
	    ast_var
	    ast_env
	    engine_param)
   (export  (start-cnst-cache!)
	    (stop-cnst-cache!)
	    *cnst-table-ref*
	    *cnst-table-set!*
	    *cons*
	    *btrue*
	    *bfalse*
	    *string->bstring*
	    *string->ucs2string*
	    *bstring->symbol*
	    *bstring->keyword*
	    *bool->bbool*
	    *long->int*
	    *make-fx-procedure*
	    *make-va-procedure*
	    *double->real*
	    *elong->belong*
	    *llong->bllong*
	    *list->vector*
	    *vector-tag-set!*))

;*---------------------------------------------------------------------*/
;*    The cache registers definition                                   */
;*---------------------------------------------------------------------*/
(define *cache-started?*        #f)

(define *cnst-table-ref*        #f)
(define *cnst-table-set!*       #f)
(define *cons*                  #f)
(define *btrue*                 #f)
(define *bfalse*                #f)
(define *string->bstring*       #f)
(define *string->ucs2string*    #f)
(define *bstring->symbol*       #f)
(define *bstring->keyword*      #f)
(define *bool->bbool*           #f)
(define *long->int*             #f)
(define *make-fx-procedure*     #f)
(define *make-va-procedure*     #f)
(define *double->real*          #f)
(define *elong->belong*         #f)
(define *llong->bllong*         #f)
(define *list->vector*          #f)
(define *vector-tag-set!*       #f)

;*---------------------------------------------------------------------*/
;*    start-cnst-cache! ...                                            */
;*---------------------------------------------------------------------*/
(define (start-cnst-cache!)
   (if (not *cache-started?*)
       (begin
	  (set! *cache-started?* #t)
	  (set! *cnst-table-ref*
		(find-global/module 'cnst-table-ref 'foreign))
	  (set! *cnst-table-set!*
		(find-global/module 'cnst-table-set! 'foreign))
	  (set! *cons*
		(find-global/module 'c-cons 'foreign))
	  (set! *btrue*
		(find-global/module 'btrue 'foreign))
	  (set! *bfalse*
		(find-global/module 'bfalse 'foreign))
	  (set! *string->bstring*
		(find-global/module 'string->bstring 'foreign))
	  (set! *string->ucs2string*
		(find-global/module 'c-utf8-string->ucs2-string 'foreign))
	  (set! *bstring->symbol*
 		(find-global/module 'c-bstring->symbol 'foreign))
	  (set! *bstring->keyword*
		(find-global/module 'c-bstring->keyword 'foreign))
	  (set! *bool->bbool*
		(find-global/module 'bool->bbool 'foreign))
	  (set! *long->int*
		(find-global/module 'long->int 'foreign))
	  (set! *make-fx-procedure*
		(find-global/module 'make-fx-procedure 'foreign))
	  (set! *make-va-procedure*
		(find-global/module 'make-va-procedure 'foreign))
	  (set! *double->real*
		(find-global/module 'double->real 'foreign))
	  (set! *elong->belong*
		(find-global/module 'elong->belong 'foreign))
	  (set! *llong->bllong*
		(find-global/module 'llong->bllong 'foreign))
	  (set! *list->vector* (find-global 'list->vector))
	  (set! *vector-tag-set!* (find-global 'vector-tag-set!))
	  #t)
       #t))

;*---------------------------------------------------------------------*/
;*    stop-cnst-cache! ...                                             */
;*---------------------------------------------------------------------*/
(define (stop-cnst-cache!)
   (set! *string->bstring*    #f)
   (set! *string->ucs2string* #f)
   (set! *bstring->symbol*    #f)
   (set! *bstring->keyword*   #f)
   (set! *bool->bbool*        #f)
   (set! *long->int*          #f)
   (set! *make-fx-procedure*  #f)
   (set! *make-va-procedure*  #f)
   (set! *double->real*       #f)
   (set! *cons*               #f)
   (set! *btrue*              #f)
   (set! *bfalse*             #f)
   #t)






