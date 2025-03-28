;*=====================================================================*/
;*    .../project/bglstone/src/bigloo/bigloo/Globalize/ginfo.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Jun 21 09:03:24 1996                          */
;*    Last change :  Fri Mar  7 07:32:59 2025 (serrano)                */
;*    Copyright   :  1996-2025 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The definition of the info structures for the globalization      */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module globalize_ginfo
   (include "Type/type.sch")
   
   (include "Ast/node.sch")
   
   (export  (wide-class sfun/Ginfo::sfun
	       ;; is a function globalized
	       (G?::bool (default #f))
	       ;; the list of called by functions
	       (cfrom (default '()))
	       ;; transitive closure of cfrom
	       (cfrom* (default #f))
	       ;; the list of called functions
	       (cto (default '()))
	       ;; transitive closure of cto
	       (cto* (default #f))
	       ;; the list of E functions
	       (cfunction (default '()))
	       ;; a place to be integrated
	       (integrator (default #unspecified))
	       ;; a place to be integrated
	       (imark (default -1))
	       ;; the function this function belongs to
	       (owner (default #f))
	       ;; a list a integrated functions
	       (integrated (default '()))
	       ;; where is it plugged
	       (plugged-in (default '()))
	       ;; a mark for the integration
	       (mark::long (default -10))
	       ;; a free search mark
	       (free-mark (default '()))
	       ;; the globalized function
	       (the-global (default #f))
	       ;; the kaptured variables
	       (kaptured (default #f))
	       ;; a globalized new-body
	       (new-body (default #f))
	       ;; used in `globalize_new-body'
	       (bmark::long (default -10))
	       ;; union mark
	       (umark::long (default -10))
	       ;; the free variable list
	       (free (default #unspecified))
	       ;; the bound variable list
	       (bound (default '())))

	    (wide-class svar/Ginfo::svar
	       ;; is the variable kaptured
	       (kaptured?::bool (default #f))
	       ;; a free-mark
	       (free-mark::long (default -10))
	       ;; a mark
	       (mark::long (default -10))
	       ;; celled ?
	       (celled?::bool (default #f)))

	    (wide-class sexit/Ginfo::sexit
	       ;; is a function globalized
	       (G?::bool (default #f))
	       ;; is the variable kaptured
	       (kaptured?::bool (default #f))
	       ;; a free-mark
	       (free-mark::long (default -10))
	       ;; a mark
	       (mark::long (default -10)))

	    (wide-class local/Ginfo::local
	       ;; is the local function escaping ?
	       (escape?::bool (default #f)))
	    
	    (wide-class global/Ginfo::global
	       ;; is the global function escaping ?
	       (escape?::bool (default #f))
	       ;; associated closure
	       (global-closure (default #f)))))
	    
   
	       
