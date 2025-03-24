;*=====================================================================*/
;*    .../prgm/project/bglstone/src/bigloo/bigloo/Cgen/walk.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Feb 21 08:37:48 1995                          */
;*    Last change :  Fri Mar  7 08:01:01 2025 (serrano)                */
;*    Copyright   :  1995-2025 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The `C generation' pass.                                         */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module cgen_walk
   (include "BackEnd/backend.sch")
   (import  tools_shape
	    tools_error
	    engine_param
	    engine_configure
	    module_module
	    module_library
	    type_type
	    type_cache
	    object_class
	    object_slots
	    ast_var
	    ast_node
	    ast_env
	    object_class
	    backend_c_emit
	    backend_c_prototype
	    backend_c_main
	    backend_c_init
	    cgen_cop
	    cgen_cgen
	    backend_backend
	    backend_cplib))

;*---------------------------------------------------------------------*/
;*    cvm-compile-functions ::cgen ...                                 */
;*---------------------------------------------------------------------*/
(define-method (cvm-compile-functions me::cgen)
   (for-each cgen-function (cvm-functions me)))

