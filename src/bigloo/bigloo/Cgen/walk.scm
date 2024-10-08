;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Cgen/walk.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Feb 21 08:37:48 1995                          */
;*    Last change :  Wed Jan 19 14:35:54 2005 (serrano)                */
;*    Copyright   :  1995-2005 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The `C generation' pass.                                         */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module cgen_walk
   
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

