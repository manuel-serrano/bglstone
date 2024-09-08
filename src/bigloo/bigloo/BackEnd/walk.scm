;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/BackEnd/walk.scm            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Aug  4 14:12:02 2003                          */
;*    Last change :  Tue Feb  1 10:57:41 2005 (serrano)                */
;*    Copyright   :  2003-05 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Drivers for code generator and linker                            */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module backend_walk
   (import engine_param
	   module_module
	   read_jvm
	   type_type
	   ast_var
	   backend_backend
	   backend_c
	   backend_jvm
	   backend_jsm
	   backend_dotnet)
   (import tools_shape)
   (export (backend-walk language ast)))

;*---------------------------------------------------------------------*/
;*    backend-walk ...                                                 */
;*---------------------------------------------------------------------*/
(define (backend-walk language functions)
   (let ((backend (backend-build language)))
      (backend-init backend functions)
      (backend-link backend (backend-compile backend))
      backend))

;*---------------------------------------------------------------------*/
;*    backend-build ...                                                */
;*---------------------------------------------------------------------*/
(define (backend-build language)
   (case language
      ((c) (if *saw* (build-sawc-backend) (build-cgen-backend)))
      ((jvm) (build-jvm-backend))
      ((.net) (build-dotnet-backend))
      ((jsm) (build-jsm-backend))
      (else (error "backend" "Unimplemented target language" language))))

;*---------------------------------------------------------------------*/
;*    backend-init ...                                                 */
;*---------------------------------------------------------------------*/
(define (backend-init backend functions)
   (backend-name-set! backend (module->qualified-type *module*))
   (backend-functions-set! backend functions) )


