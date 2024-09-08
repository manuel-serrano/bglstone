;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Heap/make.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Jan  8 08:44:08 1995                          */
;*    Last change :  Fri Nov 12 14:51:37 2004 (serrano)                */
;*    Copyright   :  1995-2004 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The creation of a library heap                                   */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module heap_make
   (include "Engine/pass.sch")
   (import  engine_param
	    tools_error
	    type_type
	    type_env
	    ast_var
	    ast_env
	    foreign_library)
   (import  tools_shape)
   (export  (make-heap)
	    (make-add-heap)))

;*---------------------------------------------------------------------*/
;*    make-heap ...                                                    */
;*---------------------------------------------------------------------*/
(define (make-heap)
   (pass-prelude "Heap" prepare-globals!)
   (set-obj-string-mode! 'pair)
   (if (not (string? *heap-name*))
       (user-error "make-heap" "Illegal heap's name" *heap-name*)
       (let ((hname (make-file-name (car *lib-dir*) *heap-name*)))
	  (let ((port (open-output-binary-file hname)))
	     (if (not (binary-port? port))
		 (error "make-heap" "Can't open output port" hname)
		 (begin
		    (output-obj port 
				(vector *target-language*
					(get-genv)
					(get-tenv)))
		    (close-binary-port port)))))))

;*---------------------------------------------------------------------*/
;*    prepare-globals! ...                                             */
;*    -------------------------------------------------------------    */
;*    Before making a heap, we reset all the occurrence slots and for  */
;*    each exported variable, we declare it as imported. We remove     */
;*    static variables.                                                */
;*---------------------------------------------------------------------*/
(define (prepare-globals!)
   (for-each-global! (lambda (g)
			;; we set importation slots
			(cond
			   ((eq? (global-import g) 'static)
			    (unbind-global! (global-id g) (global-module g)))
			   ((eq? (global-import g) 'export)
			    (global-import-set! g 'import))
			   (else
			    #unspecified))
			;; and occurrence ones
			(global-occurrence-set! g 0)
			;; mark that the variable belongs to a library
			;; 6 may 2003
			(global-library?-set! g #t)))
   #t)

;*---------------------------------------------------------------------*/
;*    make-add-heap ...                                                */
;*    -------------------------------------------------------------    */
;*    @label foreign accesors@                                         */
;*    @ref ../Foreign/library.scm:foreign accesors@                    */
;*    -------------------------------------------------------------    */
;*    PREPARE-FOREIGN-ACCESS! is not called anymore (10 jun 2001).     */
;*---------------------------------------------------------------------*/
(define (make-add-heap)
   (pass-prelude "Library heap"
;* 		 prepare-foreign-access!                               */
		 prepare-additional-globals!)
   (set-obj-string-mode! 'pair)
   (if (not (string? *additional-heap-name*))
       (user-error "make-add-heap"
		   "Illegal heap's name"
		   *additional-heap-name*)
       (let ((hname *additional-heap-name*))
	  (let ((port (open-output-binary-file hname)))
	     (if (not (binary-port? port))
		 (error "make-addd-heap" "Can't open output port" hname)
		 (begin
		    (output-obj port (vector *target-language*
					     (get-genv)
				             (get-tenv)
					     *additional-include-foreign*))
		    (close-binary-port port)))))))

;*---------------------------------------------------------------------*/
;*    prepare-additional-globals! ...                                  */
;*    -------------------------------------------------------------    */
;*    Before making an additional heap, we reset all the occurrence    */
;*    slots and for each exported variable, we declare it as imported. */
;*    We remove static variables and library variables.                */
;*---------------------------------------------------------------------*/
(define (prepare-additional-globals!)
   (for-each-global! (lambda (g)
			;; we set importation slots
			(cond
			   ((or (eq? (global-import g) 'static)
				(global-library? g))
			    (unbind-global! (global-id g) (global-module g)))
			   ((eq? (global-import g) 'export)
			    (global-import-set! g 'import))
			   (else
			    #unspecified))
			;; and occurrence ones
			(global-occurrence-set! g 0)
			;; mark that the variable belongs to a library
			;; 6 may 2003
			(global-library?-set! g #t)))
   #t)
