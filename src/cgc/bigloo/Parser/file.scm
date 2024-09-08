;*=====================================================================*/
;*    serrano/prgm/project/bglstone/src/cgc/bigloo/Parser/file.scm     */
;*    -------------------------------------------------------------    */
;*    Author      :  SERRANO Manuel                                    */
;*    Creation    :  Mon Aug 25 14:05:41 1997                          */
;*    Last change :  Tue Mar  1 08:18:38 2005 (serrano)                */
;*    -------------------------------------------------------------    */
;*    We build an Ast from a file                                      */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module parser_file
   (import parser_parser
	   parser_lexer
	   tools_speek
	   engine_param)
   (export (file->ast file-name::bstring)
	   (console->ast)))

;*---------------------------------------------------------------------*/
;*    file->ast ...                                                    */
;*---------------------------------------------------------------------*/
(define (file->ast file-name)
   (if (not (file-exists? file-name))
       (error *cgc-name* "Can't find file" file-name)
       (let* ((port (open-input-file file-name)))
	  (if (not (input-port? port))
	      (error *cgc-name* "Can't open file for input" file-name)
	      (unwind-protect
		 (begin
		    (verbose 1 "  - read [" file-name #"]\n")
		    (with-exception-handler
		       (lambda (e)
			  (error-notify e)
			  (exit 1))
		       (lambda ()
			  (read/lalrp *c-parser* *c-lexer* port))))
		 (close-input-port port))))))

;*---------------------------------------------------------------------*/
;*    console->ast ...                                                 */
;*    -------------------------------------------------------------    */
;*    When reading the source file on the console, we have first       */
;*    to read it. Build a temporary C file, expand it using Cpp,       */
;*    inlining it and C compiling it.                                  */
;*---------------------------------------------------------------------*/
(define (console->ast)
   (let ((tmp-file (make-file-name *cgc-tmp*
				   (string-append *cgc-name* ".c"))))
      (with-output-to-file tmp-file
	 (lambda ()
	    (let loop ((char (read-char)))
	       (if (eof-object? char)
		   'done
		   (begin
		      (write-char char)
		      (loop (read-char)))))))
      ;; and now we use the regular file->ast procedure
      (let ((res (file->ast tmp-file)))
	 (delete-file tmp-file)
	 res)))

