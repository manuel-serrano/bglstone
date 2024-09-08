;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Read/access.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Mar 17 11:33:41 1993                          */
;*    Last change :  Fri Nov  5 15:16:48 2004 (serrano)                */
;*    Copyright   :  1993-2004 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The module which handle access tables `module/name'              */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    Le module                                                        */
;*---------------------------------------------------------------------*/
(module read_access
   (import engine_param
	   engine_engine
	   tools_error
	   init_main
	   tools_speek)
   (export (add-access! module::symbol files::pair)
	   (read-access-file)))

;*---------------------------------------------------------------------*/
;*    add-access! ...                                                  */
;*---------------------------------------------------------------------*/
(define (add-access! module files)
   (let ((b (assq module *access-table*)))
      (if (not b)
	  (set! *access-table* (cons (cons module files) *access-table*))
	  (if (not (equal? (cdr b) files))
	      (warning "add-access!" "access redefinition -- " module " ["
		       (cdr b) " " files "]")
	      'done))))
	   
;*---------------------------------------------------------------------*/
;*    read-access-file ...                                             */
;*    -------------------------------------------------------------    */
;*    Cette fonction fait des effets de bords sur `*access-table*'     */
;*---------------------------------------------------------------------*/
(define (read-access-file)
   (define (inner-read-access-file name::bstring)
      (let ((port (open-input-file name)))
	 (verbose 2 "      [reading afile " name "]" #\Newline)
	 (if (not (input-port? port))
	     (user-error "read-access-file"
			 "Can't open access file"
			 name)
	     (unwind-protect
		(do-read-access-file name port)
		(close-input-port port)))))
   (cond
      ((not (string? *access-file*))
       (if (file-exists? *access-file-default*)
	   (inner-read-access-file *access-file-default*)
	   'done))
      ((not (file-exists? *access-file*))
       (user-error "read-access-file" "Can't find access file" *access-file*))
      (else
       (inner-read-access-file *access-file*))))

;*---------------------------------------------------------------------*/
;*    do-read-access-file ...                                          */
;*---------------------------------------------------------------------*/
(define (do-read-access-file file port)
   (let* ((obj (read port #t))
	  (eof (read port)))
      (cond
	 ((eof-object? obj)
	  (user-error file "Illegal access file format" obj))
	 ((not (eof-object? eof))
	  (user-error file "Illegal access file format" eof))
	 (else
	  (let loop ((obj obj))
	     (if (null? obj)
		 'done
		 (match-case (car obj)
		    (((and (? symbol?) ?m) (and ?f (? string?)) . ?fs)
		     (let loop ((fs     fs)
				(fnames (list f)))
			(cond
			   ((null? fs)
			    (add-access! m (reverse! fnames)))
			   ((string? (car fs))
			    (loop (cdr fs)
				  (cons (car fs) fnames)))
			   (else
			    (user-error file
					"Illegal access file format"
					(car obj)))))
		     (loop (cdr obj)))
		    (else
		     (user-error file
				 "Illegal access file format"
				 (car obj))))))))))
