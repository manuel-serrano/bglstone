;*=====================================================================*/
;*    serrano/prgm/project/bglstone/tools/statinfo/statinfo.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Aug 10 10:45:56 2001                          */
;*    Last change :  Wed Jan  8 16:47:49 2003 (serrano)                */
;*    Copyright   :  2001-03 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Extract information from a stat file                             */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module statinfo
   (main main))

;*---------------------------------------------------------------------*/
;*    main ...                                                         */
;*---------------------------------------------------------------------*/
(define (main argv)
   (if (not (and (pair? (cdr argv)) (pair? (cddr argv))))
       (error "statinfo" "Illegal command line" argv)
       (let ((file (cadr argv))
	     (infos (map string->symbol (cddr argv))))
	  (if (not (file-exists? file))
	      (warning "statinfo" "Can't find file -- " file)
	      (with-input-from-file file
		 (lambda ()
		    (let* ((exp (read))
			   (cfg (find 'configuration exp)))
		       (if (not (pair? cfg))
			   (error "statinfo" "Can't find configuration" file)
			   (for-each (lambda (key)
					(let ((cell (assq key (cdr cfg))))
					   (if (pair? cell)
					       (cond
						  ((pair? (cdr cell))
						   (print (cadr cell)))
						  ((null? (cdr cell))
						   (print ""))
						  (else
						   (print (cdr cell)))))))
				     infos)))))))))

;*---------------------------------------------------------------------*/
;*    find ...                                                         */
;*---------------------------------------------------------------------*/
(define (find key lst)
   (cond
      ((null? lst)
       #f)
      ((and (pair? (car lst)) (eq? (caar lst) key))
       (car lst))
      (else
       (find key (cdr lst)))))
       
      
						     
		       
