;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Module/checksum.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  SERRANO Manuel                                    */
;*    Creation    :  Thu Aug 21 08:38:45 1997                          */
;*    Last change :  Tue Oct 19 12:55:22 2004 (serrano)                */
;*    Copyright   :  1997-2004 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    We compute checksum for modules in order to be able to check,    */
;*    at module initialization time, that modules are coherent. Only   */
;*    exported and extern values (bindings and classes) are considered */
;*    for checksumming.                                                */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module module_checksum
   (import engine_param)
   (export (module-checksum::long mclause::pair)))

;*---------------------------------------------------------------------*/
;*    module-checksum ...                                              */
;*---------------------------------------------------------------------*/
(define (module-checksum module)
   (clause-checksum (symbol->number (cadr module)) (cddr module)))

;*---------------------------------------------------------------------*/
;*    include-checksum ...                                             */
;*---------------------------------------------------------------------*/
(define (include-checksum include checksum)
   (define directives?
      (match-lambda
	 ((directives . ?-)
	  #t)
	 (else
	  #f)))
   (let ((fi (find-file/path include *load-path*)))
      (if (and (string? fi) (file-exists? fi))
	  (let* ((port (open-input-file fi))
		 (dir  (unwind-protect (let ((exp (read port)))
					  (if (directives? exp)
					      exp
					      #f))
				       (close-input-port port))))
	     (if dir
		 (clause-checksum (atom->number checksum include) (cdr dir))
		 checksum))
	  (error 'include-checksum "Can't find include file" include))))
	      
;*---------------------------------------------------------------------*/
;*    clause-checksum ...                                              */
;*---------------------------------------------------------------------*/
(define (clause-checksum checksum clauses)
   (let loop ((clauses  clauses)
	      (checksum checksum))
      (match-case clauses
	 (()
	  checksum)
	 (((export . ?export) . ?rest)
          (loop rest (list->number checksum export)))
	 ((((or foreign extern java) . ?export) . ?rest)
	  (loop rest (extern-clause-checksum checksum export)))
	 (((include . ?files) . ?rest)
	  (let laap ((files    files)
		     (checksum checksum))
	     (if (null? files)
		 (loop rest checksum)
		 (laap (cdr files) (include-checksum (car files) checksum)))))
	 (else
	  (loop (cdr clauses) checksum)))))

;*---------------------------------------------------------------------*/
;*    symbol->number ...                                               */
;*---------------------------------------------------------------------*/
(define (symbol->number symbol)
   (define (type-component str::string)
      (let ((len (-fx (string-length str) 1)))
	 (let loop ((i 0)
		    (armed? #f))
	    
	    (cond
	       ((=fx i len)
		str)
	       ((char=? (string-ref str i) #\:)
		(if armed?
		    (substring str (+fx i 1) (+fx len 1))
		    (loop (+fx i 1) #t)))
	       (else
		(loop (+fx i 1) #f))))))
   (get-hashnumber (type-component (symbol->string symbol))))

;*---------------------------------------------------------------------*/
;*    keyword->number ...                                              */
;*---------------------------------------------------------------------*/
(define (keyword->number keyword)
   0)

;*---------------------------------------------------------------------*/
;*    atom->number ...                                                 */
;*---------------------------------------------------------------------*/
(define (atom->number checksum clause)
   (cond
      ((fixnum? clause)
       (bit-xor checksum clause))
      ((flonum? clause)
       (atom->number checksum (real->string clause)))
      ((char? clause)
       (bit-xor checksum (+fx 23 (char->integer clause))))
      ((cnst? clause)
       (bit-xor checksum (+fx 90 (cnst->integer clause))))
      ((string? clause)
       (bit-xor checksum (+fx 4 (get-hashnumber clause))))
      ((symbol? clause)
       (bit-xor checksum (+fx 150 (symbol->number clause))))
      ((keyword? clause)
       (bit-xor checksum (+fx 151 (keyword->number clause))))
      ((pair? clause)
       (list->number checksum clause))
      (else
       (warning "module checksum:Unknown clause" clause " -- "
		(find-runtime-type clause))
       0)))

;*---------------------------------------------------------------------*/
;*    list->number ...                                                 */
;*---------------------------------------------------------------------*/
(define (list->number checksum clause)
   (match-case clause
      (()
       checksum)
      (((or default assert info) . ?-)
       0)
      (else
       (if (pair? clause)
	   (list->number (atom->number checksum (car clause)) (cdr clause))
	   (atom->number checksum clause)))))
	 
;*---------------------------------------------------------------------*/
;*    extern-clause-checksum ...                                       */
;*---------------------------------------------------------------------*/
(define (extern-clause-checksum checksum clauses)
   (let loop ((clauses  clauses)
	      (checksum checksum))
      (match-case clauses
	 (()
	  checksum)
	 ;; checksum value needs to change if timestamp of C include files
	 ;; changes
	 (((include ?file) . ?rest)
	  (let* ((fullname (find-file/path file *mco-include-path*))
		 (time (if (and (string? fullname) (file-exists? fullname))
			   (flonum->fixnum
			    (elong->flonum
			     (file-modification-time fullname)))
			   0)))
	     (loop (cdr clauses)
		   (atom->number checksum time))))
	 (else
	  (loop (cdr clauses)
		(atom->number checksum (car clauses)))))))
			 
