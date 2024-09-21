;*=====================================================================*/
;*    serrano/prgm/project/bglstone/src/bague/bigloo/bague.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Pierre Weis                                       */
;*    Creation    :  Fri Apr  1 10:00:21 1994                          */
;*    Last change :  Sat Sep 21 07:06:59 2024 (serrano)                */
;*    -------------------------------------------------------------    */
;*    Resolution recursive du Baguenaudier: bench les appels de        */
;*    fonctions et les acces aux vecteurs                              */
;*    avec 21 pierres le nombre de coups est 1398101                   */
;*    avec 24 pierres le nombre de coups est 11184810                  */
;*    f (n+1) = 2*f(n) + n mod 2 avec f 1 = 1                          */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    Le module                                                        */
;*---------------------------------------------------------------------*/
(module bague
   (main main))

(define nombre-de-coups 0)
(define nombre-de-pierres 33)

(define une-pierre 1)
(define une-case-vide 0)

(define jeu (make-vector nombre-de-pierres une-pierre))

(define (init-jeu)
   (set! nombre-de-coups 0)
   (let loop ((i (-fx nombre-de-pierres 1)))
      (if (<fx i 0)
	  'done
	  (begin
	     (vector-set! jeu i une-pierre)
	     (loop (-fx i 1))))))

(define (la-case n)
   (-fx n 1))

(define (enleve-la-pierre n)
   (if (=fx (vector-ref jeu (la-case n))  une-pierre)
       (vector-set! jeu (la-case n) une-case-vide)
       (error "bague" "cannot remove a stone from an empty slot" n)))

(define (pose-la-pierre n)
   (if (=fx (vector-ref jeu (la-case n)) une-case-vide)
       (vector-set! jeu (la-case n) une-pierre)
       (error "bague" "cannot lay a stone on a non empty slot" n)))

(define (autorise-mouvement n)
   (case n
      ((1) #t)
      ((2) (=fx (vector-ref jeu (la-case 1)) une-pierre))
      (else
       (and (=fx (vector-ref jeu (la-case (-fx n 1))) une-pierre)
	    (letrec ((ok (lambda (b i)
			    (if (>fx i (la-case (-fx n 2)))
				b
				(ok (and b (=fx (vector-ref jeu i)
						une-case-vide))
				    (+fx i 1))))))
	       (ok #t 0))))))

(define (enleve-pierre n)
   (set! nombre-de-coups (+fx nombre-de-coups 1))
   (if (autorise-mouvement n)
       (enleve-la-pierre n)
       (error "bague" "forbidden action" n)))

(define (pose-pierre n)
   (set! nombre-de-coups (+fx nombre-de-coups 1))
   (if (autorise-mouvement n)
       (pose-la-pierre n)
       (error "bague" "forbidden action" n)))

(define (main argv)
   (when (pair? (cdr argv))
      (set! nombre-de-pierres (string->integer (cadr argv))))
   (letrec ((bague (lambda (n)
		      (case n
			 ((1) (enleve-pierre 1))
			 ((2) (enleve-pierre 2)
			      (enleve-pierre 1))
			 (else
			  (bague (-fx n 2))
			  (enleve-pierre n)
			  (repose (-fx n 2))
			  (bague (-fx n 1))))))
	    (repose (lambda (n)
		       (case n
			  ((1) (pose-pierre 1))
			  ((2) (pose-pierre 1)
			       (pose-pierre 2))
			  (else
			   (repose (-fx n 1))
			   (bague (-fx n 2))
			   (pose-pierre n)
			   (repose (-fx n 2)))))))
      (init-jeu)
      (bague nombre-de-pierres)
      (when (=fx nombre-de-pierres 1)
	 (print "nombre-de-coups: " nombre-de-coups))
      (let ((res (case nombre-de-pierres
		    ((1) 1)
		    ((2) 2)
		    ((10) 682)
		    ((14) 10922)
		    ((20) 699050)
		    ((24) 11184810)
		    ((25) 22369621)
		    ((26) 44739242)
		    ((27) 89478485)
		    ((28) 178956970)
		    ((29) 178956970)
		    ((30) 715827882)
		    ((31) 1431655765)
		    ((32) 2863311530)
		    ((33) 5726623061)
		    (else #f))))
	 (print "res " nombre-de-coups "/" res)
	 (if (or (not res) (=fx res nombre-de-coups)) 0 1))))
		    
       

