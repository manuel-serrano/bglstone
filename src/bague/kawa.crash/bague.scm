;*=====================================================================*/
;*    .../diffusion/article/bjvm/bench/bague/kawa.crash/bague.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Pierre Weis                                       */
;*    Creation    :  Fri Apr  1 10:00:21 1994                          */
;*    Last change :  Sat Feb 10 11:11:24 2001 (serrano)                */
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
(define (error a b c) #f)

(define nombre-de-coups 0)
(define nombre-de-pierres 28)

(define une-pierre 1)
(define une-case-vide 0)

(define jeu (make-vector nombre-de-pierres une-pierre))

(define (init-jeu)
   (set! nombre-de-coups 0)
   (let loop ((i (- nombre-de-pierres 1)))
      (if (< i 0)
	  'done
	  (begin
	     (vector-set! jeu i une-pierre)
	     (loop (- i 1))))))

(define (la-case n)
   (- n 1))

(define (enleve-la-pierre n)
   (if (eq? (vector-ref jeu (la-case n))  une-pierre)
       (vector-set! jeu (la-case n) une-case-vide)
       (error "bague" "cannot remove a stone from an empty slot" n)))

(define (pose-la-pierre n)
   (if (eq? (vector-ref jeu (la-case n)) une-case-vide)
       (vector-set! jeu (la-case n) une-pierre)
       (error "bague" "cannot lay a stone on a non empty slot" n)))

(define (autorise-mouvement n)
   (case n
      ((1) #t)
      ((2) (eq? (vector-ref jeu (la-case 1)) une-pierre))
      (else
       (and (eq? (vector-ref jeu (la-case (- n 1))) une-pierre)
	    (letrec ((ok (lambda (b i)
			    (if (> i (la-case (- n 2)))
				b
				(ok (and b (eq? (vector-ref jeu i)
						une-case-vide))
				    (+ i 1))))))
	       (ok #t 0))))))

(define (enleve-pierre n)
   (set! nombre-de-coups (+ nombre-de-coups 1))
   (if (autorise-mouvement n)
       (enleve-la-pierre n)
       (error "bague" "forbidden action" n)))

(define (pose-pierre n)
   (set! nombre-de-coups (+ nombre-de-coups 1))
   (if (autorise-mouvement n)
       (pose-la-pierre n)
       (error "bague" "forbidden action" n)))

(define (main argv)
   (letrec ((bague (lambda (n)
		      (case n
			 ((1) (enleve-pierre 1))
			 ((2) (enleve-pierre 2)
			      (enleve-pierre 1))
			 (else
			  (bague (- n 2))
			  (enleve-pierre n)
			  (repose (- n 2))
			  (bague (- n 1))))))
	    (repose (lambda (n)
		       (case n
			  ((1) (pose-pierre 1))
			  ((2) (pose-pierre 1)
			       (pose-pierre 2))
			  (else
			   (repose (- n 1))
			   (bague (- n 2))
			   (pose-pierre n)
			   (repose (- n 2)))))))
      (init-jeu)
      (bague nombre-de-pierres)
      (if (eq? nombre-de-coups 178956970)
	  0
	  1)))

(main '(toto))
       

