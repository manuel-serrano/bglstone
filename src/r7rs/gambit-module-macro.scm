(eval
 '(define-syntax module
    (lambda (stx)
      (quasiquote
       (##begin
         ,@(map
            (lambda (x)
              (if (equal? x '(include "../../r7rs/bigloo.sch"))
                  '(include "../../r7rs/gambit.scm")
                  x))
            (filter (lambda (x) (and (pair? x) (eq? (car x) 'include)))
                    (cddr (##desourcify stx)))))))))
