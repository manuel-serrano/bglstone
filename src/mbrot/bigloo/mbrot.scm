(module mbrot (main main))

(define *r -1.0)
(define *i -0.5)
(define *n 75)
(define *step 0.005)

(define (count cr ci)
   (let ((max-count 64)
         (radius^2  16.0))
      (let loop ((c 0)
                 (zr cr)
                 (zi ci))
         (if (=fx c max-count)
             c
             (let ((zr^2 (*fl zr zr))
                   (zi^2 (*fl zi zi)))
                (if (>fl (+fl zr^2 zi^2) radius^2)
                    c
                    (loop (+fx c 1)
                          (+fl (-fl zr^2 zi^2) cr)
                          (+fl (*fl 2.0 (*fl zr zi)) ci))))))))

(define (mbrot r i n step p?)
  (let loop1 ((y n) (ci i))
    (if (>fx y 0)
      (let loop2 ((x n) (cr r))
        (if (>fx x 0)
          (let ((c (count cr ci)))
             (if p? (write-char (integer->char (+fx c 32))))
            (loop2 (-fx x 1) (+fl cr step)))
          (begin
             (if p? 
                 (newline))
            (loop1 (-fx y 1) (+fl ci step))))))))

(define (run num)
   (let loop ((num num)
              (res (mbrot *r *i *n *step (=fx num 1))))
      (if (=fx num 1)
          'done
          (loop (-fx num 1) (mbrot *r *i *n *step (=fx num 2))))))

(define (main argv)
   (let ((v (if (pair? (cdr argv))
		(run (string->integer (cadr argv)))
		(run 10000))))
      (exit (if (eq? v 'done) 0 1))))

