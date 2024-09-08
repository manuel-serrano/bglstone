#!/bin/sh

if [ "$BFLAGS " = " " ]; then
  bflags=-O4
else
  bflags=$BFLAGS
fi

if [ ! -f /tmp/bgl-repl ]; then
  cat > /tmp/bgl-repl.scm <<EOF
(module foo
   (main main))

(define (main argv)
   (if (string=? (car argv) "/tmp/bgl-classic")
       (eval-evaluate-set! 'classic)
       (eval-evaluate-set! 'new))
   (let ((l (command-line))
         (f (cadr argv)))
      (set-car! l (cadr l))
      (set-cdr! l (cddr l))
      (exit (loadq f))))
EOF

  bigloo -O5 /tmp/bgl-repl.scm -o /tmp/bgl-repl
  (cd /tmp; ln -s bgl-repl bgl-classic; ln -s bgl-repl bgl-new)
fi

if [ "$# " = "0 " ]; then
  exit 0
fi

bgl=`basename $0 | sed 's/[.]sh//'`

if [ "$#" = "2" ]; then
  /tmp/$bgl $1 $2
else
  /tmp/$bgl $1
fi
