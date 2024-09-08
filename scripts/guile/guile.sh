#!/bin/sh

if [ "$GUILE " = " " ]; then
   guile=guile
else
  guile=$GUILE
fi

prelude=`dirname $0`/prelude.scm
src=$1
tmp=/tmp/guile-tmp.scm

cat $prelude > $tmp
echo "" >> $tmp
cat $src >> $tmp

if [ "$#" = "2" ]; then
  $guile -l $tmp -c "(exit (main '(\"$src\" \"$2\")))"
  res=$?
else
  $guile -l $tmp -c "(exit (main '(\"$src\")))"
  res=$?
fi

if [ "$res " = 0 ]; then
  /bin/rm -f $tmp
fi

exit $res

