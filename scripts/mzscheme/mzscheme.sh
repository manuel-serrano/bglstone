#!/bin/sh

if [ "$MZSCHEME " = " " ]; then
   mzscheme=/usr/bin/mzscheme
else
  mzscheme=$MZSCHEME
fi

prelude=`dirname $0`/prelude.scm
src=$1
tmp=/tmp/mzscheme-tmp.scm

cat $prelude > $tmp
echo "" >> $tmp
cat $src >> $tmp

if [ "$#" = "2" ]; then
  echo "(exit (main '(\"$src\" \"$2\")))" >> $tmp
else
  echo "(exit (main '(\"$src\")))" >> $tmp
fi

$mzscheme $tmp
res=$?

if [ "$res " = 0 ]; then
  /bin/rm -f $tmp
fi

exit $res

