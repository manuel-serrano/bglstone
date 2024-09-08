#!/bin/sh

if [ "$STKLOS " = " " ]; then
   stklos=stklos
else
  stklos=$STKLOS
fi

prelude=`dirname $0`/prelude.scm
src=$1
tmp=/tmp/stklos-tmp.scm

cat $prelude > $tmp
echo "" >> $tmp
cat $src >> $tmp

if [ "$#" = "2" ]; then
  $stklos -s 200000 -l $tmp -e "(exit (main '(\"$src\" \"$2\")))"
  res=$?
else
  $stklos -s 200000 -l $tmp -e "(exit (main '(\"$src\")))"
  res=$?
fi

if [ "$res " = 0 ]; then
  /bin/rm -f $tmp
fi

exit $res
