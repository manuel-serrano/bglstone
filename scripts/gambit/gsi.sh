#!/bin/sh

if [ "$GSI " = " " ]; then
   gsi="gsi -:m4096"
else
  gsi=$GSI
fi

prelude=`dirname $0`/prelude.scm
src=$1
tmp=/tmp/gsi-tmp.scm

cat $prelude > $tmp
echo "" >> $tmp
cat $src >> $tmp

if [ "$#" = "2" ]; then
  $gsi $tmp -e "(exit (main '(\"$src\" \"$2\")))"
  res=$?
else
  $gsi $tmp -e "(exit (main '(\"$src\")))"
  res=$?
fi

if [ "$res " = 0 ]; then
  /bin/rm -f $tmp
fi

exit $res

