#!/bin/sh

bgl=bgl-`basename $1 | sed 's/[.][^.]*//'`

if [ ! -f /tmp/$bgl ]; then
  if [ "$BFLAGS " = " " ]; then
    bflags="-Obench -w"
  else
    bflags=$BFLAGS
  fi

  bigloo $bflags $1 -o /tmp/$bgl
  res=$?
fi
  
if [ "$#" = "2" ]; then
  /tmp/$bgl $2
else
  /tmp/$bgl
fi
 
exit $?
