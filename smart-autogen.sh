#!/bin/sh

srcdir=${srcdir:-.}
set -ux

AUTOGEN_INPUT_CHECKSUM=`cat $srcdir/configure.ac $srcdir/aclocal.m4 | md5sum | cut -b 1-32`

CHECKSUM_FILE=autogen.checksum

if test `cat $CHECKSUM_FILE`"" = "$AUTOGEN_INPUT_CHECKSUM"; then
  exit 0
fi

set -e
${srcdir}/autogen.sh "$@"
printf "%s" $AUTOGEN_INPUT_CHECKSUM > $CHECKSUM_FILE
