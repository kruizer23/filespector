#!/bin/sh

#
# Parses command line options. Currently supported options are:
#
# DESTDIR		Installation destination base directory
# 
DESTDIR=""
for arg; do
  case $arg in
    DESTDIR=*) DESTDIR=${arg#DESTDIR=};;
  esac;
done

#
# Perform the uninstall.
#
rm -f $DESTDIR/usr/bin/filespector
rm -f $DESTDIR/usr/share/filespector/filespector.ico
rm -f $DESTDIR/usr/share/applications/filespector.desktop
rmdir $DESTDIR/usr/share/filespector

