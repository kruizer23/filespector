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
rm -f $DESTDIR/usr/bin/filecruncher
rm -f $DESTDIR/usr/share/filecruncher/filecruncher.ico
rm -f $DESTDIR/usr/share/applications/filecruncher.desktop
rmdir $DESTDIR/usr/share/filecruncher

