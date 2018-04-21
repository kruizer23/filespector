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
# Perform the install.
#
mkdir -p $DESTDIR/usr/bin
cp src/filecruncher $DESTDIR/usr/bin/
mkdir -p $DESTDIR/usr/share/filecruncher
cp src/filecruncher.ico $DESTDIR/usr/share/filecruncher/
mkdir -p $DESTDIR/usr/share/applications
cp src/filecruncher.desktop $DESTDIR/usr/share/applications/


