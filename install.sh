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
cp src/filespector $DESTDIR/usr/bin/
mkdir -p $DESTDIR/usr/share/filespector
cp src/filespector.ico $DESTDIR/usr/share/filespector/
mkdir -p $DESTDIR/usr/share/applications
cp src/filespector.desktop $DESTDIR/usr/share/applications/
for f in src/languages/*.mo; do 
  LOCALE=$(echo "$f" | cut -d'.' -f2)
  install -d -m 755 $DESTDIR/usr/share/locale/$LOCALE/LC_MESSAGES/
	install -pm 644 "$f" $DESTDIR/usr/share/locale/$LOCALE/LC_MESSAGES/filespector.mo
done


