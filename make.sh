#!/bin/sh

#
# Build the application.
# 
export LAZBUILD=$(which lazbuild)
$LAZBUILD --build-mode="Release" src/filespector.lpi
#
# Create binary version of all localization PO-files
#
export MSGFMT=$(which msgfmt)
for f in src/languages/*.*.po; do 
  $MSGFMT -o "${f%.po}.mo" "$f" 
done
 
