#!/bin/sh

#
# Build the application.
# 
export LAZBUILD=$(which lazbuild)
$LAZBUILD --build-mode="Release" src/filecruncher.lpi
 
