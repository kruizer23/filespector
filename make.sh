#!/bin/sh

#
# Build the application.
# 
export LAZBUILD=$(which lazbuild)
$LAZBUILD src/filecruncher.lpi


#
# Strip out symbols to shrink the size of the executable.
# 
strip src/filecruncher

 
