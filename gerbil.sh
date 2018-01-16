#!/bin/bash
export GERBIL_LOADPATH=.
gxi $GERBIL_HOME/lib/gxi-interactive -e '(import :ecraven/gerbil-swank)' -e '(start-swank "/tmp/gerbil-swank.txt")'
