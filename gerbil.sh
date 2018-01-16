#!/bin/bash
gxi $GERBIL_HOME/lib/gxi-interactive -e '(import :ecraven/gerbil-swank)' -e '(start-swank "/tmp/gerbil-swan.txt")'
