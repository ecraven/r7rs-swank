#!/bin/bash
dir=$(dirname "$0")
export GERBIL_LOADPATH=${GERBIL_LOADPATH:-$dir}
gxi $GERBIL_HOME/lib/gxi-interactive -e '(import :ecraven/gerbil-swank)' -e '(start-swank)'
