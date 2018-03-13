#!/bin/bash
dir=$(dirname "$0")
export GERBIL_LOADPATH=${GERBIL_LOADPATH:-$dir}
gxi --lang r7rs -e '(import (ecraven gerbil-swank))' -e '(start-swank)'
