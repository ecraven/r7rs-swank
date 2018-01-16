#!/bin/bash
export GERBIL_LOADPATH=.
gxi --lang r7rs -e '(import (ecraven gerbil-swank))' -e '(start-swank "/tmp/gerbil-swank.txt")'
