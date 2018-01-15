#!/bin/bash
gxi --lang r7rs -e '(add-load-path "/home/nex/scheme/swank/")' -e '(import (gerbil-swank))' -e '(start-swank "/tmp/foo.txt"))'
