#!/bin/bash
gxi --lang r7rs -e '(add-load-path "'$(pwd)'")' -e '(import (gerbil-swank))' -e '(start-swank "/tmp/foo.txt")'
