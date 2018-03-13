#!/bin/bash
csi -require-extension r7rs -e '(import (scheme base)) (load "chicken-swank") (start-swank)' -b
