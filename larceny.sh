#!/bin/bash
echo '(begin (import (larceny-swank)) (start-swank "/tmp/larceny-port.txt"))' | larceny -r7rs -A .
