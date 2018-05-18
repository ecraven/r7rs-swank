#!/bin/bash
echo '(begin (import (larceny-swank)) (start-swank))' | larceny -r7rs -A .
