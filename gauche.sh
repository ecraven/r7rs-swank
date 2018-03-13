#!/bin/bash
GAUCHE_KEYWORD_IS_SYMBOL=T gosh -r7 -A . -l gauche-swank.sld -e '(begin (import (gauche-swank)) (start-swank))'
