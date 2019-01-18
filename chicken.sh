#!/bin/bash
# needs chicken 5
# chicken-install r7rs srfi-1 srfi-13 srfi-14 srfi-69 
csi -R r7rs chicken-swank.scm -b -e '(start-swank)'
