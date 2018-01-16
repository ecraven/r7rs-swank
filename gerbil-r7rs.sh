#!/bin/bash
gxi --lang r7rs -e '(import (ecraven gerbil-swank))' -e '(start-swank "/tmp/gerbil-swank.txt")'
