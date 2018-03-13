#!/bin/bash
# for dev, force recompile
touch guile-swank.scm
guile -l guile-swank.scm -c "(import (guile-swank)) (start-swank)"
