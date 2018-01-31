#!/bin/bash
echo '(import (chez-swank)) (start-swank "/tmp/chez-swank.port")' | chez-scheme --libdirs '.:/home/nex/scheme/chez'

