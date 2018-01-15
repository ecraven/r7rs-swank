#!/bin/bash
export LD_LIBRARY_PATH=/home/nex/scheme/chez/arcfide/
echo '(import (chez-swank)) (start-swank "/tmp/chez-swank.port")' | chez-scheme --libdirs '.:/home/nex/scheme/chez'
#chez-scheme --libdirs .:/home/nex/scheme/chez
