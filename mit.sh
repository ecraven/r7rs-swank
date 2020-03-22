#!/bin/bash
mit-scheme --heap 300000 --load $(pwd)/mit-swank.sld  --eval '(start-swank 4005)'
