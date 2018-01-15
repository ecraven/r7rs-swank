#lang r7rs
(import (scheme base) (scheme eval) (scheme read) (scheme write) (scheme file) (scheme case-lambda) (scheme process-context) (scheme repl)
        (only (srfi 1) fold filter fold-right)
        (only (compatibility/mlist) mlist->list list->mlist)
        (only (srfi 13) string-prefix?)
        (only (racket/base) namespace? make-base-namespace namespace-require current-namespace directory-exists? current-directory build-path namespace-mapped-symbols foldr mcons mcar mcdr)
        (only (racket/file) fold-files))

(include "specific/racket.ss")
(include "common/base.scm")
(include "common/handlers.scm")
(start-swank "/tmp/racket-port.txt")
