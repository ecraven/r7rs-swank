#lang r7rs
;(define-library (racket-swank)
   (import (scheme base) (scheme eval) (scheme read) (scheme write) (scheme file) (scheme case-lambda) (scheme process-context) (scheme repl) (scheme load) (scheme char) (scheme cxr)
          (only (srfi 1) fold filter fold-right find list-index iota take cons*)
          (only (compatibility/mlist) mlist->list list->mlist)
          (only (srfi 13) string-prefix? string-contains string-replace)
          (only (racket/base) namespace? make-base-namespace namespace-require current-namespace directory-exists? current-directory build-path namespace-mapped-symbols foldr mcons mcar mcdr expand expand-once)
          (only (racket/file) fold-files)
          (only (racket/pretty) pretty-print)
          (racket/tcp) (srfi/69))
  (export start-swank)

  (include "specific/racket.ss")
  (include "common/base.scm")
  (include "common/handlers.scm")
  ;)
(start-swank)

