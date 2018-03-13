(define-library (cyclone-swank)
  (export start-swank)
  (import (scheme base) (scheme eval) (scheme read) (scheme write) (scheme file) (scheme case-lambda) (scheme process-context)
          (scheme load) (scheme char) ;; (scheme repl)
          (srfi 106) (srfi 69) (srfi 1))
  (include "specific/cyclone.ss")
  (include "common/base.scm")
  (include "common/handlers.scm")
  (begin
    (start-swank "/tmp/cyclone-port.txt")))

