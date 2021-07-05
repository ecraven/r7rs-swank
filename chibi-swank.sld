(define-library (chibi-swank)
  (export start-swank
          swank:lookup-presented-object-or-lose
          swank:lookup-presented-object
          inspect-in-emacs
          make-swank-image
          swank-register-image-converter
          swank-present
          tracing trace-define trace-let trace-let* trace-letrec trace-letrec*)
  (import (scheme base) (scheme eval) (scheme read) (scheme write)
          (scheme file) (scheme case-lambda) (scheme process-context) (scheme cxr)
          (chibi process) (chibi) (chibi string) (srfi 39) (srfi 1)
          (chibi repl) (chibi ast) (srfi 18) (srfi 95) (chibi net server)
          (chibi show) (chibi show pretty)
          (meta) (srfi 69) (chibi io) (chibi modules)
          (only (srfi 27) random-integer)
          (only (srfi 130) string-replace string-contains string-cursor->index))
  (include "specific/chibi.scm")
  (include "common/base.scm")
  (include "common/handlers.scm"))
