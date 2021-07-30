(define-library (guile-swank)
  (import (scheme base)
          (scheme write)
          (scheme case-lambda)
          (scheme read)
          (scheme char)
          (scheme eval)
          (scheme repl)
          (scheme file)
          (scheme load)
          (scheme process-context)
          (ice-9 match)
          (ice-9 binary-ports)
          (ice-9 documentation)
          (only (ice-9 ports) with-output-to-port with-error-to-port)
          (srfi srfi-69)
          (srfi srfi-11) ;; let-values
          (srfi srfi-1)
          (srfi srfi-9)
          (only (srfi srfi-13) string-contains-ci string-contains string-replace string-prefix?)
          (only (guile)
                socket PF_INET SOCK_STREAM setsockopt SOL_SOCKET SO_REUSEADDR bind AF_INET INADDR_LOOPBACK listen accept
                module-for-each module-uses module-submodules resolve-module module-kind module-name current-module module-obarray variable-bound? variable-ref
                hash-for-each hash-map->list format
                macroexpand random)
          (ice-9 pretty-print))
  (export start-swank swank:lookup-presented-object
          swank:lookup-presented-object-or-lose
          make-swank-image
          swank-register-image-converter
          inspect-in-emacs
          swank-present
          tracing
          trace-let trace-let* trace-letrec trace-letrec* trace-define)

  (include "specific/guile.scm")
  (include "common/base.scm")
  (include "common/handlers.scm")
  )
