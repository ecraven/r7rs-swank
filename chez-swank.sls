(library (chez-swank)
  (export start-swank
          swank:get-last-exception
          swank:lookup-presented-object-or-lose
          swank:lookup-presented-object
          inspect-in-emacs)
  (import (socket net)
          (srfi :69)
          (except (rename (chezscheme)
                        (bytevector-copy! %bytevector-copy!)
                        (load %load)
                        (error %error)
                        (with-output-to-string %with-output-to-string))
                  string-ci-hash make-hash-table hash-table? string-hash))
  (include "specific/chez.ss")
  (include "common/base.scm")
  (include "common/handlers.scm")
  
  ;; print everything possible
  (print-extended-identifiers #t))
