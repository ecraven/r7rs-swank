(library (chez-swank)
  (export start-swank
          swank:get-last-exception
          swank:lookup-presented-object-or-lose
          swank:lookup-presented-object)
  (import (rename (chezscheme)
                  (bytevector-copy! %bytevector-copy!)
                  (load %load)
                  (error %error)
                  (with-output-to-string %with-output-to-string))
          (socket net))
  (include "specific/chez.ss")
  (include "common/base.scm")
  (include "common/handlers.scm")
  
  ;; print everything possible
  (print-extended-identifiers #t)
)
