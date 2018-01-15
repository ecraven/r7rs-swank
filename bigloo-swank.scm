(module bigloo-swank
        (include "specific/bigloo.scm")
        (include "common/base.scm")
        (include "common/handlers.scm")
        (main main))

(define (main argv::pair)
  (display "starting") (newline) (flush-output-port)
  (start-swank "/tmp/bigloo-port.txt"))
