;; (import (scheme base) (scheme eval) (scheme read) (scheme write) (scheme file) (scheme case-lambda) (scheme process-context)
;;         (tcp) (srfi-69))
(use tcp)
(use srfi-69)
(begin
  (load "specific/chicken.scm")
  (load "common/base.scm")
  (load "common/handlers.scm"))
