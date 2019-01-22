(define (load-relative filename)
  (with-working-directory-pathname
   (directory-namestring (current-load-pathname))
   (lambda () (load filename))))

(load-relative "specific/mit.scm")
(load-relative "common/base.scm")
(load-relative "common/handlers.scm")
