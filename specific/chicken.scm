(define ($scheme-name)
  "chicken-scheme")
(define ($macroexpand-1 form)
  (##sys#expand form))
(define ($macroexpand-all form)
  (##sys#expand form))
(define ($open-tcp-server port-number port-file handler)
  (parameterize ((tcp-read-timeout #f))
    (let* ((n (or port-number (+ 10000 (pseudo-random-integer 50000))))
           (listener (tcp-listen n)))
      (handler n listener))))

(define-record-type <istate>
  (make-istate object parts actions next previous content)
  istate?
  (object istate-object)
  (parts istate-parts)
  (actions istate-actions)
  (next istate-next set-istate-next!)
  (previous istate-previous)
  (content istate-content))

(define ($tcp-server-accept listener handler)
  (let-values (((in out) (tcp-accept listener)))
        (handler in out)))

(define ($output-to-repl thunk)
  ;; basic implementation, print all output at the end, this should
  ;; be replaced with a custom output port
  (let ((o (open-output-string)))
    (parameterize ((current-output-port o))
      (let-values ((x (thunk)))
        (swank/write-string (get-output-string o) #f)
        (apply values x)))))

;; from chicken-core/csi.scm
(define ($error-description error)
  (if (##sys#structure? error 'condition)
      (let ((out (open-output-string)))
        (fprintf out "condition: ~s~%" (##sys#slot error 1))
        (for-each
         (lambda (k)
           (fprintf out " ~s~%" k)
           (let loop ((props (##sys#slot error 2)))
             (unless (null? props)
               (when (eq? k (caar props))
                 (##sys#with-print-length-limit
                  100
                  (lambda ()
                    (fprintf out "\t~s: ~s" (cdar props) (cadr props)) ))
                 (newline out))
               (loop (cddr props)))))
         (##sys#slot error 1))
        (get-output-string out))
        "unknown"))

(define ($completions prefix env-name)
  (cons '()
        prefix))

(define ($function-parameters-and-documentation name)
  (cons #f #f))

(define ($set-package name)
  (list "(user)" "(user)"))

(define ($environment name)
  (interaction-environment))

(define ($condition-trace condition)
  '())

(define ($frame-locals-and-catch-tags nr)
  '())

(define ($condition-msg condition)
  "UNKNOWN")

(define ($condition-links condition)
  '())

(define ($handle-condition exception)
  #f)

(define ($apropos name)
  "Return a list of (name type documentation) for all matches for NAME."
  '())

(define ($inspect-fallback object)
  #f)
