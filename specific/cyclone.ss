(define ($scheme-name)
  "cyclone-scheme")
(define ($open-tcp-server/accept port-number handler)
  (let ((server (make-server-socket (number->string port-number))))
    (let ((socket (socket-accept server)))
      (handler port-number (socket-input-port socket) (socket-output-port socket)))))

(define ($all-package-names)
  '())

(define ($error-description error)
  (let ((o (open-output-string)))
    (write error o)
    (get-output-string o)))

(define env (setup-environment))
(define (interaction-environment) env) ;; *global-environment* ?

(define (read-bytevector k port)
  (string->utf8 (read-string k port)))

;; (scheme cyclone util) env:all-variables

(define ($output-to-repl thunk)
  ;; basic implementation, print all output at the end, this should
  ;; be replaced with a custom output port
  (let ((o (open-output-string)))
    (parameterize ((current-output-port o)
                   (current-error-port o))
      (let-values ((x (thunk)))
        (swank/write-string (get-output-string o) #f)
        (apply values x)))))

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

(define-record-type <istate>
  (make-istate object parts actions next previous content)
  istate?
  (object istate-object)
  (parts istate-parts)
  (actions istate-actions)
  (next istate-next set-istate-next!)
  (previous istate-previous)
  (content istate-content))

(define ($apropos name)
  '())

(define ($completions prefix env-name)
  (cons '() prefix))

(define ($condition-location condition)
  #f)

(define ($frame-var-value frame index)
  #f)

(define $pretty-print display)

(define ($binding-documentation p)
  #f)

(define ($inspect-fallback object)
  #f)
