(define ($scheme-name)
  "cyclone-scheme")
(define ($open-tcp-server/accept port-number handler)
  (let ((server (make-server-socket (number->string port-number))))
    (let ((socket (socket-accept server)))
      (handler port-number (socket-input-port socket) (socket-output-port socket)))))

(define $make-hash-table make-hash-table)

(define $hash-table/put! hash-table-set!)

(define $hash-table/get hash-table-ref/default)

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
