(define ($scheme-name)
  "larceny")
(define ($all-package-names)
  '())

(define ($error-description error)
  (let ((o (open-output-string)))
    (display error o)
    (get-output-string o)))

(define ($open-tcp-server/accept port-number handler)
  (display "$open-tcp-server/accept ") (write port-number) (newline) (flush-output-port)
  (let ((s (make-server-socket port-number 'blocking)))
    (display "server socket: ") (write s) (newline) (flush-output-port)
    (let-values (((ns addr) (server-socket-accept s)))
      (display "accepted: ") (write ns) (display " ") (write addr) (newline) (flush-output-port)
      (let ((in (socket-input-port ns 'blocking 'char))
            (out (socket-output-port ns 'blocking 'char 'flush)))
        (handler port-number in out)))))

(define ($open-tcp-server port-number port-file handler)
  (let ((n (or port-number (+ 10000 (random 50000)))))
    (display "$open-tcp-server/accept ") (write n) (newline) (flush-output-port)
    (let ((s (make-server-socket n 'blocking)))
      (display "server socket: ") (write s) (newline) (flush-output-port)
      (handler n s))))

(define ($tcp-server-accept socket handler)
  (display "accepting on socket: ") (write socket) (newline) (flush-output-port)
  (let-values (((ns addr) (server-socket-accept socket)))
    (display "accepted: ") (write ns) (display " ") (write addr) (newline) (flush-output-port)
      (let ((in (socket-input-port ns 'blocking 'char))
            (out (socket-output-port ns 'blocking 'char 'flush)))
        (handler in out))))

(define ($output-to-repl thunk)
  ;; basic implementation, print all output at the end, this should
  ;; be replaced with a custom output port
  (let ((o (open-output-string)))
    (parameterize ((current-output-port o))
      (let-values ((x (thunk)))
        (swank/write-string (get-output-string o) #f)
        (apply values x)))))

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

(define ($frame-var-value frame index)
  #f)

(define ($condition-msg condition)
  "UNKNOWN")

(define ($condition-links condition)
  '())

(define ($condition-location condition)
  "Return (PATH POSITION LINE COLUMN) for CONDITION."
  #f)

(define ($handle-condition exception)
  #f)

(define $pretty-print pretty-print)

(define-record-type <istate>
  (make-istate object parts actions next previous content)
  istate?
  (object istate-object)
  (parts istate-parts)
  (actions istate-actions)
  (next istate-next set-istate-next!)
  (previous istate-previous)
  (content istate-content))

(define ($inspect-fallback object)
  #f)

(define ($apropos name)
  '())

(define ($binding-documentation value)
  "")
