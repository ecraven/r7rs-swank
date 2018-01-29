(use-modules (srfi srfi-11))
(define ($scheme-name)
  "guile")
(define* (make-server-socket port)
  (let ((sock (socket PF_INET SOCK_STREAM 0)))
    (setsockopt sock SOL_SOCKET SO_REUSEADDR 1)
    (bind sock AF_INET INADDR_LOOPBACK port)
    sock))

(define (accept-new-client server-socket)
  (match (accept server-socket)
         ((client-socket . _) client-socket)))

(define ($open-tcp-server/accept port-number handler)
  (let* ((server-socket (make-server-socket port-number))
         (dummy (listen server-socket 1))
         (port (accept-new-client server-socket)))
    (handler port-number port port)))

(define $make-hash-table make-hash-table)

(define $hash-table/put! hash-table-set!)

(define $hash-table/get hash-table-ref/default)

(define fop flush-output-port)
(define (flush-output-port . port) (fop (if (null? port) (current-output-port) (car port))))

(define (read-bytevector count port) (get-bytevector-n port count))
(define bvc! bytevector-copy!)
(define (bytevector-copy! to to-index from)
  (bvc! from 0 to to-index (bytevector-length from)))

(define write-string display)

(define ($all-package-names)
  (all-modules))

(define (with-exception-handler handler thunk)
  (catch #t
         thunk
         (lambda p
           (handler p))))

(define ($error-description error)
  (let ((o (open-output-string)))
    (write error o)
    (get-output-string o)))

(define ($output-to-repl thunk)
  ;; basic implementation, print all output at the end, this should
  ;; be replaced with a custom output port
  (let ((o (open-output-string)))
    (with-output-to-port o (lambda ()
                             (with-error-to-port o (lambda ()
                                                     (let-values ((x (thunk)))
                                                       (swank/write-string (get-output-string o) #f)
                                                       (apply values x))))))))

(define ($all-package-names)
  '())

(define (env-name->environment env-name)
 ;; TODO
  (interaction-environment))
(define (environment-bindings env)
  (define (env-binds env)
    (let ((results '()))
      (module-for-each (lambda (k v) (set! results (cons k results))) env)
      results))
  (append (env-binds env)
          (append-map env-binds (module-uses env))))
(define (string-match-forward a b)
  (let* ((a-len (string-length a))
         (b-len (string-length b))
         (min-len (min a-len b-len)))
    (let loop ((i 0))
      (if (> i min-len)
          (- i 1)
          (if (string=? (substring a 0 i) (substring b 0 i))
              (loop (+ i 1))
              (- i 1))))))
(define (longest-common-prefix strings)
  (if (null? strings)
      ""
      (fold (lambda (s1 s2) (substring s2 0 (string-match-forward s1 s2))) (car strings) (cdr strings))))
(define ($completions prefix env-name)
  (let ((matches (filter (lambda (el) (string-prefix? prefix el))
                         (map symbol->string (environment-bindings (env-name->environment env-name))))))
    (cons matches
          (longest-common-prefix matches))))
;;;; taken from geiser
(define (submodules mod)
  (hash-map->list (lambda (k v) v) (module-submodules mod)))

(define (root-modules)
  (submodules (resolve-module '() #f)))

(define (all-modules)
  (define (maybe-name m)
    (and (module-kind m) (format #f "~A" (module-name m))))
  (let* ((guile (resolve-module '(guile)))
         (roots (remove (lambda (m) (eq? m guile)) (root-modules)))
         (children (append-map all-child-modules roots)))
    (cons "(user)" (filter-map maybe-name children))))

(define* (all-child-modules mod #:optional (seen '()))
  (let ((cs (filter (lambda (m) (not (member m seen))) (submodules mod))))
    (fold (lambda (m all) (append (all-child-modules m all) all))
          (list mod)
          cs)))


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

(define ($function-parameters-and-documentation name)
  (cons #f #f))

(define ($set-package name)
  (list "(user)" "(user)"))

(define ($environment name)
  (interaction-environment))
