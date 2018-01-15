(define ($open-tcp-server/accept port-number handler)
  (let* ((socket (make-server-socket 'inet port-number ':reuse-addr? #t))
         (cs (socket-accept socket)))
    (handler port-number (socket-input-port cs) (socket-output-port cs))))

(define $make-hash-table make-hash-table)

(define $hash-table/put! hash-table-set!)

(define $hash-table/get hash-table-ref/default)

(define ($all-package-names)
  (map module-name (all-modules)))

(define (display-to-string val)
  (let ((out (open-output-string)))
    (display val out)
    (get-output-string out)))

(define ($error-description error)
  (apply string-append (error-object-message error)
                 ": "
                 (map write-to-string (error-object-irritants error))))

(define sts symbol->string)
(define (symbol->string x)
  (cond ((keyword? x)
         (string-append ":" (keyword->string x)))
        ((symbol? x)
         (sts x))
        (error "not symbol or keyword" x)))

(define ($output-to-repl thunk)
  ;; basic implementation, print all output at the end, this should
  ;; be replaced with a custom output port
  (let ((o (open-output-string)))
    (parameterize ((current-output-port o))
      (let-values ((x (thunk)))
        (swank/write-string (get-output-string o) #f)
        (apply values x)))))
