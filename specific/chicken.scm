(define ($open-tcp-server/accept port-number handler)
  (parameterize ((tcp-read-timeout #f))
    (let ((listener (tcp-listen port-number)))
      (let-values (((in out) (tcp-accept listener)))
        (handler port-number in out)))))

(define $make-hash-table make-hash-table)

(define $hash-table/put! hash-table-set!)

(define $hash-table/get hash-table-ref/default)

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
