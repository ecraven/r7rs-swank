(define ($open-tcp-server/accept port-number handler)
  (let* ((p (open-tcp-server port-number))
         (c (read p)))
    (handler port-number c c)))

(define $make-hash-table make-hash-table)

(define $hash-table/put! hash-put!)

(define $hash-table/get hash-ref)

;; TODO remove when let-values works correctly
(define-syntax let-values
  (syntax-rules ()
    ((let-values ((vals form)) body0 body ...)
     (call-with-values (lambda () form) (lambda vals body0 body ...)))))

(define ($all-package-names)
  (map expander-context-id (filter module-context? (map cdr (hash->list (current-expander-module-registry))))))

(define ($error-description error)
  (let ((o (open-output-string)))
    (display-exception error o)
    (get-output-string o)))

(define ($output-to-repl thunk)
  ;; basic implementation, print all output at the end, this should
  ;; be replaced with a custom output port
  (let ((o (open-output-string)))
    (parameterize ((current-output-port o))
      (let-values ((x (thunk)))
        (swank/write-string (get-output-string o) #f)
        (apply values x)))))

(define (env-name->environment env-name)
  (cond ((string=? env-name "(user)")
         (interaction-environment))
        (else
         (find (lambda (e) (eq? (expander-context-id e) env-name))
               (filter module-context? (map cdr (hash->list (current-expander-module-registry))))))))

(define (environment-bindings env-name)
  (let ((env (env-name->environment env-name)))
    (if env
        (map car (hash->list (expander-context-table env)))
        '())))
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
                         (map symbol->string (environment-bindings env-name)))))
    (cons matches
          (longest-common-prefix matches))))
