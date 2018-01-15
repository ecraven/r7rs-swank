(define (fluid a)
  (a))

(define ($open-tcp-server/accept port-number handler)
  (let ((socket (open-tcp-server-socket port-number (host-address-loopback))))
    (let ((p (tcp-server-connection-accept socket #t #f)))
      (handler port-number p p))))

(define $make-hash-table make-hash-table)

(define $hash-table/put! hash-table/put!)

(define $hash-table/get hash-table/get)

(define ($all-package-names)
  (map (lambda (package) (env->pstring (package/environment package)))
       (all-packages)))

(define ($error-description condition)
  (condition/report-string condition))

(define (all-packages)
  (let loop ((package (name->package '()))) ;;  system-global-package
    (cons package
          (append-map loop (package/children package)))))

(define anonymous-package-prefix "environment-")

(define unknown-environment (cons 'unknown 'environment))

(define (env->pstring env)
  (if (eq? unknown-environment env)
      "unknown environment"
      (let ((package (environment->package env)))
	(if package
	    (write-to-string (package/name package))
	    (string anonymous-package-prefix (object-hash env))))))

(define (with-exception-handler handler thunk)
  (bind-condition-handler (list condition-type:serious-condition)
                          handler
                          thunk))

(define ($output-to-repl thunk)
  ;; basic implementation, print all output at the end, this should
  ;; be replaced with a custom output port
  (let ((o (open-output-string)))
    (parameterize ((current-output-port o))
      (begin0
       (thunk)
       (swank/write-string (get-output-string o) #f)))))

(define ($completions prefix env-name)
  (let ((strings (all-completions prefix (pstring->env env-name))))
    (cons (sort strings string<?)
          (longest-common-prefix strings))))

(define (longest-common-prefix strings)
  (reduce (lambda (s1 s2)
            (substring s1 0 (string-match-forward s1 s2)))
          ""
          strings))

(define *buffer-pstring* (make-parameter #f))

(define (pstring->env pstring)
  (cond ((or (not (string? pstring))
             (not (string? (fluid *buffer-pstring*)))
             (string-ci=? (fluid *buffer-pstring*) "COMMON-LISP-USER"))
         (nearest-repl/environment))
        ((string-prefix? anonymous-package-prefix pstring)
         (let ((object
                (object-unhash
                 (string->number (string-tail pstring
                                              (string-length
                                               anonymous-package-prefix))
                                 10
                                 #t))))
           (if (not (environment? object))
               (error:wrong-type-datum object "environment"))
           object))
        (else
         (package/environment (find-package (read-from-string pstring) #t)))))

(define (all-completions prefix environment)
  (let ((prefix
         (if (fluid (environment-lookup environment 'PARAM:PARSER-CANONICALIZE-SYMBOLS?))
             (string-downcase prefix)
             prefix))
        (completions '()))
    (for-each-interned-symbol
     (lambda (symbol)
       (if (and (string-prefix? prefix (symbol-name symbol))
                (environment-bound? environment symbol))
           (set! completions (cons (symbol-name symbol) completions)))
       unspecific))
    completions))
