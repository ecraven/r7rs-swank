(define ($scheme-name)
  "kawa")
(define ($open-tcp-server/accept port-number handler)
  (let ((s (java.net.ServerSocket port-number)))
    (let ((connection-socket (s:accept)))
      (handler port-number (connection-socket:getInputStream) (gnu.kawa.io.OutPort (connection-socket:getOutputStream))))))

;; kawa reads foo:bar as ($lookup$ foo (quasiquote bar)), so equal?, not eq?
(define ($make-hash-table) (make-hashtable equal-hash equal?))

(define $hash-table/put! hashtable-set!)

(define $hash-table/get hashtable-ref)

(define ($error-description error)
  (error:toString))

(define (environment-bindings env :: gnu.mapping.SimpleEnvironment)
  (let loop ((enum :: gnu.mapping.LocationEnumeration (env:enumerateAllLocations))
             (result '()))
    (if (enum:hasMoreElements)
        (let ((current :: gnu.mapping.NamedLocation (enum:nextElement)))
          (loop enum (cons (current:getKeySymbol) result)))
        (reverse result))))
(define ($output-to-repl thunk)
  ;; basic implementation, print all output at the end, this should
  ;; be replaced with a custom output port
  (let ((o (open-output-string)))
    (parameterize ((current-output-port o))
      (let-values ((x (thunk)))
        (swank/write-string (get-output-string o) #f)
        (apply values x)))))
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
(define (env-name->environment env-name)
  (cond ((string=? env-name "(user)")
         (interaction-environment))
        (else
         (error "not implemented yet"))))
(define (find-java-matches prefix env-name)
  (if (prefix:contains ":")
      (let* ((parts (prefix:split ":"))
             (first (parts 0))
             (rest (substring prefix (+ (string-length first) 1) (string-length prefix))))
        (define (filter-relevant names)
          (filter-map (lambda (name)
                    (let ((p :: String (string-append first ":" name)))
                      (if (p:startsWith prefix)
                          p
                          #f)))
                  names))
        (let ((binding (call/cc (lambda (k) (with-exception-handler (lambda (condition) (k #f))
                                                               (lambda ()
                                                                 (eval (string->symbol first) (env-name->environment env-name))))))))
          (if binding
              (let ((c (if (instance? binding java.lang.Class) binding (binding:getClass))))
                (let ((methods (*:getMethods c))
                      (fields (*:getFields c)))
                  (append (filter-relevant (map *:getName methods))
                          (filter-relevant (map *:getName fields)))))
              '())))
      '()))
(define ($completions prefix env-name)
  (let* ((matches (filter (lambda (el :: java.lang.String) (el:startsWith prefix))
                         (map symbol->string (environment-bindings (env-name->environment env-name)))))
         (java-matches (find-java-matches prefix env-name))
         (all (append matches java-matches)))
    (cons all
          (longest-common-prefix all))))

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
