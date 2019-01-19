(define ($scheme-name)
  "kawa")
;; not sure how to express this require as an r7rs import :-/
;; (require 'syntax-utils)
;; (define ($macroexpand-1 form)
;;   (expand form))
;; (define ($macroexpand-all form)
;;   (expand form))

;; kawa reads foo:bar as ($lookup$ foo (quasiquote bar)), this leads to many problems :-/

(define (random n)
  ((java.util.concurrent.ThreadLocalRandom:current):nextInt 0 (+ n 1)))

(define ($open-tcp-server port-number port-file handler)
  (display port-number) (newline)
  (let* ((n (or port-number (+ 10000 (random 50000))))
         (socket (java.net.ServerSocket n)))
    (handler n socket)))

(define ($tcp-server-accept socket handler)
  (let ((connection-socket (socket:accept)))
      (handler (connection-socket:getInputStream) (gnu.kawa.io.OutPort (connection-socket:getOutputStream)))))

(define ($error-description error)
  (error:toString))

(define ($all-package-names)
  ;; TODO
  '("(user)"))

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
              (let ((c :: java.lang.Class (if (instance? binding java.lang.Class) binding (binding:getClass))))
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
  (map *:toString ($condition-links condition)))

(define ($condition-location stack-trace-element)
  "Return (PATH POSITION LINE COLUMN) for CONDITION."
  (let ((file (stack-trace-element:getFileName))
        (line (stack-trace-element:getLineNumber)))
    `(,file #f ,line 0)))

(define ($frame-locals-and-catch-tags nr)
  'nil)

(define ($frame-var-value frame index)
  #f)

(define ($condition-msg condition)
  (condition:toString))

(define ($condition-links condition)
  (let ((st (as vector (condition:getStackTrace))))
    (vector->list st)))

(define ($handle-condition exception)
  (when (instance? exception java.lang.Exception)
    (invoke-sldb exception)))

(define $pretty-print pprint)

(define ($inspect-fallback object)
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
  (let ((env ($environment (param:environment))))
    (environment-fold env
                      (lambda (key accumulator)
                        (if (and (symbol? key)
                                 (string-contains-ci (symbol->string key) name))
                            (let ((value (eval key env)))
                              (cons (list key (if (procedure? value) ':function ':variable) ($binding-documentation value)) accumulator))
                            accumulator))
                      '())))

(define ($binding-documentation value)
  "")

(define ($macroexpand-1 form)
  form)

(define ($macroexpand-all form)
  form)
