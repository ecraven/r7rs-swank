(define ($scheme-name)
  "racket")
(define ($macroexpand-1 form)
  (expand-once form))
(define ($macroexpand-all form)
  (expand form))
(define ($open-tcp-server port-number port-file handler)
  (let ((n (or port-number 0))
        (server (tcp-listen port-number)))
    (handler n server)))
(define ($tcp-server-accept server handler)
  (let-values (((in out) (tcp-accept server)))
      (handler in out)))

(define env (let ((n (make-base-namespace)))
              (parameterize ((current-namespace n))
                (namespace-require 'r7rs)
                (namespace-require 'r7rs/base)
                (namespace-require 'r7rs/read)
                (namespace-require 'r7rs/write))
              n))
(define (interaction-environment) env)


(define ($all-package-names)
  '()) ;;  (module-list) from geiser

;; 

(define ($error-description error)
  (let ((o (open-output-string)))
    (display error o)
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
  env)
(define (environment-bindings env)
  (namespace-mapped-symbols env))
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
      (fold (lambda (s1 s2) (substring s2 0 (string-match-forward s1 s2))) (mcar strings) (mlist->list (mcdr strings)))))
(define ($completions prefix env-name)
  (let ((matches (list->mlist (filter (lambda (el) (string-prefix? prefix el))
                                      (mlist->list (map symbol->string (list->mlist (environment-bindings (env-name->environment env-name)))))))))
    (cons (mlist->list matches)
          (longest-common-prefix matches))))

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

(define ($inspect-fallback object)
  #f)

(define $pretty-print pretty-print)

(define ($binding-documentation binding)
  #f)

(define ($condition-location condition)
  "Return (PATH POSITION LINE COLUMN) for CONDITION."
  #f)

(define ($apropos name)
  '())
