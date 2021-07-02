(define ($scheme-name)
  "gerbil-scheme")

(define (remove-underscores-and-trailing-numbers str)
  (define last (- (string-length str) 1))
  (cond ((char-numeric? (string-ref str last))
         (remove-underscores-and-trailing-numbers (substring str 0 last)))
        ((char=? (string-ref str last) #\_)
         (remove-underscores-and-trailing-numbers (substring str 0 last)))
        ((char=? (string-ref str 0) #\_)
         (remove-underscores-and-trailing-numbers (substring str 1 (+ last 1))))
        (else
         (string->symbol str))))

(define (xmap fun lst)
  (if (null? lst)
      '()
      (if (not (pair? lst))
          (fun lst)
          (cons (fun (car lst)) (xmap fun (cdr lst))))))

(define (unsyntax-bindings lst)
  (xmap (lambda (sym)
          (let ((s (symbol->string sym)))
            (remove-underscores-and-trailing-numbers s)))
        lst))

(define ($function-parameters-and-documentation name)
  (let* ((binding (call/cc (lambda (k) (with-exception-handler (lambda (c) (k #f)) (lambda () (eval (string->symbol name) (param:environment)))))))
         (source (if binding (##decompile binding) #f)))
    (if (and source
             (pair? source)
             (not (null? source)))
        (let ((s (car source)))
          (case s
            ((lambda ##lambda)
             (cons (cons (string->symbol name) (unsyntax-bindings (cadr source)))
                   (if (string? (caddr source)) (caddr source) #f)))
            (else
             (cons #f #f))))
        (cons #f #f))))

(define ($set-package name)
  (list name name))

(define ($open-tcp-server port-number port-file handler)
  (let ((n (or port-number (+ 10000 (random-integer 40000)))))
    (let* ((p (open-tcp-server n)))
      (handler n p))))

(define ($tcp-server-accept p handler)
  (let ((c (read p)))
      (handler c c)))

(define hash-table-set! hash-put!)
(define hash-table-ref/default hash-ref)
(define hash-table-size hash-length)
;;(define hash-table? hash-table?)
(%#extern (hash-table? table?))
(define (hash-table-walk table fun) (hash-for-each fun table))
(define hash-table-delete! hash-remove!)

;; TODO remove when let-values works correctly
(define-syntax let-values
  (syntax-rules ()
    ((let-values ((vals form)) body0 body ...)
     (call-with-values (lambda () form) (lambda vals body0 body ...)))))

(define ($all-package-names)
  (cons "(user)" (map expander-context-id (filter module-context? (map cdr (hash->list (current-expander-module-registry)))))))

(define ($error-description error)
  (let ((o (open-output-string)))
    (display-exception error o)
    (get-output-string o)))

(define ($output-to-repl thunk)
  ;; basic implementation, print all output at the end, this should
  ;; be replaced with a custom output port
  (let ((o (open-output-string)))
    (parameterize ((current-output-port o)
                   (current-error-port o))
      (let-values ((x (thunk)))
        (swank/write-string (get-output-string o) #f)
        (apply values x)))))
(define (env-name->environment env-name)
  (cond ((or (and (string? env-name) (string=? env-name "(user)"))
             (and (symbol? env-name) (eq? env-name 'nil)))
         (gx#current-expander-context))
        (else
         (let ((name (string->symbol env-name)))
           (find (lambda (e) (eq? (expander-context-id e) name))
                 (filter module-context? (map cdr (hash->list (current-expander-module-registry)))))))))
(define $environment env-name->environment)

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

(define ($pretty-print object)
  (pp object (current-output-port)))

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

(define %repl-result-history-ref ##repl-result-history-ref)
(define (repl-result-history-ref id)
  ;; If we are swanky
  (if (param:environment)
    (call-with-values
	(lambda () (swank:lookup-presented-object
		    (- last-presentation-id id)))
	(lambda (value exists?)
	  (unless (eq? 'nil exists?)
	    value)))
    ;;otherwise, back to normal
    (%repl-result-history-ref id)))

(define ($binding-documentation name)
  #f)

(define ($apropos name)
  '())

(define ($condition-location condition)
  #f)

(define ($macroexpand-1 form)
  form)

(define ($macroexpand-all form)
  form)
