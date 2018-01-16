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
