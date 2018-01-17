(define ($scheme-name)
  "chez-scheme")

(define (load path env)
  (%load path))

(define (error . rest)
  (apply %error 'error rest))

(define (binding-documentation p)
  ;; same as (inspect object), then hitting c
  (let ((s (((inspect/object p) 'code) 'source)))
    (if s
        (let ((form (s 'value)))
          (cond ((and (list? form)
                      (> (length form) 3)
                      (eq? (car form) 'lambda)
                      (string? (caddr form)))
                 (caddr form))
                ((and (list? form)
                      (> (length form) 2)
                      (eq? (car form) 'case-lambda))
                 '(case-lambda))
                (else
                 #f)))
        #f)))

(define (procedure-parameter-list p)
  ;; same as (inspect object), then hitting c
  (let ((s (((inspect/object p) 'code) 'source)))
    (if s
        (let ((form (s 'value)))
          (cond ((and (list? form)
                      (> (length form) 2)
                      (eq? (car form) 'lambda))
                 (cadr form))
                ((and (list? form)
                      (> (length form) 2)
                      (eq? (car form) 'case-lambda))
                 '(case-lambda))
                (else
                 #f)))
        #f)))

(define ($function-parameters-and-documentation name)
  (let* ((binding (call/cc (lambda (k) (with-exception-handler (lambda (c) (k #f)) (lambda () (eval (string->symbol name) (param:environment)))))))
         (param-list (if binding (procedure-parameter-list binding) #f))
         (signature (if param-list (cons (string->symbol name) param-list) #f))
         (doc (if binding (binding-documentation binding) #f)))
    (cons signature doc)))

(define (%write-to-string val)
  (let ((o (open-output-string)))
    (write val o)
    (get-output-string o)))

(define ($environment env-name)
  ;; don't support any changes
  (interaction-environment))

(define ($set-package env-name)
  ;; don't support any changes
  (list "(user)" "(user)"))

(define ($open-tcp-server/accept port-number handler)
     (with-tcp-server-socket
      port-number 1
      (lambda (socket)
        (let ((p (tcp-server-connection-accept socket)))
          (handler port-number p p)))))

(define $make-hash-table make-hash-table)

(define $hash-table/put! hashtable-set!)

(define $hash-table/get hashtable-ref)

(define (read-bytevector size port)
  (get-bytevector-n port size))

(define (write-string str port)
  (put-bytevector port (string->utf8 str)))

(define ($error-description error)
  (let ((o (open-output-string)))
    (display-condition error o)
    (get-output-string o)))

(define ($all-package-names)
  (map (lambda (package) (%write-to-string package))
       (cons '(user) (library-list))))

(define (bytevector-copy! to to-index from)
  (%bytevector-copy! from 0 to to-index (bytevector-length from)))

(define ($output-to-repl thunk)
  ;; basic implementation, print all output at the end, this should
  ;; be replaced with a custom output port
  (let ((o (open-output-string)))
    (parameterize ((current-output-port o)
                   (trace-output-port o)
                   (current-error-port o))
      (let-values ((x (thunk)))
        (swank/write-string (get-output-string o) #f)
        (apply values x)))))

(define (pstring->environment pstring)
  (if (or (eq? 'nil pstring)
          (string=? "(user)" pstring)
          (string=? "COMMON-LISP-USER" pstring))
      (interaction-environment)
      (environment (read-from-string pstring))))

(define (read-from-string string)
  (read (open-input-string string)))

(define (string-prefix? x y)
  (let ([n (string-length x)])
    (and (fx<= n (string-length y))
         (let prefix? ([i 0])
           (or (fx= i n)
               (and (char=? (string-ref x i) (string-ref y i))
                    (prefix? (fx+ i 1))))))))

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
      '()
      (fold-left (lambda (s1 s2) (substring s2 0 (string-match-forward s1 s2))) (car strings) (cdr strings))))

(define ($completions prefix env-name)
  (let ((matches (sort string-ci<?
                       (filter (lambda (el)
                                 (string-prefix? prefix el))
                               (map %write-to-string (environment-symbols (pstring->environment env-name))))))) ;;  (interaction-environment)
    (cons matches
          (longest-common-prefix matches))))

(define (list-index predicate list)
  (let loop ((i 0)
             (l list))
    (if (null? l)
        #f
        (if (predicate (car l))
            i
            (loop (+ i 1) (cdr l))))))
