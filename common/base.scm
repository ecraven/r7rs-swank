(define (read-packet port)
  "read 6 byte ascii hex length, then length bytes, all utf-8"
  (let* ((length (string->number (utf8->string (read-bytevector 6 port)) 16))
         (result (make-bytevector length)))
    (let loop ((result-index 0)
               (to-read length))
      (if (zero? to-read)
          (let ((res (utf8->string result)))
            (display "from slime> " log-port) (write res log-port) (newline log-port) (flush-output-port log-port)
            (message->scheme res))
          (let* ((tmp (read-bytevector to-read port))
                 (len (bytevector-length tmp)))
            (bytevector-copy! result result-index tmp)
            (loop (+ result-index len)
                  (- to-read len)))))))

(define (message->scheme string)
  (read (open-input-string string)))

(define (scheme->message form)
  (let ((o (open-output-string)))
    (write form o)
    (get-output-string o)))

(define param:slime-in-port (make-parameter #f))
(define param:slime-out-port (make-parameter #f))
(define param:environment (make-parameter #f))
(define param:current-id (make-parameter #f))
(define param:condition:msg (make-parameter "nil"))
(define param:active-continuations (make-parameter #f))
(define param:active-condition (make-parameter #f))
(define log-port (current-output-port))

(define (process-one-message)
  (let ((form (read-packet (param:slime-in-port))))
    (write-message (process-form form #f))))

(define (write-message sexp)
  (write-packet (scheme->message sexp) (param:slime-out-port)))

(define param:abort (make-parameter #f))

(define (swank/abort message)
  ((param:abort) message))

(define (process-form form env-name)
  (let ((key (car form)))
    (let ((h (find-handler key)))
      (if h
          (with-exception-handler
           (lambda (condition)
             ($handle-condition condition)
             (swank/abort ($error-description condition)))
           (lambda () (apply h (cdr form))))
          (swank/abort "No handler found.")))))

(define start-swank
  (case-lambda (() (start-swank 4005))
               ((port-or-file) (cond ((number? port-or-file)
                                      (server-loop port-or-file #f))
                                     ((string? port-or-file)
                                      (server-loop 4005 port-or-file))
                                     (else
                                      (error "Please call with port number or filename to which the automatically chosen port number will be written."))))))

(define (server-loop port-number port-file)
  (display "swank listening on " log-port) (display port-number log-port) (newline log-port) (flush-output-port log-port)
  ($open-tcp-server/accept port-number
                           (lambda (actual-port-number in out)
                             (when port-file
                               (when (file-exists? port-file)
                                 (delete-file port-file))
                               (with-output-to-file port-file
                                 (lambda ()
                                   (display actual-port-number))))
                             (parameterize ((param:slime-in-port in)
                                            (param:slime-out-port out))
                               (let loop ()
                                 (process-one-message)
                                 (loop))))))

(define (string-pad-left string size pad)
  (let* ((s-len (string-length string))
         (missing (max 0 (- size s-len))))
    (if (zero? missing)
        string
        (string-append (make-string missing pad) string))))

(define (write-packet str port)
  (let* ((bv (string->utf8 str))
         (len (bytevector-length bv))
         (hex-len (number->string len 16)))
    (when (> (string-length hex-len) 6)
      (error "Expression length exceeds 24 bits" hex-len))
    (display "to slime< " log-port) (write str log-port) (newline log-port)
    (write-string (string-pad-left hex-len 6 #\0) port)
    (write-string str port)
    (flush-output-port port)))

(define *handlers* ($make-hash-table))

(define (register-slime-handler! name function)
   ;; kawa returns non-equal for symbols with :, so we force the registered ones through READ as well
  (let ((name (read (open-input-string (symbol->string name)))))
    ($hash-table/put! *handlers* name function)))

(define (find-handler name)
  ($hash-table/get *handlers* name #f))

(define (interactive-eval sexp)
  (let-values ((vals (eval sexp (param:environment)))) ;; TODO environment
    vals))

(define *presentations* ($make-hash-table))

(define next-presentation-id (let ((count 0))
                               (lambda ()
                                 (set! count (+ count 1))
                                 count)))
(define (presentations?) #t)
(define (present value type)
  (let ((id (next-presentation-id)))
    ($hash-table/put! *presentations* id value)
    (write-message `(:presentation-start ,id ,type))
    (swank/write-string (write-to-string value) type)
    (write-message `(:presentation-end ,id ,type))
    (swank/write-string "\n" type)))

(define (write-to-string val)
  (let ((o (open-output-string)))
    (write val o)
    (get-output-string o)))

(define (unquote-string x)
  (if (and (list? x)
           (eq? (car x) 'quote))
      (cond ((string? (cadr x))
             (cadr x))
            ((eq? (cadr x) 'nil)
             "(user)")
            (else ;; TODO
             "(user"))
      x))
(define (swank/write-string val type)
  (write-message `(:write-string ,(if (string? val) val (write-to-string val)) ,@(if type (list type) '()))))

;;;; autodoc
(define (improper->proper-list lst)
  (if (pair? lst)
      (cons (car lst)
            (improper->proper-list (cdr lst)))
      (if (null? lst)
          '()
          (list '!rest lst))))

(define (find-expression-containing-swank-cursor-marker expr)
  (if (list? expr)
      (if (member '|swank::%cursor-marker%| expr)
          expr
          (find (lambda (ex)
                  (find-expression-containing-swank-cursor-marker ex))
                expr))
      #f))

(define (highlight-at-cursor signature expr)
  (let* ((form (find-expression-containing-swank-cursor-marker (cdr expr)))
         (index (list-index (lambda (el) (eq? el '|swank::%cursor-marker%|)) form)))
    (if index
        (wrap-item/index (improper->proper-list signature) (- index 1) '===> '<===)
        '())))

(define (find-string-before-swank-cursor-marker expr)
  (let ((ex (find-expression-containing-swank-cursor-marker expr)))
    (if ex
        (if (string? (car ex))
            (car ex)
            #f))))

(define (wrap-item/index lst index before-marker after-marker)
  (let loop ((i 0)
             (lst lst))
    (cond ((null? (cdr lst))
           (list before-marker (car lst) after-marker))
          ((member (car lst) '(!rest !optional))
           (cons (car lst) (loop i (cdr lst))))
          ((= i index)
           (append (list before-marker (car lst) after-marker) (cdr lst)))
          (else
           (cons (car lst)
                 (loop (+ i 1) (cdr lst)))))))

(define (with-output-to-string thunk)
  (let ((o (open-output-string)))
    (parameterize ((current-output-port o))
      (thunk))
    (get-output-string o)))

(define *throw-to-top-level* #f)
(define (sldb-loop)
  (if *throw-to-top-level*
      (begin
        (set! *throw-to-top-level* #f)
        (write-message `(:debug-return 1 1 nil)))
      (begin
        (process-one-message)
        (sldb-loop))))
(define *last-exception* #f)
(define (swank:get-last-exception)
  *last-exception*)
(define (invoke-sldb exception)
  (set! *last-exception* exception)
  (let ((msg ($condition-msg exception))
        (trace ($condition-trace exception))
        (links ($condition-links exception)))
    (write-message `(:debug 1 1 (,msg "FOO" nil)
                            (("ABORT" "abort"))
                            (,@(map (lambda (c t i)
                                      `(,i ,t))
                                    links
                                    trace
                                    (iota (length trace))))
                            ,(let ((id (param:current-id)))
                               (if id
                                   (list id)
                                   'nil))))
    (write-message `(:debug-activate 1 1))
    (parameterize ((param:condition:msg msg)
                   (param:active-continuations links)
                   (param:active-condition exception))
      (sldb-loop))))
