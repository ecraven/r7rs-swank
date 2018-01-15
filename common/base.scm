(define (read-packet port)
  "read 6 byte ascii hex length, then length bytes, all utf-8"
  (let* ((length (string->number (utf8->string (read-bytevector 6 port)) 16))
         (result (make-bytevector length)))
    (let loop ((result-index 0)
               (to-read length))
      (if (zero? to-read)
          (let ((res (utf8->string result)))
            (display "from slime> ") (write res) (newline)
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

(define (process-one-message in out)
  (let ((form (read-packet in)))
    (parameterize ((param:slime-in-port in)
                   (param:slime-out-port out))
      (write-message (process-form form #f)))))

(define (write-message sexp)
  (write-packet (scheme->message sexp) (param:slime-out-port)))

(define param:abort (make-parameter #f))

(define (swank/abort message)
  ((param:abort) message))

(define (process-form form env-name)
  (let ((key (car form)))
    (let ((h (find-handler key)))
      (if h
          (apply h (cdr form))
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
  (display "swank listening on ") (display port-number) (newline) (flush-output-port)
  ($open-tcp-server/accept port-number
                           (lambda (actual-port-number in out)
                             (when port-file
                               (when (file-exists? port-file)
                                 (delete-file port-file))
                               (with-output-to-file port-file
                                 (lambda ()
                                   (display actual-port-number))))
                             (let loop ()
                               (process-one-message in out)
                               (loop)))))

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
    (display "to slime< ") (write str) (newline)
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
  (with-exception-handler
   (lambda (condition)
     (swank/abort ($error-description condition)))
   (lambda ()
     (let-values ((vals (eval sexp (param:environment)))) ;; TODO environment
       vals))))

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
    (swank/write-string value type)
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
