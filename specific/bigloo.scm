(define ($scheme-name)
  "bigloo")
(define-syntax case-lambda
  (syntax-rules ()
    ((case-lambda (params body0 body1 ...) ...)
     (lambda args
       (cond ((= (length args) (length 'params))
              (apply (lambda params body0 body1 ...) args))
             ...)))))
(define-syntax let-values
  (syntax-rules ()
    ((let-values ((vals form)) body0 body ...)
     (receive vals form
       (begin body0 body ...)))))
(define $make-hash-table make-hashtable)
(define $hash-table/put! hashtable-put!)
(define ($hash-table/get table key default) (hashtable-get table key)) ; ignore default
(define $hash-table/count hashtable-size)
(define ($hash-table/clear! table)
  ($hash-table-walk table
                        (lambda (key value)
                          ($hash-table/remove! table key))))
(define $hash-table/remove! hashtable-remove!)
(define $hash-table-walk hashtable-for-each)
(define $hash-table? hashtable?)
(define ($error-description error)
  error)
(define ($environment name)
  (interaction-environment))
(define $pretty-print pp)
(define string->utf8 (lambda (x) x))
(define utf8->string (lambda (x) x))
(define bytevector-length string-length)
(define (bytevector-u8-ref bv n) (char->integer (string-ref bv n)))
(define (bytevector? object) #f)
(define (error . args) args) ;;TODO
(define ($open-tcp-server/accept port-number handler)
  (display "opening tcp server socket") (newline) (flush-output-port)
  (let* ((server (make-server-socket port-number))
         (socket (socket-accept server ':inbuf #t ':outbuf #t)))
    (handler port-number (socket-input socket) (socket-output socket))))
(define ($output-to-repl thunk)
  ;; basic implementation, print all output at the end, this should
  ;; be replaced with a custom output port
  (let ((o (open-output-string)))
    (parameterize ((current-output-port o))
      (let-values ((x (thunk)))
        (swank/write-string (get-output-string o) #f)
        (apply values x)))))
(define fop flush-output-port)
(define (flush-output-port . args)
  (if (null? args)
      (fop (current-output-port))
      (fop (car args))))
(define (read-bytevector size port)
  (read-chars size port))
(define ($all-package-names)
  '())
(define (make-bytevector size)
  (make-string size))
(define exact inexact->exact)
(define (make-parameter x)
  (let ((val x))
    (lambda args
      (if (null? args) val
          (set! val (car args))))))

(define dynamic-bindings '())

(define-syntax parameterize
  (syntax-rules ()
    ((parameterize () body0 body ...)
     (begin body0 body ...))
    ((parameterize ((binding form) rest ...) body0 body ...)
     (dynamic-wind (lambda ()
                     (set! dynamic-bindings (cons (binding) dynamic-bindings))
                     (binding form))
         (lambda ()
           (parameterize (rest ...) body0 body ...))
         (lambda ()
           (binding (car dynamic-bindings))
           (set! dynamic-bindings (cdr dynamic-bindings)))))))

(define (bytevector-copy! to at from)
  (let ((len (string-length from)))
    (let loop ((i 0))
      (if (= i len)
          'done
          (begin
            (string-set! to (+ at i) (string-ref from i))
            (loop (+ i 1)))))))
(define ($frame-var-value frame index)
  #f)
(define ($binding-documentation name)
  #f)
(define ($inspect-fallback object)
  #f)
(define (symbol->string s)
  (cond ((keyword? s)
         (keyword->string s))
        (else
         (symbol->string s))))
(define ($condition-msg condition)
  "UNKNOWN")

(define ($condition-links condition)
  '())

(define ($handle-condition exception)
  #f)

(define ($condition-trace condition)
  '())

(define ($completions prefix env-name)
  '())

(define ($set-package env-name)
  (list "(user)" "(user)"))

(define ($function-parameters-and-documentation name)
  (cons #f #f))

(define ($frame-locals-and-catch-tags nr)
  '())

(define ($apropos name)
  '())

(define-record-type <istate>
  (make-istate object parts actions next previous content)
  istate?
  (object istate-object)
  (parts istate-parts)
  (actions istate-actions)
  (next istate-next set-istate-next!)
  (previous istate-previous)
  (content istate-content))
