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
(define ($error-description error)
  error)
(define string->utf8 (lambda (x) x))
(define utf8->string (lambda (x) x))
(define bytevector-length string-length)
(define (error . args) args) ;;TODO
(define ($open-tcp-server/accept port-number handler)
  (display "opening tcp server socket") (newline) (flush-output-port)
  (let* ((server (make-server-socket port-number))
         (socket (socket-accept server ':inbuf #t ':outbuf #t)))
    (handler port-number (socket-input socket) (socket-output socket))))
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
(define sts symbol->string)
(define (symbol->string s)
  (cond ((keyword? s)
         (keyword->string s))
        (else
         (symbol->string s))))
