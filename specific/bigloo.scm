;;;; srfi-39
(define make-parameter
  (lambda (init . conv)
    (let ((converter
           (if (null? conv) (lambda (x) x) (car conv))))
      (let ((global-cell
             (cons #f (converter init))))
        (letrec ((parameter
                  (lambda new-val
                    (let ((cell (dynamic-lookup parameter global-cell)))
                      (cond ((null? new-val)
                             (cdr cell))
                            ((null? (cdr new-val))
                             (set-cdr! cell (converter (car new-val))))
                            (else ; this case is needed for parameterize
                             (converter (car new-val))))))))
          (set-car! global-cell parameter)
          parameter)))))

(define-syntax parameterize
  (syntax-rules ()
    ((parameterize ((expr1 expr2) ...) body ...)
     (dynamic-bind (list expr1 ...)
                   (list expr2 ...)
                   (lambda () body ...)))))

(define dynamic-bind
  (lambda (parameters values body)
    (let* ((old-local
            (dynamic-env-local-get))
           (new-cells
            (map (lambda (parameter value)
                   (cons parameter (parameter value #f)))
                 parameters
                 values))
           (new-local
            (append new-cells old-local)))
      (dynamic-wind
          (lambda () (dynamic-env-local-set! new-local))
          body
          (lambda () (dynamic-env-local-set! old-local))))))

(define dynamic-lookup
  (lambda (parameter global-cell)
    (or (assq parameter (dynamic-env-local-get))
        global-cell)))

(define dynamic-env-local '())

(define dynamic-env-local-get
  (lambda () dynamic-env-local))

(define dynamic-env-local-set!
  (lambda (new-env) (set! dynamic-env-local new-env)))
;;;; end srfi-39
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
(define make-hash-table make-hashtable)
(define hash-table-set! hashtable-put!)
(define (hash-table-ref/default table key default) (hashtable-get table key)) ; ignore default
(define hash-table-size hashtable-size)
(define hash-table-delete! hashtable-remove!)
(define hash-table-walk hashtable-for-each)
(define hash-table? hashtable?)

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
(define ($open-tcp-server port-number port-file handler)
  (let* ((n (or port-number (+ 10000 (random 50000))))
         (server (make-server-socket n)))
    (handler n server)))
(define ($tcp-server-accept server handler)
  (let ((socket (socket-accept server ':inbuf #t ':outbuf #t)))
    (handler (socket-input socket) (socket-output socket))))
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

(define (list-index predicate list)
  (let loop ((i 0)
             (l list))
    (if (null? l)
        #f
        (if (predicate (car l))
            i
            (loop (+ i 1) (cdr l))))))
(define ($condition-location condition)
  #f)
;;;; HACK!
(define (string-replace string replacement start end)
  (string-append (substring string 0 start) (substring string end (string-length string))))
