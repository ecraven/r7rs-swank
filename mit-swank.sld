(define (load-relative filename)
  (with-working-directory-pathname
   (directory-namestring (current-load-pathname))
   (lambda () (load filename))))

(define-syntax case-lambda
  (syntax-rules ()
    ((case-lambda (params body0 body1 ...) ...)
     (lambda args
       (cond ((= (length args) (length 'params))
              (apply (lambda params body0 body1 ...) args))
             ...)))))

(define (read-bytevector size port)
  (let ((str (make-string size)))
    (read-string! str port)
    str))

(define utf8->string identity-procedure)
(define string->utf8 identity-procedure)
(define (make-bytevector size) (make-string size))
(define bytevector-length string-length)
(define (bytevector-copy! to at from #!optional start end)
  ;; from chibi
  (let* ((start (if (default-object? start) 0 start))
         (end (if (default-object? end) (bytevector-length from) end))
         (limit (min end (+ start (- (bytevector-length to) at)))))
    (if (<= at start)
        (do ((i at (+ i 1)) (j start (+ j 1)))
            ((>= j limit))
          (bytevector-u8-set! to i (bytevector-u8-ref from j)))
        (do ((i (+ at (- end start 1)) (- i 1)) (j (- limit 1) (- j 1)))
            ((< j start))
          (bytevector-u8-set! to i (bytevector-u8-ref from j))))))
(define (bytevector-u8-ref bytevector k)
  (vector-8b-ref bytevector k))
(define (bytevector-u8-set! bytevector k byte)
  (vector-8b-set! bytevector k byte))
(define flush-output-port flush-output)
(define-syntax let-values
  (syntax-rules ()
    ((let-values ((vals form)) body0 body ...)
     (receive vals (let ((r form))
                     ;; TODO this is a total hack, which doesn't even work well...
                     (if (and (compiled-closure? r)
                              (let ((di (compiled-code-block/debugging-info (compiled-entry/block (compiled-closure->entry r)))))
                                (if (and di (vector? di) (eq? (vector-ref di 0) 'debugging-info-wrapper))
                                    (string=? "runtime/global.inf" (vector-ref di 3))
                                    #t ;; hm... anything simpler to do?
                                    )))
                         r
                         (apply values (list r))))
       (begin body0 body ...)))))
(define (interaction-environment) (->environment '(user)))
(define exit %exit)
(load-relative "specific/mit.scm")
(load-relative "common/base.scm")
(load-relative "common/handlers.scm")
