(define ($scheme-name)
  "chez-scheme")

(define ($condition-msg condition)
  (let ((o (open-output-string)))
    (display-condition condition o)
    (newline o)
    (get-output-string o)))

(define ($condition-trace condition)
  (define (display current)
    (let* ((code (current 'code))
           (source (if code (code 'source) #f))
           (name (or (and (code 'name)
                          (string->symbol (code 'name)))
                     (current 'value)))
           (location (let-values ((sp (current 'source-path)))
                       sp))
           (v (if source (source 'value) #f)))
      (let ((s (write-to-string (if v v (current 'value)))))
        (if v
            (string-append (write-to-string name) " (" (write-to-string location) ")" " - " s)
            s))))
  (map display ($condition-links condition)))
(define (load path env)
  (%load path))

(define (error . rest)
  (apply %error 'error rest))

(define ($handle-condition exception)
  (when (continuation-condition? exception)
    (invoke-sldb exception)))

(define ($condition-links condition)
  (let ((k (inspect/object (condition-continuation condition))))
    (let loop ((result '())
               (current k)
               (i 0))
      (if (> i 10)
          (reverse result)
          (if (eq? 'continuation (current 'type))
              (loop (cons current result)
                    (current 'link)
                    (+ i 1))
              (reverse result))))))

(define ($binding-documentation p)
  ;; same as (inspect object), then hitting c
  
  (let ((o (inspect/object p)))
    (if (eq? (o 'type) 'procedure)
        (let ((s ((o 'code) 'source)))
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
                       (write-to-string (cons 'case-lambda (map car (cdr form)))))
                      (else
                       #f)))
              #f))
        #f)))

;; TODO: this needs to get passed the actual parameters
;; to decide which case-lambda form to show
(define (matching-case-lambda-form forms)
  (car forms))
(define (procedure-parameter-list p)
  ;; same as (inspect object), then hitting c
  (let ((o (inspect/object p)))
    (if (eq? 'procedure (o 'type))
        (let ((s ((o 'code) 'source)))
          (if s
              (let ((form (s 'value)))
                (cond ((and (list? form)
                            (> (length form) 2)
                            (eq? (car form) 'lambda))
                       (cadr form))
                      ((and (list? form)
                            (> (length form) 2)
                            (eq? (car form) 'case-lambda))
                       (let ((forms (map car (cdr form))))
                         ;; TODO: this needs to get passed the actual parameters
                         ;; to decide which case-lambda form to show
                         (matching-case-lambda-form forms)))
                      (else
                       #f)))
              #f))
        #f)))

(define (built-in-signature value)
  (let ((q (get-hash-table *built-in-signatures* value #f)))
    (if q (car q) #f)))
(define (built-in-documentation value)
  (let ((q (get-hash-table *built-in-signatures* value #f)))
    (if q (cdr q) #f)))

(define *built-in-signatures* (make-hash-table))

(define (define-built-in-doc value signature documentation)
  (put-hash-table! *built-in-signatures* value (cons signature documentation)))

(define ($function-parameters-and-documentation name)
  (when (zero? ($hash-table/count *built-in-signatures*))
    (init-built-in-signatures))
  (let* ((binding (call/cc (lambda (k) (with-exception-handler (lambda (c) (k #f)) (lambda () (eval (string->symbol name) (param:environment)))))))
         (param-list (if binding (procedure-parameter-list binding) #f))
         (signature (if param-list
                        (cons (string->symbol name) param-list)
                        (built-in-signature binding)))
         (doc (or (if binding ($binding-documentation binding) #f)
                  (built-in-documentation binding))))
    (cons signature doc)))

;; (define (%write-to-string val)
;;   (let ((o (open-output-string)))
;;     (write val o)
;;     (get-output-string o)))

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
(define $hash-table/count hashtable-size)
(define $hash-table? hash-table?)
(define $hash-table-walk hash-table-for-each)
(define $hash-table/clear! hashtable-clear!)
(define $hash-table/remove! hashtable-delete!)

(define (read-bytevector size port)
  (get-bytevector-n port size))

(define (write-string str port)
  (put-bytevector port (string->utf8 str)))

(define ($error-description error)
  (let ((o (open-output-string)))
    (display-condition error o)
    (get-output-string o)))

(define ($all-package-names)
  (map (lambda (package) (write-to-string package))
       (cons '(user) (library-list))))

(define (bytevector-copy! to to-index from)
  (%bytevector-copy! from 0 to to-index (bytevector-length from)))

(define repl-port (make-custom-textual-input/output-port "swank repl port"
            ;; read
            (lambda (string start count)
              0
              )
            ;; write
            (lambda (string start count)
              (swank/write-string (substring string start (+ start count)) #f)
              count)
            ;; getpos
            #f
            ;; setpos
            #f
            ;; close
            (lambda ()
              (display "Cannot close repl port\n" log-port))))

(define ($output-to-repl thunk)
  ;; basic implementation, print all output at the end, this should
  ;; be replaced with a custom output port
  (parameterize ((current-output-port repl-port)
                 (current-error-port repl-port)
                 (trace-output-port repl-port)
                 (console-output-port repl-port)
                 (console-error-port repl-port))
    (let-values ((v (thunk)))
      (flush-output-port repl-port)
      (apply values v))))

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
                               (map write-to-string (environment-symbols (pstring->environment env-name))))))) ;;  (interaction-environment)
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

(define (continuation-variables ins)
  (let* ((len (ins 'length)))
    (map (lambda (i)
           `(:name ,(write-to-string (or ((ins 'ref i) 'name) '|UNKNOWN|))
                   :id 0
                   :value ,(write-to-string (((ins 'ref i) 'ref) 'value))))
         (iota len))))

(define ($frame-locals-and-catch-tags nr)
  `(,(continuation-variables (list-ref (param:active-continuations) nr))
    nil))

(define ($frame-var-value frame index)
  ((((list-ref (param:active-continuations) frame) 'ref index) 'ref) 'value))


(define take list-head)
(define drop list-tail)
(define-structure (istate object parts actions next previous content))

(define $pretty-print pretty-print)

(define ($inspect-fallback object)
  (cond ((record? object)
         (let* ((type (record-rtd object))
                (field-names (vector->list (record-type-field-names type)))
                (len (length field-names))
                (field-values (map (lambda (i) ((record-accessor type i) object)) (iota len))))
           (let loop ((n field-names)
                      (v field-values))
             (if (null? n)
                 (stream)
                 (stream-cons (inspector-line (car n) (car v))
                              (loop (cdr n) (cdr v)))))))
        (else #f)))

;;;; srfi-13 parts
(define (string-replace s1 s2 start1 end1) ;; . start2+end2
  (let* ((s1-len (string-length s1))
         (s2-len (string-length s2))
         (total-len (+ (- s1-len (- end1 start1)) s2-len))
         (result (make-string total-len)))
    (string-copy! s1 0 result 0 start1)
    (string-copy! s2 0 result start1 s2-len)
    (string-copy! s1 end1 result (+ start1 s2-len) (- s1-len end1))
    result))

(define string-contains
  (case-lambda
   ((text pattern)
    (string-contains text pattern 0 (string-length text)))
   ((text pattern start)
    (string-contains text pattern start (string-length text)))
   ((text pattern start end)
    (let* ((d-len (string-length pattern))
           (s-len (string-length text))
           (end (- end d-len)))
      (let loop ((i start))
        (if (> i end)
            #f
            (if (string-prefix? pattern (substring text i s-len))
                i
                (loop (+ i 1)))))))))

(define ($apropos name)
  "Return a list of (name type documentation) for all matches for NAME."
  (let ((lst (apropos-list name)))
    (map (lambda (sym)
           (let* ((binding (call/cc (lambda (k) (with-exception-handler (lambda (c) (k #f)) (lambda () (eval sym (param:environment)))))))
                  (doc ($binding-documentation binding))
                  (type (cond ((procedure? binding) ':function)
                              (else ':variable))))
             (list sym type doc)))
         (filter symbol? lst)) ;; ignore other packages for now, only use direct symbols
    ))
(define (init-built-in-signatures)
  (define-built-in-doc + '(+ . n)
    "")
  (define-built-in-doc - '(- n0 . n)
    "")
  (define-built-in-doc foreign-alloc '(foreign-alloc n)
    "Allocate N bytes.")
  ;; syntax...
  ;; (define-built-in-doc ftype-set! '(ftype-set! ftype-name (a ...) fptr-expr [index] val-expr)
  ;;   "")
  ;; (define-built-in-doc ftype-ref '(ftype-ref ftype-name (a ...) fptr-expr [index])
  ;;   "")
  (define-built-in-doc make-date '(make-date nsec sec min hour day mon year offset)
    "")
  )
