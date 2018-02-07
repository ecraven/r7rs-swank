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
(define param:environment (make-parameter (interaction-environment)))
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
  ($open-tcp-server/accept port-number
                           (lambda (actual-port-number in out)
                             (display "swank listening on " log-port) (display port-number log-port) (newline log-port) (flush-output-port log-port)
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

(define (swank:lookup-presented-object id)
  "Retrieve the object corresponding to ID.
The secondary value indicates the absence of an entry."
  ;; it seems we often get (list 'quote id)
  (if (list? id)
      (swank:lookup-presented-object (cadr id))
      (let* ((nope (list #f))
             (val ($hash-table/get
                   *presentations*
                   ;; emacs seems to ask for a number with a decimal,
                   ;; aka inexact
                   (exact id) nope)))
        (values (if (eq? nope val) #f val)
                (if (eq? nope val) 'nil 't)))))

(define (swank:lookup-presented-object-or-lose id)
  "Get the result of the previous REPL evaluation with ID."
  (call-with-values (lambda () (swank:lookup-presented-object id))
    (lambda (object found?)
      (if (eq? found? 'nil)
          (swank/abort
           (string-append "Attempt to access unrecorded object (id "
                          (number->string id)")"))
          object))))

(define (replace-readtime-lookup-presented-object-or-lose string)
  "For presentations, emacs passes something that common lisp evals at read time. The resulting object is very different than what gerbil or gambits #. tries to do, so we do everything at run time"
  (let* ((pattern "#.(swank:lookup-presented-object-or-lose ")
	 (start (string-contains string pattern)))
    (if start
        (replace-readtime-lookup-presented-object-or-lose
         (string-replace string "" start (+ 2 start)))
        string)))

(define last-presentation-id 0)
(define (next-presentation-id)
  (set! last-presentation-id (+ last-presentation-id 1))
  last-presentation-id)

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

(define (display-to-string val)
  (let ((o (open-output-string)))
    (display val o)
    (get-output-string o)))

(define (unquote-number n)
  "If N is '17, return 17. Otherwise return N."
  (if (and (pair? n)
           (eq? (car n) 'quote)
           (pair? (cdr n))
           (number? (cadr n)))
      (cadr n)
      n))

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
  (define (f expr exit)
    (if (list? expr)
        (if (member '|swank::%cursor-marker%| expr)
            expr
            (let ((res (find (lambda (ex)
                               (f ex exit))
                             expr)))
              (if res (exit res) res)))
        #f))
  (call-with-current-continuation (lambda (exit) (f expr exit))))

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

(define inspector-state #f)

;; see specific for the record type definition of istate

(define (reset-inspector)
  (set! inspector-state #f))

(define (inspect-object object)
  (let ((previous inspector-state)
        (content (swank/inspect object))
        (parts ($make-hash-table))
        (actions ($make-hash-table)))
    (set! inspector-state (make-istate object parts actions #f previous content))
    (if previous (set-istate-next! previous inspector-state))
    (istate->elisp inspector-state)))

(define (ellipsize str)
  (if (> (string-length str) 100)
      (string-append (substring str 0 97) "...")
      str))

(define (istate->elisp inspector-state)
  `(:title ,(ellipsize (write-to-string (istate-object inspector-state)))
           :id ,(assign-istate-index (istate-object inspector-state) (istate-parts inspector-state))
           :content ,(prepare-inspector-range (istate-parts inspector-state)
                                              (istate-actions inspector-state)
                                              (istate-content inspector-state)
                                              0
                                              100)))

(define (assign-istate-index object parts)
  (let ((i ($hash-table/count parts)))
    ($hash-table/put! parts i object)
    i))

(define (assign-istate-action-index fun actions)
  (let ((i ($hash-table/count actions)))
    ($hash-table/put! actions i fun)
    i))

(define (prepare-inspector-range parts actions content from to)
  (let* ((cs (substream content from to))
         (ps (prepare-inspector-parts cs parts actions)))
    (list ps
          (if (< (length cs) (- to from))
              (+ from (length cs))
              (+ to 1000))
          from to)))

(define (prepare-inspector-parts ps parts actions)
  (define (line label value . others)
    `(,(string-append (display-to-string label) ": ")
      (:value ,(ellipsize (write-to-string value)) ,(assign-istate-index value parts))
      "\n"))
  (define (action label action)
    `((:action ,label ,(assign-istate-action-index action actions))))
  (define (value val)
    `((:value ,(ellipsize (write-to-string val)) ,(assign-istate-index val parts))))
  (apply append (map (lambda (p)
                       (cond ((string? p) (list p))
                             ((symbol? p) (list (symbol->string p)))
                             (else
                              (case (car p)
                                ((line) (apply line (cdr p)))
                                ((action) (apply action (cdr p)))
                                ((value) (apply value (cdr p)))
                                ((newline) (list "\n"))
                                (else (error "Invalid part:" p))))))
                     ps)))

(define (swank/inspect object)
  (cond ((boolean? object)     (inspect-boolean object))
        ((symbol? object)      (inspect-symbol object))
        ((char? object)        (inspect-char object))
        ((integer? object)     (inspect-integer object))
        ((pair? object)        (inspect-pair object))
        ((vector? object)      (inspect-vector object))
        ((bytevector? object)  (inspect-bytevector object))
        ((string? object)      (inspect-string object))
        (($hash-table? object) (inspect-hash-table object))
        (else
         (inspect-unknown object))))

(define (inspector-line label value . values)
  `(line ,label ,value ,@values))

(define (inspect-char object)
  (stream (inspector-line "Character code" (char->integer object))
          (inspector-line "Lower case" (char-downcase object))
          (inspector-line "Upper case" (char-upcase object))))

(define (inspect-integer object)
  (stream "Value: " (number->string object)
          " = #x" (number->string object 16)
          " = #o" (number->string object 8)
          " = #b" (number->string object 2)
          "\n"
          (inspector-line "Character" (integer->char object))))

(define (inspect-pair pair)
  (if (or (pair? (cdr pair))
          (null? (cdr pair)))
      (let loop ((l1 pair) (l2 pair) (i 0))
        (cond ((pair? l1)
               (stream-cons (inspector-line i (car l1))
                            (let ((l1 (cdr l1)))
                              (if (eq? l1 l2)
                                  (stream "{circular list detected}")
                                  (loop l1
                                        (if (odd? i) (cdr l2) l2)
                                        (+ i 1))))))
              ((null? l1) (stream))
              (else (stream (inspector-line "tail" (cdr l1))))))
      (stream (inspector-line "car" (car pair))
              (inspector-line "cdr" (cdr pair)))))

(define (inspect-boolean object)
  (stream (inspector-line "Boolean" object)))

(define (inspect-symbol object)
  (stream (inspector-line "Symbol" object)))

(define (inspect-unknown object)
  (let ((specific ($inspect-fallback object)))
    (if specific
        specific
        (stream (inspector-line "Object" object)))))

(define (inspect-vector object)
  (stream-cons (inspector-line "Length" (vector-length object))
               (let ((len (vector-length object)))
                 (let loop ((i 0))
                   (if (< i len)
                       (stream-cons (inspector-line i (vector-ref object i))
                                    (loop (+ i 1)))
                       (stream))))))

(define (inspect-hash-table object)
  (let ((elements '()))
    ($hash-table-walk object
                          (lambda (key value)
                            (set! elements (cons* `(value ,key) " => " `(value ,value) " " `(action "[remove entry]" ,(lambda () ($hash-table/remove! object key))) '(newline) elements))))
    (stream-cons (inspector-line "Length" ($hash-table/count object))
                 (stream-cons `(action "[clear hash table]" ,(lambda () ($hash-table/clear! object)))
                              (stream-cons '(newline)
                                           (apply stream elements))))))

(define (inspect-bytevector object)
  (stream-cons (inspector-line "Length" (bytevector-length object))
               (let ((len (bytevector-length object)))
                 (let loop ((i 0))
                   (if (< i len)
                       (stream-cons (inspector-line i (bytevector-u8-ref object i))
                                    (loop (+ i 1)))
                       (stream))))))

(define (inspect-string object)
  (stream-cons (inspector-line "Length" (string-length object))
               (let ((len (string-length object)))
                 (let loop ((i 0))
                   (if (< i len)
                       (stream-cons (inspector-line i (string-ref object i))
                                    (loop (+ i 1)))
                       (stream))))))

(define (binding-value symbol)
  (call/cc (lambda (k) (with-exception-handler (lambda (c) (k #f)) (lambda () (eval symbol (param:environment)))))))

(define (describe-symbol name)
  (let* ((value (binding-value (string->symbol name)))
         (doc ($binding-documentation value)))
    (string-append name "\n"
                   (if doc doc "")
                   "\n"
                   "It is bound to:\n" (write-to-string value))))
;;;; streams
(define-syntax swank/delay
  (syntax-rules ()
    ((delay expr)
     (lambda () expr))))

(define (swank/force promise) (promise))

(define (stream-cons a b)
  (cons a (swank/delay b)))

(define (stream-car p)
  (car p))

(define (stream-cdr p)
  (swank/force (cdr p)))

(define (stream . rest)
  (list->stream rest))

(define (list->stream list)
  (if (pair? list)
      (stream-cons (car list) (list->stream (cdr list)))
      '()))

(define (substream s from to)
  (let loop ((i 0)
             (l '())
             (s s))
    (cond ((or (if (null? to) #f (= i to)) (null? s)) (reverse l))
          ((< i from) (loop (+ i 1) l (stream-cdr s)))
          (else (loop (+ i 1) (cons (stream-car s) l) (stream-cdr s))))))

