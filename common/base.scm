(define debug? #f)

(define (debug-log . rest)
  (when debug?
    (for-each (lambda (r)
                (if (string? r)
                    (display r log-port)
                    (write r log-port)))
              rest)
    (newline log-port)
    (flush-output-port log-port)))

(define (hash-table-clear! table)
  "Remove all entries from TABLE."
  (hash-table-walk table
                   (lambda (key value)
                     (hash-table-delete! table key))))

(define (read-all port)
  "Read forms from port until eof, return a list"
  (let loop ((result '())
             (form (read port)))
    (if (eof-object? form)
        (reverse result)
        (loop (cons form result)
              (read port)))))
(define (read-packet port)
  "Read 6 byte ascii hex length, then length bytes, all utf-8"
  (let* ((length (string->number (utf8->string (read-bytevector 6 port)) 16))
         (result (make-bytevector length)))
    (let loop ((result-index 0)
               (to-read length))
      (if (zero? to-read)
          (let ((res (utf8->string result)))
            (debug-log "from slime> " res)
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
          `(:return (:abort "no handler found") nil)))))
(define start-swank
  (case-lambda (() (start-swank 4005))
               ((port-or-file) (cond ((number? port-or-file)
                                      (server-loop port-or-file #f))
                                     ((string? port-or-file)
                                      (server-loop #f port-or-file))
                                     (else
                                      (error "Please call with port number or filename to which the automatically chosen port number will be written."))))))
(define (server-loop port-number port-file)
  ($open-tcp-server
   port-number port-file
   (lambda (actual-port-number data)
     (unless port-file
       (debug-log "swank listening on port " actual-port-number))
     (when port-file
       (when (file-exists? port-file)
         (delete-file port-file))
       (with-output-to-file port-file
         (lambda ()
           (display actual-port-number))))
     ($tcp-server-accept data
                         (lambda (in out)
                           (parameterize ((param:slime-in-port in)
                                          (param:slime-out-port out))
                             (let loop ()
                               (process-one-message)
                               (loop))))))))

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
    (debug-log "to slime< " str)
    (write-bytevector (string->utf8 (string-pad-left hex-len 6 #\0)) port)
    (write-bytevector bv port)
    (flush-output-port port)))

(define *handlers* (make-hash-table))

(define (register-slime-handler! name function)
  ;; kawa returns non-equal for symbols with :, so we force the registered ones through READ as well
  (let ((name (read (open-input-string (symbol->string name)))))
    (hash-table-set! *handlers* name function)))

(define (find-handler name)
  (hash-table-ref/default *handlers* name #f))

(define (interactive-eval sexp)
  (let-values ((vals (eval sexp (param:environment)))) ;; TODO environment
    vals))

(define *presentations* (make-hash-table))

(define (swank:lookup-presented-object id)
  "Retrieve the object corresponding to ID.
The secondary value indicates the absence of an entry."
  ;; it seems we often get (list 'quote id)
  (if (list? id)
      (swank:lookup-presented-object (cadr id))
      (let* ((nope (list #f))
             (val (hash-table-ref/default
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

;; based on https://en.wikibooks.org/wiki/Algorithm_Implementation/Miscellaneous/Base64
(define (base64-encode bv)
  (let* ((chars "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")
         (c (remainder (bytevector-length bv) 3))
         ;; padding at end
         (p (if (> c 0) (make-string (- 3 c) #\=) ""))
         (r (open-output-string))
         ;; add trailing zeros
         (s (bytevector-append bv (if (> c 0) (make-bytevector (- 3 c) 0) (bytevector)))))

    (do ((i 0 (+ i 3)))
        ((>= i (bytevector-length s)))
      (if (and (> i 0)
               (zero? (remainder (* 4 (/ i 3)) 76)))
          (display "\n" r)) ;; "\r\n" ?
      (let* ((n (+ (* (bytevector-u8-ref s i) 256 256)
                   (* (bytevector-u8-ref s (+ i 1)) 256)
                   (bytevector-u8-ref s (+ i 2))))
             (n0 (remainder (quotient n 262144) 64))
             (n1 (remainder (quotient n 4096) 64))
             (n2 (remainder (quotient n 64) 64))
             (n3 (remainder n 64)))
        (display (string-ref chars n0) r)
        (display (string-ref chars n1) r)
        (display (string-ref chars n2) r)
        (display (string-ref chars n3) r)))
    (let ((out (get-output-string r)))
      (string-append (substring out 0 (- (string-length out) (string-length p))) p))))
(define (encode-swank-image-data bytevector)
  (base64-encode bytevector))
(define (presentations?) #t)
(define (present value type)
  (or (swank-convert-to-image value type)
      (let ((id (next-presentation-id)))
        (hash-table-set! *presentations* id value)
        (write-message `(:presentation-start ,id ,type))
        (swank/write-string (write-to-string value) type)
        (write-message `(:presentation-end ,id ,type))
        (swank/write-string "\n" type)
        'nil)))

(define (swank-convert-to-image value type)
  (let loop ((cs *image-converters*))
    (if (null? cs)
        (if (swank-image? value)
            (if (swank-image-filename value)
                (write-message `(:write-image ((:type ,(swank-image-type value)
                                                      ,@(list ':file (swank-image-filename value))))
                                              ,(swank-image-string value)))
                (write-message `(:write-image ((:type ,(swank-image-type value)
                                                      ,@(list ':data (encode-swank-image-data (swank-image-data value)))))
                                              ,(swank-image-string value))))
            #f)
        (let ((c (car cs)))
          (if ((car c) value)
              (swank-convert-to-image ((cdr c) value) type)
              (loop (cdr cs)))))))

(define (write-to-string val)
  (let ((o (open-output-string)))
    (write val o)
    (get-output-string o)))

(define (read-from-string str)
  (read (open-input-string str)))

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
    (write-message `(:debug 1 1 (,msg ,(write-to-string exception) nil)
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
        (parts (make-hash-table))
        (actions (make-hash-table)))
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
  (let ((i (hash-table-size parts)))
    (hash-table-set! parts i object)
    i))

(define (assign-istate-action-index fun actions)
  (let ((i (hash-table-size actions)))
    (hash-table-set! actions i fun)
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
        ((hash-table? object)  (inspect-hash-table object))
        ((vector? object)      (inspect-vector object))
        ((bytevector? object)  (inspect-bytevector object))
        ((string? object)      (inspect-string object))
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
    (hash-table-walk object
                     (lambda (key value)
                       (set! elements (cons* `(value ,key) " => " `(value ,value) " " `(action "[remove entry]" ,(lambda () (hash-table-delete! object key))) '(newline) elements))))
    (stream-cons (inspector-line "Length" (hash-table-size object))
                 (stream-cons `(action "[clear hash table]" ,(lambda () (hash-table-clear! object)))
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

(define (object-documentation name object)
  (let ((doc ($binding-documentation object)))
    (string-append name "\n"
                   (if doc doc "")
                   "\n"
                   "It is bound to:\n" (write-to-string object))))

(define (describe-symbol name)
  (let* ((value (binding-value (string->symbol name))))
    (object-documentation name value)))

;;;; streams
(define-syntax swank/delay
  (syntax-rules ()
    ((swank/delay expr)
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

(define *traces* '())
(define-record-type trace-entry
  (%make-trace-entry id spec parent children arguments return-values)
  trace-entry?
  (id trace-entry-id)
  (spec trace-entry-spec)
  (parent trace-entry-parent)
  (children trace-entry-children set-trace-entry-children!)
  (arguments trace-entry-arguments)
  (return-values trace-entry-return-values set-trace-entry-return-values!))

(define (make-trace-entry id spec parent arguments)
  (let ((te (%make-trace-entry id spec parent '() arguments '())))
    (set! *traces* (cons te *traces*))
    te))

(define (trace-entry-add-child! trace-entry child)
  (set-trace-entry-children! trace-entry (cons child (trace-entry-children trace-entry)))
  trace-entry)

(define *param:current-trace* (make-parameter #f))

(define *next-trace-id* -1)
(define (next-trace-id)
  (set! *next-trace-id* (+ *next-trace-id* 1))
  *next-trace-id*)
(define (reset-trace-ids!)
  (set! *next-trace-id* -1))
(define (traces) *traces*)
(define (clear-traces!)
  (set! *traces* '()))
(define (tracing fun spec)
  (lambda args
    (let* ((ct (*param:current-trace*))
           (te (make-trace-entry (next-trace-id) spec ct args)))
      (when ct
        (trace-entry-add-child! ct te))
      (parameterize ((*param:current-trace* te))
        (let-values ((retvals (apply fun args)))
          (set-trace-entry-return-values! te retvals)
          (apply values retvals))))))

(define (add-index+to-string lst)
  (let loop ((l lst)
             (i 0)
             (result '()))
    (if (null? l)
        (reverse result)
        (loop (cdr l)
              (+ i 1)
              (cons (list i (write-to-string (car l))) result)))))

(define (describe-trace-for-emacs trace-entry)
  `(,(trace-entry-id trace-entry)
    ,(if (trace-entry-parent trace-entry)
         (trace-entry-id (trace-entry-parent trace-entry))
         'nil)
    ,(if (symbol? (trace-entry-spec trace-entry))
         (symbol->string (trace-entry-spec trace-entry))
         (trace-entry-spec trace-entry))
    ,(add-index+to-string (trace-entry-arguments trace-entry))
    ,(add-index+to-string (trace-entry-return-values trace-entry))))

(define (find-trace trace-index)
  (let loop ((t (traces)))
    (cond ((null? t)
           #f)
          ((= (trace-entry-id (car t)) trace-index)
           (car t))
          (else
           (loop (cdr t))))))
(define *pre-trace-values* '())
(define (add-pre-tracing-value! sym value)
  (set! *pre-trace-values* (cons (cons sym value) *pre-trace-values*)))
(define (pre-trace-values)
  *pre-trace-values*)
(define (%pre-tracing-value symbol)
  (let loop ((t *pre-trace-values*))
    (cond ((null? t)
           #f)
          ((eq? symbol (caar t))
           (cdar t))
          (else
           (loop (cdr t))))))
(define (untrace! sym)
  (interactive-eval `(set! ,sym (%pre-tracing-value ',sym)))
  (remove-pre-tracing-value! sym)
  (remove-active-trace! sym)
  (string-append (symbol->string sym) " is now untraced for trace dialog."))
(define (trace! sym)
  (let ((value (car (interactive-eval sym))))
    (if (procedure? value)
        (begin
          (add-pre-tracing-value! sym value)
          (interactive-eval `(set! ,sym (tracing ,sym ',sym)))
          (add-active-trace! sym)
          (string-append (symbol->string sym) " is now traced for trace dialog."))
        (string-append (symbol->string sym) " is not bound to a procedure, not tracing " (write-to-string value)))))
(define (remove-pre-tracing-value! sym)
  (set! *pre-trace-values* (let loop ((lst *pre-trace-values*))
                             (if (null? lst)
                                 lst
                                 (if (eq? (caar lst) sym)
                                     (cdr lst)
                                     (cons (car lst) (loop (cdr lst))))))))
(define (add-active-trace! sym)
  (set! *active-traces* (cons sym *active-traces*)))
(define (traced-symbol? sym)
  (memq sym *active-traces*))
(define (remove-active-trace! sym)
  (set! *active-traces* (let loop ((lst *active-traces*))
                          (if (null? lst)
                              lst
                              (if (eq? (car lst) sym)
                                  (cdr lst)
                                  (cons (car lst) (loop (cdr lst))))))))
(define (find-trace-arg trace index)
  (list-ref (trace-entry-arguments trace) index))
(define (find-trace-retval trace index)
  (list-ref (trace-entry-return-values trace) index))
(define-syntax trace-let
  (syntax-rules ()
    ((trace-let loop ((name value) ...) body0 body ...)
     (let loop ((name value) ...)
       (let* ((ct (*param:current-trace*))
              (te (make-trace-entry (next-trace-id) (string-append "let " (symbol->string 'loop)) ct (list name ...))))
         (when ct
           (trace-entry-add-child! ct te))
         (parameterize ((*param:current-trace* te))
           (let-values ((retvals (begin
                                   body0 body ...)))
             (set-trace-entry-return-values! te retvals)
             (apply values retvals))))))
    ((trace-let ((name value) ...) body0 body ...)
     (let ((name value) ...)
       (let* ((ct (*param:current-trace*))
              (te (make-trace-entry (next-trace-id) (string-append "let") ct (list name ...))))
         (when ct
           (trace-entry-add-child! ct te))
         (parameterize ((*param:current-trace* te))
           (let-values ((retvals (begin
                                   body0 body ...)))
             (set-trace-entry-return-values! te retvals)
             (apply values retvals))))))))
(define-syntax trace-let*
  (syntax-rules ()
    ((trace-let* ((name value) ...) body0 body ...)
     (let* ((name value) ...)
       (let* ((ct (*param:current-trace*))
              (te (make-trace-entry (next-trace-id) (string-append "let*") ct (list name ...))))
         (when ct
           (trace-entry-add-child! ct te))
         (parameterize ((*param:current-trace* te))
           (let-values ((retvals (begin
                                   body0 body ...)))
             (set-trace-entry-return-values! te retvals)
             (apply values retvals))))))))

(define-syntax trace-letrec
  (syntax-rules ()
    ((trace-letrec ((name value) ...) body0 body ...)
     (letrec ((name value) ...)
       (let* ((ct (*param:current-trace*))
              (te (make-trace-entry (next-trace-id) (string-append "letrec") ct (list name ...))))
         (when ct
           (trace-entry-add-child! ct te))
         (parameterize ((*param:current-trace* te))
           (let-values ((retvals (begin
                                   body0 body ...)))
             (set-trace-entry-return-values! te retvals)
             (apply values retvals))))))))

(define-syntax trace-letrec*
  (syntax-rules ()
    ((trace-letrec* ((name value) ...) body0 body ...)
     (letrec* ((name value) ...)
       (let* ((ct (*param:current-trace*))
              (te (make-trace-entry (next-trace-id) (string-append "letrec*") ct (list name ...))))
         (when ct
           (trace-entry-add-child! ct te))
         (parameterize ((*param:current-trace* te))
           (let-values ((retvals (begin
                                   body0 body ...)))
             (set-trace-entry-return-values! te retvals)
             (apply values retvals))))))))
(define-syntax trace-define
  (syntax-rules ()
    ((trace-define (name . args) body0 body ...)
     (define name (tracing (lambda args body0 body ...) 'name)))
    ((trace-define name (lambda args body0 body ...))
     (define name (tracing (lambda args body0 body ...) 'name)))))
;;;; convenience
(define (inspect-in-emacs object)
  (write-message `(:inspect ,(inspect-object object) nil nil))
  #t)

;;;; swank image support
(define-record-type swank-image
  (make-swank-image type filename data string)
  swank-image?
  (type swank-image-type)
  (filename swank-image-filename)
  (data swank-image-data)
  (string swank-image-string))

(define *image-converters* '())

(define (swank-present thing)
  (present thing 'nil))

(define (swank-register-image-converter predicate converter)
  (unless (procedure? predicate)
    (error "swank-register-image-converter: predicate must be a procedure" predicate))
  (unless (procedure? converter)
    (error "swank-register-image-converter: converter must be a procedure" converter))
  (set! *image-converters* (cons (cons predicate converter) *image-converters*)))

