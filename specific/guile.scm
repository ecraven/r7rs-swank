;;(use-modules (srfi srfi-11))
(define ($scheme-name)
  "guile")

(define ($macroexpand-1 form)
  (macroexpand form))

(define ($macroexpand-all form)
  (macroexpand form))

(define (make-server-socket port)
  (let ((sock (socket PF_INET SOCK_STREAM 0)))
    (setsockopt sock SOL_SOCKET SO_REUSEADDR 1)
    (bind sock AF_INET INADDR_LOOPBACK port)
    sock))

(define (accept-new-client server-socket)
  (match (accept server-socket)
         ((client-socket . _) client-socket)))

(define ($open-tcp-server port-number port-file handler)
  (let* ((n (or port-number (+ 10000 (random 50000))))
         (server-socket (make-server-socket n))
         (dummy (listen server-socket 1)))
    (handler n server-socket)))

(define ($tcp-server-accept socket handler)
  (let ((port (accept-new-client socket)))
    (handler port port)))

(define ($all-package-names)
  (all-modules))

(define ($error-description error)
  (let ((o (open-output-string)))
    (write error o)
    (get-output-string o)))

(define ($output-to-repl thunk)
  ;; basic implementation, print all output at the end, this should
  ;; be replaced with a custom output port
  (let ((o (open-output-string)))
    (with-output-to-port o (lambda ()
                             (with-error-to-port o (lambda ()
                                                     (let-values ((x (thunk)))
                                                       (swank/write-string (get-output-string o) #f)
                                                       (apply values x))))))))

(define (env-name->environment env-name)
 ;; TODO
  (interaction-environment))
(define (environment-bindings env)
  (define (env-binds env)
    (let ((results '()))
      (module-for-each (lambda (k v) (set! results (cons k results))) env)
      results))
  (append (env-binds env)
          (append-map env-binds (module-uses env))))
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
      ""
      (fold (lambda (s1 s2) (substring s2 0 (string-match-forward s1 s2))) (car strings) (cdr strings))))
(define ($completions prefix env-name)
  (let ((matches (filter (lambda (el) (string-prefix? prefix el))
                         (map symbol->string (environment-bindings (env-name->environment env-name))))))
    matches))
;;;; taken from geiser
(define (submodules mod)
  (hash-map->list (lambda (k v) v) (module-submodules mod)))

(define (root-modules)
  (submodules (resolve-module '() #f)))

(define (all-modules)
  (define (maybe-name m)
    (and (module-kind m) (format #f "~A" (module-name m))))
  (let* ((guile (resolve-module '(guile)))
         (roots (remove (lambda (m) (eq? m guile)) (root-modules)))
         (children (append-map all-child-modules roots)))
    (cons "(user)" (filter-map maybe-name children))))

(define (all-child-modules mod . seen)
  (let ((cs (filter (lambda (m) (not (member m (if (null? seen) seen (car seen))))) (submodules mod))))
    (fold (lambda (m all) (append (all-child-modules m all) all))
          (list mod)
          cs)))

;; we keep the frames returned by $condition-trace, so that frame-locals-and-catch-tags
;; can access the same frames with the same ordering
(define *stored-frames* #())

(define ($condition-trace condition)
  ;; we ignore condition for now, since I didn't find a way of getting
  ;; stack information out of it.
  (let* ((stack (make-stack #t))
         ;; Inspired by from guile/module/system/repl/error-handling.scm
         ;; Fetch most recent start-stack tag.
         (tag (and (pair? (fluid-ref %stacks))
                   (cdr (fluid-ref %stacks))))
         (frames (narrow-stack->vector
                  stack
                  ;; Jump over the first frames containing:
                  ;;    make-stack, $condition-trace and invoke-sldb
                  3
                  tag)))
    (set! *stored-frames* frames)
    (vector->list
     (vector-map (lambda (fr)
                   (format #f "~a"
                           (frame-call-representation fr)))
                 frames))))


(define ($frame-locals-and-catch-tags nr)
  (let* ((frames *stored-frames*)
         (bindings (frame-bindings (vector-ref frames nr)))
         (entries (map (lambda (i b)
                         `(:name ,(format #f "~a" (binding-name b))
                           :id ,i
                           :value ,(format #f "~a" (binding-ref b))))
                       (iota (length bindings))
                       bindings)))
    (list entries (list 'nil))))

(define ($frame-var-value frame-index binding-index)
  (let* ((fr (vector-ref *stored-frames* frame-index))
         (bindings (frame-bindings fr)))
    (binding-ref (list-ref bindings binding-index))))

(define ($condition-msg condition)
  (apply format
         `(#f
           ,(exception-message condition)
           ,@(or (exception-irritants condition) ; may return #f
                 '()))))

(define ($condition-links condition)
  (vector->list
   (vector-map (lambda (fr)
                 (let ((src (frame-source fr)))
                   (if src
                       (let ((file (source:file src))
                             (line (source:line src))
                             (col (source:column src)))
                         (list file #f line col))
                       #f)))
               *stored-frames*)))

(define ($handle-condition exception)
  (invoke-sldb exception))

(define ($function-parameters-and-documentation name)
  (cons #f #f))

(define (get-valid-module-name name)
  (with-input-from-string name read))

(define ($set-package name)
  (let ((mod (resolve-module (get-valid-module-name name) #t #:ensure #f)))
    (when mod
      (set-current-module mod)))
  (list name name))

(define ($environment name)
  (interaction-environment))

(define $pretty-print pretty-print)

(define-record-type <istate>
  (make-istate object parts actions next previous content)
  istate?
  (object istate-object)
  (parts istate-parts)
  (actions istate-actions)
  (next istate-next set-istate-next!)
  (previous istate-previous)
  (content istate-content))

(define (inspect-class obj)
  (apply stream
         `(,(inspector-line "Name" (class-name obj))

           "Super classes: "
           ,@(build-multi-value (class-direct-supers obj))
           (newline)

           "Direct Slots: "
           ,@(build-multi-value (class-direct-slots obj))
           (newline)

           "Sub classes: "
           ,@(build-multi-value
              (let ((subs (class-direct-subclasses obj)))
                (if (null? subs)
                    (list 'nil)
                    subs)))
           (newline)

           "Precedence List: "
           ,@(build-multi-value
              (let ((subs (class-precedence-list obj)))
                (if (null? subs)
                    (list 'nil)
                    subs)))
           (newline)
           (newline)
           "All Slots:"
           (newline)
           ,@(all-slots-for-inspector obj)
           )))

(define (inspect-instance obj)
  (let ((cls (class-of obj)))
    (apply stream
           `(,(inspector-line "Class" cls)
             "--------------------"
             (newline)
             ,@(fold (lambda (s acc)
                       (let ((sname (slot-definition-name s)))
                         (if (slot-bound? obj sname)
                             (cons (inspector-line sname
                                                   (slot-ref obj sname))
                                   acc)
                             acc)))
                     '()
                     (class-slots cls))))))

(define (inspect-record r)
  (let* ((rtd (record-type-descriptor r))
         (fields (record-type-fields rtd)))
    (apply stream
           `(,(format #f "The object is a Record of type ~a."
                      (record-type-name rtd))
             (newline)
             ,@(map (lambda (f)
                      (let ((proc (record-accessor rtd f)))
                        (inspector-line (format #f "~a" f)
                                        (proc r))))
                    fields)))))

(define ($inspect-fallback obj)
  (cond ((is-a? obj <class>)
         (inspect-class obj))
        ((instance? obj)
         (inspect-instance obj))
        ((record? obj)
         (inspect-record obj))
        (else #f)))

(define (all-slots-for-inspector obj)
  (map (lambda (s)
         (inspector-line (slot-definition-name s)
                         (slot-definition-init-value s)))
       (class-slots obj)))

(define ($apropos name)
  ;; based on guile's ice-9/session.scm
  (let* ((pattern name)
         (uses (module-uses (current-module)))
         (modules (cons (current-module)
                        (if (and (not (null? uses))
                                 (eq? (module-name (car uses))
                                      'duplicates))
                            (cdr uses)
                            uses))))
    (let ((results '()))
      (for-each
       (lambda (module)
         (let* ((name (module-name module))
                (obarray (module-obarray module)))
           ;; XXX - should use hash-fold here
           (hash-for-each
            (lambda (symbol variable)
              (if (string-contains-ci (symbol->string symbol) pattern)
                  (let* ((binding
                          (if (variable-bound? variable)
                              (variable-ref variable)
                              #f))
                         (documentation ($binding-documentation binding))
                         (type (if (procedure? binding) ':function ':variable)))
                    (set! results (cons (list symbol type documentation) results)))))
            obarray)))
       modules)
      results)))

(define ($binding-documentation value)
  (cond ((procedure? value) (procedure-documentation value))
        (else "No documentation.")))

(define ($condition-location condition)
  ;; a hack. Since in Guile we can't get a backtrace out of a condition
  ;; object, $condition-links returns directly a list of locations. So
  ;; we just pass it further. See swank:frame-source-location
  condition)
