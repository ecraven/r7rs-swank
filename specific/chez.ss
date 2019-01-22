(define ($scheme-name)
  "chez-scheme")

(define ($macroexpand-1 form)
  (expand form))

(define ($macroexpand-all form)
  (expand form))

(define ($condition-msg condition)
  (let ((o (open-output-string)))
    (display-condition condition o)
    (newline o)
    (get-output-string o)))

(define ($condition-location condition)
  "Return (PATH POSITION LINE COLUMN) for CONDITION."
  (call-with-values (lambda () (condition 'source-path))
    (let ()
      (define (p path)
        (if (char=? #\/ (string-ref path 0))
            path
            (string-append (current-directory) "/" path)))
      (case-lambda
       (() #f)
       ((path pos) (list (p path) pos #f #f))
       ((path line column) (list (p path) #f line column))))))

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
  (let ((q (hash-table-ref/default *built-in-signatures* value #f)))
    (if q (car q) #f)))
(define (built-in-documentation value)
  (let ((q (hash-table-ref/default *built-in-signatures* value #f)))
    (if q (cdr q) #f)))

(define *built-in-signatures* (make-hash-table))

(define (define-built-in-doc value signature documentation)
  (hash-table-set! *built-in-signatures* value (cons signature documentation)))

(define ($function-parameters-and-documentation name)
  (when (zero? (hash-table-size *built-in-signatures*))
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

(define ($open-tcp-server port-number port-file handler)
  (print-record #f)
  (let ((n (or port-number (+ 10000 (random 50000)))))
    (with-tcp-server-socket
     n 1
     (lambda (socket)
       (handler n socket)))))
(define ($tcp-server-accept socket handler)
  (let ((p (tcp-server-connection-accept socket)))
    (handler p p)))

(define (read-bytevector size port)
  (get-bytevector-n port size))

(define (write-bytevector bv port)
  (put-bytevector port bv))

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
  ;; Equivalence
  (define-built-in-doc eqv? '(eqv? obj1 obj2)
    "")
  (define-built-in-doc eq? '(eq? obj1 obj2)
    "")
  (define-built-in-doc equal? '(equal? obj1 obj2)
    "")

  ;; Procedure
  (define-built-in-doc procedure? '(procedure? obj)
    "Returns #t if obj is a procedure, otherwise returns #f.")

  ;; Arithmetic
  ;; Numerical type predicates
  (define-built-in-doc number? '(number? obj)
    "Returns #t if obj is a number, and #f otherwise.")
  (define-built-in-doc complex? '(complex? obj)
    "Returns #t if obj is a complex number, and #f otherwise.")
  (define-built-in-doc real? '(real? obj)
    "Returns #t if obj is a real number, and #f otherwise.")
  (define-built-in-doc rational? '(rational? obj)
    "Returns #t if obj is a rational number, and #f otherwise.")
  (define-built-in-doc integer? '(integer? obj)
    "Returns #t if obj is an integer number, and #f otherwise.")
  (define-built-in-doc real-valued? '(real-valued? obj)
    (string-append
     "Returns #t if the object is a number object and is equal "
     "in the sense of = to some real number object, or if the "
     "object is a NaN, or a complex number object whose real "
     "part is a NaN and whose imaginary part is zero in the "
     "sense of zero?."))
  (define-built-in-doc rational-valued? '(rational-valued? obj)
    (string-append
     "Returns #t if the object is a number object and is equal "
     "in the sense of = to some object of the rational type, and "
     "otherwise return #f."))
  (define-built-in-doc integer-valued? '(integer-valued? obj)
    (string-append
     "Returns #t if the object is a number object and is equal "
     "in the sense of = to some object of the integer type, and "
     "otherwise return #f."))
  (define-built-in-doc exact? '(exact? z)
    "Returns #t if z is exact, and #f otherwise.")
  (define-built-in-doc inexact? '(inexact? z)
    "Returns #t if z is inexact, and #f otherwise.")
  (define-built-in-doc inexact '(inexact z)
    (string-append
     "Returns an inexact representation of z. If inexact number "
     "objects of the appropriate type have bounded precision, then "
     "the value returned is an inexact number object that is nearest "
     "to the argument. The procedure is idempotent."))
  (define-built-in-doc exact '(exact z)
    (string-append
     "Returns an exact representation of z. The value returned is "
     "the exact number object that is numerically closest to the "
     "argument; in most cases, the result of this procedure should "
     "be numerically equal to its argument. The procedure is "
     "idempotent."))
  ;; Arithmetic operations
  (define-built-in-doc = '(= z1 z2 z3 ...)
    "Returns #t if the arguments are equal, and #f otherwise.")
  (define-built-in-doc < '(< x1 x2 x3 ...)
    (string-append
     "Returns #t if the arguments are monotonically increasing, "
     "and #f otherwise."))
  (define-built-in-doc > '(> x1 x2 x3 ...)
    (string-append
     "Returns #t if the arguments are monotonically decreasing, "
     "and #f otherwise."))
  (define-built-in-doc <= '(<= x1 x2 x3 ...)
    (string-append
     "Returns #t if the arguments are monotonically nondecreasing, "
     "and #f otherwise."))
  (define-built-in-doc >= '(>= x1 x2 x3 ...)
    (string-append
     "Returns #t if the arguments are monotonically nonincreasing, "
     "and #f otherwise."))
  (define-built-in-doc zero? '(zero? x)
    "Returns #t if the number object is = to zero, and #f otherwise.")
  (define-built-in-doc positive? '(postitive? x)
    (string-append
     "Returns #t if the number object is greater than zero, and #f "
     "otherwise."))
  (define-built-in-doc negative? '(negative? x)
    (string-append
     "Returns #t if the number object is less than zero, and #f "
     "otherwise."))
  (define-built-in-doc odd? '(odd? x)
    "Returns #t if the number object is odd, and #f otherwise.")
  (define-built-in-doc even? '(even? x)
    "Returns #t if the number object is even, and #f otherwise.")
  (define-built-in-doc finite? '(finite? x)
    (string-append
     "Returns #t if the number object is not an infinity and not a NaN, "
     "and #f otherwise."))
  (define-built-in-doc infinite? '(infinite? x)
    "Returns #t if the number object is an infinity, and #f otherwise.")
  (define-built-in-doc nan? '(nan? x)
    "Returns #t if the number object is a NaN, and #f otherwise.")
  (define-built-in-doc max '(max x1 x2 ...)
    (string-append
     "Returns the maximum of the arguments.\n\n"
     "If any argument is inexact, then the result is also inexact."))
  (define-built-in-doc min '(min x1 x2 ...)
    (string-append
     "Returns minimum of the arguments."
     "If any argument is inexact, then the result is also inexact."))
  (define-built-in-doc + '(+ z1 ...)
    (string-append
     "Returns the sum of the arguments."))
  (define-built-in-doc * '(* z1 ...)
    (string-append
     "Returns the product of the arguments."))
  (define-built-in-doc - '(- z1 z2 ...)
    (string-append
     "With one argument, returns the additive inverse of its "
     "argument. With two or more arguments, returns the difference "
     "of its arguments, associating to the left."))
  (define-built-in-doc / '(/ z1 z2 ...)
    (string-append
     "With one argument, returns the multiplicative inverse of its "
     "argument. With two or more arguments, this procedure returns "
     "the quotient of its arguments, associating to the left. "
     "If all of the arguments are exact, then the divisors must "
     "all be nonzero."))
  (define-built-in-doc abs '(abs x)
    "Returns the absolute value of its argument.")
  ;; Skipped documentation for div-and-mod, div, mod, div0-and-mod0,
  ;; div0, mod0.
  (define-built-in-doc div-and-mod '(div-and-mod x1 x2)
    "")
  (define-built-in-doc div '(div x1 x2)
    "")
  (define-built-in-doc mod '(mod x1 x2)
    "")
  (define-built-in-doc div0-and-mod0 '(div0-and-mod0 x1 x2)
    "")
  (define-built-in-doc div0 '(div0 x1 x2)
    "")
  (define-built-in-doc mod0 '(mod0 x1 x2)
    "")
  (define-built-in-doc gcd '(gcd n1 ...)
    (string-append
     "Returns the greatest common divisor of its arguments. "
     "The result is always non-negative."))
  (define-built-in-doc lcm '(lcm n1 ...)
    (string-append
     "Returns the least common multiple of its arguments. "
     "The result is always non-negative."))
  (define-built-in-doc numerator '(numerator q)
    (string-append
     "Returns the numerator of the argument. The result is computed "
     "as if the argument was represented as a fraction in lowest "
     "terms."))
  (define-built-in-doc denominator '(denominator q)
    (string-append
     "Returns the denominator of the argument. The result is computed "
     "as if the argument was represented as a fraction in lowest "
     "terms. The denominator is always positive. The denominator of 0 "
     "is defined to be 1."))
  (define-built-in-doc floor '(floor x)
    "")
  (define-built-in-doc ceiling '(ceiling x)
    "")
  (define-built-in-doc truncate '(truncate x)
    "")
  (define-built-in-doc round '(round x)
    "")
  (define-built-in-doc rationalize '(rationalize x1 x2)
    "")
  (define-built-in-doc exp '(exp z)
    "")
  (define-built-in-doc log '(log z1 [z2])
    "")
  (define-built-in-doc sin '(sin z)
    "")
  (define-built-in-doc cos '(cos z)
    "")
  (define-built-in-doc tan '(tan z)
    "")
  (define-built-in-doc asin '(asin z)
    "")
  (define-built-in-doc acos '(acos z)
    "")
  (define-built-in-doc atan '(atan x1 [x2])
    "")
  (define-built-in-doc sqrt '(sqrt z)
    "")
  (define-built-in-doc exact-integer-sqrt '(exact-integer-sqrt k)
    "")
  (define-built-in-doc expt '(expt z1 z2)
    "")
  (define-built-in-doc make-rectangular '(make-rectanguler x1 x2)
    "")
  (define-built-in-doc make-polar '(make-polar x3 x4)
    "")
  (define-built-in-doc real-part '(real-part z)
    "")
  (define-built-in-doc imag-part '(imag-part z)
    "")
  (define-built-in-doc magnitude '(magnitutde z)
    "")
  (define-built-in-doc angle '(angle z)
    "")
  ; Numerical Input and Output
  (define-built-in-doc number->string '(number->string z [radix [precision]])
    "")
  (define-built-in-doc string->number '(string->number string [radix])
    "")

  ;; Boolean
  (define-built-in-doc not '(not obj)
    "Returns #t if obj is #f, and returns #f otherwise.")
  (define-built-in-doc boolean? '(boolean? obj)
    "Returns #t if obj is either #t or #f and returns #f otherwise.")
  (define-built-in-doc boolean=? '(boolean=? bool1 bool2 bool3 ...)
    "Returns #t if the booleans are the same.")

  ;; Pair and list
  (define-built-in-doc pair? '(pair? obj)
    "Returns #t if obj is a pair, and otherwise returns #f.")
  (define-built-in-doc cons '(cons obj1 obj2)
    (string-append
     "Returns a newly allocated pair whose car is obj1 and whose "
     "cdr is obj2. The pair is guaranteed to be different (in the "
     "sense of eqv?) from every existing object."))
  (define-built-in-doc car '(car pair)
    "Returns the contents of the car field of pair.")
  (define-built-in-doc cdr '(cdr pair)
    "Returns the contents of the cdr field of pair.")
  ;; Skipping caar, cadr, ..., cdddar, cddddr, total of 28.
  (define-built-in-doc null? '(null? obj)
    "Returns #t if obj is the empty list, #f otherwise.")
  (define-built-in-doc list? '(list? obj)
    (string-append
     "Returns #t if obj is a list, #f otherwise. By definition, "
     "all lists are chains of pairs that have finite length and are"
     "terminated by the empty list."))
  (define-built-in-doc list '(list obj ...)
    "Returns a newly allocated list of its arguments.")
  (define-built-in-doc length '(length list)
    "Returns the length of list.")
  (define-built-in-doc append '(append list ... obj)
    (string-append
     "Returns a possibly improper list consisting of the elements "
     "of the first list followed by the elements of the other lists, "
     "with obj as the cdr of the final pair. An improper list "
     "results if obj is not a list.\n\n"
     "If append constructs a nonempty chain of pairs, it is always "
     "newly allocated. If no pairs are allocated, obj is returned."))
  (define-built-in-doc reverse '(reverse list)
    (string-append
     "Returns a newly allocated list consisting of the elements of "
     "list in reverse order."))
  (define-built-in-doc list-tail '(list-tail list k)
    (string-append
     "Returns the subchain of pairs of list obtained by omitting "
     "the first k elements. List should be a list of size at "
     "least k."))
  (define-built-in-doc list-ref '(list-ref list k)
    (string-append
     "Returns the k th element of list. List must be a list whose "
     "length is at least k + 1."))
  (define-built-in-doc map '(map proc list1 list2 ...)
    (string-append
     "Applies proc element-wise to the elements of the lists and "
     "returns a list of the results, in order. Proc is always "
     "called in the same dynamic environment as itself. The "
     "order in which proc is applied to the elements of the "
     "lists is unspecified. If multiple returns occur from map, "
     "the values returned by earlier returns are not mutated.\n\n"
     "The lists should all have the same length. Proc should "
     "accept as many arguments as there are lists and return a "
     "single value. Proc should not mutate any of the lists."))
  (define-built-in-doc for-each '(for-each proc list1 list2 ...)
    (string-append
     "Applies proc element-wise to the elements of the lists "
     "for its side effects, in order from the first elements "
     "to the last. Proc is always called in the same dynamic "
     "environment as for-each itself. The return values of "
     "for-each are unspecified.\n\n"
     "The lists should all have the same length. Proc should "
     "accept as many arguments as there are lists. Proc should "
     "not mutate any of the lists."))

  ;; Symbol
  (define-built-in-doc symbol? '(symbol? obj)
    "Returns #t if obj is a symbol, otherwise returns #f.")
  (define-built-in-doc symbol->string '(symbol->string symbol)
    "Returns the name of symbol as an immutable string.")
  (define-built-in-doc symbol=? '(symbol=? symbol1 symbol2 symbol3 ...)
    (string-append
     "Returns #t if the symbols are the same, i.e., if their names "
     "are spelled the same."))
  (define-built-in-doc string->symbol '(string->symbol string)
    "Returns the symbol whose name is string.")

  ;; Character
  (define-built-in-doc char? '(char?)
    "Returns #t if obj is a character, otherwise returns #f.")
  (define-built-in-doc char->integer '(char->integer char)
    (string-append
     "Given a character, char->integer returns its Unicode scalar "
     "value as an exact integer object."))
  (define-built-in-doc integer->char '(integer->char sv)
    (string-append
     "For a Unicode scalar value sv, i.e., a non-negative exact "
     "integer object in [0, #xD7FF] ⋃ [#xE000, #x10FFFF], "
     "integer->char returns its associated character."))
  (define-built-in-doc char=? '(char=? char1 char2 char3 ...)
    "Returns #t when its arguments are equivalent characters.")
  (define-built-in-doc char<? '(char<? char1 char2 char3 ...)
    (string-append
     "Returns #t when its arguments are monotonically increasing "
     "character (Unicode scalar) values."))
  (define-built-in-doc char>? '(char>? char1 char2 char3 ...)
    (string-append
     "Returns #t when its arguments are monotonically decreasing "
     "character (Unicode scalar) values."))
  (define-built-in-doc char<=? '(char<=? char1 char2 char3 ...)
    (string-append
     "Returns #t when its arguments are monotonically "
     "nondecreasing character (Unicode scalar) values."))
  (define-built-in-doc char>=? '(char>=? char1 char2 char3 ...)
    (string-append
     "Returns #t when its arguments are monotonically "
     "nonincreasing character (Unicode scalar) values."))

  ;; String
  (define-built-in-doc string? '(string? obj)
    "Returns #t if obj is a string, otherwise returns #f.")
  (define-built-in-doc make-string '(make-string k [char])
    (string-append
     "Returns a newly allocated string of length k. If char is "
     "given, then all elements of the string are initialized to "
     "char, otherwise the contents of the string are unspecified."))
  (define-built-in-doc string '(string char ...)
    "Returns a newly allocated string composed of the arguments.")
  (define-built-in-doc string-length '(string-length string)
    (string-append
     "Returns the number of characters in the given string as an "
     "exact integer object."))
  (define-built-in-doc string-ref '(string-ref string k)
    (string-append
     "Returns character k of string using zero-origin indexing. "
     "K must be a valid index of string."))
  (define-built-in-doc string=? '(string=? string1 string2 string3 ...)
    (string-append
     "Returns #t if the strings are the same length and contain the "
     "same characters in the same positions. Otherwise, returns #f."))
  (define-built-in-doc string<? '(string<? string1 string2 string3 ...)
    (string-append
     "The procedure is the lexicographic ordering on strings induced "
     "by the ordering char<? on characters. If two strings differ in "
     "length but are the same up to the length of the shorter "
     "string, the shorter string is considered to be lexicographically "
     "less than the longer string."))
  (define-built-in-doc string>? '(string>? string1 string2 string3 ...)
    (string-append
     "The procedure is the lexicographic ordering on strings induced "
     "by the ordering char>? on characters. If two strings differ in "
     "length but are the same up to the length of the shorter "
     "string, the longer string is considered to be lexicographically "
     "more than the shorter string."))
  (define-built-in-doc string<=? '(string<=? string1 string2 string3 ...)
    (string-append
     "The procedure is the lexicographic ordering on strings induced "
     "by the ordering char<=? on characters. If two strings differ in "
     "length but are the same up to the length of the shorter "
     "string, the shorter string is considered to be lexicographically "
     "less than the longer string."))
  (define-built-in-doc string>=? '(string>=? string1 string2 string3 ...)
    (string-append
     "The procedure is the lexicographic ordering on strings induced "
     "by the ordering char>=? on characters. If two strings differ in "
     "length but are the same up to the length of the shorter "
     "string, the longer string is considered to be lexicographically "
     "more than the shorter string."))
  (define-built-in-doc substring '(substring string start end)
    (string-append
     "Returns a newly allocated string formed from the characters of "
     "string beginning with index start (inclusive) and ending with "
     "index end (exclusive).\n\n"
     "String must be a string, and start and end must be exact "
     "integer objects satisfying "
     "0 ≤ start ≤ end ≤ (string-length string)."))
  (define-built-in-doc string-append '(string-append string ...)
    (string-append
     "Returns a newly allocated string whose characters form the "
     "concatenation of the given strings."))
  (define-built-in-doc string->list '(string->list string)
    (string-append
     "Returns a newly allocated list of the characters that make "
     "up the given string. The string->list and list->string "
     "procedures are inverses so far as equal? is concerned."))
  (define-built-in-doc list->string '(list->string list)
    (string-append
     "Returns a newly allocated string formed from the characters "
     "in list. List must be a list of characters. The string->list "
     "and list->string procedures are inverses so far as equal? "
     "is concerned."))
  (define-built-in-doc string-for-each
    '(string-for-each proc string1 string2 ...)
    (string-append
     "Applies proc element-wise to the characters of the strings "
     "for its side effects, in order from the first characters to "
     "the last. Proc is always called in the same dynamic environment "
     "as string-for-each itself. The return values of string-for-each "
     "are unspecified. The strings must all have the same length. "
     "Proc should accept as many arguments as there are strings.\n\n"
     "Analogous to for-each."))
  (define-built-in-doc string-copy '(string-copy string)
    "Returns a newly allocated copy of the given string.")

  ;; Vector
  (define-built-in-doc vector? '(vector? obj)
    "Returns #t if obj is a vector. Otherwise returns #f.")
  (define-built-in-doc make-vector '(make-vector k [fill])
    (string-append
     "Returns a newly allocated vector of k elements. "
     "If a second argument is given, then each element "
     "is initialized to fill. Otherwise the initial "
     "contents of each element is unspecified."))
  (define-built-in-doc vector '(vector obj ...)
    (string-append
     "Returns a newly allocated vector whose elements contain "
     "the given arguments. Analogous to list."))
  (define-built-in-doc vector-length '(vector-length vector)
    (string-append
     "Returns the number of elements in vector as an exact "
     "integer object."))
  (define-built-in-doc vector-ref '(vector-ref vector k)
    (string-append
     "Returns the contents of element k of vector. "
     "K must be a valid index of vector."))
  (define-built-in-doc vector-set! '(vector-set! vector k obj)
    (string-append
     "Stores obj in element k of vector, and returns "
     "unspecified values. K must be a valid index of vector."))
  (define-built-in-doc vector->list '(vector->list vector)
    (string-append
     "Returns a newly allocated list of the objects contained "
     "in the elements of vector."))
  (define-built-in-doc list->vector '(list->vector list)
    (string-append
     "Returns a newly created vector initialized to the "
     "elements of the list list."))
  (define-built-in-doc vector-fill! '(vector-fill! vector fill)
    (string-append
     "Stores fill in every element of vector and returns "
     "unspecified values."))
  (define-built-in-doc vector-map '(vector-map proc vector1 vector2 ...)
    (string-append
     "Applies proc element-wise to the elements of the vectors "
     "and returns a vector of the results, in order. Proc is "
     "always called in the same dynamic environment as vector-map "
     "itself. The order in which proc is applied to the elements "
     "of the vectors is unspecified. If multiple returns occur "
     "from vector-map, the return values returned by earlier "
     "returns are not mutated.\n\n"
     "The vectors must all have the same length. Proc should "
     "accept as many arguments as there are vectors and return "
     "a single value.\n\n"
     "Analogous to map."))
  (define-built-in-doc vector-for-each
    '(vector-for-each proc vector1 vector2 ...)
    (string-append
     "Applies proc element-wise to the elements of the vectors for "
     "its side effects, in order from the first elements to the "
     "last. Proc is always called in the same dynamic environment "
     "as vector-for-each itself. The return values of vector-for-each "
     "are unspecified.\n\n"
     "The vectors must all have the same length. Proc should accept "
     "as many arguments as there are vectors.\n\n"
     "Analogous to for-each."))

  ;; Error and violation
  (define-built-in-doc error '(error who message irritant1 ...)
    "")
  (define-built-in-doc assertion-violation
    '(assertion-violation who message irritant1 ...)
    "")

  ;; Control features
  (define-built-in-doc apply '(apply proc arg1 ... rest-args)
    "")
  (define-built-in-doc values '(values obj ...)
    "")
  (define-built-in-doc call-with-values '(call-with-values producer consumer)
    "")
  (define-built-in-doc dynamic-wind '(dynamic-wind before thunk after)
    "")
  

  (define-built-in-doc foreign-alloc '(foreign-alloc n)
    "Allocate N bytes.")
  (define-built-in-doc foreign-free '(foreign-alloc address)
    "Allocate N bytes.")
  (define-built-in-doc foreign-ref '(foreign-ref type address offset)
    "")
  (define-built-in-doc foreign-set! '(foreign-set! type address offset value)
    "")
  (define-built-in-doc foreign-sizeof '(foreign-sizeof type)
    "")
  ;; syntax...
  ;; (define-built-in-doc ftype-set! '(ftype-set! ftype-name (a ...) fptr-expr [index] val-expr)
  ;;   "")
  ;; (define-built-in-doc ftype-ref '(ftype-ref ftype-name (a ...) fptr-expr [index])
  ;;   "")
  (define-built-in-doc make-date '(make-date nsec sec min hour day mon year offset)
    "")

  (define-built-in-doc sort '(sort predicate list)
    ""))

