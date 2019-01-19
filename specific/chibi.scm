;;;; srfi 13
(define %string-contains string-contains)
(define (string-contains string pattern)
  (let ((x (%string-contains string pattern)))
    (if x
        (string-cursor->index string x)
        x)))
(define ($scheme-name)
  "chibi-scheme")

(define ($macroexpand-1 form)
  (macroexpand form))

(define ($macroexpand-all form)
  (macroexpand form))
;; chibi does not allow running code between opening the socket
;; and accepting with (chibi net server) :-/
(define ($open-tcp-server port-number port-file handler)
  (let ((n (or port-number (+ 10000 (random-integer 50000)))))
    ;; HACK: the port should be printed by base.scm, but as commented
    ;; above, chibi does not let us run code there
    (display "listening on port ") (display n) (newline) (flush-output-port)
    (when port-file
         (when (file-exists? port-file)
           (delete-file port-file))
         (with-output-to-file port-file
           (lambda ()
             (display n))))
    ;; end of HACK
    (run-net-server n (lambda (in out sock addr)
                        (handler n (list in out sock))))))

(define ($tcp-server-accept lst handler)
  (handler (car lst) (cadr lst)))

(define ($all-package-names)
  (map display-to-string (map car *modules*)))

(define (display-to-string val)
  (let ((out (open-output-string)))
    (display val out)
    (get-output-string out)))

(define ($error-description error)
  (apply string-append (error-object-message error)
         ": "
         (map write-to-string (error-object-irritants error))))

(define ($output-to-repl thunk)
  ;; basic implementation, print all output at the end, this should
  ;; be replaced with a custom output port
  (let ((o (open-output-string)))
    (parameterize ((current-output-port o))
      (let-values ((x (thunk)))
        (swank/write-string (get-output-string o) #f)
        (apply values x)))))
(define (all-exports env)
  (delete-duplicates
   (let lp ((env env) (res '()))
     (if (not env)
	 res
	 (lp (env-parent env) (append (env-exports env) res))))))
(define (longest-common-prefix strings)

  (cond ((null? strings)
	 "")
	((= (length strings) 1)
	 (car strings))
	(else
	 (let ((max-length (apply min (map string-length strings))))
	   (let loop ((i 1))
	     (if (all-string=? (map (lambda (el)
				      (substring el 0 i))
				    strings))
		 (if (= i max-length)
		     (substring (car strings) 0 i)
		     (loop (+ i 1)))
		 (substring (car strings) 0 (- i 1))))))))
(define *active-package-name* '(user))
(define (all-string=? lst)
  (let loop ((first (car lst))
	     (rest (cdr lst)))
    (if (null? rest)
	#t
	(if (not (string=? first (car rest)))
	    #f
	    (loop first (cdr rest))))))
(define (find-module name)
  ;; (display "find-module ")
  ;; (write name)
  ;; (newline)
  ;; TODO: fix this
  (if (eq? 'COMMON-LISP-USER name)
      (set! name '(chibi swank)))
  (let ((res (assoc name *modules*)))
    res))
(define ($completions prefix env-name)
  (if (not (find-module '(user)))
    (add-module! '(user) (make-module '() ($environment env-name) '())))
  (let* ((package (if (not (string? env-name)) *active-package-name* (let ((in (open-input-string env-name))) (read in)))))
    (display "package: ") (display package) (newline)
    (let* ((env (module-env (cdr (find-module package)))))
      (let* ((bindings (all-exports env)))
	(let ((completions (sort (map symbol->string (filter (lambda (el)
							       (string-prefix? prefix (symbol->string el)))
							     bindings))
				 string<?)))
	  (cons completions
		(longest-common-prefix completions)))))))

(define ($function-parameters-and-documentation name)
  (cons #f #f))

(define ($set-package name)
  (list "(user)" "(user)"))

(define ($environment name)
  (interaction-environment))

(define ($condition-trace condition)
  '())

(define ($condition-location condition)
  "Return (PATH POSITION LINE COLUMN) for CONDITION."
  #f)

(define ($frame-locals-and-catch-tags nr)
  '())

(define ($frame-var-value frame index)
  #f)

(define ($condition-msg condition)
  "UNKNOWN")

(define ($condition-links condition)
  '())

(define ($handle-condition exception)
  #f)

(define ($pretty-print object)
  (show (current-output-port) (pretty object)))

(define-record-type <istate>
  (make-istate object parts actions next previous content)
  istate?
  (object istate-object)
  (parts istate-parts)
  (actions istate-actions)
  (next istate-next set-istate-next!)
  (previous istate-previous)
  (content istate-content))

(define ($inspect-fallback object)
  #f)

(define ($apropos name)
  "Return a list of (name type documentation) for all matches for NAME."
  '())

(define ($binding-documentation p)
  #f)
