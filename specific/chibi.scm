(define ($open-tcp-server/accept port-number handler)
  ;; TODO: port number
  (run-net-server port-number (lambda (in out sock addr)
                                (handler port-number in out))))

(define $make-hash-table make-hash-table)

(define $hash-table/put! hash-table-set!)

(define $hash-table/get hash-table-ref/default)

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
    (add-module! '(user) (make-module '() (interaction-environment) '())))
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

