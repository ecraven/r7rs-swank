(load-option 'srfi-13)
;; still no proper multiple values :-/
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



(define ($scheme-name)
  "mit-scheme")
(define (fluid a)
  (a))

(define ($open-tcp-server port-number port-file handler)
  (let* ((n (or port-number 0))
         (socket (open-tcp-server-socket port-number (host-address-loopback))))
    (handler n socket)))

(define ($tcp-server-accept socket handler)
  (let ((p (tcp-server-binary-connection-accept socket #t #f)))
    (handler p p)))

(define hash-table-set! hash-table/put!)

(define hash-table-ref/default hash-table/get)

(define ($all-package-names)
  (map (lambda (package) (env->pstring (package/environment package)))
       (all-packages)))

(define ($handle-condition exception)
  (invoke-sldb exception))

(define ($condition-msg condition)
  (condition/report-string condition))

(define ($condition-location condition)
  "Return (PATH POSITION LINE COLUMN) for CONDITION."
  #f)

(define ($condition-links condition)
  ;; TODO
  (map (lambda (x) 'foo) ($condition-trace condition)))

(define (pp-to-string o)
  (string-trim (with-output-to-string (lambda () (pp o)))))

(define debugger-hide-system-code? #f)

(define (system-frame? stack-frame)
  (stack-frame/repl-eval-boundary? stack-frame))

(define (stack-frame->subproblem frame number)
  (receive (expression environment subexpression)
      (stack-frame/debugging-info frame)
    (make-subproblem frame expression environment subexpression number)))
(define-record-type <subproblem>
    (make-subproblem stack-frame expression environment subexpression number)
    subproblem?
  (stack-frame subproblem/stack-frame)
  (expression subproblem/expression)
  (environment subproblem/environment)
  (subexpression subproblem/subexpression)
  (number subproblem/number))
(define-record-type <browser-line>
    (%make-bline start-mark object type parent depth next prev offset
		 properties)
    bline?

  ;; Index of this bline within browser lines vector.  #F if line is
  ;; invisible.
  (index bline/index set-bline/index!)

  ;; Line start within browser buffer.  #F if line is invisible.
  (start-mark bline/start-mark set-bline/start-mark!)

  ;; Object that this line represents.
  (object bline/object)

  ;; Type of OBJECT.  This type is specific to the browser; it tells
  ;; the browser how to manipulate OBJECT.
  (type bline/type)

  ;; BLINE representing the object that this object is a component of,
  ;; or #F if none.
  (parent bline/parent)

  ;; Nonnegative integer indicating the depth of this object in the
  ;; component nesting.
  (depth bline/depth)

  ;; BLINEs representing the objects that are adjacent to this one in
  ;; the component ordering, or #F if none.
  (next bline/next set-bline/next!)
  (prev bline/prev)

  ;; Nonnegative integer indicating the position of this object in the
  ;; component ordering.
  (offset bline/offset)

  (properties bline/properties))
(define (make-bline object type parent prev)
  (let ((bline
	 (%make-bline #f
		      object
		      type
		      parent
		      (if parent (+ (bline/depth parent) 1) 0)
		      #f
		      prev
		      (if prev (+ (bline/offset prev) 1) 0)
		      (make-1d-table))))
    (if prev
	(set-bline/next! prev bline))
    bline))
(define-record-type <reduction>
    (make-reduction subproblem expression environment number)
    reduction?
  (subproblem reduction/subproblem)
  (expression reduction/expression)
  (environment reduction/environment)
  (number reduction/number))
(define (subproblem/reductions subproblem)
  (let ((frame (subproblem/stack-frame subproblem)))
    (let loop ((reductions (stack-frame/reductions frame)) (n 0))
      (if (pair? reductions)
	  (cons (make-reduction subproblem
				(caar reductions)
				(cadar reductions)
				n)
		(loop (cdr reductions) (+ n 1)))
	  '()))))

(define (continuation->blines continuation)
  (let ((beyond-system-code #f))
    (let loop ((frame (continuation/first-subproblem continuation))
	       (prev #f)
	       (n 0))
      (if (not frame)
	  '()
	  (let* ((next-subproblem
		  (lambda (bline)
		    (loop (stack-frame/next-subproblem frame)
			  bline
			  (+ n 1))))
		 (walk-reductions
		  (lambda (bline reductions)
		    (cons bline
			  (let loop ((reductions reductions) (prev #f))
			    (if (null? reductions)
				(next-subproblem bline)
				(let ((bline
				       (make-bline (car reductions)
						   'bline-type:reduction
						   bline
						   prev)))
				  (cons bline
					(loop (cdr reductions) bline))))))))
		 (continue
		  (lambda ()
		    (let* ((subproblem (stack-frame->subproblem frame n)))
		      (if debugger:student-walk?
			  (let ((reductions
				 (subproblem/reductions subproblem)))
			    (if (null? reductions)
				(let ((bline
				       (make-bline subproblem
						   'bline-type:subproblem
						   #f
						   prev)))
				  (cons bline
					(next-subproblem bline)))
				(let ((bline
				       (make-bline (car reductions)
						   'bline-type:reduction
						   #f
						   prev)))
				  (walk-reductions bline
						   (if (> n 0)
						       '()
						       (cdr reductions))))))
			  (walk-reductions
			   (make-bline subproblem
				       'bline-type:subproblem
				       #f
				       prev)
			   (subproblem/reductions subproblem)))))))
	    (cond ((and (not debugger-hide-system-code?)
			(system-frame? frame))
		   (loop (stack-frame/next-subproblem frame)
			 prev
			 n))
		  ((or ;; (and limit (>= n limit))
		       (if (system-frame? frame)
			   (begin (set! beyond-system-code #t) #t)
			   #f)
		       beyond-system-code)
		   (list (make-continuation-bline continue #f prev)))
		  (else (continue))))))))

(define ($condition-trace condition)
  (define from 0)
  (define (combination->list c)
    (read-from-string (pp-to-string c)))
  (let* ((blines (drop (continuation->blines (condition/continuation condition)) from))
         (blines-count (length blines))
         (count (take (iota blines-count from) blines-count)))
    (append (map (lambda (i bline)
                   (let ((o (bline/object bline)))
                     (cond ((reduction? o) (string "R" (reduction/number o) ": " (pp-to-string (reduction/expression o))))
                           ((subproblem? o) (string "S" (subproblem/number o) ": " (pp-to-string (subproblem/subexpression o)) "\nin " (pp-to-string (subproblem/expression o))))
                           (else (pp-to-string o)))))
                 count
                 blines))))

(define ($function-parameters-and-documentation name)
  (cons '(+ a b) "documentation"))

(define (string-replace s1 s2 start1 end1) ;; . start2+end2
  (let* ((s1-before (substring s1 0 start1))
         (s1-after (substring s1 end1 (string-length s1))))
    (string-append s1-before s2 s1-after)))

(define ($inspect-fallback object)
  (cond ((record? object)
         (let* ((type (record-type-descriptor object))
                (field-names (record-type-field-names type))
                (len (length field-names))
                (field-values (map (lambda (i) ((record-accessor type i) object)) field-names)))
           (let loop ((n field-names)
                      (v field-values))
             (if (null? n)
                 (stream)
                 (stream-cons (inspector-line (car n) (car v))
                              (loop (cdr n) (cdr v)))))))
        (else #f)))

(define ($environment env-name)
  ;; TODO
  (interaction-environment))

(define ($error-description condition)
  (condition/report-string condition))

(define (all-packages)
  (let loop ((package (name->package '()))) ;;  system-global-package
    (cons package
          (append-map loop (package/children package)))))

(define anonymous-package-prefix "environment-")

(define unknown-environment (cons 'unknown 'environment))

(define (env->pstring env)
  (if (eq? unknown-environment env)
      "unknown environment"
      (let ((package (environment->package env)))
	(if package
	    (write-to-string (package/name package))
	    (string anonymous-package-prefix (object-hash env))))))

(define (with-exception-handler handler thunk)
  (bind-condition-handler (list condition-type:serious-condition)
                          handler
                          thunk))

(define ($output-to-repl thunk)
  ;; basic implementation, print all output at the end, this should
  ;; be replaced with a custom output port
  (let ((o (open-output-string)))
    (parameterize ((current-output-port o))
      (begin0
       (thunk)
       (swank/write-string (get-output-string o) #f)))))

(define ($completions prefix env-name)
  (let ((strings (all-completions prefix (pstring->env env-name))))
    (cons (sort strings string<?)
          (longest-common-prefix strings))))

(define (longest-common-prefix strings)
  (reduce (lambda (s1 s2)
            (substring s1 0 (string-match-forward s1 s2)))
          ""
          strings))

(define *buffer-pstring* (make-parameter #f))

(define (pstring->env pstring)
  (cond ((or (not (string? pstring))
             (not (string? (fluid *buffer-pstring*)))
             (string-ci=? (fluid *buffer-pstring*) "COMMON-LISP-USER"))
         (nearest-repl/environment))
        ((string-prefix? anonymous-package-prefix pstring)
         (let ((object
                (object-unhash
                 (string->number (string-tail pstring
                                              (string-length
                                               anonymous-package-prefix))
                                 10
                                 #t))))
           (if (not (environment? object))
               (error:wrong-type-datum object "environment"))
           object))
        (else
         (package/environment (find-package (read-from-string pstring) #t)))))

(define (all-completions prefix environment)
  (let ((prefix
         (if (fluid (environment-lookup environment 'PARAM:PARSER-CANONICALIZE-SYMBOLS?))
             (string-downcase prefix)
             prefix))
        (completions '()))
    (for-each-interned-symbol
     (lambda (symbol)
       (if (and (string-prefix? prefix (symbol-name symbol))
                (environment-bound? environment symbol))
           (set! completions (cons (symbol-name symbol) completions)))
       unspecific))
    completions))

(define-record-type <istate>
  (make-istate object parts actions next previous content)
  istate?
  (object istate-object)
  (parts istate-parts)
  (actions istate-actions)
  (next istate-next set-istate-next!)
  (previous istate-previous)
  (content istate-content))

(define ($frame-locals-and-catch-tags nr)
  `(nil
    nil))
