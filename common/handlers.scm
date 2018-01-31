(define-syntax define-slime-handler
  (syntax-rules ()
    ((define-slime-handler (name . params) body0 body1 ...)
     (register-slime-handler! 'name (lambda params body0 body1 ...)))))

(define-slime-handler (:emacs-rex sexp env-name thread id)
  (call-with-current-continuation
   (lambda (exit)
     (parameterize ((param:abort (lambda (message)
                                   (exit `(:return (:abort ,message)
                                                   ,id))))
                    (param:environment ($environment env-name))
                    (param:current-id id))
       `(:return (:ok ,(process-form sexp env-name))
                            ,id)))))

(define-slime-handler (swank:connection-info)
  `(:pid 123
         :style :spawn
         :encoding (:coding-systems ("utf-8-unix"))
         :lisp-implementation (:type "Scheme" :name ,($scheme-name) :version 123 :program "/usr/bin/scheme")
         :machine (:instance "host" :type "X86-64")
         :features (:swank)
         :modules ("SWANK-ARGLISTS" "SWANK-REPL" "SWANK-PRESENTATIONS")
         :package (:name "(user)" :prompt "(user)")
         :version "2.20"
         ))

(define-slime-handler (swank:swank-require packages)
  '())

(define-slime-handler (swank:init-presentations)
  '())

(define-slime-handler (cl:nth-value num form)
  (call-with-values
      (lambda () (process-form form (param:environment)))
    (lambda values (list-ref values num))))

(define-slime-handler (swank:lookup-presented-object num)
  (swank:lookup-presented-object num))

(define-slime-handler (swank-repl:create-repl . args)
  (list "(user)" "(user)"))

(define-slime-handler (swank-repl:listener-eval form)
  (let* ((form (replace-readtime-lookup-presented-object-or-lose form))
	 (results ($output-to-repl (lambda () (interactive-eval (read (open-input-string form)))))))
    (for-each (lambda (val)
                (if (presentations?)
		  (present val ':repl-result)
		  (swank/write-string val 'repl-result)))
              results)
    '()))

(define-slime-handler (swank:quit-lisp)
  (exit 0))

(define-slime-handler (swank:list-all-package-names . args)
  ($all-package-names))

(define-slime-handler (swank:completions prefix env-name)
  (let ((completions+prefix ($completions prefix (unquote-string env-name))))
    (list (car completions+prefix)
          (cdr completions+prefix))))

(define-slime-handler (swank:simple-completions prefix env-name)
  ;; TODO: for now, just copy swank:completions
  (let ((completions+prefix ($completions prefix (unquote-string env-name))))
    (list (car completions+prefix)
          (cdr completions+prefix))))

(define-slime-handler (swank:compile-string-for-emacs form buffer position filename policy)
  ;; TODO: for now, just evaluate, copy of listener-eval
  (let ((results ($output-to-repl (lambda () (interactive-eval (read (open-input-string form)))))))
    (for-each (lambda (val)
		(swank/write-string val 'repl-result))
              results)
    `(:compilation-result nil t 0.001 nil nil)))

(define-slime-handler (swank:load-file filename)
  (let ((results ($output-to-repl (lambda () (load filename (interaction-environment))))))
    'loaded))

(define-slime-handler (swank:set-package name)
  ($set-package name))

(define-slime-handler (swank:autodoc expr . params)
  (let* ((op-string (find-string-before-swank-cursor-marker expr)))
    (if op-string
        (let* ((signature+doc ($function-parameters-and-documentation op-string))
               (signature (car signature+doc))
               (doc (cdr signature+doc)))
          (let ((answer (if signature (let* ((signature (highlight-at-cursor signature expr)))
                                  (with-output-to-string (lambda ()
                                                           (write signature)
                                                           (if doc
                                                               (begin
                                                                 (display " -- ")
                                                                 (display doc))))))
                            ':not-available)))
            (list answer 't)))
        (list ':not-available 't))))
(define-slime-handler (swank:frame-locals-and-catch-tags nr)
  ;; (:return
  ;;  (:ok
  ;;   (((:name "X" :id 0 :value "1")
  ;;     (:name "Y" :id 0 :value "0"))
  ;;    nil))
  ;;  8)
  ($frame-locals-and-catch-tags nr))
(define-slime-handler (swank:throw-to-toplevel)
  (set! *throw-to-top-level* #t)
  (swank/abort (param:condition:msg)))
(define-slime-handler (swank:backtrace from to)
  ;; (:return
  ;;  (:ok
  ;;   (((:name "X" :id 0 :value "1")
  ;;     (:name "Y" :id 0 :value "0"))
  ;;    nil))
  ;;  8)
  (let ((trace ($condition-trace (param:active-condition))))
    (let ((from (min from (length trace)))
          (to (min to (length trace))))
      (take (list-tail trace from) (- to from)))))
