(import (racket/tcp) (srfi/69))
(define ($scheme-name)
  "racket")
(define ($open-tcp-server/accept port-number handler)
  (let ((server (tcp-listen port-number)))
    (let-values (((in out) (tcp-accept server)))
      (handler port-number in out))))

(define $make-hash-table make-hash-table)

(define $hash-table/put! hash-table-set!)

(define $hash-table/get hash-table-ref/default)
(define env (let ((n (make-base-namespace)))
              (parameterize ((current-namespace n))
                (namespace-require 'r7rs)
                (namespace-require 'r7rs/base)
                (namespace-require 'r7rs/read)
                (namespace-require 'r7rs/write))
              n))
(define (interaction-environment) env)


(define ($all-package-names)
  '()) ;;  (module-list) from geiser

;; 

(define ($error-description error)
  (let ((o (open-output-string)))
    (display error o)
    (get-output-string o)))

(define ($output-to-repl thunk)
  ;; basic implementation, print all output at the end, this should
  ;; be replaced with a custom output port
  (let ((o (open-output-string)))
    (parameterize ((current-output-port o))
      (let-values ((x (thunk)))
        (swank/write-string (get-output-string o) #f)
        (apply values x)))))

(define (env-name->environment env-name)
  env)
(define (environment-bindings env)
  (namespace-mapped-symbols env))
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
      (fold (lambda (s1 s2) (substring s2 0 (string-match-forward s1 s2))) (mcar strings) (mlist->list (mcdr strings)))))
(define ($completions prefix env-name)
  (let ((matches (list->mlist (filter (lambda (el) (string-prefix? prefix el))
                                      (mlist->list (map symbol->string (list->mlist (environment-bindings (env-name->environment env-name)))))))))
    (cons (mlist->list matches)
          (longest-common-prefix matches))))
