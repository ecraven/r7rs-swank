(define-library (ecraven gerbil-swank)
  (export start-swank
	  swank:lookup-presented-object swank:lookup-presented-object-or-lose
	  repl-result-history-ref)
  (import (scheme base) (scheme file) (scheme write) (scheme read) (scheme eval) (scheme repl) (scheme process-context) (scheme load) (scheme cxr) (scheme char)
          (std srfi |13|) (std srfi |1|)
          (only (gerbil/gambit) open-tcp-server pp random-integer)
          (only (gerbil/core) hash-ref hash-put! hash-table? hash-for-each hash-length
                make-hash-table exit  hash->list filter hash-remove!
                values->list ;; values->list should go when let-values works
                ) 
          (only (gerbil/expander) module-context? module-context-ns expander-context-id expander-context-table current-expander-module-registry)
          (only (gerbil/gambit/exceptions) display-exception))
  (include "specific/gerbil.scm")
  (include "common/base.scm")
  (include "common/handlers.scm"))
