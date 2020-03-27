(declare (usual-integrations))
;;;; srfi 13
(define (string-start+end str start end)
  (let ((s (if (eq? #!default start)
               0 start))
        (e (if (eq? #!default end)
               (string-length str) end)))
    (values s e)))

(define (string-start+end+start+end s1 start1 end1 s2 start2 end2)
  (let ((s (if (eq? #!default start1)
               0 start1))
        (e (if (eq? #!default end1)
               (string-length s1) end1))
        (sx (if (eq? #!default start2)
               0 start2))
        (ex (if (eq? #!default end2)
               (string-length s2) end2)))
    (values s e sx ex)))

(define (string-map proc s #!optional start end)
  (receive (start end)
      (string-start+end s start end)
  (let* ((len (- end start))
         (ans (make-string len)))
    (do ((i (- end 1) (- i 1))
         (j (- len 1) (- j 1)))
        ((< j 0))
      (string-set! ans j (proc (string-ref s i))))
    ans)))

(define (string-map! proc s #!optional start end)
  (receive (start end)
      (string-start+end s start end)
    (do ((i (- end 1) (- i 1)))
        ((< i start))
      (string-set! s i (proc (string-ref s i))))))

(define (string-fold kons knil s #!optional start end)
  (receive (start end)
      (string-start+end s start end)
    (let lp ((v knil) (i start))
      (if (< i end) (lp (kons (string-ref s i) v) (+ i 1))
          v))))

(define (string-fold-right kons knil s #!optional start end)
  (receive (start end)
      (string-start+end s start end)
    (let lp ((v knil) (i (- end 1)))
      (if (>= i start) (lp (kons (string-ref s i) v) (- i 1))
          v))))

(define (string-tabulate proc len)
  (let ((s (make-string len)))
    (do ((i (- len 1) (- i 1)))
        ((< i 0))
      (string-set! s i (proc i)))
    s))

(define (string-for-each proc s #!optional start end)
  (receive (start end)
      (string-start+end s start end)
    (let lp ((i start))
      (if (< i end)
          (begin (proc (string-ref s i))
                 (lp (+ i 1)))))))

(define (string-for-each-index proc s #!optional start end)
  (receive (start end)
      (string-start+end s start end)
    (let lp ((i start))
      (if (< i end) (begin (proc i) (lp (+ i 1)))))))




;;(define string-hash-ci string-ci-hash)
(define string= string=?)
(define string< string<?)
(define string> string>?)
(define string<= string<=?)
(define string>= string>=?)
(define (string<> a b) (not (string= a b)))

(define string-ci= string-ci=?)
(define string-ci< string-ci<?)
(define string-ci> string-ci>?)
(define string-ci<= string-ci<=?)
(define string-ci>= string-ci>=?)
(define (string-ci<> a b) (not (string-ci= a b)))

(define (substring/shared s #!optional start end)
  (receive (start end)
      (string-start+end s start end)
    (if (and (zero? start) (= end (string-length s))) s
        (substring s start end))))

(define (string-take s n)
  (substring/shared s 0 n))

(define (string-take-right s n)
  (let ((len (string-length s)))
    (substring/shared s (- len n) len)))

(define (string-drop s n)
  (let ((len (string-length s)))
    (substring/shared s n len)))

(define (string-drop-right s n)
  (let ((len (string-length s)))
    (substring/shared s 0 (- len n))))

(define (string-pad s n #!optional char start end)
  (receive (start end)
      (string-start+end s start end)
    (let ((char (if (eq? #!default char) #\space char)))
      (let ((len (- end start)))
        (if (<= n len)
            (substring/shared s (- end n) end)
            (let ((ans (make-string n char)))
              (string-copy! ans (- n len) s start end)
              ans))))))

(define (string-copy! to tstart from #!optional fstart fend)
  (receive (fstart fend)
      (string-start+end from fstart fend)
    (if (> fstart tstart)
        (do ((i fstart (+ i 1))
             (j tstart (+ j 1)))
            ((>= i fend))
          (string-set! to j (string-ref from i)))
        (do ((i (- fend 1)                    (- i 1))
             (j (+ -1 tstart (- fend fstart)) (- j 1)))
            ((< i fstart))
          (string-set! to j (string-ref from i))))))

(define (string-skip str criterion #!optional start end)
  (receive (start end)
      (string-start+end str start end)
    (cond ((char? criterion)
           (let lp ((i start))
             (and (< i end)
                  (if (char=? criterion (string-ref str i))
                      (lp (+ i 1))
                      i))))
          ((char-set? criterion)
           (let lp ((i start))
             (and (< i end)
                  (if (char-in-set? (string-ref str i) criterion)
                      (lp (+ i 1))
                      i))))
          ((procedure? criterion)
           (let lp ((i start))
             (and (< i end)
                  (if (criterion (string-ref str i)) (lp (+ i 1))
                      i))))
          (else (error "Second param is neither char-set, char, or predicate procedure."
                       string-skip criterion)))))

(define (string-skip-right str criterion #!optional start end)
  (receive (start end)
      (string-start+end str start end)
    (cond ((char? criterion)
           (let lp ((i (- end 1)))
             (and (>= i start)
                  (if (char=? criterion (string-ref str i))
                      (lp (- i 1))
                      i))))
          ((char-set? criterion)
           (let lp ((i (- end 1)))
             (and (>= i start)
                  (if (char-in-set? (string-ref str i) criterion)
                      (lp (- i 1))
                      i))))
          ((procedure? criterion)
           (let lp ((i (- end 1)))
             (and (>= i start)
                  (if (criterion (string-ref str i)) (lp (- i 1))
                      i))))
          (else (error "CRITERION param is neither char-set or char."
                       string-skip-right criterion)))))

(define (string-trim-both s #!optional criterion start end)
    (receive (start end)
        (string-start+end s start end)
      (let ((criterion (if (eq? #!default criterion) char-set:whitespace criterion)))
        (cond ((string-skip s criterion start end) =>
               (lambda (i)
                 (substring/shared s i (+ 1 (string-skip-right s criterion i end)))))
              (else "")))))


(define (string-filter criterion s #!optional start end)
    (receive (start end)
        (string-start+end s start end)
    (if (procedure? criterion)
        (let* ((slen (- end start))
               (temp (make-string slen))
               (ans-len (string-fold (lambda (c i)
                                       (if (criterion c)
                                           (begin (string-set! temp i c)
                                                  (+ i 1))
                                           i))
                                     0 s start end)))
          (if (= ans-len slen) temp (substring temp 0 ans-len)))

        (let* ((cset (cond ((char-set? criterion) criterion)
                           ((char? criterion) (char-set criterion))
                           (else (error "string-delete criterion not predicate, char or char-set" criterion))))

               (len (string-fold (lambda (c i) (if (char-in-set? c cset)
                                                   (+ i 1)
                                                   i))
                                 0 s start end))
               (ans (make-string len)))
          (string-fold (lambda (c i) (if (char-in-set? c cset)
                                         (begin (string-set! ans i c)
                                                (+ i 1))
                                         i))
                       0 s start end)
          ans))))

(define (string-count s criterion #!optional start end)
  (receive (start end)
      (string-start+end s start end)
    (cond ((char? criterion)
           (do ((i start (+ i 1))
                (count 0 (if (char=? criterion (string-ref s i))
                             (+ count 1)
                             count)))
               ((>= i end) count)))

          ((char-set? criterion)
           (do ((i start (+ i 1))
                (count 0 (if (char-in-set? (string-ref s i) criterion)
                             (+ count 1)
                             count)))
               ((>= i end) count)))

          ((procedure? criterion)
           (do ((i start (+ i 1))
                (count 0 (if (criterion (string-ref s i)) (+ count 1) count)))
               ((>= i end) count)))

          (else (error "CRITERION param is neither char-set or char."
                       string-count criterion)))))

(define (reverse-list->string clist)
  (let* ((len (length clist))
         (s (make-string len)))
    (do ((i (- len 1) (- i 1))   (clist clist (cdr clist)))
        ((not (pair? clist)))
      (string-set! s i (car clist)))
    s))

(define (string-index str criterion #!optional start end)
    (receive (start end)
      (string-start+end str start end)
    (cond ((char? criterion)
           (let lp ((i start))
             (and (< i end)
                  (if (char=? criterion (string-ref str i)) i
                      (lp (+ i 1))))))
          ((char-set? criterion)
           (let lp ((i start))
             (and (< i end)
                  (if (char-in-set? (string-ref str i) criterion) i
                      (lp (+ i 1))))))
          ((procedure? criterion)
           (let lp ((i start))
             (and (< i end)
                  (if (criterion (string-ref str i)) i
                      (lp (+ i 1))))))
          (else (error "Second param is neither char-set, char, or predicate procedure."
                       string-index criterion)))))

(define (string-index-right str criterion #!optional start end)
    (receive (start end)
      (string-start+end str start end)
    (cond ((char? criterion)
           (let lp ((i (- end 1)))
             (and (>= i start)
                  (if (char=? criterion (string-ref str i)) i
                      (lp (- i 1))))))
          ((char-set? criterion)
           (let lp ((i (- end 1)))
             (and (>= i start)
                  (if (char-in-set? (string-ref str i) criterion) i
                      (lp (- i 1))))))
          ((procedure? criterion)
           (let lp ((i (- end 1)))
             (and (>= i start)
                  (if (criterion (string-ref str i)) i
                      (lp (- i 1))))))
          (else (error "Second param is neither char-set, char, or predicate procedure."
                       string-index-right criterion)))))

(define (string-concatenate strings)
  (let* ((total (do ((strings strings (cdr strings))
                     (i 0 (+ i (string-length (car strings)))))
                    ((not (pair? strings)) i)))
         (ans (make-string total)))
    (let lp ((i 0) (strings strings))
      (if (pair? strings)
          (let* ((s (car strings))
                 (slen (string-length s)))
            (string-copy! ans i s 0 slen)
            (lp (+ i slen) (cdr strings)))))
    ans))

(define (string-concatenate-reverse string-list #!optional final end)
  (let* ((final (if (string? final) final ""))
         (end (if (and (integer? end)
                       (exact? end)
                       (<= 0 end (string-length final)))
                  end
                  (string-length final))))
    (let ((len (let lp ((sum 0) (lis string-list))
                 (if (pair? lis)
                     (lp (+ sum (string-length (car lis))) (cdr lis))
                     sum))))

      (%finish-string-concatenate-reverse len string-list final end))))

(define (%finish-string-concatenate-reverse len string-list final end)
  (let ((ans (make-string (+ end len))))
    (string-copy! ans len final 0 end)
    (let lp ((i len) (lis string-list))
      (if (pair? lis)
          (let* ((s   (car lis))
                 (lis (cdr lis))
                 (slen (string-length s))
                 (i (- i slen)))
            (string-copy! ans i s 0 slen)
            (lp i lis))))
    ans))

(define char-set:graphic-no-whitespace
  (char-set-difference char-set:graphic char-set:whitespace))

(define (string-tokenize s #!optional token-chars start end)
  (receive (start end)
      (string-start+end s start end)
    (let ((token-chars (if (char-set? token-chars) token-chars char-set:graphic-no-whitespace)))
      (let lp ((i end) (ans '()))
        (cond ((and (< start i) (string-index-right s token-chars start i)) =>
               (lambda (tend-1)
                 (let ((tend (+ 1 tend-1)))
                   (cond ((string-skip-right s token-chars start tend-1) =>
                          (lambda (tstart-1)
                            (lp tstart-1
                                (cons (substring s (+ 1 tstart-1) tend)
                                      ans))))
                         (else (cons (substring s start tend) ans))))))
              (else ans))))))

(define (string-contains text pattern #!optional start1 end1 start2 end2)
  (receive (t-start t-end p-start p-end)
      (string-start+end+start+end text start1 end1 pattern start2 end2)
    (%kmp-search pattern text char=? p-start p-end t-start t-end)))

(define (string-contains-ci text pattern #!optional start1 end1 start2 end2)
    (receive (t-start t-end p-start p-end)
      (string-start+end+start+end text start1 end1 pattern start2 end2)
    (%kmp-search pattern text char-ci=? p-start p-end t-start t-end)))

(define (%kmp-search pattern text c= p-start p-end t-start t-end)
  (let ((plen (- p-end p-start))
	(rv (make-kmp-restart-vector pattern c= p-start p-end)))

    ;; The search loop. TJ & PJ are redundant state.
    (let lp ((ti t-start) (pi 0)
	     (tj (- t-end t-start)) ; (- tlen ti) -- how many chars left.
	     (pj plen))		 ; (- plen pi) -- how many chars left.

      (if (= pi plen)
	  (- ti plen)			; Win.
	  (and (<= pj tj)		; Lose.
	       (if (c= (string-ref text ti) ; Search.
		       (string-ref pattern (+ p-start pi)))
		   (lp (+ 1 ti) (+ 1 pi) (- tj 1) (- pj 1)) ; Advance.
		   (let ((pi (vector-ref rv pi))) ; Retreat.
		     (if (= pi -1)
			 (lp (+ ti 1) 0  (- tj 1) plen) ; Punt.
			 (lp ti       pi tj       (- plen pi))))))))))

(define (make-kmp-restart-vector pattern #!optional c= start end)
  (receive (start end)
      (string-start+end pattern start end)
    (let ((c= (if (procedure? c=) c= char=?)))
    (let* ((rvlen (- end start))
	   (rv (make-vector rvlen -1)))
      (if (> rvlen 0)
	  (let ((rvlen-1 (- rvlen 1))
		(c0 (string-ref pattern start)))

	    ;; Here's the main loop. We have set rv[0] ... rv[i].
	    ;; K = I + START -- it is the corresponding index into PATTERN.
	    (let lp1 ((i 0) (j -1) (k start))
	      (if (< i rvlen-1)
		  ;; lp2 invariant:
		  ;;   pat[(k-j) .. k-1] matches pat[start .. start+j-1]
		  ;;   or j = -1.
		  (let lp2 ((j j))
		    (cond ((= j -1)
			   (let ((i1 (+ 1 i)))
			     (if (not (c= (string-ref pattern (+ k 1)) c0))
				 (vector-set! rv i1 0))
			     (lp1 i1 0 (+ k 1))))
			  ;; pat[(k-j) .. k] matches pat[start..start+j].
			  ((c= (string-ref pattern k) (string-ref pattern (+ j start)))
			   (let* ((i1 (+ 1 i))
				  (j1 (+ 1 j)))
			     (vector-set! rv i1 j1)
			     (lp1 i1 j1 (+ k 1))))

			  (else (lp2 (vector-ref rv j)))))))))
      rv))))

(define (string-xreplace s1 s2 start1 end1 #!optional start2 end2)
  (receive (start2 end2)
      (string-start+end s2 start2 end2)
    (let* ((slen1 (string-length s1))
	   (sublen2 (- end2 start2))
	   (alen (+ (- slen1 (- end1 start1)) sublen2))
	   (ans (make-string alen)))
      (%string-copy! ans 0 s1 0 start1)
      (%string-copy! ans start1 s2 start2 end2)
      (%string-copy! ans (+ start1 sublen2) s1 end1 slen1)
      ans)))

(define (%string-copy! to tstart from fstart fend)
  (if (> fstart tstart)
      (do ((i fstart (+ i 1))
	   (j tstart (+ j 1)))
	  ((>= i fend))
	(string-set! to j (string-ref from i)))

      (do ((i (- fend 1)                    (- i 1))
	   (j (+ -1 tstart (- fend fstart)) (- j 1)))
	  ((< i fstart))
	(string-set! to j (string-ref from i)))))

(define (string-delete criterion s #!optional start end)
  (receive (start end)
      (string-start+end s start end)
    (if (procedure? criterion)
	(let* ((slen (- end start))
	       (temp (make-string slen))
	       (ans-len (string-fold (lambda (c i)
				       (if (criterion c) i
					   (begin (string-set! temp i c)
						  (+ i 1))))
				     0 s start end)))
	  (if (= ans-len slen) temp (substring temp 0 ans-len)))

	(let* ((cset (cond ((char-set? criterion) criterion)
			   ((char? criterion) (char-set criterion))
			   (else (error "string-delete criterion not predicate, char or char-set" criterion))))
	       (len (string-fold (lambda (c i) (if (char-in-set? c cset)
					      i
					      (+ i 1)))
				 0 s start end))
	       (ans (make-string len)))
	  (string-fold (lambda (c i) (if (char-in-set? c cset)
				    i
				    (begin (string-set! ans i c)
					   (+ i 1))))
		       0 s start end)
	  ans))))
;; added
(define (string-subst str from to)
  (let ((from-len (string-length from))
        (to-len (string-length to)))
    (let loop ((str str)
               (start 0))
      (let ((pos (string-contains str from start)))
        (if (not pos) str
            (loop (string-xreplace str to pos (+ pos from-len))
                  (+ pos to-len)))))))

;;;; end of srfi-13
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
  (let* ((n (or port-number (+ 10000 (random-integer 50000)))) ;; TODO: pass 0, then somehow read the actual port from the socket
         (socket (open-tcp-server-socket n (host-address-loopback))))
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
(define (procedure-parameters symbol env)
  (let ((type (environment-reference-type env symbol)))
    (let ((ans (if (eq? type 'normal)
                   (let ((binding (environment-lookup env symbol)))
                     (if (and binding
                              (procedure? binding))
                         (cons symbol (read-from-string (string-trim (with-output-to-string
                                                                       (lambda () (pa binding))))))
                         #f))
                   #f)))
      ans)))
(define ($binding-documentation p)
  ;; same as (inspect object), then hitting c
  #f)
(define ($function-parameters-and-documentation name)
  ;; TODO
  (let ((binding #f))
    (cons (procedure-parameters (string->symbol name) ($environment param:environment))
          ($binding-documentation binding))))

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
