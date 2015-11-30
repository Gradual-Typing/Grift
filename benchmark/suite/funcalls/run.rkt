#lang racket

(define this-dir
  (path-only (path->complete-path (find-system-path 'run-file))))


(define-syntax-rule (path dir base  ... ext)
  (build-path this-dir dir (string-append base ... ext)))

(define-syntax-rule (file dir base  ... ext)
  (path->string (path dir base ... ext)))


;; freshly compile the c tests
(define (c-compile base)
  (define out (open-output-string))
  (define src (file "src" base ".c"))
  (define asm (file "tmp" base ".s"))
  (define exe (file "exe" base ".out"))
  (parameterize ([current-output-port out]
                 [current-error-port out])
    (unless (or  (system (format "cc ~a -O3 -o ~a" src exe))
                 (system (format "cc ~a -O3 -S -o ~a" src asm)))
      (raise (exn (format "compile-c ~a:" base (get-output-string))
                  (current-continuation-marks))))))

(c-compile "c-direct")
(c-compile "c-regIndirect")
(c-compile "c-stkIndirect")
(c-compile "c-memIndirect")

;; and the schml src files
(require "../../../src/compile.rkt")
(define (schml-compile base)
  (with-output-to-string
    (lambda ()
      (compile (path "src" base ".schml")
           #:cast-rep 'Twosomes
           #:output (path "exe" base "-twosomes" ".out")
           #:keep-c (path "tmp" base "-twosomes" ".c")
           #:keep-a (path "tmp" base "-twosomes" ".s")
           #:cc-opt "-w -O3"
           #:mem (* 4096 100000))
      (compile (path "src" base ".schml")
               #:cast-rep 'Coercions
               #:output (path "exe" base "-coercions" ".out")
               #:keep-c (path "tmp" base "-coercions" ".c")
               #:keep-a (path "tmp" base "-coercions" ".s")
               #:cc-opt "-w -O3"
               #:mem (* 4096 100000))
      (void))))

(schml-compile "gtlc-static")
(schml-compile "gtlc-wrapped")
(schml-compile "gtlc-dynamic")

(define (check-for x)
  (with-output-to-file "/dev/null" #:exists 'append
    (lambda () (system (format "which ~a" x)))))

(define (gambit-compile exe base)
  (system
   (format
    (string-append
     exe " -o exe/~a.out"
     " -prelude \"(declare (standard-bindings)) (declare (block))\" "
     " -cc-options -O3 -exe src/~a.scm")
    base base)))

(define gambit? #f)
(when gambit?
  (cond
    [(check-for "gsc")
     (gambit-compile "gsc" "gambit-dynamic")]
    [(check-for "gambitc")
     (gambit-compile "gambitc" "gambit-dynamic")]
    [else (set! gambit? #f) (display "omitting gambit tests")]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(newline)
(display "Everything is compiled. Let's go.")
(newline)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Run a single benchmark n times
(define (run-benchmark n b)
  (for/list ([i (in-range n)])
    (let* ([out (open-output-string)]
           [suc (parameterize ([current-output-port out]
                               [current-error-port out])
                  (system b))]
           [out (get-output-string out)])
      (unless suc
        (error 'run-benchmark "~a failed with ~a" b out))
      out)))

;; run a list of benchmarks
(define (run-benchmark* count b*)
  (define (append-strings a*)
    (apply string-append (map (lambda (a) (format " ~a " a)) a*)))
  (for/list ([b b*])
    (match-let ([`(,parse ,base . ,args) b])
      (let* ([a* (append-strings args)]
             [c  (file "exe" base ".out")]
             [s* (run-benchmark count (string-append c " " a*))]
             [d* (map (parse b) s*)])
        (cons base d*)))))

(define ((parse-c b) s)
  (let* ([rx #px"^time \\(sec\\): (\\d+.\\d+)\n$"]
         [m? (regexp-match rx s)])
    (unless m?
      (error 'parse-c "failed to parse ~a with ~a" b s))
    (cadr m?)))

(define ((parse-schml b) s)
  (let* ([rx #px"^time \\(sec\\): (\\d+.\\d+)\nUnit : \\(\\)\n$"]
         [m? (regexp-match rx s)])
    (unless m?
      (error 'parse-schml "failed to parse ~a with ~a" b s))
    (cadr m?)))

(define benchmarks
  `((,parse-c "c-direct" 1000000)
    (,parse-c "c-regIndirect" 1000000)
    (,parse-c "c-stkIndirect" 1000000)
    (,parse-c "c-memIndirect" 1000000)
    (,parse-schml "gtlc-static-twosomes")
    (,parse-schml "gtlc-static-coercions")
    (,parse-schml "gtlc-wrapped-twosomes")
    (,parse-schml "gtlc-wrapped-coercions")
    (,parse-schml "gtlc-dynamic-twosomes")
    (,parse-schml "gtlc-dynamic-coercions")
    ,@(if gambit? '((,parse-gambit "gambit-dynamic")) '())))

(define data (run-benchmark* 10 benchmarks))

(define current-hash
  (or (let ([out (open-output-string)])
        (parameterize ([current-output-port out])
          (and (system "git rev-parse --short HEAD")
               (let ((s (get-output-string out)))
                 ;; get rid of a trailing newline
                 (substring s 0 (- (string-length s) 1))))))
      "log"))

(define log-file (build-path this-dir (format "logs/~a.csv" current-hash)))

(define log (open-output-file log-file #:exists 'replace))


(for* ([b (in-list data)]
       [r (in-list (cdr b))])
  (fprintf log "~a, ~a\n" (car b) r))

(printf "wrote results to:\n\t~a\n" (path->string log-file))
