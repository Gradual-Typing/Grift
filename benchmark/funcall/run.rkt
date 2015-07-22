#lang typed/racket


;; freshly compile the c tests
(c-compile "direct.c")
(c-compile "regIndirect.c")
(c-compile "stkIndirect.c")
(c-compile "memIndirect.c")

;; freshly compile the schml varients
(schml-compile "static.schml")
(schml-compile "wrapped.schml")
(schml-compile "dynamic.schml")

;; freshly compiler the gambit contender
#;
(let* ([opts " -prelude \"(declare (standard-bindings)) (declare (block)) "]
       [opts (string-append opts " -exe -cc-options -O3 ")]))
  (system "gsc ~a -exe src/gambit.scm -o tmp/bin/gambit.scm" opts f)


(newline)
(display "Everything is compiled. Let's go.")
(newline)

(: run-benchmark (Natural -> (String -> (Listof String))))
(define ((run-benchmark n) b)
  (for/list ([i (in-range n)])
    (let* ([out (with-output-to-string
                  (lambda () (system (string-append "tmp/bin/" b))))]
           [m*? (regexp-match #px"\\d+.\\d+" out)]
           [m*  (or m*? '())])
      ;;(display (format "~a ~a ~a\n" b i out))
      (if  (or (null? m*)
               (not (car m*)))
           (format "failure in benchmark ~a iteration ~a" b i)
           (car m*)))))

(: make-datum (Natural -> (String -> (Vector Real Real))))
(define ((make-datum n) d)
  (let ([f (string->number d)])
    (unless (real? f) (error 'make-datum "given non number ~a" d))
    (vector n f)))

(: run-benchmark* (Natural (Listof String) -> (Listof (Vector Real Real))))
(define (run-benchmark* count b*)
  (for/fold ([acc : (Listof (Vector Real Real)) '()])
            ([b : String (in-list b*)]
             [i : Natural (in-naturals)])
    (let* ([d* : (Listof String) ((run-benchmark count) b)]
           [d* : (Listof (Vector Real Real)) (map (make-datum i) d*)])
      (printf "~a -> ~a\n" b i)
      (append d* acc))))

(define data
  (run-benchmark*
   10
   (list "direct.c 1000000"
         "regIndirect.c 1000000"
         "stkIndirect.c 1000000"
         "memIndirect.c 1000000"
         "static.schml"
         "wrapped.schml"
         "dynamic.schml"
         #;"gambit.scm"
         )))

(require plot/typed)

(plot (points data)
      #:x-min -0.5
      #:x-max 7.5
      #:y-min 0
      #:y-max 0.2
      #:width 500
      #:height 700
      #:title "Summary of Results"
      #:x-label "benchmark #"
      #:y-label "time (sec)"
      #:out-file "plot.png")
