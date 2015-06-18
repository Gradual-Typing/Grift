#lang typed/racket

(require schml/src/compile)

(define (c-compile [f : String])
  (system (format "cc src/c/~a -O3 -o tmp/bin/~a" f f))
  #;(system (format "mv tmp/bin/tmp tmp/bin/~a" f))
  (system (format "cc src/c/~a -O3 -S -o tmp/asm/~a.s" f f)))

(define (schml-compile [f : String])
  (let ([p (build-path (format "src/schml/~a" f))])
    (compile/conf p
                  (Config p
                          'Lazy-D
                          (build-path (format "tmp/bin/~a" f))
                          (build-path (format "tmp/c/~a.c" f))
                          #t
                          '()
                          (build-path (format "tmp/asm/~a.s" f))))))

(c-compile "loop.c")
(c-compile "direct.c")
(c-compile "regIndirect.c")
(c-compile "stackIndirect.c")

;; freshly compile all c files
#;(system "cc src/c/loop.c -O3 -o tmp/bin/loop.c")
#;(system "cc src/c/direct.c -O3 -o tmp/bin/direct.c")
#;(system "cc src/c/regIndirect.c -O3 -o tmp/bin/regIndirect.c")
#;(system "cc src/c/stackIndirect.c -O3 -o tmp/bin/stackIndirect.c")

;; save the assembly for easy checking
#;(system "cc src/c/loop.c -O3 -S -o tmp/asm/loop.c.s")
#;(system "cc src/c/direct.c -O3 -S -o tmp/asm/direct.c.s")
#;(system "cc src/c/regIndirect.c -S -O3 -o tmp/asm/regIndirect.c.s")
#;(system "cc src/c/stackIndirect.c -O3 -S -o tmp/asm/stackIndirect.c.s")

;; freshly compile the schml varients
(schml-compile "loop.schml")
(schml-compile "static.schml")
(schml-compile "wrapped.schml")
(schml-compile "dynamic.schml")

(newline)
(display "Everything is compiled. Let go.")
(newline)

(: run-benchmark (Integer -> (String -> (Listof String))))
(define ((run-benchmark i) b)
  (if (= i 0)
      '()
      (let* ([out (with-output-to-string (lambda () (system (string-append "tmp/bin/" b))))]
             [m*? (regexp-match #px"\\d+.\\d+" out)]
             [m*  (or m*? '())])
        (display (format "~a ~a\n" b out))
        (cond
          [(null? m*)          ((run-benchmark (sub1 i)) b)]
          [(not (car m*))      ((run-benchmark (sub1 i)) b)]
          [else (cons (car m*) ((run-benchmark (sub1 i)) b))]))))

(: make-datum (Integer -> (String -> (Vector Real Real))))
(define ((make-datum i) d)
  (let ([n (string->number d)])
    (unless (real? n) (error 'make-datum "given non number ~a" d))
    (vector i n)))

(: run-benchmark* (Integer (Listof String) -> (Listof (Vector Real Real))))
(define (run-benchmark* count b*)
  (for/fold ([acc : (Listof (Vector Real Real)) '()])
            ([b : String (in-list b*)]
             [i : Integer (in-naturals)])
    (let* ([d* : (Listof String) ((run-benchmark count) b)]
           [d* : (Listof (Vector Real Real)) (map (make-datum i) d*)])
      (append d* acc))))

(define data
  (run-benchmark*
   10
   (list "loop.c 1000000"
         "direct.c 1000000"
         "regIndirect.c 1000000"
         "stackIndirect.c 1000000"
         "loop.schml"
         "static.schml"
         "wrapped.schml"
         "dynamic.schml")))

(require plot/typed)
(plot (points data)
      #:x-min -0.5
      #:x-max 7.5
      #:y-min 0
      #:y-max 0.5
      #:width 500
      #:height 500
      #:title "Summary of Results"
      #:x-label "benchmark #"
      #:y-label "time (sec)"
      #:out-file "plot.png")
