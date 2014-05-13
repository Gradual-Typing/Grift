#lang racket
(require rackunit rackunit/text-ui
         Schml/framework/paths
         Schml/compiler/compile
         Schml/framework/build-compiler)

(provide run-tests
         all-tests
         compiler-tests
         srcloc-tests
         config)

(define config 
  (make-parameter 
   (compiler-config 'ld 'none 'none)))

(define-syntax stdcc
  (syntax-rules ()
    ((_ cc p n)
     (cc (simplify-path (build-path test-suite-path p n)) (config)))))

(define-syntax test-file
  (syntax-rules ()
    ((_ cc p n) (test-not-exn n (lambda () (stdcc cc p n))))))

(define compiler-tests
  (test-suite "compiler"
    (test-suite "valid"
      (test-file compiler "dev-valid" "1.schml")
      (test-file compiler "dev-valid" "2.schml")
      (test-file compiler "dev-valid" "3.schml")
      (test-file compiler "dev-valid" "4.schml")
      (test-file compiler "dev-valid" "5.schml"))))

(define srcloc-tests
  (test-suite "source locations"
    ;; Unit tests exploring and testing the behavior
    ;; of source locations in the read and parse passes
    (let ()
      (local-require Schml/language/syntax
                     Schml/compiler/read
                     Schml/compiler/parse
                     syntax/srcloc)
      (let* ((cc read)
             (cc (lambda (f)
                   (cc
                    (simplify-path
                     (build-path test-suite-path "dev-valid" f))
                    (config)))))
        (test-suite "read"
          (test-pred "source" string?
                     (syntax-source
                      (car (File-stx* (cc "4.schml")))))
          (test-pred "line"  number?
                     (syntax-line
                      (car (File-stx* (cc "4.schml")))))
          (test-pred "column" number?
                     (syntax-column
                      (car (File-stx* (cc "4.schml")))))
          (test-false "source-location?"
                      (source-location?
                       (syntax-source
                        (car (File-stx* (cc "4.schml")))))))))))

(define all-tests
  (test-suite "All"
             compiler-tests
             srcloc-tests))

(module+ main 
  (let ((trace (make-parameter 'none))
        (check (make-parameter 'none))
        (cast-sem (make-parameter 'ld))
        (get-ls (lambda (p) (let ((p (p))) (if (pair? p) p '())))))
    (command-line #:program "Schml-compiler-tests"
                  #:once-any
                  [("-v" "--trace-all") 
                   "Print the input and output to passes"
                   (trace 'all)]
                  #:multi
                  [("-t" "--trace-passes") str-pass
                   "Same but only turns on tracing for a pass"
                   (trace (cons (string->symbol str-pass) (get-ls trace)))]
                  #:once-any
                  [("-v" "--trace-all") 
                   "Run type checks for data between passes"
                   (check 'all)]
                  #:multi
                  [("-c" "--trace-passes") str-pass
                   "Same but only turns on checking for a specific pass"
                   (check (cons (string->symbol str-pass) (get-ls check)))]
                  #:args ()
                  (parameterize 
                   ([config (compiler-config (cast-sem) (trace) (check))])
                   (run-tests all-tests)))))

     
