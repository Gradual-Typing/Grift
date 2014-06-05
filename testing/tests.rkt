#lang racket
(require rackunit rackunit/text-ui
         Schml/framework/paths
         Schml/framework/build-compiler)

(provide (all-defined-out))

(define config 
  (make-parameter 
   (compiler-config #f #f 'none 'none)))

(define (test-compiler path config expected)
  (local-require Schml/compiler/read
                 Schml/compiler/parse
                 Schml/compiler/type-check
                 Schml/compiler/insert-implicit-casts
                 (prefix-in iic: Schml/testing/ast-interps/insert-implicit-casts)
                 Schml/compiler/closures/make-closures-explicit)
  (compose-compiler
   (path config)
   read parse type-check insert-implicit-casts
   (ast-> ast (check-equal? (iic:interp ast config) expected))
   make-closures-explicit))

(define-syntax test-compile
  (syntax-rules ()
    ((_ p ... n e)
     (test-case n
       (with-handlers
           ([exn? (lambda (temp)
                    ((error-display-handler) (exn-message temp) temp)
                    (raise temp))])
         (test-compiler
          (simplify-path (build-path test-suite-path p ... n))
          (config)
          e))))))

(define ld-tests
  (test-suite "lazy downcast"
    (test-suite "valid"
      (test-compile "const-false.schml" #f)
      (test-compile "const-true.schml" #t)
      (test-compile "const-one.schml" 1)
      (test-compile "const-ninetynine.schml" 99)
      (test-compile "const-larg-int.schml" 123456)
      (test-compile "prim-minus.schml" 80)
      (test-compile "prim-lt.schml" #f)
      (test-compile "prim-gt.schml" #t)
      (test-compile "prim-plus.schml" 120)
      (test-compile "prim-times.schml" 2000)
      (test-compile "lambda1.schml" 'procedure)
      (test-compile "lambda2.schml" 'procedure)
      (test-compile "lambda3.schml" 'procedure)
      (test-compile "lambda4.schml" 'procedure)
      (test-compile "lambda5.schml" 'procedure)
      (test-compile "lambda6.schml" 'procedure)
      (test-compile "lambda7.schml" 'procedure)
      (test-compile "lambda8.schml" 'procedure)
      (test-compile "let0.schml" #t)
      (test-compile "let1.schml" 2)
      (test-compile "let2.schml" #f)
      (test-compile "let3.schml" 1)
      (test-compile "let4.schml" 2)
      (test-compile "let5.schml" 4)
      (test-compile "let6.schml" 0)
      (test-compile "let7.schml" 'dynamic)
      (test-compile "let8.schml" 0)
      (test-compile "let9.schml" 100)
      (test-compile "let10.schml" 'dynamic)
      (test-compile "let11.schml" #f)
      (test-compile "let12.schml" 'dynamic)
      (test-compile "let13.schml" 'dynamic)
      (test-compile "let14.schml" 5)
      (test-compile "let15.schml" 'procedure)
      (test-compile "let16.schml" 7)
      (test-compile "let17.schml" #f)
      (test-compile "let18.schml" #f)
      (test-compile "let19.schml" #f)
      (test-compile "let20.schml" #t)
      (test-compile "if0.schml" 0)
      (test-compile "if1.schml" 0)
      (test-compile "if2.schml" 1)
      (test-compile "if3.schml" 4)
      (test-compile "fact5.schml" 120)

      ;; pretty printer doesn't like this currently
      ;; and it isn not yet type safe
      ;;(test-file compiler "compiler" "simple-map.schml" 3)
      )))

(define all-tests
  (test-suite "All" ld-tests))

(module+ main 
  (let ((trace (make-parameter 'none))
        (check (make-parameter 'none))
        (cast-strictness (make-parameter 'lazy))
        (cast-blame (make-parameter 'downcast))
        (get-ls (lambda (p) (let ((p (p))) (if (pair? p) p '())))))
    (command-line #:program "Schml-compiler-tests"
                  #:once-any
                  [("-v" "--trace-all") 
                   "Print the input and output to passes"
                   (trace 'all)]
                  #:multi
                  [("-t" "--trace-pass") str-pass
                   "Same but only turns on tracing for a pass"
                   (trace (cons (string->symbol str-pass) (get-ls trace)))]
                  #:once-any
                  [("-s" "--check-all") 
                   "Run type checks for data between passes"
                   (check 'all)]
                  #:multi
                  [("-c" "--check-pass") str-pass
                   "Same but only turns on checking for a specific pass"
                   (check (cons (string->symbol str-pass) (get-ls check)))]
                  #:args ()
                  (parameterize 
                   ([config (compiler-config (cast-strictness)
                                             (cast-blame)
                                             (trace)
                                             (check))])
                   (run-tests all-tests 'verbose)))))

     
