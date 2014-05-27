#lang racket
(require rackunit rackunit/text-ui
         Schml/framework/paths
         Schml/compiler/compile
         Schml/framework/build-compiler
         Schml/testing/ast-interps/insert-implicit-casts)

(provide (all-defined-out))

(define config 
  (make-parameter 
   (compiler-config #f #f 'none 'none)))

(define-syntax stdcc
  (syntax-rules ()
    ((_ cc p ... n)
     (interp (cc (simplify-path (build-path test-suite-path p ... n)) (config))
             (config)))))

(define-syntax test-equal?/exn
  (syntax-rules ()
    ((_ name test expect)
     (test-equal? name
      (with-handlers 
          ([exn? (lambda (temp)
                   ((error-display-handler) (exn-message temp) temp)
                   (raise temp))])
        test)
      expect))))

(define-syntax test-file
  (syntax-rules ()
    ((_ cc p ... n e)
     (test-equal?/exn n (stdcc cc p ... n) e))))

(define ld-tests
  (test-suite "lazy downcast"
    (test-suite "valid"
      (test-file compiler "compiler" "const-false.schml" #f)
      (test-file compiler "compiler" "const-true.schml" #t)
      (test-file compiler "compiler" "const-one.schml" 1)
      (test-file compiler "compiler" "const-ninetynine.schml" 99)
      (test-file compiler "compiler" "const-larg-int.schml" 123456)
      (test-file compiler "compiler" "prim-minus.schml" 80)
      (test-file compiler "compiler" "prim-lt.schml" #f)
      (test-file compiler "compiler" "prim-gt.schml" #t)
      (test-file compiler "compiler" "prim-plus.schml" 120)
      (test-file compiler "compiler" "prim-times.schml" 2000)
      (test-file compiler "compiler" "lambda1.schml" 'procedure)
      (test-file compiler "compiler" "lambda2.schml" 'procedure)
      (test-file compiler "compiler" "lambda3.schml" 'procedure)
      (test-file compiler "compiler" "lambda4.schml" 'procedure)
      (test-file compiler "compiler" "lambda5.schml" 'procedure)
      (test-file compiler "compiler" "lambda6.schml" 'procedure)
      (test-file compiler "compiler" "lambda7.schml" 'procedure)
      (test-file compiler "compiler" "lambda8.schml" 'procedure)
      (test-file compiler "compiler" "let0.schml" #t)
      (test-file compiler "compiler" "let1.schml" 2)
      (test-file compiler "compiler" "let2.schml" #f)
      (test-file compiler "compiler" "let3.schml" 1)
      (test-file compiler "compiler" "let4.schml" 2)
      (test-file compiler "compiler" "let5.schml" 4)
      (test-file compiler "compiler" "let6.schml" 0)
      (test-file compiler "compiler" "let7.schml" 'dynamic)
      (test-file compiler "compiler" "let8.schml" 0)
      (test-file compiler "compiler" "let9.schml" 100)
      (test-file compiler "compiler" "let10.schml" 'dynamic)
      (test-file compiler "compiler" "let11.schml" #f)
      (test-file compiler "compiler" "let12.schml" 'dynamic)
      (test-file compiler "compiler" "let13.schml" 'dynamic)
      (test-file compiler "compiler" "let14.schml" 5)
      (test-file compiler "compiler" "let15.schml" 'procedure)
      (test-file compiler "compiler" "let16.schml" 7)
      (test-file compiler "compiler" "let17.schml" #f)
      (test-file compiler "compiler" "let18.schml" #f)
      (test-file compiler "compiler" "let19.schml" #f)
      (test-file compiler "compiler" "let20.schml" #t)
      (test-file compiler "compiler" "if0.schml" 0)
      (test-file compiler "compiler" "if1.schml" 0)
      (test-file compiler "compiler" "if2.schml" 1)
      (test-file compiler "compiler" "if3.schml" 4)
      
      (test-file compiler "compiler" "fact5.schml" 120)
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

     
