#lang racket
(require rackunit rackunit/text-ui
         Schml/framework/paths
         Schml/compiler/compile
         Schml/framework/build-compiler)

(provide (all-defined-out))

(define config 
  (make-parameter 
   (compiler-config #f #f 'none 'none)))

(define-syntax stdcc
  (syntax-rules ()
    ((_ cc p ... n)
     (cc (simplify-path (build-path test-suite-path p ... n)) (config)))))

(define-syntax test-pred/exn
  (syntax-rules ()
    ((_ n p t)
     (test-pred n p 
      (with-handlers 
	  ([exn? 
	    (lambda (e)
	      ((error-display-handler) (exn-message e) e)
	      (fail))])
	t)))))

(define-syntax test-file
  (syntax-rules ()
    ((_ cc p n) (test-pred/exn n (lambda a #t) (stdcc cc p n)))))

(define ld-tests
  (test-suite "lazy downcast"
    (test-suite "valid"
      (test-file compiler "compiler" "const-bool.schml") 
      (test-file compiler "compiler" "const-false.schml")
      (test-file compiler "compiler" "const-true.schml")
      (test-file compiler "compiler" "const-int.schml")
      (test-file compiler "compiler" "const-one.schml")
      (test-file compiler "compiler" "const-ninetynine.schml")
      (test-file compiler "compiler" "const-larg-int.schml")
      (test-file compiler "compiler" "prim-minus.schml")
      (test-file compiler "compiler" "prim-lt.schml")
      (test-file compiler "compiler" "prim-gt.schml")
      (test-file compiler "compiler" "prim-plus.schml")
      (test-file compiler "compiler" "prim-times.schml")
      (test-file compiler "compiler" "lambda1.schml")
      (test-file compiler "compiler" "lambda2.schml")
      (test-file compiler "compiler" "lambda3.schml")
      (test-file compiler "compiler" "lambda4.schml")
      (test-file compiler "compiler" "lambda5.schml")
      (test-file compiler "compiler" "lambda6.schml")
      (test-file compiler "compiler" "lambda7.schml")
      (test-file compiler "compiler" "lambda8.schml")
      (test-file compiler "compiler" "let0.schml")
      (test-file compiler "compiler" "let1.schml")
      (test-file compiler "compiler" "let2.schml")
      (test-file compiler "compiler" "let3.schml")
      (test-file compiler "compiler" "let4.schml")
      (test-file compiler "compiler" "let5.schml")
      (test-file compiler "compiler" "let6.schml")
      (test-file compiler "compiler" "let7.schml")
      (test-file compiler "compiler" "let8.schml")
      (test-file compiler "compiler" "let9.schml")
      (test-file compiler "compiler" "let10.schml")
      (test-file compiler "compiler" "let11.schml")
      (test-file compiler "compiler" "let12.schml")
      (test-file compiler "compiler" "let13.schml")
      ;; (test-file compiler "compiler" "if0.schml")
      ;; (test-file compiler "compiler" "if1.schml")
      ;; (test-file compiler "compiler" "if2.schml")
      ;; (test-file compiler "compiler" "if3.schml")
      ;; (test-file compiler "compiler" "if4.schml")
      ;; (test-file compiler "compiler" "if5.schml")
      ;; (test-file compiler "compiler" "if6.schml")
      ;; (test-file compiler "compiler" "if7.schml")
      ;; (test-file compiler "compiler" "if8.schml")
      ;; (test-file compiler "compiler" "if9.schml")
      ;; (test-file compiler "compiler" "if10.schml")
      ;; (test-file compiler "compiler" "if11.schml")
      ;; (test-file compiler "compiler" "if12.schml")
      ;; (test-file compiler "compiler" "if13.schml")
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

     
