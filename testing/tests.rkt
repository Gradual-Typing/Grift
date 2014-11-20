#lang typed/racket/base/no-check ;; Not actually type-checked

(require rackunit 
	 rackunit/text-ui
	 schml/testing/values
         schml/testing/test-compile)

(provide (all-defined-out))

;; sets up a variable with the path to the test suite
(require pkg/lib)
(define schml-path (pkg-directory "schml"))
(define test-path (build-path schml-path "testing"))
(define test-suite-path (build-path test-path "suite"))
(define test-tmp-path (build-path test-path "tmp")) 

(define-syntax test-file
  (syntax-rules ()
    ((_ p ... n e)
     (ann (test-case n (test-compile (simplify-path (build-path test-suite-path p ... n)) e)) Test))))

(define test-data-base : Test
  (test-suite 
   "all tests"
   ;; Syntax Tests
   ;; Base Line tests
   (test-file "const-false.schml" (boole #f))
   (test-file "const-true.schml" (boole #t))
   (test-file "const-one.schml" (integ 1))
   (test-file "const-ninetynine.schml" (integ 99))
   (test-file "const-larg-int.schml" (integ 123456))
   (test-file "prim-minus.schml" (integ 80))
   (test-file "prim-lt.schml" (boole #f))
   (test-file "prim-gt.schml" (boole #t))
   (test-file "prim-plus.schml" (integ 120))
   (test-file "prim-times.schml" (integ 2000))
   ;; Specific Features
   (test-file "lambda1.schml" (function))
   (test-file "lambda2.schml" (function))
   (test-file "lambda3.schml" (function))
   (test-file "lambda4.schml" (function))
   (test-file "lambda5.schml" (function))
   (test-file "lambda6.schml" (function))
   (test-file "lambda7.schml" (function))
   (test-file "lambda8.schml" (function))
   (test-file "let0.schml" (boole #t))
   (test-file "let1.schml" (integ 2))
   (test-file "let2.schml" (boole #f))
   (test-file "let3.schml" (integ 1))
   (test-file "let4.schml" (integ 2))
   (test-file "let5.schml" (integ 4))
   (test-file "let6.schml" (integ 0))
   (test-file "let7.schml" (dynamic))
   (test-file "let8.schml" (integ 0))
   (test-file "let9.schml" (integ 100))
   (test-file "let10.schml" (dynamic))
   (test-file "let11.schml" (boole #f))
   (test-file "let12.schml" (dynamic))
   (test-file "let13.schml" (dynamic))
   (test-file "let14.schml" (integ 5))
   (test-file "let15.schml" (function))
   (test-file "let16.schml" (integ 7))
   (test-file "let17.schml" (boole #f))
   (test-file "let18.schml" (boole #f))
   (test-file "let19.schml" (boole #f))
   (test-file "let20.schml" (boole #t))
   (test-file "if0.schml" (integ 0))
   (test-file "if1.schml" (integ 0))
   (test-file "if2.schml" (integ 1))
   (test-file "if3.schml" (integ 4))
   (test-file "blame1.schml" (integ 2))
   (test-file "blame2.schml" (blame #t "Right"))
   (test-file "blame3.schml" (blame #f "Correct"))
   (test-file "blame4.schml" (blame #t #f))
   (test-file "blame5.schml" (blame #f (not-lbl "Fail")))
   (test-file "blame6.schml" (blame #f (not-lbl "Fail")))
   (test-file "blame7.schml" (integ 2))
   (test-file "blame8.schml" (blame #t #f))
   (test-file "blame9.schml" (blame #t "Pass"))
   ;; Multi  arg function
   (test-file "blame10.schml" (blame #f (not-lbl "Fail")))
   (test-file "blame11.schml" (blame #f (not-lbl "Fail")))
   (test-file "blame12.schml" (blame #f "Pass"))
   (test-file "blame13.schml" (blame #f "Pass"))
   ;; basic computations
   (test-file "fact5.schml" (integ 120))
   (test-file "fact-dyn-6.schml" (integ 720))
   (test-file "fact-static-6.schml" (integ 720))
   ))

(module+ main
  (unless (directory-exists? test-tmp-path)
    (make-directory test-tmp-path))
  (parameterize ([compiler-config (Config 'Lazy-D
                                          (build-path test-tmp-path "t.out")
                                          (build-path test-tmp-path "t.c"))])
    (run-tests test-data-base 'verbose)))

