#lang racket
;; The testing framework is untyped because rackunit
;; is not working in typed racket
(require rackunit 
	 rackunit/text-ui
         Schml/framework/build-compiler
         Schml/framework/errors
	 Schml/testing/values)
(provide (all-defined-out))

;; sets up a variable with the path to the test suite
(require pkg/lib)
(define schml-path (pkg-directory "Schml"))
(define testing-path (build-path schml-path "testing"))
(define test-suite-path (build-path testing-path "suite"))


(define compiler-config 
  (make-parameter
   (Config 'Lazy-D)))

(struct success ())
(struct failed (obj))

(define (compile path expected)
  ;; The micro compilers
  (local-require Schml/compiler/schml/reduce-to-cast-calculus
		 Schml/compiler/casts/impose-cast-semantics
		 Schml/compiler/closures/make-closures-explicit
		 )
  ;; The intermediary interpreters
  (local-require Schml/testing/ast-interps/cast-lang-interp
		 Schml/testing/ast-interps/lambda-lang-interp
		 Schml/testing/ast-interps/data-lang-interp
		 )
  (let ((config (compiler-config)))
    (with-handlers ([exn:schml:type:static? 
		     (lambda (e) 
		       (begin 
			 (check value=? (blame #t (exn-message e)) expected)
			 (success)))])
      (let* ([c0  (reduce-to-cast-calculus path config)]
	     [_   (check value=? (cast-lang-interp c0 config) expected)]
	     [l0  (impose-cast-semantics c0 config)]
	     [_   (check value=? (lambda-lang-interp l0 config) expected)]
	     [d0  (make-closures-explicit l0 config)]
	     [_   (check value=? (data-lang-interp d0 config) expected)])
	(success)))))

(define-syntax compile-test
  (syntax-rules ()
    ((_ p ... n e)
     (test-pred
      n
      success?
      (compile 
	 (simplify-path (build-path test-suite-path p ... n))
	 e)))))

(define test-data-base
  (test-suite 
   "all tests"
   (compile-test "const-false.schml" (boole #f))
   (compile-test "const-true.schml" (boole #t))
   (compile-test "const-one.schml" (integ 1))
   (compile-test "const-ninetynine.schml" (integ 99))
   (compile-test "const-larg-int.schml" (integ 123456))
   (compile-test "prim-minus.schml" (integ 80))
   (compile-test "prim-lt.schml" (boole #f))
   (compile-test "prim-gt.schml" (boole #t))
   (compile-test "prim-plus.schml" (integ 120))
   (compile-test "prim-times.schml" (integ 2000))
   (compile-test "lambda1.schml" (function))
   (compile-test "lambda2.schml" (function))
   (compile-test "lambda3.schml" (function))
   (compile-test "lambda4.schml" (function))
   (compile-test "lambda5.schml" (function))
   (compile-test "lambda6.schml" (function))
   (compile-test "lambda7.schml" (function))
   (compile-test "lambda8.schml" (function))
   (compile-test "let0.schml" (boole #t))
   (compile-test "let1.schml" (integ 2))
   (compile-test "let2.schml" (boole #f))
   (compile-test "let3.schml" (integ 1))
   (compile-test "let4.schml" (integ 2))
   (compile-test "let5.schml" (integ 4))
   (compile-test "let6.schml" (integ 0))
   (compile-test "let7.schml" (dynamic))
   (compile-test "let8.schml" (integ 0))
   (compile-test "let9.schml" (integ 100))
   (compile-test "let10.schml" (dynamic))
   (compile-test "let11.schml" (boole #f))
   (compile-test "let12.schml" (dynamic))
   (compile-test "let13.schml" (dynamic))
   (compile-test "let14.schml" (integ 5))
   (compile-test "let15.schml" (function))
   (compile-test "let16.schml" (integ 7))
   (compile-test "let17.schml" (boole #f))
   (compile-test "let18.schml" (boole #f))
   (compile-test "let19.schml" (boole #f))
   (compile-test "let20.schml" (boole #t))
   (compile-test "if0.schml" (integ 0))
   (compile-test "if1.schml" (integ 0))
   (compile-test "if2.schml" (integ 1))
   (compile-test "if3.schml" (integ 4))
   (compile-test "fact5.schml" (integ 120))
   (compile-test "blame1.schml" (integ 2))
   (compile-test "blame2.schml" (blame #t "Right"))
   (compile-test "blame3.schml" (blame #f "Correct"))
   (compile-test "blame4.schml" (blame #t #f))
   (compile-test "blame5.schml" (blame #f (not-lbl "Fail")))
   (compile-test "blame6.schml" (blame #f (not-lbl "Fail")))
   (compile-test "blame7.schml" (integ 2))
   (compile-test "blame8.schml" (blame #t #f))
   (compile-test "blame9.schml" (blame #t "Pass"))
   ;; Multi  arg function
   (compile-test "blame10.schml" (blame #f (not-lbl "Fail")))
   (compile-test "blame11.schml" (blame #f (not-lbl "Fail")))
   (compile-test "blame12.schml" (blame #f "Pass"))
   (compile-test "blame13.schml" (blame #f "Pass"))
   ))
      ;;(test-file compiler "compiler" "simple-map.schml" 3)
      
(module+ main
  (let ((cast-semantics (make-parameter 'Lazy-D))
	(get-ls (lambda (p) (let ((p (p))) (if (pair? p) p '())))))
    (command-line 
     #:program "Schml-compiler-tests"
     #:args ()
     (parameterize 
	 ([compiler-config (Config (cast-semantics))])
       (run-tests test-data-base 'verbose)))))

