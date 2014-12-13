#lang typed/racket/base/no-check ;; Not actually type-checked

(require rackunit 
	 rackunit/text-ui
	 schml/testing/values
         schml/testing/test-compile
         schml/testing/paths
         schml/src/helpers)

(provide (all-defined-out))

;; sets up a variable with the path to the test suite

(define-syntax test-file
  (syntax-rules ()
    ((_ p ... n e)
     (ann (test-case n (test-compile (simplify-path (build-path test-suite-path p ... n)) e)) Test))))

(define test-data-base : Test
  (test-suite 
   "all tests"
   ;; Bools
   (test-file "const-false.schml" (boole #f))
   (test-file "const-true.schml" (boole #t))
   ;; Ints
   (test-file "const-one.schml" (integ 1))
   (test-file "const-ninetynine.schml" (integ 99))
   (test-file "const-larg-int.schml" (integ 123456))
   ;; Primitive operators TODO (should add more to test corners)
   (test-file "prim-plus.schml"  (integ 10))
   (test-file "prim-minus.schml" (integ 10))
   (test-file "prim-times.schml" (integ 10))
   (test-file "prim-divides.schml" (integ 10))
   (test-file "prim-band.schml" (integ 10))
   (test-file "prim-bor.schml" (integ 10))
   (test-file "prim-shiftl.schml" (integ 10))
   (test-file "prim-shiftr.schml" (integ 10))
   
   ;; Primitive relational operators should test more corner cases
   (test-file "prim-eq.schml" (boole #t))
   (test-file "prim-lt.schml" (boole #t))
   (test-file "prim-gt.schml" (boole #t))
   (test-file "prim-le.schml" (boole #t))
   (test-file "prim-ge.schml" (boole #t))
   
   ;; If
   (test-file "if0.schml" (integ 0))
   (test-file "if1.schml" (integ 0))
   (test-file "if2.schml" (integ 1))
   (test-file "if3.schml" (integ 4))

   ;; Ascription
   (test-file "ascribe-dyn.schml"       (dynamic))
   (test-file "ascribe-int-good.schml"  (integ 10))
   (test-file "ascribe-bool-good.schml" (boole #t))
   (test-file "ascribe-int-bad.schml"   (blame #t "Pass"))
   (test-file "ascribe-bool-bad.schml"  (blame #t "Pass"))

   ;; Lambda
   (test-file "lambda1.schml" (function))
   (test-file "lambda2.schml" (function))
   (test-file "lambda3.schml" (function))
   (test-file "lambda4.schml" (function))
   (test-file "lambda5.schml" (function))
   (test-file "lambda6.schml" (function))
   (test-file "lambda7.schml" (function))
   (test-file "lambda8.schml" (function))

   ;; Let
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

   ;; Letrec
   (test-file "letrec1.schml" (dynamic))
   (test-file "letrec2.schml" (function))
   (test-file "letrec3.schml" (dynamic))
   (test-file "letrec4.schml" (boole #t))
   (test-file "letrec5.schml" (boole #t))
   (test-file "letrec6.schml" (integ 1))

   ;; Are we blaming the correct label
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
   
   ;; basic computations that may make it to being bench marks
   ;; factorial 
   (test-file "fact5.schml" (integ 120))
   (test-file "fact7.schml" (integ 5040))
   (test-file "fact10.schml" (integ 3628800))

   (test-file "fact-dyn-6.schml" (integ 720))
   (test-file "fact-static-6.schml" (integ 720))
   ;; even and odd 
   (test-file "odd-20-static.schml" (boole #f))
   (test-file "odd-20-hybrid1.schml" (boole #f))
   (test-file "odd-20-hybrid2.schml" (boole #f))
   (test-file "odd-20-hybrid3.schml" (boole #f))
   (test-file "odd-20-hybrid4.schml" (boole #f))
   (test-file "odd-20-hybrid5.schml" (boole #f))
   (test-file "odd-20-dynamic.schml" (dynamic))
   (test-file "even-odd-cps-herman.schml" (boole #t))
   ;; ackermans these are too long for the test suite
   (test-file "ack-1-2-static.schml" (integ 4))
   (test-file "ack-2-3-static.schml" (integ 9))
   ;; These are too long perhaps make an long flag or something
   ;;(test-file "ack-3-10-static.schml" (integ 125))
   ;;(test-file "ack-4-1-static.schml"  (integ 65533))
   ;;(test-file "ack-static.schml"      (boole #t))   
   ))

(module+ main
  (unless (directory-exists? test-tmp-path)
    (make-directory test-tmp-path))
  (call-with-output-file
      (build-path test-tmp-path "t.log.txt") #:exists 'replace
      (lambda ([f : Output-Port])
        (parameterize ([compiler-config (Config 'Lazy-D
                                                (build-path test-tmp-path "t.out")
                                                (build-path test-tmp-path "t.c"))]
                       [current-log-port f]
                       ;;[traces '(All Vomit)]
                       ;;[current-error-port f]
                       )
          (run-tests test-data-base 'verbose)))))

