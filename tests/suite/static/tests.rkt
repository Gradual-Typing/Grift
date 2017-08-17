#lang typed/racket/base

(require "../../rackunit.rkt"
         "../../test-compile.rkt")

(provide
 ;; Tests that static variant of GTLC is correct
 ;; that the type system rejects programs
 ;; that require implicit casts
 static-tests
 ;; Checks that gradual implementations of GTLC
 ;; implement corect statically typed behavior
 statically-typed-gradual-tests)

(define possitive-tests : Test
  (test-suite
   "possitive"
   (test-file "static" "const-unit.schml" (unit))
   ;; Bools
   (test-file "static" "const-false.schml" (bool #f))
   (test-file "static" "const-true.schml"  (bool #t))
   ;; Ints
   (test-file "static" "const-one.schml"   (int 1))
   (test-file "static" "const-negative.schml" (int -5))
   (test-file "static" "const-ninetynine.schml" (int 99))
   (test-file "static" "const-larg-int.schml" (int 123456))
   (test-file "static" "const-char-a.schml" (char #\a))
   (test-file "static" "const-char-null.schml" (char #\nul))
   (test-file "static" "let-const-char-z.schml" (char #\z))
   ;; Primitive character operators
   (test-file "static" "print-char-b.schml" (unit))
   (test-file "static" "char-to-int.schml" (int 42))
   (test-file "static" "int-to-char.schml" (char #\A))   
   ;; Primitive operators TODO (should add more to test corners)
   (test-file "static" "prim-plus.schml"  (int 10))
   (test-file "static" "prim-minus.schml" (int 10))
   (test-file "static" "prim-times.schml" (int 10))
   (test-file "static" "prim-divides.schml" (int 10))
   (test-file "static" "prim-band.schml" (int 10))
   (test-file "static" "prim-bor.schml" (int 10))
   (test-file "static" "prim-shiftl.schml" (int 10))
   (test-file "static" "prim-shiftr.schml" (int 10))
   (test-file "static" "mod1.schml" (int 42))
   ;; Primitive relational operators should test more corner cases
   (test-file "static" "prim-eq.schml" (bool #t))
   (test-file "static" "prim-lt.schml" (bool #t))
   (test-file "static" "prim-gt.schml" (bool #t))
   (test-file "static" "prim-le.schml" (bool #t))
   (test-file "static" "prim-ge.schml" (bool #t))
   ;; If
   (test-file "static" "if0.schml" (int 0))
   (test-file "static" "if1.schml" (int 0))
   (test-file "static" "if2.schml" (int 1))
   (test-file "static" "if3.schml" (int 4))
   ;; Lambda
   (test-file "static" "lambda1.schml" (function))
   (test-file "static" "lambda2.schml" (function))
   (test-file "static" "lambda3.schml" (function))
   (test-file "static" "lambda4.schml" (function))
   (test-file "static" "lambda5.schml" (function))
   (test-file "static" "lambda6.schml" (function))
   (test-file "static" "lambda8.schml" (function))
   ;; Let
   (test-file "static" "let0.schml" (bool #t))
   (test-file "static" "let1.schml" (int 2))
   (test-file "static" "let2.schml" (bool #f))
   (test-file "static" "let3.schml" (int 1))
   (test-file "static" "let4.schml" (int 2))
   (test-file "static" "let5.schml" (int 4))
   (test-file "static" "let6.schml" (int 0))
   (test-file "static" "let8.schml" (int 0))
   (test-file "static" "let9.schml" (int 100))
   (test-file "static" "let10.schml" (bool #f))
   (test-file "static" "let11.schml" (bool #f))
   (test-file "static" "let13.schml" (int 5))
   (test-file "static" "let18.schml" (bool #f))
   (test-file "static" "let19.schml" (bool #f))
   ;; ;; Read - Int
   (test-file "static" "read-int0.schml" (int 42))
   (test-file "static" "read-int1.schml" (bool #t))
   (test-file "static" "read-int2.schml" (int 42))
   ;; Failes due to bad input not static type
   ;; checking
   (test-file "static" "read-int3.schml" (blame #f #f))
   ;; ;; Letrec
   (test-file "static" "letrec1.schml" (function))
   (test-file "static" "letrec2.schml" (function))
   (test-file "static" "letrec3.schml" (int 1))
   (test-file "static" "letrec6.schml" (int 1))
   (test-file "static" "letrec7.schml" (int 2))
   (test-file "static" "letrec8.schml" (int 0))
   ;; ;; Ascription
   ;; (test-file "static" "int-dyn-int.schml"   (int 1))
   ;; (test-file "static" "ascribe-dyn.schml"       (dyn))
   ;; (test-file "static" "ascribe-int-good.schml"  (int 10))
   ;; (test-file "static" "ascribe-bool-good.schml" (bool #t))
   ;; (test-file "static" "ascribe-int-bad.schml"   (blame #t "Pass"))
   ;; (test-file "static" "ascribe-bool-bad.schml"  (blame #t "Pass"))
   ;; ;; blame
   ;; (test-file "static" "blame2.schml" (blame #t "Right"))
   ;; (test-file "static" "blame3.schml" (blame #f "Correct"))
   ;; (test-file "static" "blame4.schml" (blame #t #f))
   ;; (test-file "static" "blame5.schml" (blame #f (not-lbl "Fail")))
   ;; (test-file "static" "blame6.schml" (blame #f (not-lbl "Fail")))
   ;; (test-file "static" "blame7.schml" (int 2))
   ;; (test-file "static" "blame8.schml" (blame #t #f))
   ;; (test-file "static" "blame9.schml" (blame #t "Pass"))
   ;; ;; Multi  arg function
   ;; (test-file "static" "blame10.schml" (blame #f (not-lbl "Fail")))
   ;; (test-file "static" "blame11.schml" (blame #f (not-lbl "Fail")))
   ;; (test-file "static" "blame12.schml" (blame #f "Pass"))
   ;; (test-file "static" "blame13.schml" (blame #f "Pass"))
   ;; (test-file "static" "blame14.schml" (blame #f "Pass"))
   ;; ;; Switches
   ;; (test-file "static" "switch0.schml"   (bool #t))
   ;; (test-file "static" "switch1.schml"   (bool #t))
   ;; (test-file "static" "switch2.schml"   (bool #t))
   ;; (test-file "static" "switch3.schml"   (bool #t))
   ))

;; Shows that the static typed version rejects programs that
;; require implicit casts
(define negative-tests : Test
  (test-suite
   "negative"
   (test-file "static" "let7.schml" (blame #t #f))
   (test-file "static" "let16.schml" (blame #t #f))
   (test-file "static" "let17.schml" (blame #t #f))
   (test-file "static" "let20.schml" (blame #t #f))
   
   (test-file "static" "letrec4.schml" (blame #t #f))
   (test-file "static" "letrec5.schml" (blame #t #f))
   (test-file "static" "letrec9.schml" (blame #t #f))
   (test-file "static" "letrec10.schml" (blame #t #f))
   (test-file "static" "letrec11.schml" (blame #t #f))
   (test-file "static" "letrec12.schml" (blame #t #f))
   (test-file "static" "letrec13.schml" (blame #t #f))
   (test-file "static" "letrec14.schml" (blame #t #f))
   (test-file "static" "letrec15.schml" (blame #t #f))
   (test-file "static" "letrec16.schml" (blame #t #f))
   (test-file "static" "letrec17.schml" (blame #t #f))
   (test-file "static" "letrec18.schml" (blame #t #f))))


(define static-tests : Test
  (test-suite
   "static tests"
   #:before (lambda () (display "static test running ... "))
   #:after (lambda () (display "done\n"))
   possitive-tests
   negative-tests))

;; This test suite describes the behavior of gradual implementations
;; on the static test suite
;; We leave out any tests that should have failed due to rejection
;; of non-statically typed programs because they are tested in core.
(define statically-typed-gradual-tests : Test
  (test-suite
   "static tests"
   #:before (lambda () (display "static test running ... "))
   #:after (lambda () (display "done\n"))
   ;; negative test pass type cheking in a gradual setting
   ;; therefore are included in core gradual testing
   possitive-tests))
