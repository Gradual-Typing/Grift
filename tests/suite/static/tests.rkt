#lang typed/racket/base/no-check

(require rackunit
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
   (test-file "static" "const-unit.grift" (unit))
   ;; Bools
   (test-file "static" "const-false.grift" (bool #f))
   (test-file "static" "const-true.grift"  (bool #t))
   (test-file "static" "read-bool1.grift" (int 42))
   ;; Ints
   (test-file "static" "const-one.grift"   (int 1))
   (test-file "static" "const-negative.grift" (int -5))
   (test-file "static" "const-ninetynine.grift" (int 99))
   (test-file "static" "const-larg-int.grift" (int 123456))
   (test-file "static" "const-char-a.grift" (char #\a))
   (test-file "static" "const-char-null.grift" (char #\nul))
   (test-file "static" "let-const-char-z.grift" (char #\z))
   ;; Primitive character operators
   (test-file "static" "print-char-b.grift" (unit))
   (test-file "static" "char-to-int.grift" (int 42))
   (test-file "static" "int-to-char.grift" (char #\A))   
   ;; Primitive operators TODO (should add more to test corners)
   (test-file "static" "prim-plus.grift"  (int 10))
   (test-file "static" "prim-minus.grift" (int 10))
   (test-file "static" "prim-times.grift" (int 10))
   (test-file "static" "prim-divides.grift" (int 10))
   (test-file "static" "prim-band.grift" (int 10))
   (test-file "static" "prim-bor.grift" (int 10))
   (test-file "static" "prim-shiftl.grift" (int 10))
   (test-file "static" "prim-shiftr.grift" (int 10))
   (test-file "static" "mod1.grift" (int 42))
   ;; Primitive relational operators should test more corner cases
   (test-file "static" "prim-eq.grift" (bool #t))
   (test-file "static" "prim-lt.grift" (bool #t))
   (test-file "static" "prim-gt.grift" (bool #t))
   (test-file "static" "prim-le.grift" (bool #t))
   (test-file "static" "prim-ge.grift" (bool #t))
   ;; If
   (test-file "static" "if0.grift" (int 0))
   (test-file "static" "if1.grift" (int 0))
   (test-file "static" "if2.grift" (int 1))
   (test-file "static" "if3.grift" (int 4))
   ;; Lambda
   (test-file "static" "lambda1.grift" (function))
   (test-file "static" "lambda2.grift" (function))
   (test-file "static" "lambda3.grift" (function))
   (test-file "static" "lambda4.grift" (function))
   (test-file "static" "lambda5.grift" (function))
   (test-file "static" "lambda6.grift" (function))
   (test-file "static" "lambda8.grift" (function))
   ;; Begin
   (test-file "static" "begin7.grift" (int 7))
   ;; Let
   (test-file "static" "letBegin.grift" (int 5))
   (test-file "static" "let0.grift" (bool #t))
   (test-file "static" "let1.grift" (int 2))
   (test-file "static" "let2.grift" (bool #f))
   (test-file "static" "let3.grift" (int 1))
   (test-file "static" "let4.grift" (int 2))
   (test-file "static" "let5.grift" (int 4))
   (test-file "static" "let6.grift" (int 0))
   (test-file "static" "let8.grift" (int 0))
   (test-file "static" "let9.grift" (int 100))
   (test-file "static" "let10.grift" (bool #f))
   (test-file "static" "let11.grift" (bool #f))
   (test-file "static" "let13.grift" (int 5))
   (test-file "static" "let18.grift" (bool #f))
   (test-file "static" "let19.grift" (bool #f))
   ;; ;; Read - Int
   (test-file "static" "read-int0.grift" (int 42))
   (test-file "static" "read-int1.grift" (bool #t))
   (test-file "static" "read-int2.grift" (int 42))
   ;; Failes due to bad input not static type
   ;; checking
   (test-file "static" "read-int3.grift" (blame #f #f))
   ;; ;; Letrec
   (test-file "static" "letrec1.grift" (function))
   (test-file "static" "letrec2.grift" (function))
   (test-file "static" "letrec3.grift" (int 1))
   (test-file "static" "letrec6.grift" (int 1))
   (test-file "static" "letrec7.grift" (int 2))
   (test-file "static" "letrec8.grift" (int 0))

   (test-file "static" "timer-start.grift" (unit))
   (test-file "static" "timer-stop.grift" (unit))
   (test-file "static" "timer-test.grift" (unit))

   ;; Tail Call works without casts
   (test-file "static" "tail-call.grift" (unit))

   ;; ;; Ascription
   ;; (test-file "static" "int-dyn-int.grift"   (int 1))
   ;; (test-file "static" "ascribe-dyn.grift"       (dyn))
   ;; (test-file "static" "ascribe-int-good.grift"  (int 10))
   ;; (test-file "static" "ascribe-bool-good.grift" (bool #t))
   ;; (test-file "static" "ascribe-int-bad.grift"   (blame #t "Pass"))
   ;; (test-file "static" "ascribe-bool-bad.grift"  (blame #t "Pass"))
   ;; ;; blame
   ;; (test-file "static" "blame2.grift" (blame #t "Right"))
   ;; (test-file "static" "blame3.grift" (blame #f "Correct"))
   ;; (test-file "static" "blame4.grift" (blame #t #f))
   ;; (test-file "static" "blame5.grift" (blame #f (not-lbl "Fail")))
   ;; (test-file "static" "blame6.grift" (blame #f (not-lbl "Fail")))
   ;; (test-file "static" "blame7.grift" (int 2))
   ;; (test-file "static" "blame8.grift" (blame #t #f))
   ;; (test-file "static" "blame9.grift" (blame #t "Pass"))
   ;; ;; Multi  arg function
   ;; (test-file "static" "blame10.grift" (blame #f (not-lbl "Fail")))
   ;; (test-file "static" "blame11.grift" (blame #f (not-lbl "Fail")))
   ;; (test-file "static" "blame12.grift" (blame #f "Pass"))
   ;; (test-file "static" "blame13.grift" (blame #f "Pass"))
   ;; (test-file "static" "blame14.grift" (blame #f "Pass"))
   ;; ;; Switches
   ;; (test-file "static" "switch0.grift"   (bool #t))
   ;; (test-file "static" "switch1.grift"   (bool #t))
   ;; (test-file "static" "switch2.grift"   (bool #t))
   ;; (test-file "static" "switch3.grift"   (bool #t))
   ))

;; Shows that the static typed version rejects programs that
;; require implicit casts
(define negative-tests : Test
  (test-suite
   "negative"
   (test-file "static" "let7.grift" (blame #t #f))
   (test-file "static" "let16.grift" (blame #t #f))
   (test-file "static" "let17.grift" (blame #t #f))
   (test-file "static" "let20.grift" (blame #t #f))
   
   (test-file "static" "letrec4.grift" (blame #t #f))
   (test-file "static" "letrec5.grift" (blame #t #f))
   (test-file "static" "letrec9.grift" (blame #t #f))
   (test-file "static" "letrec10.grift" (blame #t #f))
   (test-file "static" "letrec11.grift" (blame #t #f))
   (test-file "static" "letrec12.grift" (blame #t #f))
   (test-file "static" "letrec13.grift" (blame #t #f))
   (test-file "static" "letrec14.grift" (blame #t #f))
   (test-file "static" "letrec15.grift" (blame #t #f))
   (test-file "static" "letrec16.grift" (blame #t #f))
   (test-file "static" "letrec17.grift" (blame #t #f))
   (test-file "static" "letrec18.grift" (blame #t #f))))


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
