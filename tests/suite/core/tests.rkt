#lang typed/racket/base

(require "../../rackunit.rkt"
         "../../test-compile.rkt")

(provide (all-defined-out))

(define tiny-tests : Test
  (test-suite
   "tiny tests"
   #:before (lambda () (display "tiny tests running ... "))
   #:after (lambda () (display "done\n"))
   (test-file "core" "const-unit.schml" (unit))
   ;; Bools
   (test-file "core" "const-false.schml" (bool #f))
   (test-file "core" "const-true.schml"  (bool #t))
   ;; Ints
   (test-file "core" "const-one.schml"   (int 1))
   (test-file "core" "const-negative.schml" (int -5))
   (test-file "core" "const-ninetynine.schml" (int 99))
   (test-file "core" "const-larg-int.schml" (int 123456))
   (test-file "core" "const-char-a.schml" (char #\a))
   (test-file "core" "const-char-null.schml" (char #\nul))
   (test-file "core" "let-const-char-z.schml" (char #\z))
   ;; Primitive character operators
   (test-file "core" "print-char-b.schml" (unit))
   (test-file "core" "char-to-int.schml" (int 42))
   (test-file "core" "int-to-char.schml" (char #\A))
   
   ;; Primitive operators TODO (should add more to test corners)
   (test-file "core" "prim-plus.schml"  (int 10))
   (test-file "core" "prim-minus.schml" (int 10))
   (test-file "core" "prim-times.schml" (int 10))
   (test-file "core" "prim-divides.schml" (int 10))
   (test-file "core" "prim-band.schml" (int 10))
   (test-file "core" "prim-bor.schml" (int 10))
   (test-file "core" "prim-shiftl.schml" (int 10))
   (test-file "core" "prim-shiftr.schml" (int 10))
   (test-file "core" "mod1.schml" (int 42))
   ;; Primitive relational operators should test more corner cases
   (test-file "core" "prim-eq.schml" (bool #t))
   (test-file "core" "prim-lt.schml" (bool #t))
   (test-file "core" "prim-gt.schml" (bool #t))
   (test-file "core" "prim-le.schml" (bool #t))
   (test-file "core" "prim-ge.schml" (bool #t))
   ;; If
   (test-file "core" "if0.schml" (int 0))
   (test-file "core" "if1.schml" (int 0))
   (test-file "core" "if2.schml" (int 1))
   (test-file "core" "if3.schml" (int 4))
   ;; Lambda
   (test-file "core" "lambda1.schml" (function))
   (test-file "core" "lambda2.schml" (function))
   (test-file "core" "lambda3.schml" (function))
   (test-file "core" "lambda4.schml" (function))
   (test-file "core" "lambda5.schml" (function))
   (test-file "core" "lambda6.schml" (function))
   (test-file "core" "lambda7.schml" (function))
   (test-file "core" "lambda8.schml" (function))
   ;; Let
   (test-file "core" "let0.schml" (bool #t))
   (test-file "core" "let1.schml" (int 2))
   (test-file "core" "let2.schml" (bool #f))
   (test-file "core" "let3.schml" (int 1))
   (test-file "core" "let4.schml" (int 2))
   (test-file "core" "let5.schml" (int 4))
   (test-file "core" "let6.schml" (int 0))
   (test-file "core" "let7.schml" (dyn))
   (test-file "core" "let8.schml" (int 0))
   (test-file "core" "let9.schml" (int 100))
   (test-file "core" "let11.schml" (bool #f))
   ))

(define core-tests : Test
  (test-suite
   "core tests"
   #:before (lambda () (display "core test running ... "))
   #:after (lambda () (display "done\n"))
   ;; Let
   (test-file "core" "let10.schml" (dyn))
   (test-file "core" "let12.1.schml" (dyn))
   (test-file "core" "let12.2.schml" (dyn))
   (test-file "core" "let12.3.schml" (blame #f #f))
   (test-file "core" "let12.schml" (dyn))
   (test-file "core" "let13.schml" (dyn))
   (test-file "core" "let14.schml" (int 5))
   (test-file "core" "let15.schml" (function))
   (test-file "core" "let16.schml" (int 7))
   (test-file "core" "let17.schml" (bool #f))
   (test-file "core" "let20.schml" (bool #t))
   ;; Letrec
   (test-file "core" "letrec1.schml" (dyn))
   (test-file "core" "letrec2.schml" (function))
   (test-file "core" "letrec3.schml" (dyn))
   (test-file "core" "letrec4.schml" (bool #t))
   (test-file "core" "letrec5.schml" (bool #t))
   (test-file "core" "letrec6.schml" (int 1))
   (test-file "core" "letrec7.schml" (int 2))
   (test-file "core" "letrec8.schml" (int 0))
   (test-file "core" "letrec9.schml" (function))
   (test-file "core" "letrec10.schml" (function))
   (test-file "core" "letrec11.schml" (function))
   (test-file "core" "letrec12.schml" (function))
   (test-file "core" "letrec13.schml" (function))
   (test-file "core" "letrec14.schml" (function))
   ;; Ascription
   (test-file "core" "int-dyn-int.schml"   (int 1))
   (test-file "core" "ascribe-dyn.schml"       (dyn))
   (test-file "core" "ascribe-int-good.schml"  (int 10))
   (test-file "core" "ascribe-bool-good.schml" (bool #t))
   (test-file "core" "ascribe-int-bad.schml"   (blame #t "Pass"))
   (test-file "core" "ascribe-bool-bad.schml"  (blame #t "Pass"))
   ;; blame
   (test-file "core" "blame2.schml" (blame #t "Right"))
   (test-file "core" "blame3.schml" (blame #f "Correct"))
   (test-file "core" "blame4.schml" (blame #t #f))
   (test-file "core" "blame5.schml" (blame #f (not-lbl "Fail")))
   (test-file "core" "blame6.schml" (blame #f (not-lbl "Fail")))
   (test-file "core" "blame7.schml" (int 2))
   (test-file "core" "blame8.schml" (blame #t #f))
   (test-file "core" "blame9.schml" (blame #t "Pass"))
   ;; Multi  arg function
   (test-file "core" "blame10.schml" (blame #f (not-lbl "Fail")))
   (test-file "core" "blame11.schml" (blame #f (not-lbl "Fail")))
   (test-file "core" "blame12.schml" (blame #f "Pass"))
   (test-file "core" "blame13.schml" (blame #f "Pass"))
   (test-file "core" "blame14.schml" (blame #f "Pass"))
   ;; Switches
   (test-file "core" "switch0.schml"   (bool #t))
   (test-file "core" "switch1.schml"   (bool #t))
   (test-file "core" "switch2.schml"   (bool #t))
   (test-file "core" "switch3.schml"   (bool #t))
   ))
