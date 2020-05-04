#lang typed/racket/base/no-check

(require rackunit
         "../../test-compile.rkt")

(provide (all-defined-out))

(define tiny-tests : Test
  (test-suite
   "tiny tests"
   #:before (lambda () (display "tiny tests running ... "))
   #:after (lambda () (display "done\n"))
   (test-file "core" "const-unit.grift" (unit))
   ;; Bools
   (test-file "core" "const-false.grift" (bool #f))
   (test-file "core" "const-true.grift"  (bool #t))
   ;; Ints
   (test-file "core" "const-one.grift"   (int 1))
   (test-file "core" "const-negative.grift" (int -5))
   (test-file "core" "const-ninetynine.grift" (int 99))
   (test-file "core" "const-larg-int.grift" (int 123456))
   ;; Characters
   (test-file "core" "const-char-a.grift" (char #\a))
   (test-file "core" "const-char-null.grift" (char #\nul))
   (test-file "core" "let-const-char-z.grift" (char #\z))
   ;; Primitive character operators
   (test-file "core" "print-char-b.grift" (unit))
   (test-file "core" "char-to-int.grift" (int 42))
   (test-file "core" "int-to-char.grift" (char #\A))
   
   ;; Primitive operators TODO (should add more to test corners)
   (test-file "core" "prim-plus.grift"  (int 10))
   (test-file "core" "prim-minus.grift" (int 10))
   (test-file "core" "prim-times.grift" (int 10))
   (test-file "core" "prim-divides.grift" (int 10))
   (test-file "core" "prim-band.grift" (int 10))
   (test-file "core" "prim-bor.grift" (int 10))
   (test-file "core" "prim-shiftl.grift" (int 10))
   (test-file "core" "prim-shiftr.grift" (int 10))
   (test-file "core" "mod1.grift" (int 42))
   ;; Primitive relational operators should test more corner cases
   (test-file "core" "prim-eq.grift" (bool #t))
   (test-file "core" "prim-lt.grift" (bool #t))
   (test-file "core" "prim-gt.grift" (bool #t))
   (test-file "core" "prim-le.grift" (bool #t))
   (test-file "core" "prim-ge.grift" (bool #t))
   ;; If
   (test-file "core" "if0.grift" (int 0))
   (test-file "core" "if1.grift" (int 0))
   (test-file "core" "if2.grift" (int 1))
   (test-file "core" "if3.grift" (int 4))
   ;; Lambda
   (test-file "core" "lambda1.grift" (function))
   (test-file "core" "lambda2.grift" (function))
   (test-file "core" "lambda3.grift" (function))
   (test-file "core" "lambda4.grift" (function))
   (test-file "core" "lambda5.grift" (function))
   (test-file "core" "lambda6.grift" (function))
   (test-file "core" "lambda7.grift" (function))
   (test-file "core" "lambda8.grift" (function))
   ;; Let
   (test-file "core" "let0.grift" (bool #t))
   (test-file "core" "let1.grift" (int 2))
   (test-file "core" "let2.grift" (bool #f))
   (test-file "core" "let3.grift" (int 1))
   (test-file "core" "let4.grift" (int 2))
   (test-file "core" "let5.grift" (int 4))
   (test-file "core" "let6.grift" (int 0))
   (test-file "core" "let7.grift" (dyn))
   (test-file "core" "let8.grift" (int 0))
   (test-file "core" "let9.grift" (int 100))
   (test-file "core" "let11.grift" (bool #f))
   ))

(define core-tests : Test
  (test-suite
   "core tests"
   #:before (lambda () (display "core test running ... "))
   #:after (lambda () (display "done\n"))
   ;; Simple dynamic tests
   (test-file "core" "float1.grift" (bool #t))
   ;; Let
   (test-file "core" "let10.grift" (dyn))
   (test-file "core" "let12.1.grift" (dyn))
   (test-file "core" "let12.2.grift" (dyn))
   (test-file "core" "let12.3.grift" (blame #f #f))
   (test-file "core" "let12.grift" (dyn))
   (test-file "core" "let13.grift" (dyn))
   (test-file "core" "let14.grift" (int 5))
   (test-file "core" "let15.grift" (function))
   (test-file "core" "let16.grift" (int 7))
   (test-file "core" "let17.grift" (bool #f))
   (test-file "core" "let20.grift" (bool #t))
   ;; Letrec
   (test-file "core" "letrec1.grift" (dyn))
   (test-file "core" "letrec2.grift" (function))
   (test-file "core" "letrec3.grift" (dyn))
   (test-file "core" "letrec4.grift" (bool #t))
   (test-file "core" "letrec5.grift" (bool #t))
   (test-file "core" "letrec6.grift" (int 1))
   (test-file "core" "letrec7.grift" (int 2))
   (test-file "core" "letrec8.grift" (int 0))
   (test-file "core" "letrec9.grift" (function))
   (test-file "core" "letrec10.grift" (function))
   (test-file "core" "letrec11.grift" (function))
   (test-file "core" "letrec12.grift" (function))
   (test-file "core" "letrec13.grift" (function))
   (test-file "core" "letrec14.grift" (function))
   ;; Ascription
   (test-file "core" "int-dyn-int.grift"   (int 1))
   (test-file "core" "ascribe-dyn.grift"       (dyn))
   (test-file "core" "ascribe-int-good.grift"  (int 10))
   (test-file "core" "ascribe-bool-good.grift" (bool #t))
   (test-file "core" "ascribe-int-bad.grift"   (blame #t "Pass"))
   (test-file "core" "ascribe-bool-bad.grift"  (blame #t "Pass"))
   ;; blame
   (test-file "core" "blame2.grift" (blame #t "Right"))
   (test-file "core" "blame3.grift" (blame #f "Correct"))
   (test-file "core" "blame4.grift" (blame #t #f))
   (test-file "core" "blame5.grift" (blame #f (not-lbl "Fail")))
   (test-file "core" "blame6.grift" (blame #f (not-lbl "Fail")))
   (test-file "core" "blame7.grift" (int 2))
   (test-file "core" "blame8.grift" (blame #t #f))
   (test-file "core" "blame9.grift" (blame #t "Pass"))
   ;; Multi  arg function
   (test-file "core" "blame10.grift" (blame #f (not-lbl "Fail")))
   (test-file "core" "blame11.grift" (blame #f (not-lbl "Fail")))
   (test-file "core" "blame12.grift" (blame #f "Pass"))
   (test-file "core" "blame13.grift" (blame #f "Pass"))
   (test-file "core" "blame14.grift" (blame #f "Pass"))
   ;; Switches
   (test-file "core" "switch0.grift"   (bool #t))
   (test-file "core" "switch1.grift"   (bool #t))
   (test-file "core" "switch2.grift"   (bool #t))
   (test-file "core" "switch3.grift"   (bool #t))
   ;; And
   (test-file "core" "and1.grift"   (bool #f))
   ;; Or
   (test-file "core" "or1.grift"   (bool #t))
   ;; Cond
   (test-file "core" "cond1.grift"   (int 42))

   ;; Casts
   (test-file "core" "project-int.grift" (bool #t))
   
   (test-file "core" "y-combinator-part.grift" (int 720))
   (test-file "core" "y-combinator-dynamic.grift" (int 720))
   ))
