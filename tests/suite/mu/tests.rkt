#lang typed/racket/base/no-check

(require rackunit
         "../../test-compile.rkt")

(provide (all-defined-out))

(define recursive-type-tests : Test
  (test-suite
   "recursive type tests"
   #:before (lambda () (display "recursive type tests running ... "))
   #:after (lambda () (display "done\n"))
   ;; Statically Incorrect
   (test-file "mu" "0.grift" (blame #t #f))
   (test-file "mu" "0.1.grift" (blame #t #f))
   ;; Statically Correct
   (test-file "mu" "1.grift" (function))
   (test-file "mu" "2.grift" (unit))
   (test-file "mu" "3.grift" (function))
   (test-file "mu" "4.grift" (function))
   (test-file "mu" "5.grift" (function))
   (test-file "mu" "6.grift" (int 1))
   (test-file "mu" "7.grift" (int 1))
   ;; Gradually Incorrect
   (test-file "mu" "0.2.grift" (blame #t #f))
   (test-file "mu" "0.3.grift" (blame #t #f))
   ;; Gradually Correct
   (test-file "mu" "8.grift" (function))
   (test-file "mu" "9.grift" (unit))
   (test-file "mu" "10.grift" (function))
   (test-file "mu" "11.grift" (function))
   (test-file "mu" "12.grift" (function))
   (test-file "mu" "12.1.grift" (function))
   (test-file "mu" "12.2.grift" (dyn))
   (test-file "mu" "12.3.grift" (function))
   (test-file "mu" "12.4.grift" (dyn))
   (test-file "mu" "12.5.grift" (function))
   (test-file "mu" "12.6.grift" (function))
   (test-file "mu" "13.grift" (int 1))
   (test-file "mu" "13.1.grift" (tuple))
   (test-file "mu" "13.2.grift" (tuple))
   (test-file "mu" "14.grift" (int 1))
   (test-file "mu" "dyn-ops.grift" (int 2))))
