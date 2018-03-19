#lang typed/racket/base

(require "../../rackunit.rkt"
         "../../test-compile.rkt")

(provide (all-defined-out))

(define recursive-type-tests : Test
  (test-suite
   "recursive type tests"
   #:before (lambda () (display "recursive type tests running ... "))
   #:after (lambda () (display "done\n"))
   (test-file "mu" "0.grift" (blame #t #f))
   (test-file "mu" "0.1.grift" (blame #t #f))
   (test-file "mu" "1.grift" (function))
   (test-file "mu" "2.grift" (unit))
   (test-file "mu" "3.grift" (function))
   (test-file "mu" "4.grift" (function))
   (test-file "mu" "5.grift" (function))
   (test-file "mu" "6.grift" (int 1))
   (test-file "mu" "7.grift" (function))
   ))
