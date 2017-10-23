#lang typed/racket

(require typed/rackunit
         typed/rackunit/text-ui
         grift/testing/test-compile)

(provide (all-defined-out))

(define-syntax test-file
  (make-test-file "core/values"))

(define core-tests : Test
  (test-suite
   "core tests"
   #:before (lambda () (display "value tests running ... "))
   #:after (lambda () (display "done\n"))

   ;; Bools
   (test-file "true.grift"  (bool #t))
   (test-file "false.grift" (bool #f))
   ;; Ints
   (test-file "1.grift"   (int 1))
   (test-file "neg7.grift"   (int -7))
   (test-file "99.grift" (int 99))
   (test-file "123456.grift" (int 123456))
   ))
