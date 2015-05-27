#lang typed/racket

(require typed/rackunit
         typed/rackunit/text-ui
         schml/testing/test-compile)

(provide (all-defined-out))

(define-syntax test-file
  (make-test-file "core/values"))

(define core-tests : Test
  (test-suite
   "core tests"
   ;; Bools
   (test-file "true.schml"  (bool #t))
   (test-file "false.schml" (bool #f))
   ;; Ints
   (test-file "1.schml"   (int 1))
   (test-file "neg7.schml"   (int -7))
   (test-file "99.schml" (int 99))
   (test-file "123456.schml" (int 123456))
   ))
