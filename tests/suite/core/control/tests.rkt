#lang typed/racket

(require typed/rackunit
         typed/rackunit/text-ui
         grift/tests/test-compile)

(provide (all-defined-out))

(define core-tests : Test
  (test-suite
   #:before (lambda () (display "control tests running ... "))
   #:after (lambda () (display "done\n"))
   (test-file "begin.grift" (int 7))
   (test-file "letBegin.grift" (int 5))
   ))
