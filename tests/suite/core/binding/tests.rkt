#lang typed/racket

(require typed/rackunit
         typed/rackunit/text-ui
         grift/tests/test-compile)

(provide (all-defined-out))

(define binding-tests : Test
  (test-suite
   "binding"
   #:before (lambda () (display "binding tests running ... "))
   #:after (lambda () (display "done\n"))
   (test-file "letrec1.grift" (int 7))
   (test-file "letrec2.grift" (int 7))
   ))
