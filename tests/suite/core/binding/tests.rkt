#lang typed/racket

(require typed/rackunit
         typed/rackunit/text-ui
         schml/testing/test-compile)

(provide (all-defined-out))

(define binding-tests : Test
  (test-suite
   "binding"
   (test-file "letrec1.schml" (int 7))
   (test-file "letrec2.schml" (int 7))
   ))
