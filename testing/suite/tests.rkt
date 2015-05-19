#lang typed/racket

(require typed/rackunit
         schml/testing-compile
         schml/testing/paths)



(require "core/tests.rkt"
         "boxes/tests.rkt"
         "program/tests.rkt"
         "large/tests.rkt")

(provide (all-defined-out)
         (all-from-out "core/tests.rkt"
                       "boxes/tests.rkt"
                       "program/tests.rkt"
                       "large/tests.rkt"))

(define most-tests : Test
  (test-suite "most tests"
    core-tests
    boxes-tests
    program-tests))

(define all-tests : Test
  (test-suite "all tests"
   most-tests
   large-tests))
