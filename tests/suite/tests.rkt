#lang typed/racket

(require typed/rackunit
         "../test-compile.rkt")

(require "core/tests.rkt"
         "boxes/tests.rkt"
         "tools/tests.rkt"
         "program/tests.rkt"
         "large/tests.rkt")

(provide (all-defined-out)
         (all-from-out
          "core/tests.rkt"
          "boxes/tests.rkt"
          "tools/tests.rkt"
          "program/tests.rkt"
          "large/tests.rkt"))

(define most-tests : Test
  (test-suite "most tests"
    core-tests
    boxes-tests
    tool-tests
    program-tests))

(define all-tests : Test
  (test-suite "all tests"
   most-tests
   large-tests))
