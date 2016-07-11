#lang typed/racket

(require "../rackunit.rkt"
         "../test-compile.rkt")

(require "core/tests.rkt"
         "boxes/tests.rkt"
         "vectors/tests.rkt"
         "tuples/tests.rkt"
         "tools/tests.rkt"
         "program/tests.rkt"
         "large/tests.rkt")

(provide (all-defined-out)
         (all-from-out
          "core/tests.rkt"
          "boxes/tests.rkt"
          "vectors/tests.rkt"
          "tuples/tests.rkt"
          "tools/tests.rkt"
          "program/tests.rkt"
          "large/tests.rkt"))

(define most-tests : Test
  (test-suite "most tests"
    core-tests box-tests vector-tests tuple-tests tool-tests program-tests))

(define all-tests : Test
  (test-suite "all tests"
   most-tests
   large-tests))
