#lang typed/racket

(require "../../rackunit.rkt"
         "../../test-compile.rkt")

(provide (all-defined-out))

(define large-tests : Test
  (test-suite
   "large tests"
   #:before (lambda () (display "large tests running ... "))
   #:after (lambda () (display "done\n"))
   (test-file/no-checks "large" "ack-3-5-static.grift" (int 253))
   (test-file/no-checks "large" "ack-4-1-static.grift"  (int 65533))
   (test-file/no-checks "large" "ack-static.grift"      (bool #t))
   (test-file/no-checks "large" "guarded-writes-alot.grift" (int 0))))
