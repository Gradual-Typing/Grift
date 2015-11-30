#lang typed/racket

(require "../../rackunit.rkt"
         "../../test-compile.rkt")

(provide (all-defined-out))

(define large-tests : Test
  (test-suite
   "large tests"
   #:before (lambda () (display "large tests running ... "))
   #:after (lambda () (display "done\n"))
   (test-file/no-checks "large" "ack-3-5-static.schml" (int 253))
   (test-file/no-checks "large" "ack-4-1-static.schml"  (int 65533))
   (test-file/no-checks "large" "ack-static.schml"      (bool #t))
   (test-file/no-checks "large" "guarded-writes-alot.schml" (int 0))))
