#lang typed/racket

(require "../../rackunit.rkt"
         "../../test-compile.rkt")

(provide (all-defined-out))

(define large-tests : Test
  (test-suite
   "large tests"
   #:before (lambda () (display "large tests running ... "))
   #:after (lambda () (display "done\n"))

   (test-file "large" "ack-3-10-static.schml" (int 125))
   (test-file "large" "ack-4-1-static.schml"  (int 65533))
   (test-file "large" "ack-static.schml"      (bool #t))))
