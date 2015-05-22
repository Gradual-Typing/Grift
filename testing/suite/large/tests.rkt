#lang typed/racket

(require typed/rackunit
         schml/testing/test-compile
         schml/testing/paths)

(provide (all-defined-out))

(define large-tests : Test
  (test-suite
   "large tests"
   (test-file "large" "ack-3-10-static.schml" (int 125))
   (test-file "large" "ack-4-1-static.schml"  (int 65533))
   (test-file "large" "ack-static.schml"      (bool #t))))
