#lang typed/racket

(require typed/rackunit
         schml/testing-compile
         schml/testing/paths)

(provide (all-defined-out))

(define program-tests : Test
  (test-suite
   "Program Tests"
   ;; various factorial functions
   (test-file "program" "fact5.schml" (int 120))
   (test-file "program" "fact7.schml" (int 5040))
   (test-file "program" "fact10.schml" (int 3628800))
   (test-file "program" "fact-dyn-6.schml" (int 720))
   (test-file "program" "fact-static-6.schml" (int 720))
   ;; even and odd
   (test-file "program" "odd-20-static.schml" (bool #f))
   (test-file "program" "odd-20-hybrid1.schml" (bool #f))
   (test-file "program" "odd-20-hybrid2.schml" (bool #f))
   (test-file "program" "odd-20-hybrid3.schml" (bool #f))
   (test-file "program" "odd-20-hybrid4.schml" (bool #f))
   (test-file "program" "odd-20-hybrid5.schml" (bool #f))
   (test-file "program" "odd-20-dynamic.schml" (dyn))
   (test-file "program" "even-odd-cps-herman.schml" (bool #t))
   ;; ackermans these are too long for the test suite
   (test-file "program" "ack-1-2-static.schml" (int 4))
   (test-file "program" "ack-2-3-static.schml" (int 9))
   ;; These are too long perhaps make an long flag or something
   ;;(test-file "ack-3-10-static.schml" (int 125))
   ;;(test-file "ack-4-1-static.schml"  (int 65533))
   ;;(test-file "ack-static.schml"      (bool #t))
   ))
