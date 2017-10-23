#lang typed/racket

(require "../../rackunit.rkt"
         "../../test-compile.rkt")

(provide (all-defined-out))

(define program-tests : Test
  (test-suite
   "Program Tests"
   #:before (lambda () (display "program tests running ... "))
   #:after  (lambda () (display "done\n"))
   ;; various factorial functions
   (test-file "program" "fact5.grift" (int 120))
   (test-file "program" "fact7.grift" (int 5040))
   (test-file "program" "fact10.grift" (int 3628800))
   (test-file "program" "fact-dyn-6.grift" (int 720))
   (test-file "program" "fact-static-6.grift" (int 720))
   (test-file "program" "fact-church-5.grift" (int 120))
   ;; even and odd
   (test-file "program" "odd-20-static.grift" (bool #f))
   (test-file "program" "odd-20-hybrid1.grift" (bool #f))
   (test-file "program" "odd-20-hybrid2.grift" (bool #f))
   (test-file "program" "odd-20-hybrid3.grift" (bool #f))
   (test-file "program" "odd-20-hybrid4.grift" (bool #f))
   (test-file "program" "odd-20-hybrid5.grift" (bool #f))
   (test-file "program" "odd-20-dynamic.grift" (dyn))
   (test-file "program" "even-odd-cps-herman.grift" (bool #t))
   ;; ackermans these are too long for the test suite
   (test-file "program" "ack-1-2-static.grift" (int 4))
   (test-file "program" "ack-2-3-static.grift" (int 9))
   ;; These are too long perhaps make an long flag or something
   ))
