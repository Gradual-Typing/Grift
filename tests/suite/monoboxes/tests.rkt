#lang typed/racket

(require "../../rackunit.rkt"
         "../../test-compile.rkt")

(provide (all-defined-out))

(define monobox-tests : Test
  (test-suite
   "Monotonic References"
   #:before (lambda () (display "monotonic boxes tests running ... "))
   #:after (lambda () (display "done\n"))
   (test-file "monoboxes" "mbox0.schml" (mbox))
   (test-file "monoboxes" "mbox1.schml" (mbox))
   (test-file "monoboxes" "mbox2.schml" (int 2))
   (test-file "monoboxes" "mbox3.schml" (int 3))
   (test-file "monoboxes" "mbox4.schml" (int 4))
   (test-file "monoboxes" "mbox5.schml" (dyn))
   (test-file "monoboxes" "mbox6.schml" (int 6))
   (test-file "monoboxes" "mbox7-1.schml" (int 42))
   (test-file "monoboxes" "mbox7-2.schml" (int 42))
   (test-file "monoboxes" "mbox7-3.schml" (int 42))
   (test-file "monoboxes" "mbox7-4.schml" (int 42))
   (test-file "monoboxes" "mbox7-5.schml" (int 42))
   (test-file "monoboxes" "mbox7-6.schml" (int 42))
   (test-file "monoboxes" "mbox7-7.schml" (int 42))
   ;; (test-file "monoboxes" "mbox8.schml" (int 42))
   (test-file "monoboxes" "mbox9.schml" (bool #f))
   (test-file "monoboxes" "mbox10.schml" (int 42))
   (test-file "monoboxes" "mbox11.schml" (int 42))
   (test-file "monoboxes" "mbox12.schml" (int 42))
   (test-file "monoboxes" "stress.schml" (int 4950))
   ))
