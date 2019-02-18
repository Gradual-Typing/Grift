#lang typed/racket/no-check

(require rackunit
         "../../test-compile.rkt")

(provide (all-defined-out))

(define monobox-tests : Test
  (test-suite
   "Monotonic References"
   #:before (lambda () (display "monotonic boxes tests running ... "))
   #:after (lambda () (display "done\n"))
   (test-file "monoboxes" "mbox0.grift" (mbox))
   (test-file "monoboxes" "mbox1.grift" (mbox))
   (test-file "monoboxes" "mbox2.grift" (int 2))
   (test-file "monoboxes" "mbox3.grift" (int 3))
   (test-file "monoboxes" "mbox4.grift" (int 4))
   (test-file "monoboxes" "mbox5.grift" (dyn))
   (test-file "monoboxes" "mbox6.grift" (int 6))
   (test-file "monoboxes" "mbox7-1.grift" (int 42))
   (test-file "monoboxes" "mbox7-2.grift" (int 42))
   (test-file "monoboxes" "mbox7-3.grift" (int 42))
   (test-file "monoboxes" "mbox7-4.grift" (int 42))
   (test-file "monoboxes" "mbox7-5.grift" (int 42))
   (test-file "monoboxes" "mbox7-6.grift" (int 42))
   (test-file "monoboxes" "mbox7-7.grift" (int 42))
   ;; (test-file "monoboxes" "mbox8.grift" (int 42))
   (test-file "monoboxes" "mbox9.grift" (bool #f))
   (test-file "monoboxes" "mbox10.grift" (int 42))
   (test-file "monoboxes" "mbox11.grift" (int 42))
   (test-file "monoboxes" "mbox12.grift" (int 42))
   (test-file "monoboxes" "stress.grift" (int 4950))
   ))
