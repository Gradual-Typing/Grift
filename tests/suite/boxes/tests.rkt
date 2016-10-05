#lang typed/racket

(require "../../rackunit.rkt"
         "../../test-compile.rkt")

(provide (all-defined-out))

(define box-tests : Test
  (test-suite
   "Guarded References"
   #:before (lambda () (display "box tests running ... "))
   #:after (lambda () (display "done\n"))
   (test-file "boxes" "gbox0.schml" (gbox))
   (test-file "boxes" "gbox1.schml" (gbox))
   (test-file "boxes" "gbox2.schml" (int 2))
   (test-file "boxes" "gbox3.schml" (int 3))
   (test-file "boxes" "gbox4.schml" (int 4))
   (test-file "boxes" "gbox5.schml" (dyn))
   (test-file "boxes" "gbox6.schml" (int 6))
   (test-file "boxes" "gbox7-1.schml" (int 42))
   (test-file "boxes" "gbox7-2.schml" (int 42))
   (test-file "boxes" "gbox7-3.schml" (int 42))
   (test-file "boxes" "gbox7-4.schml" (int 42))
   (test-file "boxes" "gbox7-5.schml" (int 42))
   (test-file "boxes" "gbox7-6.schml" (int 42))
   (test-file "boxes" "gbox7-7.schml" (int 42))
   (test-file "boxes" "gbox8.schml" (int 42))
   (test-file "boxes" "gbox9.schml" (bool #f))
   (test-file "boxes" "gbox11.schml" (int 42))
   ))
