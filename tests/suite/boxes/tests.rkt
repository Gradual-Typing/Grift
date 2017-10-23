#lang typed/racket

(require "../../rackunit.rkt"
         "../../test-compile.rkt")

(provide (all-defined-out))

(define box-tests : Test
  (test-suite
   "Guarded References"
   #:before (lambda () (display "box tests running ... "))
   #:after (lambda () (display "done\n"))
   (test-file "boxes" "gbox0.grift" (gbox))
   (test-file "boxes" "gbox1.grift" (gbox))
   (test-file "boxes" "gbox2.grift" (int 2))
   (test-file "boxes" "gbox3.grift" (int 3))
   (test-file "boxes" "gbox4.grift" (int 4))
   (test-file "boxes" "gbox5.grift" (dyn))
   (test-file "boxes" "gbox6.grift" (int 6))
   (test-file "boxes" "gbox7-1.grift" (int 42))
   (test-file "boxes" "gbox7-2.grift" (int 42))
   (test-file "boxes" "gbox7-3.grift" (int 42))
   (test-file "boxes" "gbox7-4.grift" (int 42))
   (test-file "boxes" "gbox7-5.grift" (int 42))
   (test-file "boxes" "gbox7-6.grift" (int 42))
   (test-file "boxes" "gbox7-7.grift" (int 42))
   (test-file "boxes" "gbox8.grift" (int 42))
   (test-file "boxes" "gbox9.grift" (bool #f))
   (test-file "boxes" "gbox11.grift" (int 42))
   ))
