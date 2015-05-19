#lang typed/racket

(require typed/rackunit
         schml/testing-compile
         schml/testing/paths)

(provide (all-defined-out))

(define boxes-tests : Test
  (test-suite
   "Guarded References"
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
   (test-file "boxes" "gbox7.schml" (int 7))
   (test-file "boxes" "gbox8.schml" (bool #t))
   (test-file "boxes" "gbox9.schml" (bool #f))
   ))




