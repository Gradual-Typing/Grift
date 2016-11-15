#lang typed/racket

(require "../../rackunit.rkt"
         "../../test-compile.rkt")

(provide (all-defined-out))

(define monovector-tests : Test
  (test-suite
   "Monotonic Vectors"
   #:before (lambda () (display "monotonic vector tests running ... "))
   #:after (lambda () (display "done\n"))
   (test-file "monovectors" "mvect0.schml" (mvect))
   (test-file "monovectors" "mvect1.schml" (mvect))
   (test-file "monovectors" "mvect2.schml" (int 2))
   (test-file "monovectors" "mvect3.schml" (int 3))
   (test-file "monovectors" "mvect4.schml" (int 4))
   (test-file "monovectors" "mvect5.schml" (dyn))
   (test-file "monovectors" "mvect6.schml" (int 6))
   (test-file "monovectors" "mvect7-1.schml" (int 42))
   (test-file "monovectors" "mvect7-2.schml" (int 42))
   (test-file "monovectors" "mvect7-3.schml" (int 42))
   (test-file "monovectors" "mvect7-4.schml" (int 42))
   (test-file "monovectors" "mvect7-5.schml" (int 42))
   (test-file "monovectors" "mvect7-6.schml" (int 42))
   (test-file "monovectors" "mvect7-7.schml" (int 42))
   ;; (test-file "monovectors" "mvect8.schml" (int 42))
   (test-file "monovectors" "mvect9.schml" (bool #f))
   (test-file "monovectors" "mvect12.schml" (int 42))
   (test-file "monovectors" "mvect13.schml" (int 42))
   ))
