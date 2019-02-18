#lang typed/racket/no-check

(require rackunit
         "../../test-compile.rkt")

(provide (all-defined-out))

(define monovector-tests : Test
  (test-suite
   "Monotonic Vectors"
   #:before (lambda () (display "monotonic vector tests running ... "))
   #:after (lambda () (display "done\n"))
   (test-file "monovectors" "mvect0.grift" (mvect))
   (test-file "monovectors" "mvect1.grift" (mvect))
   (test-file "monovectors" "mvect2.grift" (int 2))
   (test-file "monovectors" "mvect3.grift" (int 3))
   (test-file "monovectors" "mvect4.grift" (int 4))
   (test-file "monovectors" "mvect5.grift" (dyn))
   (test-file "monovectors" "mvect6.grift" (int 6))
   (test-file "monovectors" "mvect7-1.grift" (int 42))
   (test-file "monovectors" "mvect7-2.grift" (int 42))
   (test-file "monovectors" "mvect7-3.grift" (int 42))
   (test-file "monovectors" "mvect7-4.grift" (int 42))
   (test-file "monovectors" "mvect7-5.grift" (int 42))
   (test-file "monovectors" "mvect7-6.grift" (int 42))
   (test-file "monovectors" "mvect7-7.grift" (int 42))
   ;; (test-file "monovectors" "mvect8.grift" (int 42))
   (test-file "monovectors" "mvect9.grift" (bool #f))
   (test-file "monovectors" "mvect12.grift" (int 100))
   (test-file "monovectors" "mvect13.grift" (int 42))
   (test-file "monovectors" "mvect14.grift" (int 0))
   ))
