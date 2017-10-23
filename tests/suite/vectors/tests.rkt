#lang typed/racket

(require "../../rackunit.rkt"
         "../../test-compile.rkt")

(provide (all-defined-out))

(define vector-tests : Test
  (test-suite
   "Guarded Vectors"
   #:before (lambda () (display "vector tests running ... "))
   #:after (lambda () (display "done\n"))
   (test-file "vectors" "gvect0.grift" (gvect))
   (test-file "vectors" "gvect1.grift" (gvect))
   (test-file "vectors" "gvect2.grift" (int 2))
   (test-file "vectors" "gvect3.grift" (int 3))
   (test-file "vectors" "gvect4.grift" (int 4))
   (test-file "vectors" "gvect5.grift" (dyn))
   (test-file "vectors" "gvect6.grift" (int 6))
   (test-file "vectors" "gvect7-1.grift" (int 42))
   (test-file "vectors" "gvect7-2.grift" (int 42))
   (test-file "vectors" "gvect7-3.grift" (int 42))
   (test-file "vectors" "gvect7-4.grift" (int 42))
   (test-file "vectors" "gvect7-5.grift" (int 42))
   (test-file "vectors" "gvect7-6.grift" (int 42))
   (test-file "vectors" "gvect7-7.grift" (int 42))
   (test-file "vectors" "gvect8.grift" (int 42))
   (test-file "vectors" "gvect9.grift" (bool #f))
   (test-file "vectors" "gvect12.grift" (int 100))
   ))
