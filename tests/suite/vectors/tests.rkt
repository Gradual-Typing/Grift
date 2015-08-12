#lang typed/racket

(require "../../rackunit.rkt"
         "../../test-compile.rkt")

(provide (all-defined-out))

(define vectors-tests : Test
  (test-suite
   "Guarded Vectors"
   (test-file "vectors" "gvect0.schml" (gvect))
   (test-file "vectors" "gvect1.schml" (gvect))
   (test-file "vectors" "gvect2.schml" (int 2))
   (test-file "vectors" "gvect3.schml" (int 3))
   (test-file "vectors" "gvect4.schml" (int 4))
   (test-file "vectors" "gvect5.schml" (dyn))
   (test-file "vectors" "gvect6.schml" (int 6))
   (test-file "vectors" "gvect7-1.schml" (int 42))
   (test-file "vectors" "gvect7-2.schml" (int 42))
   (test-file "vectors" "gvect7-3.schml" (int 42))
   (test-file "vectors" "gvect7-4.schml" (int 42))
   (test-file "vectors" "gvect7-5.schml" (int 42))
   (test-file "vectors" "gvect7-6.schml" (int 42))
   (test-file "vectors" "gvect7-7.schml" (int 42))
   (test-file "vectors" "gvect8.schml" (int 42))
   (test-file "vectors" "gvect9.schml" (bool #f))
   ))
