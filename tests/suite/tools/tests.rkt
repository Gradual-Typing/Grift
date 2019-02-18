#lang typed/racket/no-check

(require rackunit
         "../../test-compile.rkt")

(provide (all-defined-out))

(define tool-tests : Test
  (test-suite
   "tools"
   #:before (lambda () (display "tool tests running ... "))
   #:after (lambda () (display "done\n"))
   (test-file "tools" "repeat0.grift"  (unit))
   (test-file "tools" "repeat1.grift"  (unit))
   (test-file "tools" "repeat2.grift"   (bool #f))
   (test-file "tools" "repeat2.1.grift" (bool #f))
   (test-file "tools" "repeat3.grift"   (bool #t))
   (test-file "tools" "repeat3.1.grift" (bool #t))
   (test-file "tools" "repeat4.grift"  (int 42))
   (test-file "tools" "repeat5.grift"  (int 100))
   (test-file "tools" "repeat5.1.grift"  (int 100))
   (test-file "tools" "repeat6.grift"  (int 100))
   (test-file "tools" "repeat6.1.grift"  (int 100))
   (test-file "tools" "repeat7.grift"  (int 5))
   (test-file "tools" "repeat8.grift"  (int 9))
   (test-file "tools" "repeat9.grift"  (bool #t))

   (test-file "tools" "time1.grift"  (unit))
   (test-file "tools" "time2.grift"  (unit))
   (test-file "tools" "time3.grift"  (blame #f #f))
   (test-file "tools" "time4.grift"  (unit))

   (test-file "tools" "both1.grift" (unit))
   (test-file "tools" "both1.1.grift" (unit))
;;   (test-file "tools" "both2.grift" (unit))
   ))
