#lang typed/racket

(require "../../rackunit.rkt"
         "../../test-compile.rkt")

(provide (all-defined-out))

(define tool-tests : Test
  (test-suite
   "tools"
   #:before (lambda () (display "tool tests running ... "))
   #:after (lambda () (display "done\n"))
   (test-file "tools" "repeat0.schml"  (unit))
   (test-file "tools" "repeat1.schml"  (unit))
   (test-file "tools" "repeat2.schml"  (bool #f))
   (test-file "tools" "repeat3.schml"  (bool #t))
   (test-file "tools" "repeat4.schml"  (int 42))
   (test-file "tools" "repeat5.schml"  (int 1000))
   (test-file "tools" "repeat6.schml"  (int 1000))
   (test-file "tools" "repeat7.schml"  (int 10))
   (test-file "tools" "repeat8.schml"  (int 9))
   (test-file "tools" "repeat9.schml"  (bool #t))
   (test-file "tools" "repeat10.schml" (bool #t))

   (test-file "tools" "time1.schml"  (unit))
   (test-file "tools" "time2.schml"  (unit))
   (test-file "tools" "time3.schml"  (blame #f #f))
   (test-file "tools" "time4.schml"  (unit))

   (test-file "tools" "both.schml" (unit))
   (test-file "tools" "both1.schml" (unit))
   ;(test-file "tools" "both2.schml" (unit))
   ;(test-file "tools" "both3.schml" (unit))

   ;;(test-file "tools" "time5.schml"  (unit))
   ;; (test-file "tools" "time6.schml"  (unit))
   ;; (test-file "tools" "time7.schml"  (unit))
   ;; (test-file "tools" "time8.schml"  (unit))
   ;; (test-file "tools" "time9.schml"  (unit))
   ;; (test-file "tools" "time10.schml" (unit))
   ))
