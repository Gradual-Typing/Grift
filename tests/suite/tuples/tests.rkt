#lang typed/racket

(require "../../rackunit.rkt"
         "../../test-compile.rkt")

(provide (all-defined-out))

(define tuple-tests : Test
  (test-suite
   "Tuples"
   #:before (lambda () (display "tuples tests running ... "))
   #:after (lambda () (display "done\n"))
   (test-file "tuples" "tuple0.grift" (tuple))
   (test-file "tuples" "tuple1.grift" (tuple))
   (test-file "tuples" "tuple2.grift" (int 2))
   (test-file "tuples" "tuple3.grift" (int 3))
   (test-file "tuples" "tuple4.grift" (dyn))
   (test-file "tuples" "tuple5.grift" (int 42))
   (test-file "tuples" "tuple6.grift" (int 42))
   (test-file "tuples" "tuple7.grift" (int 42))
   (test-file "tuples" "tuple8.grift" (int 42))
   ))
