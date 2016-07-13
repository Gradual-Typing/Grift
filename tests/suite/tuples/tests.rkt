#lang typed/racket

(require "../../rackunit.rkt"
         "../../test-compile.rkt")

(provide (all-defined-out))

(define tuple-tests : Test
  (test-suite
   "Tuples"
   #:before (lambda () (display "tuples tests running ... "))
   #:after (lambda () (display "done\n"))
   (test-file "tuples" "tuple0.schml" (tuple))
   (test-file "tuples" "tuple1.schml" (tuple))
   (test-file "tuples" "tuple2.schml" (int 2))
   (test-file "tuples" "tuple3.schml" (int 3))
   (test-file "tuples" "tuple4.schml" (dyn))
   (test-file "tuples" "tuple5.schml" (int 42))
   (test-file "tuples" "tuple6.schml" (int 42))
   (test-file "tuples" "tuple7.schml" (int 42))
   (test-file "tuples" "tuple8.schml" (int 42))
   ))
