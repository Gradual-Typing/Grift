#lang typed/racket/base
;;; 9/27/2017 - added types for typed racket

(require racket/fixnum racket/pretty)
;;; TAK -- A vanilla version of the TAKeuchi function.

(: tak : Fixnum Fixnum Fixnum -> Fixnum)
(define (tak x y z)
  (if (fx>= y x)
      z
      (tak (tak (fx- x 1) y z)
           (tak (fx- y 1) z x)
           (tak (fx- z 1) x y))))

(let* ([x : Any (read)]
       [y : Any (read)]
       [z : Any (read)])
  ;; These fixnum? checks are additional work that the original dynamic
  ;; code didn't have to do. Is there any way of making it disappear?
  (unless (and (fixnum? x) (fixnum? y) (fixnum? z))
    (error 'invalid-input "expected 3 fixnum values"))
  (pretty-print (tak x y z)))
