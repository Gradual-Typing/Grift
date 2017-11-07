#lang racket/base
;;; TAK -- A vanilla version of the TAKeuchi function.

;;; Here we actively choose to not use racket racket/fixnum. Use of
;;; generic numeric ops is disadvantage for racket but there is no
;;; safe version of fixnum operations that avoids the overhead of
;;; contracts, and we are only interested in comparing safe code.  The
;;; racket/fixnum safe operations are generally no faster than using
;;; generic primitives like +. (According to the documentation)

(define (tak x y z)
  (if (>= y x)
      z
      (tak (tak (- x 1) y z)
           (tak (- y 1) z x)
           (tak (- z 1) x y))))

(define (run-benchmark)
  (let* ([x (read)]
         [y (read)]
         [z (read)])
    (display (tak x y z))))

(time (run-benchmark))

