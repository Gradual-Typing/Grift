#lang typed/racket/base

(require racket/pretty)

;;; ARRAY1 -- One of the Kernighan and Van Wyk benchmarks.
;;; 9/27/2017 added types to support typed-racket by Andre Kuhlenschmidt
;;; 10/9/2017 changed to use internal timing (Andre)
;;;
;;; We actively choose to not use racket racket/fixnum. Use of generic
;;; numeric ops is disadvantage for racket but there is no safe
;;; version of fixnum operations that avoids the overhead of
;;; contracts, and we are only interested in comparing safe code.  The
;;; racket/fixnum safe operations are generally no faster than using
;;; generic primitives like +. (According to the documentation)


(: create-x : Integer -> (Vectorof Integer))
(define (create-x n)
  (define result : (Vectorof Integer) (make-vector n))
  (do : (Vectorof Integer)
    ((i : Integer 0 (+ i 1)))
    ((>= i n) result)
    (vector-set! result i i)))

(: create-y : (Vectorof Integer) -> (Vectorof Integer))
(define (create-y x)
  (let* ((n : Integer (vector-length x))
         (result : (Vectorof Integer) (make-vector n)))
    (do : (Vectorof Integer)
      ((i : Integer (- n 1) (- i 1)))
      ((< i 0) result)
      (vector-set! result i (vector-ref x i)))))

(: my-try : Integer -> Integer)
(define (my-try n)
  (vector-length (create-y (create-x n))))

(: go : Integer Integer Integer -> Integer)
(define (go m n r)
  (if (> m 0)
      (go (- m 1) n (my-try n))
      r))

(define (main)
  (let* ((input1 : Any (read))
         (input2 : Any (read)))
    (unless (and (fixnum? input1) (fixnum? input2))
      (error 'array.rkt "invalid-input: expected 2 fixnums"))
    (pretty-print (go input1 input2 0))))

(time (main))
