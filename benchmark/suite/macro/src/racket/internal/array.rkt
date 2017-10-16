#lang racket/base

(require racket/fixnum racket/pretty)

;;; ARRAY1 -- One of the Kernighan and Van Wyk benchmarks.

(define (create-x n)
  (define result (make-vector n))
  (do ((i 0 (fx+ i 1)))
      ((fx>= i n) result)
    (vector-set! result i i)))

(define (create-y x)
  (let* ((n (vector-length x))
         (result (make-vector n)))
    (do ((i (fx- n 1) (fx- i 1)))
        ((fx< i 0) result)
      (vector-set! result i (vector-ref x i)))))

(define (my-try n)
  (vector-length (create-y (create-x n))))

(define (go m n r)
  (if (fx> m 0)
      (go (fx- m 1) n (my-try n))
      r))

(define (main)
  (let* ((input1 (read))
         (input2 (read)))
    (pretty-print (go input1 input2 0))))

(time (main))
