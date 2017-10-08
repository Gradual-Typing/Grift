#lang typed/racket/base

(require racket/fixnum racket/pretty)

;;; ARRAY1 -- One of the Kernighan and Van Wyk benchmarks.
;;; 9/27/2017 added types to support typed-racket by Andre Kuhlenschmidt

(: create-x : Fixnum -> (Vectorof Fixnum))
(define (create-x n)
  (define result : (Vectorof Fixnum) (make-vector n))
  (do : (Vectorof Fixnum)
    ((i : Fixnum 0 (fx+ i 1)))
    ((fx>= i n) result)
    (vector-set! result i i)))

(: create-y : (Vectorof Fixnum) -> (Vectorof Fixnum))
(define (create-y x)
  (let* ((n : Fixnum (vector-length x))
         (result : (Vectorof Fixnum) (make-vector n)))
    (do : (Vectorof Fixnum)
      ((i : Fixnum (fx- n 1) (fx- i 1)))
      ((fx< i 0) result)
      (vector-set! result i (vector-ref x i)))))

(: my-try : Fixnum -> Fixnum)
(define (my-try n)
  (vector-length (create-y (create-x n))))

(: go : Fixnum Fixnum Fixnum -> Fixnum)
(define (go m n r)
  (if (fx> m 0)
      (go (fx- m 1) n (my-try n))
      r))

(let* ((input1 : Any (read))
       (input2 : Any (read)))
  (unless (and (fixnum? input1) (fixnum? input2))
    (error 'array.rkt "invalid-input: expected 2 fixnums"))
  (pretty-print (go input1 input2 0)))
