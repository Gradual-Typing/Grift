#lang typed/racket/base
;;; 10/9/2017 changed to use internal timing (Andre)
;;;
;;; We actively choose to not use racket racket/fixnum. Use of generic
;;; numeric ops is disadvantage for racket but there is no safe
;;; version of fixnum operations that avoids the overhead of
;;; contracts, and we are only interested in comparing safe code.  The
;;; racket/fixnum safe operations are generally no faster than using
;;; generic primitives like +. (According to the documentation)

(: sort : (Vectorof Integer) Integer Integer -> Integer)
(define (sort a p r)
  (if (< p r)
      (let ([q (partition a p r)])
        (begin
          (sort a p (- q 1))
          (sort a (+ q 1) r)))
      0))

(: partition : (Vectorof Integer) Integer Integer -> Integer)
(define (partition a p r)
  ;; Why does this box exist?
  (let ([i : (Boxof Integer) (box (- p 1))]
        [x (vector-ref a r)])
    (begin
      (let loop : Integer ([j : Integer p])
       (if (< j r)
           (begin
             (if (<= (vector-ref a j) x)
                 (begin
                   (set-box! i (+ (unbox i) 1))
                   (swap a (unbox i) j))
                 0)
             (loop (+ j 1)))
           0))
      (swap a (+ (unbox i) 1) r)
      (+ (unbox i) 1))))

(: swap : (Vectorof Integer) Integer Integer -> Integer)
(define (swap a i j)
  (if (= i j)
      0
      (let ([t (vector-ref a i)])
        (begin
          (vector-set! a i (vector-ref a j))
          (vector-set! a j t)
          0))))

(define (main)
  (let ([size (read)])
    (unless (fixnum? size)
      (error 'quicksort "invalid input: expected fixnum"))
    (let ([a : (Vectorof Integer) (make-vector size 1)])
      (let loop ([i : Integer 0])
        (when (< i size)
          (let ([e (read)])
            (unless (fixnum? e)
              (error 'quicksort.rkt "invalid input: expected integer"))
            (vector-set! a i e)
            (loop (+ i 1)))))
      (sort a 0 (- size 1))
      (display (vector-ref a (- size 1))))))

(time (main))
