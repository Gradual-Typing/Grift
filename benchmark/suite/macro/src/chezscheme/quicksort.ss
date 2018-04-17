#! /usr/bin/env scheme-script
(import (chezscheme))

(define (quicksort a p r)
  (if (fx< p r)
      (let ([q (sort_partition a p r)])
	(begin
	  (quicksort a p (fx- q 1))
	  (quicksort a (fx+ q 1) r)))
      0))

(define (sort_partition a p r)
  (let ([i (box (fx- p 1))]
	[x (vector-ref a r)])
    (begin
      (let loop ([j p])
	(if (< j r)
	    (begin
	      (if (<= (vector-ref a j) x)
		  (begin
		    (set-box! i (fx+ (unbox i) 1))
		    (swap a (unbox i) j))
		  0)
	      (loop (fx+ j 1)))
            0))
      (swap a (fx+ (unbox i) 1) r)
      (fx+ (unbox i) 1))))

(define (swap a i j)
  (if (fx= i j)
      0
      (let ([t (vector-ref a i)])
	(begin
	  (vector-set! a i (vector-ref a j))
	  (vector-set! a j t)
	  0))))

(define (run-benchmark)
  (let ([size (read)])
    (let ([a (make-vector size 1)])
      (begin
        (let loop ([i 0])
          (if (fx< i size)
              (begin
                (vector-set! a i (read))
                (loop (fx+ i 1)))
              0))
        (quicksort a 0 (fx- size 1))
        (display (vector-ref a (fx- size 1)))
        (newline)))))

(time (run-benchmark))
  
