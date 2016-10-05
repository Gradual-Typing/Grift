#!/usr/bin/gsi-script -:d0

(declare
  (standard-bindings)
  (fixnum))

(define (main arg)
  (let ([size (string->number arg)])
    (let ([a (make-vector size)])
      (letrec ([sort (lambda (a p r)
		       (if (< p r)
			   (let ([q (partition a p r)])
			     (begin
			       (sort a p (- q 1))
			       (sort a (+ q 1) r)))
			   0))]
	       [partition (lambda (a p r)
			    (let ([i (box (- p 1))]
				  [x (vector-ref a r)])
			      (begin
				(let loop ([j p])
				  (if (< j r)
				      (begin
					(if (<= (vector-ref a j) x)
					    (begin
					      (set-box! i (+ (unbox i) 1))
					      (swap a (unbox i) j))
					    0)
					(loop (+ j 1)))))
				(swap a (+ (unbox i) 1) r)
				(+ (unbox i) 1))))]
	       
	       [swap (lambda (a i j)
		       (if (= i j)
			   0
			   (let ([t (vector-ref a i)])
			     (begin
			       (vector-set! a i (vector-ref a j))
			       (vector-set! a j t)
			       0))))]
	       [init (lambda (n a) (if (= n 0) a (begin (vector-set! a (- size n) n) (init (- n 1) a))))])
	(begin
	  (init size a)
	  (time (sort a 0 (- size 1)) (current-output-port))
	  (pretty-print (vector-ref a (- size 1))))))))
