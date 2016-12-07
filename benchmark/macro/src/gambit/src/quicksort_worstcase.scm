#!/u/dalmahal/bin/bin/gsi-script -:d0

(declare
 (fixnum)
 (standard-bindings))

(define (sort a p r)
  (if (< p r)
      (let ([q (partition a p r)])
	(begin
	  (sort a p (- q 1))
	  (sort a (+ q 1) r)))
      0))

(define (partition a p r)
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
      (+ (unbox i) 1))))

(define (swap a i j)
  (if (= i j)
      0
      (let ([t (vector-ref a i)])
	(begin
	  (vector-set! a i (vector-ref a j))
	  (vector-set! a j t)
	  0))))

(define (vector-init a i n)
  (if (< i n)
      (begin
	(vector-set! a i (read))
	(vector-init a (+ i 1) n))
      a))

(define (main)
  (let* ([size (read)]
	 [v (vector-init (make-vector size) 0 size)])
    (sort v 0 (- size 1))
    (display (vector-ref v (- size 1)))))
