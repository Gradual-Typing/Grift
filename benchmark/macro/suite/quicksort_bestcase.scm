#!/u/dalmahal/bin/bin/gsi-script -:d0

(define (readlines filename)
  (call-with-input-file filename
    (lambda (p)
      (let loop ((line (read-line p))
                 (result '()))
        (if (eof-object? line)
            (list->s16vector (reverse (map string->number result)))
            (loop (read-line p) (cons line result)))))))

(let ([a (readlines "nums")])
  (letrec ([sort (lambda (a p r)
		   (if (< p r)
		       (let ([q (partition a p r)])
			 (begin
			   (sort a p (- q 1))
			   (sort a (+ q 1) r)))
		       0))]
	   [partition (lambda (a p r)
			(let ([i (box (- p 1))]
			      [x (s16vector-ref a r)])
			  (begin
			    (let loop ([j p])
			      (if (< j r)
				  (begin
				    (if (<= (s16vector-ref a j) x)
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
		       (let ([t (s16vector-ref a i)])
			 (begin
			   (s16vector-set! a i (s16vector-ref a j))
			   (s16vector-set! a j t)
			   0))))]
	   [init (lambda (n a) (if (= n 0) a (begin (s16vector-set! a (- 10000 n) n) (init (- n 1) a))))])
    (begin
      (time (sort a 0 9999) (current-output-port))
      (pretty-print (s16vector-ref a 9999)))))
