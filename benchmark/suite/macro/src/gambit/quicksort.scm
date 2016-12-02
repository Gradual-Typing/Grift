#!/u/dalmahal/bin/bin/gsi-script -:d0

(declare
 (fixnum)
 (standard-bindings))

(define (readlines p sep)
  (let loop ((line (read-line p sep))
	     (result '()))
    (if (eof-object? line)
	(let ([a (reverse (cdr result))])
	  (cons (car a) (list->vector (cdr a))))
	(loop (read-line p sep) (cons (string->number line) result)))))

(define (main)
  (let* ([a (readlines (current-input-port) #\ )]
	 [size (car a)]
	 [arr (cdr a)])
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
	     [init (lambda (n a) (if (= n 0) a (begin (vector-set! a (- 10000 n) n) (init (- n 1) a))))])
      (begin
	(sort arr 0 (- size 1))
	(pretty-print (vector-ref arr (- size 1)))))))
