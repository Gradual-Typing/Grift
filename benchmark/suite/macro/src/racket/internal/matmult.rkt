#lang racket/base

(require racket/fixnum)

(define (create l1 l2)
  (let ([x (make-vector (fx* l1 l2) 0)])
    (let loop1 ([i 0])
      (if (fx< i l1)
	  (let loop2 ([j 0])
	    (begin
	      (if (fx< j l2)
		  (begin
		    (vector-set! x (fx+ (fx* l2 i) j) (fx+ j i))
		    (loop2 (fx+ 1 j)))
		  (loop1 (fx+ 1 i)))))
	  x))))

(define (mult x x1 x2 y y1 y2)
  (let ([r (make-vector (fx* y2 x1) 0)])
    (let loop1 ([i 0])
      (if (fx< i x1)
	  (let loop2 ([j 0])
	    (if (fx< j y2)
		(let loop3 ([k 0])
		  (if (fx< k y1)
		      (begin
			(vector-set! r (fx+ (fx* i y2) j)
				     (fx+ (vector-ref r (fx+ (fx* i y2) j))
					  (fx* (vector-ref x (fx+ (fx* i x2) k))
					       (vector-ref y (fx+ (fx* k y2) j)))))
			(loop3 (fx+ k 1)))
		      (loop2 (fx+ j 1))))
		(loop1 (fx+ i 1))))
	  r))))

(define (main)
  (let ([size (read)])
    (let ([ar size]
          [ac size]
          [br size]
          [bc size])
      (let ([a (create ar ac)]
            [b (create br bc)])
        (display
         (vector-ref (mult a ar ac b br bc) (fx- (fx* ar bc) 1)))))))

(time (main))

