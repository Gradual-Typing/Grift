#!gsi-script -:d0

(define (main arg)
  (letrec ([f (lambda (x) x)]
	   [g (lambda (x) 42)])
    (let ([iters (string->number arg)]
	  [acc (box 0)])
      (let ([r (box (if (> iters 0) f g))])
	(let ([id (unbox r)])
	  (letrec ([run-test (lambda (i acc) (id acc))])
	    (time
	     (let loop ([i iters])
	       (cond
		[(zero? i) (unbox acc)]
		[else (set-box! acc (run-test i (unbox acc))) (loop (- i 1))]))
	     (current-output-port))))))))

