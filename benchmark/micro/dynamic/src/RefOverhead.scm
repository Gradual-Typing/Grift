#!/u/dalmahal/bin/bin/gsi-script -:d0


(define (main arg)
  (let ([iters (string->number arg)]
	[acc (box 0)]
	[ref (box 0)])
    (letrec ([run-test (lambda (i acc) i)])
      (begin
	(time
	 (let loop ([i iters])
	   (cond
	    [(zero? i) (unbox acc)]
	    [else (set-box! acc (run-test i (unbox acc))) (loop (- i 1))]))
	 (current-output-port))))))
