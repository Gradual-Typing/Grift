#!/u/dalmahal/bin/bin/gsi-script -:d0

(declare
 (gambit-scheme)
 (standard-bindings)
 (extended-bindings)
 (run-time-bindings)
 (not inline)
 (not constant-fold)
 (not lambda-lift)
 (not proper-tail-calls)
 (generative-lambda))

(define (main arg)
  (letrec ([add1 (lambda (x) (+ x 1))])
    (let ([iters (string->number arg)])
      (begin
	(time
	 (let loop ([i iters])
	   (cond
	    [(zero? i) (pretty-print iters)]
	    [else (add1 i) (loop (- i 1))]))
	 (current-output-port))))))
