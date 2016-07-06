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
  (let ([iters (string->number arg)])
    (time
     (let loop ([i iters])
       (cond
	[(zero? i) i]
	[else (loop (- i 1))]))
     (current-output-port))))
