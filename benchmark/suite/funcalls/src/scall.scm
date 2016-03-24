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
  (letrec ([f (lambda (x) (if (= x 0) 0 (f (- x 1))))])
    (let ([iters (string->number arg)])
      (time
       (f iters)
       (current-output-port)))))
