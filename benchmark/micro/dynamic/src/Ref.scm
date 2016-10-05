#!gsi-script

(define (main arg)
  (let ([iters (string->number arg)]
	[ref (box 0)])
    (letrec ([run-test
              (lambda (i acc)
                (begin
                  (set-box! ref acc)
                  (unbox ref)))])
      (time
         (let loop ([i iters] [acc 42])
           (cond
            [(zero? i) acc]
            [else (loop (- i 1) (run-test i acc))]))
         (current-output-port)))))
