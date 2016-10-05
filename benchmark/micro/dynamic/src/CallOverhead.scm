#!gsi-script

(define (main arg)
  (letrec ([id (lambda (x) x)]
	   [const (lambda (x) -1)])
    (let* ([iters (string->number arg)]
           [id-ref (box (if (> iters 0) id const))]
           [id-dynamic (unbox id-ref)])
      (letrec ([run-test (lambda (i acc) acc)])
        (time
         (let loop ([i iters] [acc 42])
           (cond
            [(zero? i) acc]
            [else (loop (- i 1) (run-test i acc))]))
         (current-output-port))))))

